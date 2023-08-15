# Paquetes --- -------------------------------------------------------------
library(tidyverse)
library(forecast)
library(zoo)
library(timetk)
library(sweep)
library(readxl)
library(patchwork)

# funciones creadas para el trabajo --- -----------------------------------
source("scripts/funciones.R")

# Data --- ----------------------------------------------------------------
detalles_sectores <- read_excel("data/detalles_sectores.xlsx")

pib_sectores <- get_pib_sectores(
    modalidad = "real",
    acumulado = FALSE,
    homogenea_91 = TRUE
) |>
    filter(year <= 2018)

pib_sectores <- get_pib_sectores(
    modalidad = "real",
    acumulado = FALSE,
    homogenea_91 = TRUE
) |>
    group_by(sector) %>% 
    mutate(
        crecimiento = (pib_2007 / dplyr::lag(pib_2007, 4)) - 1,
        crecimiento = crecimiento * 100
    ) %>% 
    ungroup()

# Agrega detalles de los sectores
pib_sectores <- pib_sectores %>%
    left_join(detalles_sectores)

pib_sectores_sample <- pib_sectores |>
    filter(year <= 2018)

pib_sectores_outsample <- pib_sectores_outsample |>
    filter(year >= 2018 & year <= 2020)

# Modeling ----------------------------------------------------------------
by_sectores <- pib_sectores_sample %>% 
    dplyr::filter(agregacion %in% c("sector", "pib", "impuestos")) %>% 
    select(fecha, year, trimestre, sector, pib_2007, crecimiento) %>% 
    group_by(sector) %>% 
    nest()

# Agregando elementos por sector
by_sectores <- by_sectores %>% 
    mutate(
        # Objeto ts ---- ---- ---- ---
        ts = map(
            data, 
            ~ .x %>%
                select(crecimiento) %>% 
                ts(start = c(1991, 1), frequency = 4)
        ),
        
        # auto arima --- --- --- --- --- 
        model = map(
            ts,
            auto.arima,
            seasonal = TRUE 
        ),
        
        # Forecast --- --- --- --- 
        fcast = map(
            model,
            forecast::forecast,
            h = 8
        ),
        
        # summary de los modelos ---
        sweep = map(
            fcast,
            sw_sweep,
            fitted = FALSE,
            timetk_index = TRUE,
            rename_index = "fecha"
        ),
        
        # rmse historico --- --- ---
        errores_rmse = map(
            ts,
            error_historico,
            n.valid = 8
        ),
        
        # error absoluto --- --- ---
        errores_me = map(
            ts,
            error_historico,
            n.valid = 8,
            medida_error = "ME"
        )
        
    )

# Fan plots ---------------------------------------------------------------

pesos_w <- tibble::tribble(
                                           ~sector,            ~pesos,
                                    "Agropecuario",       0.052985682,
                                      "Industrias",   0.2846100381743,
                                       "Servicios", 0.588893220381438,
    "Impuestos a la producción netos de subsidios",      0.0735110592
)

fanplots_sectores <- pmap(
    list(
        serie = by_sectores[["ts"]] %>% map(~tail(.x, 52)),
        fcast_values = by_sectores[["fcast"]] %>% map("mean"),
        step_error = by_sectores[["errores_rmse"]] %>% map("Errores agregados")
    ),
    
    fanplot_density
)

