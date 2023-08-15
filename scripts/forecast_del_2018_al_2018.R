# Paquetes --- -------------------------------------------------------------
library(tidyverse)
library(forecast)
library(zoo)
library(timetk)
library(sweep)
library(readxl)

# funciones creadas para el trabajo --- -----------------------------------
source("scripts/funciones.R")

# Objetos y data --- -------------------------------------------------------

# detalles de los sectores (Al final esto)
detalles_sectores <- read_excel("data/detalles_sectores.xlsx")

pib_sectores <- get_pib_sectores(
  modalidad = "real",
  acumulado = FALSE,
  homogenea_91 = TRUE
) |>
  group_by(sector) %>% 
  mutate(
    crecimiento = (pib_2007 / dplyr::lag(pib_2007, 4)) - 1,
    crecimiento = crecimiento * 100,
    recesion = as.numeric(crecimiento <= 0)
  ) %>% 
  ungroup()

pib_sectores <- pib_sectores %>%
  left_join(detalles_sectores)

pib_sectores_sample <- pib_sectores |>
  filter(year <= 2017)

pib_sectores_outsample <- pib_sectores |>
  filter(fecha > "2017-09-01", year < 2020)

# Pesos por sector
pesos_w <- tribble(
  ~sector, ~pesos,
  "Agropecuario", 5.2985682 / 100,
  "Industrias", 28.46100381743 / 100,
  "Servicios", 58.8893220381438 / 100,
  "Impuestos a la producción netos de subsidios", 7.35110592000 / 100
)

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

# Resultados --------------------------------------------------------------
fanplots_sectores <- pmap(
  list(
    serie = by_sectores[["ts"]] %>% map(~tail(.x, 52)),
    fcast_values = by_sectores[["fcast"]] %>% map("mean"),
    step_error = by_sectores[["errores_rmse"]] %>% map("Errores agregados")
  ),
  
  fanplot_density
) |>
  setNames(by_sectores$sector)

# Agregacion de los errores sectoriales
errores_rmse_sectores <- by_sectores$errores_rmse %>%
  map("Hitorico errores") |>
  set_names(by_sectores$sector)

errores_me_sectores <- by_sectores$errores_me %>%
  map("Hitorico errores")  |>
  set_names(by_sectores$sector)

errores_agregados <- agrega_errores(
  list_errores_me = errores_me_sectores[1:4],
  list_errores_rmse = errores_rmse_sectores[1:4],
  pesos = pesos_w$pesos
) %>% 
  `[[` ("Error agregado") %>% 
  as_tibble() %>%
  setNames("error") %>% 
  rowid_to_column(var = "horizonte")

# Agregación de pronostico sectorial
mean_agregado <- by_sectores[1:4,][["fcast"]] %>%
  set_names(by_sectores[1:4,][["sector"]]) %>% 
  map("mean") %>% 
  map(~tibble(mean = .x) %>% rowid_to_column(var = "horizonte")) %>% 
  bind_rows(.id = "sector") %>% 
  left_join(pesos_w, by = "sector") %>% 
  group_by(horizonte) %>% 
  summarise( pib = sum(mean * pesos)) %>% 
  left_join(errores_agregados, by = "horizonte")

# Visualizaciones ---------------------------------------------------------

fanplot_with_series <- function(fanplot_list, sector_to_plot) {
  fanplot_list[[sector_to_plot]] +
    geom_line(
      data = filter(pib_sectores_outsample, sector == sector_to_plot),
      aes(x = fecha, y = crecimiento),
      color = "red"
    ) +
    scale_linetype(labels = c("Mediana", "50%", '90%')) +
    theme_minimal() +
    scale_fill_gradient(low = "gray19", high = "gray87") +
    scale_x_date(breaks = scales::pretty_breaks(6)) +
    labs(
      title = sector_to_plot,
      x = NULL,
      y = "Crecimiento Interanual",
      fill = "",
      linetype = "Intervalos"
    ) +
    theme(legend.position = "bottom")
}

(fanplot_agropecuario <- fanplot_with_series(fanplots_sectores, "Agropecuario"))
(fanplot_industrias <- fanplot_with_series(fanplots_sectores, "Industrias"))
(fanplot_servicios <- fanplot_with_series(fanplots_sectores, "Servicios"))
(fanplot_impuestos <- fanplot_with_series(
  fanplots_sectores,
  "Impuestos a la producción netos de subsidios")
)
(fanplot_pib <- fanplot_with_series(fanplots_sectores, "Producto Interno Bruto"))


(fanplot_agregacion <- fanplot_density(
  serie = by_sectores[5,][["ts"]][[1]],
  fcast_values = mean_agregado$pib,
  step_error = mean_agregado$error
) + 
  geom_line(
    data = filter(pib_sectores_outsample, str_detect(sector, "^Producto")),
    aes(x = fecha, y = crecimiento),
    color = "red"
  ) +
  scale_linetype(labels = c("Mediana", "50%", '90%')) +
  theme_minimal() +
  scale_fill_gradient(low = "gray19", high = "gray87") +
  labs(
    title = "PIB",
    x = NULL,
    y = "Crecimiento Interanual",
    fill = "",
    linetype = "Intervalos"
  ) +
  theme(legend.position = "bottom")
)

