
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

# Modeling --- -------------------------------------------------------------

# Objeto anidado con data sectorial 
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

# Fanplot de los sectores -------------------------------------------------

# Lista con los parámetros simulaciones para los Fanplot de los sectores
fanplots_sectores <- pmap(
  list(
    # Serie original
    serie = by_sectores[["ts"]] %>% map(~tail(.x, 52)),
    # Senda central del forecast de cada sector
    fcast_values = by_sectores[["fcast"]] %>% map("mean"),
    # Error promedio de a cada paso
    step_error = by_sectores[["errores_rmse"]] %>% map("Errores agregados")
  ),
  
  fanplot_density
)

# Agregación del forecast de los sectores ----

# Pesos por sector
pesos_w <- tribble(
  ~sector, ~pesos,
  "Agropecuario", 5.2985682 / 100,
  "Industrias", 28.46100381743 / 100,
  "Servicios", 58.8893220381438 / 100,
  "Impuestos a la producción netos de subsidios", 7.35110592000 / 100)

# agregacion de los errores sectoriales
errores_rmse_sectores <- by_sectores$errores_rmse %>% map("Hitorico errores")
errores_me_sectores <- by_sectores$errores_me %>% map("Hitorico errores")

errores_agregados <- agrega_errores(
  list_errores_me = errores_me_sectores[1:4],
  list_errores_rmse = errores_rmse_sectores[1:4],
  pesos = pesos_w$pesos
) %>% 
  `[[` ("Error agregado") %>% 
  as_tibble() %>% setNames("error") %>% 
  rowid_to_column(var = "horizonte")

# agregación de pronostico sectorial
suppressWarnings(
  mean_agregado <- by_sectores[1:4,][["fcast"]] %>% 
    set_names(by_sectores[1:4,][["sector"]]) %>% 
    map("mean") %>% 
    map(~tibble(mean = .x) %>% 
          rowid_to_column(var = "horizonte")) %>% 
    bind_rows(.id = "sector") %>% 
    left_join(pesos_w) %>% 
    group_by(horizonte) %>% 
    summarise( pib = sum(mean * pesos)) %>% 
    left_join(errores_agregados)
)

# Modelo del PIB con variable dummy para recesión ----
pib_dummy_ts <- pib_sectores %>%
  dplyr::filter(agregacion == "pib") %>% 
  select(fecha, crecimiento, recesion) %>% 
  filter(!is.na(crecimiento)) |> 
  select(crecimiento, recesion) %>% 
  ts(start = c(1992, 1), frequency = 4)

modelo_dummy <- pib_dummy_ts[,"crecimiento"] %>% 
  auto.arima(xreg = pib_dummy_ts[, "recesion"])

fcast_dummy <- modelo_dummy %>% 
  forecast(h = 8, xreg = c(1, 1, 1, 1, 0.65, 0, 0, 0))

# Summary
data_fcast_dummy <- fcast_dummy %>%
  sweep::sw_sweep() %>% 
  rename(crecimiento = value) %>% 
  mutate(
    lo.80 = ifelse(is.na(lo.80), crecimiento, lo.80),
    lo.95 = ifelse(is.na(lo.95), crecimiento, lo.95),
    hi.80 = ifelse(is.na(hi.80), crecimiento, hi.80),
    hi.95 = ifelse(is.na(hi.95), crecimiento, hi.95)
  ) 

# Comparación modelo general y el por agregación --------------------------

# Distribuciones del modelo geneal 

distribucion_general <-  purrr::map2(
  by_sectores[["fcast"]][[5]]$mean %>% as.numeric,
  by_sectores[["errores_rmse"]][[5]]$`Errores agregados` %>% as.numeric(),
  ~rnorm(
    n = 500,
    mean = .x,
    sd = .y
  ) %>% 
    as_tibble() %>% 
    setNames("crecimiento")
) %>% 
  dplyr::bind_rows(.id = "horizonte") %>%
  mutate(horizonte = parse_number(horizonte),
         origen = "Serie orifinal")


distribucion_agregado <- purrr::map2(
  mean_agregado$pib,
  mean_agregado$error,
  ~rnorm(n = 500, mean = .x, sd = .y) %>% 
    as_tibble() %>% setNames("crecimiento")
) %>% 
  dplyr::bind_rows(.id = "horizonte") %>% 
  mutate(horizonte = parse_number(horizonte),
         origen = "Por agregación")

(comparacion_densidad_pronostico <-  bind_rows(
  distribucion_agregado,
  distribucion_general
) %>% 
  ggplot(aes(x = crecimiento, fill = origen, y = factor(horizonte, levels = 8:1))) +
  ggridges::geom_density_ridges(alpha = 0.6) +
  scale_x_continuous(breaks = scales::pretty_breaks(6)) +
  scale_fill_manual(values = c("gray61", "gray22")) +
  labs(x = "Crecimiento", y = "Horizonte de pronóstico", fill = "Metodología") +
  theme_plex() +
  theme(legend.position = "bottom"))


# Comparación de los errores históricos de los modelos
by_sectores[["errores_rmse"]][[5]]$`Hitorico errores` %>% 
  as_tibble() %>% 
  pivot_longer(cols = h1:h8, names_to = "horizonte", values_to = "rmse") %>% 
  ggplot(aes(x = rmse, y = factor(horizonte, levels = paste0("h", 8:1)))) +
  ggridges::geom_density_ridges() +
  theme_minimal() +
  labs(y = "Horizonte de pornóstico",
       x = "Raíz del error cuadrático")

# Visualizaciones --- ------------------------------------------------------

# Gráficos del pib por sectores

(p_pib_sectores <-  pib_sectores %>% 
  dplyr::filter(agregacion %in% c("sector", "pib"#, "impuestos"
                                  )) %>% 
  mutate(sector = fct_inorder(sector)) %>% 
  ggplot(aes(x = fecha, y = crecimiento)) +
  geom_line(size = 1) +
  facet_wrap(~sector, scales = "free_x") +
  theme_plex() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.line.x = element_line(color = "black")
  ) +
  labs(
    x = NULL,
    y = "Crecimiento interanual"
  ))

# Fanplots de los sectores -----------------

# Famplot Sector Agropecuario
(fanplot_agropecuario <- fanplots_sectores[[1]] +
     geom_line(
         data = filter(pib_sectores_outsample, sector == "Agropecuario"),
         aes(x = fecha, y = crecimiento),
         color = "red"
         ) +
     scale_linetype(labels = c("Mediana", "50%", '90%')) +
     theme_minimal() +
     scale_fill_gradient(low = "gray19", high = "gray87") +
     labs(
       title = "Agropecuario",
       x = NULL,
       y = "Crecimiento Interanual",
       fill = "",
       linetype = "Intervalos"
     ) +
     theme(legend.position = "bottom")
 )

# Fanplot Industrias
(fanplot_industrias <- fanplots_sectores[[2]] +
        geom_line(
            data = filter(pib_sectores_outsample, sector == "Industrias"),
            aes(x = fecha, y = crecimiento),
            color = "red"
        ) +
  scale_linetype(labels = c("Mediana", "50%", '90%')) +
  theme_minimal() +
  scale_fill_gradient(low = "gray19", high = "gray87") +
  labs(
    title = "Industrias",
    x = NULL,
    y = "Crecimiento Interanual",
    fill = "",
    linetype = "Intervalos"
  ) +
  theme(
    legend.position = "bottom"
  ))

# Fanplot Servicio
(fanplot_servicios <- fanplots_sectores[[3]] +
  scale_linetype(labels = c("Mediana", "50%", '90%')) +
        geom_line(
            data = filter(pib_sectores_outsample, sector == "Servicios"),
            aes(x = fecha, y = crecimiento),
            color = "red"
        ) +
  theme_minimal() +
  scale_fill_gradient(low = "gray19", high = "gray87") +
  labs(
    title = "Servicios",
    x = NULL,
    y = "Crecimiento Interanual",
    fill = "",
    linetype = "Intervalos"
  ) +
  theme(
    legend.position = "bottom"
  ))

# Fanplot PIB
(fanplot_impuestos <- fanplots_sectores[[4]] +
        geom_line(
            data = filter(pib_sectores_outsample, str_detect(sector, "^Impuestos")),
            aes(x = fecha, y = crecimiento),
            color = "red"
        ) +
  scale_linetype(labels = c("Mediana", "50%", '90%')) +
  theme_minimal() +
  scale_fill_gradient(low = "gray19", high = "gray87") +
  labs(
    title = "Impuestos a la Producción",
    x = NULL,
    y = "Crecimiento Interanual",
    fill = "",
    linetype = "Intervalos"
  ) +
  theme(
    legend.position = "bottom"
  ))

# Grid de fanplots sectores
(grid_fanplot_sectores <- (
    fanplot_agropecuario +
    fanplot_industrias +
    fanplot_servicios + 
    fanplot_impuestos
    ) +
    plot_layout(guides = "collect") & theme(legend.position = 'bottom'))


# Gráfcio del forecast de los sectores  (No se incluye en el trabajo)
(p_forecast_sectores <-
    by_sectores %>%
        dplyr::filter(str_detect(sector, "Producto", negate = TRUE)) %>% 
        unnest(sweep) %>% 
        ungroup() %>% 
        mutate(sector = fct_inorder(sector)) %>% 
        dplyr::filter(fecha > 2006.75) %>% 
        ggplot(aes(x = fecha, y = crecimiento, linetype = key)) +
        facet_wrap(~sector, scales = "free") +
        theme_plex() +
        geom_ribbon(aes(x = fecha, ymax = hi.95, ymin = lo.95),
        alpha = 0.5) +
        geom_ribbon(aes(x = fecha, ymax = hi.80, ymin = lo.80), alpha = 0.5) +
        geom_line(size = 1) +
        theme(
            strip.text = element_text(face = "bold"),
            axis.title = element_text(face = "bold"),
            axis.line.x = element_line(color = "black"),
            legend.position = "none")
    )

# Análisis de los residuos de los modelos individuales -----

residual_plot <- by_sectores[["model"]] %>% 
  map("residuals") %>% 
  setNames(by_sectores[["sector"]]) %>% 
  map2( .y = by_sectores[["sector"]], ~autoplot(.x) +
          labs(title = .y, x = NULL, y = "Residuo") +
          theme_minimal())

residual_line <- cowplot::plot_grid(plotlist = residual_plot[1:4])

# Distribucion de los residuos

residual_dist <- by_sectores[["model"]] %>% 
  map("residuals") %>% 
  setNames(by_sectores[["sector"]]) %>% 
  map2( .y = by_sectores[["sector"]],
        ~.x %>%
          as_tibble %>% 
          ggplot(aes(x = x)) +
          geom_histogram(bins = 10) +
          #geom_density() +
          labs(title = .y, y = "Frecuencia", x = "Residuo") +
          theme_minimal())

residual_hist <- cowplot::plot_grid(plotlist = residual_dist[1:4])

# Fanplot del pronóstico agregado ----

(fanplot_agregacion <- fanplot_density(
  serie = by_sectores[5,][["ts"]][[1]],
  fcast_values = mean_agregado$pib,
  step_error = mean_agregado$error) +
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
  theme(legend.position = "bottom"))

# Gráficos del modelo del PIB con Dummys

# Gráfico del forecast modelo con dummy
(plot_fcast_dummy <- data_fcast_dummy %>% 
  dplyr::filter(index > 2006.75) %>% 
  ggplot(aes(x = index)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95, fill = "95")) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = "80")) +
  geom_line(aes(x = index, y = crecimiento), size = 1) +
  scale_fill_manual(values =  c("gray70", high = "gray87")) +
  theme_minimal() +
  scale_x_yearmon(breaks = scales::pretty_breaks(9)) +
  labs(
    title = "Pronostico del Crecimiento del PIB",
    subtitle = "Escenario con dummy de reseción de 4 trimestres",
    x = NULL,
    y = "Crecimiento Interanual",
    fill = "Intervalos",
    linetype = "Intervalos"
  ) +
  theme(legend.position = "bottom"))


# Fanplot modelo con dummy de recesión -----
errores_dummy_model <- error_historico_d1(
  x = pib_dummy_ts[,"crecimiento"],
  xreg_d =  pib_dummy_ts[, "recesion"],
  medida_error = "RMSE",
  n.valid = 8
)

(fanplot_dummy <- fanplot_density(pib_dummy_ts[,"crecimiento"], 
                                 fcast_values = fcast_dummy$mean, 
                                 simulation_size = 500,
                                 step_error = errores_dummy_model$`Errores agregados`) +
  scale_linetype(labels = c("Mediana", "50%", '90%')) +
  theme_minimal() +
  scale_fill_gradient(low = "gray19", high = "gray87") +
  scale_x_date(breaks = scales::pretty_breaks(10)) +
  labs(
    title = NULL,
    x = NULL,
    y = "Crecimiento Interanual",
    fill = "",
    linetype = "Intervalos"
  ) +
  theme(legend.position = "bottom"))

# Guardar visualizaciones --- ----------------------------------------------

# Fanplot sector agropecuario
ggsave(
  "graficos/fanplot_agropecuario.png",
  fanplot_agropecuario,
  height = 5,
  width = 8,
  dpi = 500,
)

# Fanplot Servicios
ggsave(
  "graficos/fanplot_servicios.png",
  fanplot_servicios,
  height = 5,
  width = 8,
  dpi = 500,
)

# Fanplot industrias
ggsave(
  "graficos/fanplot_industrias.png",
  fanplot_industrias,
  height = 5,
  width = 8,
  dpi = 500,
)

# Fanplot impuestos
ggsave(
  "graficos/fanplot_impuestos.png",
  fanplot_impuestos,
  height = 5,
  width = 8,
  dpi = 500
)

# Fanplot grid sectores
ggsave(
  "graficos/grid_fanplot_sectores.png",
  grid_fanplot_sectores,
  height = 5,
  width = 8,
  dpi = 500
)

# Gráficos de los residuos de los modelos
# Linea de los residuos
ggsave(
  "graficos/residual_sector_line.png",
  residual_line,
  height = 5,
  width = 8,
  dpi = 500
)

# Histograma de los residuos
ggsave(
  "graficos/residual_sector_hist.png",
  residual_hist,
  height = 5,
  width = 8,
  dpi = 500
)

# Forecast del modelo con dummy
ggsave(
  "graficos/forecast_pib_dummy.PNG",
  plot_fcast_dummy + labs(title = NULL, subtitle = NULL),
  height = 5,
  width = 8,
  dpi = 800
)

# Forecast del modelo con variable categorica
# ggsave(
#   "graficos/forecast_pib_categorico.PNG",
#   plot_fcast_categorica,
#   height = 5,
#   width = 8,
#   dpi = 800
# )

# Fanplot del pib como resultado de la agregación de pronóstico
ggsave(
  "graficos/fanplot_agregacion.PNG",
  fanplot_agregacion,
  height = 5,
  width = 8,
  dpi = 800
)


ggsave(
  "graficos/comparacion_densidad_pronostico.PNG",
  comparacion_densidad_pronostico,
  height = 5,
  width = 8,
  dpi = 800
)

# Forecast using a xrg model
ggsave(
  "graficos/fanplot_dummy.PNG",
  fanplot_dummy,
  height = 5,
  width = 8,
  dpi = 800
)

