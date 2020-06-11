library(tidyverse)
library(readxl)

# Descarga la data del PIB por sectores ---------------------------------------

get_pib_sectores <- function(modalidad = "real", acumulado = FALSE, homogenea_91 = FALSE) {
  `%>%` <- magrittr::`%>%`
  
  
  if(homogenea_91 == FALSE){
    # Url de descarga
    url <- paste0(
      "https://cdn.bancentral.gov.do/documents/",
      "estadisticas/sector-real/documents/",
      "pib_origen_2007.xlsx?v=1586996101374"
    ) } else {
      url <- paste0(
        "https://cdn.bancentral.gov.do/documents/",
        "estadisticas/sector-real/documents/",
        "pib_origen_retro.xlsx?v=1588471346723"
      )
    }
  
  
  # Ruta temporal del archivo
  path <- tempfile(pattern = "pib", fileext = ".xlsx")
  # descarga del archivo
  download.file(url, path, quiet = TRUE, mode = "wb")
  
  
  if(modalidad == "real" & homogenea_91 == FALSE & acumulado == FALSE) {
    # PIB real trimestral -----------------------------------------------------
    suppressMessages(
      pib_sectores <- readxl::read_excel(
        path = path,
        sheet = "PIBK_Trim",
        skip = 9,
        n_max = 31,
        col_names = FALSE)
    )
    
    suppressMessages(
      pib_incidencia <- readxl::read_excel(
        path = path,
        sheet = "PIBK_Trim",
        skip = 81,
        n_max = 31,
        col_names = FALSE)
    )
    
    
    pib_header <- c(
      "sector",
      seq(as.Date("2007-01-01"), by = "quarter",
          length.out = (ncol(pib_sectores) - 1)))
    
    pib_sectores <- pib_sectores %>%
      setNames(pib_header) %>%
      tidyr::pivot_longer(
        cols = -sector,
        names_to = "trimestre",
        values_to = "pib_2007"
      )
    
    pib_incidencia <- pib_incidencia %>%
      setNames(pib_header) %>%
      tidyr::pivot_longer(
        cols = -sector,
        names_to = "trimestre",
        values_to = "incidencia"
      )
    
    pib_sectores <-
      dplyr::left_join(pib_sectores, pib_incidencia) %>%
      dplyr::mutate(
        fecha = as.numeric(trimestre),
        fecha = lubridate::as_date(fecha),
        year = lubridate::year(fecha),
        trimestre = lubridate::quarter(fecha)
      ) %>%
      dplyr::select(fecha, year, trimestre, dplyr::everything())
    
    #return(pib_sectores)
    
  } else if (modalidad == "nominal" & homogenea_91 == FALSE & acumulado == FALSE) {
    # PIB nominal trimestral -----------------------------------------------------
    suppressMessages(
      pib_sectores <- readxl::read_excel(
        path = path,
        sheet = "PIB$_Trim",
        skip = 9,
        n_max = 31,
        col_names = FALSE)
    )
    
    suppressMessages(
      pib_pond <- readxl::read_excel(
        path = path,
        sheet = "PIB$_Trim",
        skip = 45,
        n_max = 31,
        col_names = FALSE)
    )
    
    pib_header <- c(
      "sector",
      seq(as.Date("2007-01-01"), by = "quarter",
          length.out = (ncol(pib_sectores) - 1)))
    
    pib_sectores <- pib_sectores %>%
      setNames(pib_header) %>%
      tidyr::pivot_longer(
        cols = -sector,
        names_to = "trimestre",
        values_to = "pib_2007"
      )
    
    pib_pond <- pib_pond %>%
      setNames(pib_header) %>%
      tidyr::pivot_longer(
        cols = -sector,
        names_to = "trimestre",
        values_to = "ponderacion"
      )
    
    pib_sectores <- dplyr::left_join(pib_sectores, pib_pond) %>%
      dplyr::mutate(
        fecha = as.numeric(trimestre),
        fecha = lubridate::as_date(fecha),
        year = lubridate::year(fecha),
        trimestre = lubridate::quarter(fecha)
      ) %>%
      dplyr::select(fecha, year, trimestre, dplyr::everything())
    
  } else if (modalidad == "nominal" & homogenea_91 == FALSE & acumulado == TRUE) {
    # PIB nominal acumulado ---------------------------------------------------
    suppressMessages(
      pib_sectores <- readxl::read_excel(
        path = path,
        sheet = "PIB$_Trim_Acum",
        skip = 9,
        n_max = 31,
        col_names = FALSE)
    )
    
    suppressMessages(
      pib_pond <- readxl::read_excel(
        path = path,
        sheet = "PIB$_Trim_Acum",
        skip = 45,
        n_max = 31,
        col_names = FALSE)
    )
    
    pib_header <- c(
      "sector",
      seq(as.Date("2007-01-01"), by = "quarter",
          length.out = (ncol(pib_sectores) - 1)))
    
    pib_sectores <- pib_sectores %>%
      setNames(pib_header) %>%
      tidyr::pivot_longer(
        cols = -sector,
        names_to = "trimestre",
        values_to = "pib_2007"
      )
    
    pib_pond <- pib_pond %>%
      setNames(pib_header) %>%
      tidyr::pivot_longer(
        cols = -sector,
        names_to = "trimestre",
        values_to = "ponderacion"
      )
    
    pib_sectores <- dplyr::left_join(pib_sectores, pib_pond) %>%
      dplyr::mutate(
        fecha = as.numeric(trimestre),
        fecha = lubridate::as_date(fecha),
        year = lubridate::year(fecha),
        trimestre = lubridate::quarter(fecha)
      ) %>%
      dplyr::select(fecha, year, trimestre, dplyr::everything())
    
    # return(pib_sectores)
  } else if(modalidad == "real" & homogenea_91 == FALSE & acumulado == TRUE) {
    # PIB real acumulado ------------------------------------------------------
    suppressMessages(
      pib_sectores <- readxl::read_excel(
        path = path,
        sheet = "PIBK_Trim_Acum",
        skip = 9,
        n_max = 31,
        col_names = FALSE)
    )
    
    suppressMessages(
      pib_incidencia <- readxl::read_excel(
        path = path,
        sheet = "PIBK_Trim_Acum",
        skip = 81,
        n_max = 31,
        col_names = FALSE)
    )
    
    
    pib_header <- c(
      "sector",
      seq(as.Date("2007-01-01"), by = "quarter",
          length.out = (ncol(pib_sectores) - 1)))
    
    pib_sectores <- pib_sectores %>%
      setNames(pib_header) %>%
      tidyr::pivot_longer(
        cols = -sector,
        names_to = "trimestre",
        values_to = "pib_2007"
      )
    
    pib_incidencia <- pib_incidencia %>%
      setNames(pib_header) %>%
      tidyr::pivot_longer(
        cols = -sector,
        names_to = "trimestre",
        values_to = "incidencia"
      )
    
    pib_sectores <-
      dplyr::left_join(pib_sectores, pib_incidencia) %>%
      dplyr::mutate(
        fecha = as.numeric(trimestre),
        fecha = lubridate::as_date(fecha),
        year = lubridate::year(fecha),
        trimestre = lubridate::quarter(fecha)
      ) %>%
      dplyr::select(fecha, year, trimestre, dplyr::everything())
    
    #return(pib_sectores)
    
  } else if(modalidad == "real" & homogenea_91 == TRUE & acumulado == FALSE) {
    # PIB real trimestral serie homogenea -------------------------------------
    suppressMessages(
      pib_sectores <- readxl::read_excel(
        path = path,
        sheet = "PIBK_Trim",
        skip = 9,
        n_max = 21,
        col_names = FALSE)
    )
    
    suppressMessages(
      pib_incidencia <- readxl::read_excel(
        path = path,
        sheet = "PIBK_Trim",
        skip = 61,
        n_max = 21,
        col_names = FALSE)
    )
    
    
    pib_header <- c(
      "sector",
      seq(as.Date("1991-01-01"), by = "quarter",
          length.out = (ncol(pib_sectores) - 1)))
    
    pib_sectores <- pib_sectores %>%
      setNames(pib_header) %>%
      tidyr::pivot_longer(
        cols = -sector,
        names_to = "trimestre",
        values_to = "pib_2007"
      )
    
    pib_incidencia <- pib_incidencia %>%
      setNames(pib_header) %>%
      tidyr::pivot_longer(
        cols = -sector,
        names_to = "trimestre",
        values_to = "incidencia"
      )
    
    pib_sectores <-
      dplyr::left_join(pib_sectores, pib_incidencia) %>%
      dplyr::mutate(
        fecha = as.numeric(trimestre),
        fecha = lubridate::as_date(fecha),
        year = lubridate::year(fecha),
        trimestre = lubridate::quarter(fecha)
      ) %>%
      dplyr::select(fecha, year, trimestre, dplyr::everything())
    
    #return(pib_sectores)
  } else if (modalidad == "nominal" & homogenea_91 == TRUE & acumulado == FALSE) {
    # PIB nominal trimestral serie homogenea ----------------------------------
    suppressMessages(
      pib_sectores <- readxl::read_excel(
        path = path,
        sheet = "PIB$_Trim",
        skip = 9,
        n_max = 21,
        col_names = FALSE)
    )
    
    suppressMessages(
      pib_pond <- readxl::read_excel(
        path = path,
        sheet = "PIB$_Trim",
        skip = 35,
        n_max = 21,
        col_names = FALSE)
    )
    
    pib_header <- c(
      "sector",
      seq(as.Date("1991-01-01"), by = "quarter",
          length.out = (ncol(pib_sectores) - 1)))
    
    pib_sectores <- pib_sectores %>%
      setNames(pib_header) %>%
      tidyr::pivot_longer(
        cols = -sector,
        names_to = "trimestre",
        values_to = "pib_2007"
      )
    
    pib_pond <- pib_pond %>%
      setNames(pib_header) %>%
      tidyr::pivot_longer(
        cols = -sector,
        names_to = "trimestre",
        values_to = "ponderacion"
      )
    
    pib_sectores <- dplyr::left_join(pib_sectores, pib_pond) %>%
      dplyr::mutate(
        fecha = as.numeric(trimestre),
        fecha = lubridate::as_date(fecha),
        year = lubridate::year(fecha),
        trimestre = lubridate::quarter(fecha)
      ) %>%
      dplyr::select(fecha, year, trimestre, dplyr::everything())
    
  }  else if (modalidad == "nominal" & homogenea_91 == TRUE & acumulado == TRUE) {
    # PIB nominal acumulado serie homogenea -----------------------------------
    suppressMessages(
      pib_sectores <- readxl::read_excel(
        path = path,
        sheet = "PIB$_Trim Acum",
        skip = 9,
        n_max = 21,
        col_names = FALSE)
    )
    
    suppressMessages(
      pib_pond <- readxl::read_excel(
        path = path,
        sheet = "PIB$_Trim Acum",
        skip = 35,
        n_max = 21,
        col_names = FALSE)
    )
    
    pib_header <- c(
      "sector",
      seq(as.Date("1991-01-01"), by = "quarter",
          length.out = (ncol(pib_sectores) - 1)))
    
    pib_sectores <- pib_sectores %>%
      setNames(pib_header) %>%
      tidyr::pivot_longer(
        cols = -sector,
        names_to = "trimestre",
        values_to = "pib_2007"
      )
    
    pib_pond <- pib_pond %>%
      setNames(pib_header) %>%
      tidyr::pivot_longer(
        cols = -sector,
        names_to = "trimestre",
        values_to = "ponderacion"
      )
    
    pib_sectores <- dplyr::left_join(pib_sectores, pib_pond) %>%
      dplyr::mutate(
        fecha = as.numeric(trimestre),
        fecha = lubridate::as_date(fecha),
        year = lubridate::year(fecha),
        trimestre = lubridate::quarter(fecha)
      ) %>%
      dplyr::select(fecha, year, trimestre, dplyr::everything())
    
  } else if(modalidad == "real" & homogenea_91 == TRUE & acumulado == TRUE) {
    # PIB real acumulado serie homogenea --------------------------------------
    suppressMessages(
      pib_sectores <- readxl::read_excel(
        path = path,
        sheet = "PIBK_Trim Acum",
        skip = 9,
        n_max = 21,
        col_names = FALSE)
    )
    
    suppressMessages(
      pib_incidencia <- readxl::read_excel(
        path = path,
        sheet = "PIBK_Trim Acum",
        skip = 61,
        n_max = 21,
        col_names = FALSE)
    )
    
    
    pib_header <- c(
      "sector",
      seq(as.Date("1991-01-01"), by = "quarter",
          length.out = (ncol(pib_sectores) - 1)))
    
    pib_sectores <- pib_sectores %>%
      setNames(pib_header) %>%
      tidyr::pivot_longer(
        cols = -sector,
        names_to = "trimestre",
        values_to = "pib_2007"
      )
    
    pib_incidencia <- pib_incidencia %>%
      setNames(pib_header) %>%
      tidyr::pivot_longer(
        cols = -sector,
        names_to = "trimestre",
        values_to = "incidencia"
      )
    
    pib_sectores <-
      dplyr::left_join(pib_sectores, pib_incidencia) %>%
      dplyr::mutate(
        fecha = as.numeric(trimestre),
        fecha = lubridate::as_date(fecha),
        year = lubridate::year(fecha),
        trimestre = lubridate::quarter(fecha)
      ) %>%
      dplyr::select(fecha, year, trimestre, dplyr::everything())
    
  }
  
  
  # detalle_sectores <- structure(
  #   list(sector = c(
  #     "Agropecuario", "Subsector Agricola","Ganadería, Silvicultura y Pesca",
  #     "Industrias", "Explotación de Minas y Canteras",
  #     "Manufactura Local", "Industrias de Alimentos", "Elaboración de Bebidas y Productos de Tabaco",
  #     "Fabricación de Productos de la Refinación de Petróleo y Quimicos",
  #     "Otras Manufacturas", "Manufactura Zonas Francas", "Construcción",
  #     "Servicios", "Energía y Agua", "Comercio", "Hoteles, Bares y Restaurantes",
  #     "Transporte y Almacenamiento", "Comunicaciones", "Intermediación Financiera, Seguros y Actividades Conexas",
  #     "Actividades Inmobiliarias y de Alquiler", "Enseñanza", "Enseñanza de Mercado",
  #     "Enseñanza No de Mercado", "Salud", "Salud de Mercado", "Salud No de Mercado",
  #     "Otras Actividades de Servicios de Mercado", "Administración Pública y Defensa; Seguridad Social de Afiliación Obligatoria y Otros Servicios",
  #     "Valor Agregado", "Impuestos a la producción netos de subsidios",
  #     "Producto Interno Bruto"),
  #     agregacion = c(
  #       "sector", "subsector",
  #       "subsector", "sector", "subsector", "subsector", "actividad",
  #       "actividad", "actividad", "actividad", "subsector", "subsector",
  #       "sector", "subsector", "subsector", "subsector", "subsector",
  #       "subsector", "subsector", "subsector", "subsector", "actividad",
  #       "actividad", "subsector", "actividad", "actividad", "subsector",
  #       "subsector", "valor agregado", "impuestos", "pib"),
  #     nombre_sector = c(
  #       "Agropecuario",
  #       "Agropecuario", "Agropecuario", "Industrias", "Industrias", "Industrias",
  #       "Industrias", "Industrias", "Industrias", "Industrias", "Industrias",
  #       "Industrias", "Servicios", "Servicios", "Servicios", "Servicios",
  #       "Servicios", "Servicios", "Servicios", "Servicios", "Servicios",
  #       "Servicios", "Servicios", "Servicios", "Servicios", "Servicios",
  #       "Servicios", "Servicios", "valor agregado", "impuestos", "pib"),
  #     nombre_subsector = c(
  #       "Agropecuario", "Subsector Agricola",
  #       "Ganadería, Silvicultura y Pesca", "Industrias", "Explotación de Minas y Canteras",
  #       "Manufactura Local", "Manufactura Local", "Manufactura Local",
  #       "Manufactura Local", "Manufactura Local", "Manufactura Zonas Francas",
  #       "Construcción", "Servicios", "Energía y Agua", "Comercio", "Hoteles, Bares y Restaurantes",
  #       "Transporte y Almacenamiento", "Comunicaciones", "Intermediación Financiera, Seguros y Actividades Conexas",
  #       "Actividades Inmobiliarias y de Alquiler", "Enseñanza", "Enseñanza",
  #       "Enseñanza", "Salud", "Salud", "Salud", "Otras Actividades de Servicios de Mercado",
  #       "Administración Pública y Defensa; Seguridad Social de Afiliación Obligatoria y Otros Servicios",
  #       "valor agregado", "impuestos", "pib")
  #   ),
  #   row.names = c(NA, -31L),
  #   class = c("tbl_df", "tbl", "data.frame")
  # )
  # 
  # pib_sectores <- dplyr::left_join(pib_sectores, detalle_sectores)
  return(pib_sectores)
  
}  


# Densidad de pronostico  -------------------------------------------------


# Función de evaluación ------------------------
  # Solo se reescribe la original para hacerla menos restrictiva
  EvaluacionBCRD <- function(f, x){
    # Escrita por Nerys Ramírez: versión 6/12/2019
    n <- length(x)
    test <- 1:n
    error <- (x - f)[test]
    pe <- error / x[test] * 100
    
    me <- mean(error, na.rm = TRUE)
    mse <- mean(error ^ 2, na.rm = TRUE)
    mae <- mean(abs(error), na.rm = TRUE)
    mape <- mean(abs(pe), na.rm = TRUE)
    mpe <- mean(pe, na.rm = TRUE)
    
    fpe <- (c(f[2:n]) / c(x[1:(n - 1)]) - 1)[test - 1]
    ape <- (c(x[2:n]) / c(x[1:(n - 1)]) - 1)[test - 1]
    theil <- sqrt(sum((fpe - ape) ^ 2, na.rm = TRUE) / sum(ape ^ 2, na.rm = TRUE))
    
    # Se coloca este Condicional para poder evaluar pronosticos a 1 paso
    if (length(f)==1){
      r1 <- NA
    } else {
      r1 <- acf(error, plot = FALSE, lag.max = 2, na.action = na.pass)$acf[2, 1, 1]
    }
    
    out <- c(me, mse, sqrt(mse), mae, mpe, mape, r1, theil)
    names(out) <- c("ME", "MSE", "RMSE", "MAE", "MPE", "MAPE","ACF1", "Theil's U")
    
    return(out)
    
  }
  

# evaluacion de forecast --------------------------------------------------

  EvaluacionBCRDh <- function(f, x, horizontes=c(1:6,12)){
    # 
    vh <- horizontes
    Resultado<-c()
    
    for(i in vh){
      performance<-as.vector(EvaluacionBCRD(f[1:i], x[1:i]))
      Resultado<-rbind(Resultado, performance)
    }
    
    # Resultados de la función
    row.names(Resultado) <- as.character(horizontes)
    colnames(Resultado) <- c("ME", "MSE", "RMSE", "MAE", "MPE", "MAPE","ACF1", "Theil's U")
    
    return(Resultado)
  }  


# Tema para ggplot --------------------------------------------------------

  
  theme_plex <- function(base_size = 11,
                         strip_text_size = 12,
                         strip_text_margin = 5,
                         subtitle_size = 13,
                         subtitle_margin = 10,
                         plot_title_size = 16,
                         plot_title_margin = 10,
                         ...) {
    ret <- ggplot2::theme_minimal(base_family = "IBMPlexSans",
                                  base_size = base_size, ...)
    ret$strip.text <- ggplot2::element_text(
      hjust = 0, size = strip_text_size,
      margin = ggplot2::margin(b = strip_text_margin),
      family = "IBMPlexSans-Medium"
    )
    ret$plot.subtitle <- ggplot2::element_text(
      hjust = 0, size = subtitle_size,
      margin = ggplot2::margin(b = subtitle_margin),
      family = "IBMPlexSans"
    )
    ret$plot.title <- ggplot2::element_text(
      hjust = 0, size = plot_title_size,
      margin = ggplot2::margin(b = plot_title_margin),
      family = "IBMPlexSans-Bold"
    )
    ret
  }
  
### Historico de errores de pronostico ----------------------------------------------------
  
error_historico <- function(x,
                            ventana=round(length(!is.na(x))*0.7),
                            n.valid,
                            recursiva=TRUE,
                            medida_error="RMSE",
                            agregar = "mean"){
# x = time serie vector
# ventana = longitud del vector de estimación. Default= 70% de la muestra
# n.valid = periodos proyectados
# medida_error:  ME;MSE;RMSE;MAE;MPE;MAPE;ACF1;Theil's U [medida de error que queires guardar]
# agregar: qué función deseas agregar de los errores. 

# Ejemplo: 
# error_historico(AirPassengers,ventana=70,n.valid=5,recursiva=TRUE,medida_error="RMSE",agregar = "median") 

  t_obs <- length(x[!is.na(x)])
  Errores_histo <- c()
  c<-1
  
  for (obs in (ventana+n.valid):t_obs){
    
   # Extrae las observaciones con que se van a trabajar
    data.ejercicio<-x[c:obs]
    
   # Prepara los datos:
    n.train <- length(data.ejercicio)- n.valid  # intervalo de estimación
    x.train <- data.ejercicio[1:n.train]        # Datos del modelo
    x.test <-  data.ejercicio[-c(1:n.train)]
  
    # Modelo Autoarima Móvil
    modelo <- auto.arima(x.train, seasonal=T)
    x.hat <- forecast::forecast(modelo, h=n.valid)$mean
    error_pronH <- EvaluacionBCRDh(f=x.hat, x=x.test, h=1:n.valid)[,medida_error]
    
    Errores_histo <- rbind(Errores_histo,error_pronH)
    
    if (recursiva == FALSE){c <- c+1}   #"Movil"
    
  } #Cierrra el bucle
 
  colnames(Errores_histo) <- paste("h",1:n.valid, sep="") 
  
  errores_h <- apply(Errores_histo, MARGIN=2, FUN=get(agregar), na.rm = TRUE)
  
  info <- rbind(paste("Medida de errro: ", medida_error, sep=""),
                paste("Horizonte: ", n.valid, sep=""),
                paste("Función agregar: ", agregar, sep="") )
  
  resultados <- list("Informacion"=info, 
                   "Modelo Arima"=modelo,
                   "Hitorico errores"=Errores_histo,
                   "Errores agregados"=errores_h)
  
  return(resultados)
} #Cierra la función
  
### Agrega errorres ----------------------------------------------------

componentalVAR <- function(data,pesos=rep(0.25,4),alpha=0.99,inversion=1){
  # Data es un vector de errores de cada sector 
  # peso son las ponderaciones de los sectores
  
  data_rentabilidad <- returns(data)
  Ea <- apply(data_rentabilidad, MARGIN=2, FUN=mean, na.rm = TRUE)*100*12
  Va <- apply(data_rentabilidad, MARGIN=2, FUN=sd, na.rm = TRUE)*sqrt(256)
  sigma <- var(data_rentabilidad, na.rm = TRUE)
  
  esperanza_cartera <- t(pesos)%*%Ea
  varianza_cartera <- t(pesos)%*%sigma%*%pesos
  vol_cartera <- sqrt(varianza_cartera)*sqrt(256)
  
  var_cartera_99 <-  esperanza_cartera - qnorm(alpha,0,1)*vol_cartera
  var_nominal <- inversion*var_cartera_99
  
  # **********************************
  pesos_n = pesos*inversion
  varianza_n <- t(pesos_n)%*%sigma%*%pesos_n
  var_nominal <- esperanza_cartera - qnorm(alpha,0,1)*sqrt(var_nominal1)
  
  rent_hist <- data_rentabilidad[-1,]%*%pesos
  
  cov_a_c <- cov(cbind(data_rentabilidad[-1,],rent_hist))[,4]*inversion
  beta_i <- cov_a_c/varianza_n
  delta_b <- beta_i*var_nominal
  
  incremental_var <- (pesos_n+1-pesos_n) * delta_b
  componental_var <- beta_i*pesos_n*var_nominal
  return(componental_var/sum(componental_var)*100)
}



# Función para el Fanplot
  # Fan plot
  
  fanplot_density <- function(serie, fcast_values, step_error, simulation_size = 1000) {
    
    frecuencia <- frequency(serie)
    
    if(frecuencia == 4) {
      to_date <- zoo::as.yearqtr
      factor <- 3
    } else if(frecuencia == 12) {
      to_date <- zoo::as.yearmon()
      factor <- 1
    } else if(frecuencia == 1){
      factor <- 12
    } else {
      print("Esta función solo admite series de tiempo anuales,\n
          trimestrales y mensuales")
    }
    
    set.seed(123)
    
    fan_data <- purrr::map2(
      fcast_values,
      step_error,
      ~rnorm(
        n = simulation_size,
        mean = .x,
        sd = .y
      ) %>% 
        as.data.frame() %>% 
        setNames("y")
    ) %>% 
      dplyr::bind_rows(.id = "x") %>% 
      dplyr::mutate(Sim = paste0("X", x)) %>% 
      dplyr::mutate(
        x = parse_number(x)
      )
    
    data_observada <- serie %>% 
      tibble::as_tibble() %>%
      setNames("y") %>% 
      dplyr::mutate(
        x = timetk::tk_index(serie),
        x = to_date(x),
        x = as.Date(x)
      ) %>% 
      dplyr::filter(x > "2006-12-31")
    
    fan_data <-   fan_data %>% 
      dplyr::mutate(
        x = dplyr::last(data_observada$x) + months(x * factor)
      ) %>% 
      dplyr::bind_rows(
        tibble(
          x = last(data_observada$x),
          y = rnorm(simulation_size, mean = last(data_observada$y), sd = 0.00001)
        ),
        .
      )
    
    fan_plot <- ggplot() +
      geom_line(
        data = data_observada,
        mapping = aes(x = x, y = y),
        size = 1
      ) +
      ggfan::geom_fan(
        data = fan_data,
        aes(x = x, y = y),
        show.legend = FALSE
      ) +
      ggfan::geom_interval(
        data = fan_data,
        aes(x = x, y = y)
      ) 
    
    return(fan_plot)
    
  }
  
# Agrega errores 2 -----------------------------------------
 # list_errores_rmse <- readRDS("errores_sectores.RDS")
 # list_errores_me <-readRDS("errores_me_sectore.RDS")
  
  agrega_errores <- function(list_errores_rmse, 
                             list_errores_me, 
                             pesos, 
                             horizontes=ncol(list_errores_rmse[[1]])){
    
    num_obs <- nrow(list_errores_rmse[[1]])
    hk <-c()
    errores_pro <- c()
    
    for (jh in 1:horizontes){
      
      List_errorh <- lapply(list_errores_rmse, `[`, ,jh)
      List_errorh_e <- lapply(list_errores_me, `[`, ,jh)
      
      #Vuelve la list una matrix, pero quita a columna del ipc total
      matrix_errorh <- matrix(unlist(List_errorh), nrow = num_obs, byrow = FALSE)
      matrix_errorh_e <- matrix(unlist(List_errorh_e), nrow = num_obs, byrow = FALSE)
      
      mat_var_cov <- var(matrix_errorh_e)
      
      h <- t(pesos)%*%mat_var_cov%*%pesos
      
      errores_pro <- rbind(errores_pro, colMeans(matrix_errorh))
      hk <-c(hk,h)
    }
    
    names(hk) <- paste("h",1:horizontes, sep="") 
    row.names(errores_pro) <- names(hk)
    
    resultados <- list("Promedio errores individuales"=errores_pro,
                       "Error agregado"=hk)
    return(resultados)
  }
  
  
# Agrega errores 1 -----------------------------------------

agrega_errores2 <- function(list_errores_rmse, 
                           list_errores_me, 
                           pesos, 
                           horizontes=ncol(list_errores_rmse[[1]])){
    
    num_obs <- nrow(list_errores_rmse[[1]])
    hk <-c()
    errores_pro <- c()
      
    for (jh in 1:horizontes){
      
      List_errorh <- lapply(list_errores_rmse, `[`, ,jh)
      List_errorh_e <- lapply(list_errores_me, `[`, ,jh)
      
      #Vuelve la list una matrix, pero quita a columna del ipc total
      matrix_errorh <- matrix(unlist(List_errorh[-length(List_errorh)]), nrow = num_obs, byrow = FALSE)
      matrix_errorh_e <- matrix(unlist(List_errorh_e[-length(List_errorh_e)]), nrow = num_obs, byrow = FALSE)
      
      mat_var_cov <- var(matrix_errorh_e)
      
      h <- t(pesos)%*%mat_var_cov%*%pesos
      
      errores_pro <- rbind(errores_pro, colMeans(matrix_errorh))
      hk <-c(hk,h)
    }
    
    names(hk) <- paste("h",1:horizontes, sep="") 
    row.names(errores_pro) <- names(hk)
    
    resultados <- list("Promedio errores individuales"=errores_pro,
                       "Error agregado"=hk)
    return(resultados)
}
  
#  Componente VaR de los errores ----------------------
  
  component_errores <- function(list_errores_rmse, 
                                list_errores_me, 
                                horizontes=8, 
                                pesos, 
                                inversion_0=1,
                                alpha=0.99){
    
    composicion_errorH <- c()
    n_list <- length(list_errores_rmse)
    
    for (columna in 1:horizontes){
      
      List_errorh <- lapply(list_errores_rmse, `[`, ,columna)
      List_errorh_e <- lapply(list_errores_me, `[`, ,columna)
      
      num_obs <- length(List_errorh[[1]])
      #    matrix_errorh <- matrix(unlist(List_errorh[-n_list]), nrow = num_obs, byrow = FALSE)
      matrix_errorh <- matrix(unlist(List_errorh[-n_list]), nrow = num_obs, byrow = FALSE)
      matrix_errorh_e <- matrix(unlist(List_errorh_e[-n_list]), nrow = num_obs, byrow = FALSE)
      
      Ea <- c(4.590,5.118,4.846,6.893)/100  #promedio historico del crecimiento [DEBE CAMBIARSE]
      Va <- apply(List_errorh, MARGIN=2, FUN=median, na.rm = TRUE)
      sigma <- var(matrix_errorh_e)
      
      esperanza_cartera <- t(pesos)%*%Ea
      varianza_cartera <- t(pesos)%*%sigma%*%pesos  #VaRc
      vol_cartera <- sqrt(varianza_cartera)
      
      var_cartera <-  esperanza_cartera - qnorm(1-alpha,0,1)*vol_cartera
      var_nominal <- inversion_0*var_cartera
      
      # **********************************
      pesos_n = pesos*inversion_0
      varianza_n <- t(pesos_n)%*%sigma%*%pesos_n
      var_nominal <- esperanza_cartera - qnorm(1-alpha,0,1)*sqrt(varianza_n)
      
      error_agre_hist <- matrix_errorh%*%pesos_n
      
      cov_a_c <- cov(cbind(matrix_errorh,error_agre_hist))[n_list,-n_list]*inversion_0    #covarianza con error global
      beta_i <- cov_a_c/varianza_n   #Beta_i
      delta_b <- beta_i*var_nominal  #delta VaR: incremental_var
      
      componental_var <- var_nominal*beta_i*pesos_n   #CVaR 
      
      # if (sum(componental_var)!=var_nominal){
      #   print("Algo va mal, la descomposición no suma el VAR total")
      #   break}
      
      composicion <- componental_var/sum(componental_var)*100
      composicion_errorH <- rbind(composicion_errorH,composicion)
    }
    
    
    row.names(composicion_errorH) <- paste("h",1:horizontes, sep="") 
    colnames(composicion_errorH) <- names(List_errorh)[-n_list]
    
    resultEA <-  list("composicion error"=composicion_errorH,
                      "Var nominal" = var_nominal,
                      "Cvari" = componental_var,
                      "Sum cVar" = sum(componental_var))
    
    return(resultEA)
    
  }
  
# Error historico
  
error_historico_d1 <- function(x, xreg_d, ventana=round(length(!is.na(x))*0.7),n.valid,
                              recursiva=TRUE, medida_error="RMSE", agregar = "mean"){
# x = time serie vector
# ventana = longitud del vector de estimación. Default= 70% de la muestra
# n.valid = periodos proyectados
# medida_error:  ME;MSE;RMSE;MAE;MPE;MAPE;ACF1;Theil's U [medida de error que queires guardar]
# agregar: qué función deseas agregar de los errores. 
    
# Ejemplo: 
# error_historico(AirPassengers,,ventana=70,n.valid=5,recursiva=TRUE,medida_error="RMSE",agregar = "median") 
    
    t_obs <- length(x[!is.na(x)])
    Errores_histo <- c()
    c<-1
    
    for (obs in (ventana+n.valid):t_obs){
      
      # Extrae las observaciones con que se van a trabajar
      data.ejercicio <- x[c:obs]
      dummy.ejercicio <- xreg_d[c:obs]
      
      # Prepara los datos:
      n.train <- length(data.ejercicio)- n.valid  # intervalo de estimación
      x.train <- data.ejercicio[1:n.train]        # Datos del modelo
      x.test <-  data.ejercicio[-c(1:n.train)]
      
      d.train <- dummy.ejercicio[1:n.train]
      d.test <-  dummy.ejercicio[-c(1:n.train)]
        
      # Modelo Autoarima Móvil
      modelo <- auto.arima(x.train, xreg=d.train, seasonal=T)
      x.hat <- forecast::forecast(modelo, xreg=d.test, h=n.valid)$mean
      error_pronH <- EvaluacionBCRDh(f=x.hat, x=x.test, h=1:n.valid)[,medida_error]
      
      Errores_histo <- rbind(Errores_histo,error_pronH)
      
      if (recursiva == FALSE){c <- c+1}   #"Movil"
      
    } #Cierrra el bucle
    
    colnames(Errores_histo) <- paste("h",1:n.valid, sep="") 
    
    errores_h <- apply(Errores_histo, MARGIN=2, FUN=get(agregar), na.rm = TRUE)
    
    info <- rbind(paste("Medida de errro: ", medida_error, sep=""),
                  paste("Horizonte: ", n.valid, sep=""),
                  paste("Función agregar: ", agregar, sep="") )
    
    resultados<-list("Informacion"=info, 
           "Modelo Arima"=modelo,
           "Hitorico errores"=Errores_histo,
           "Errores agregados"=errores_h)
    
    return(resultados)
  } #Cierra la función
  