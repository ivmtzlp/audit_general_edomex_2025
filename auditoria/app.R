library(shiny)
library(leaflet)
library(readr)
library(purrr)
library(tibble)
library(sf)
library(stringr)
library(dplyr)
library(tidyr)
library(glue)
library(ggplot2)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library(colorRamps)
library(gt)
library(shinyjs)
library(highcharter)
library(bslib)
library(googleway)

options(survey.lonely.psu ="remove")

datos_municipios <- list(
  "Atizapan De Zaragoza" = list(
    catalogo_seccion_cluster = read_rds("data/catalogo_seccion_cluster_atiz.rda"),
    enc_shp_car = readr::read_rds("data/enc_shp_atiz.rda") %>%
      mutate(interior_cluster = dplyr::if_else(
        condition = (as.numeric(distancia) != 0),
        true = "Fuera",
        false = "Dentro"
      )),
    mapa_base_car = read_rds("data/mapa_base_atiz.rda"),
    preguntas = read_rds("data/clase_pregunta_atiz.rda")
  ),
  "Nezahualcoyotl" = list(
    catalogo_seccion_cluster = read_rds("data/catalogo_seccion_cluster_neza.rda"),
    enc_shp_car = readr::read_rds("data/enc_shp_neza.rda") %>%
      mutate(interior_cluster = dplyr::if_else(
        condition = (as.numeric(distancia) != 0),
        true = "Fuera",
        false = "Dentro"
      )),
    mapa_base_car = read_rds("data/mapa_base_neza.rda"),
    preguntas = read_rds("data/clase_pregunta_neza.rda")
  ),
  "Texcoco" = list(
    catalogo_seccion_cluster = read_rds("data/catalogo_seccion_cluster_texcoco.rda"),
    enc_shp_car = readr::read_rds("data/enc_shp_texcoco.rda") %>%
      mutate(interior_cluster = dplyr::if_else(
        condition = (as.numeric(distancia) != 0),
        true = "Fuera",
        false = "Dentro"
      )),
    mapa_base_car = read_rds("data/mapa_base_texcoco.rda"),
    preguntas = read_rds("data/clase_pregunta_texcoco.rda")
  ),
  "Toluca" = list(
    catalogo_seccion_cluster = read_rds("data/catalogo_seccion_cluster_toluca.rda"),
    enc_shp_car = readr::read_rds("data/enc_shp_toluca.rda") %>%
      mutate(interior_cluster = dplyr::if_else(
        condition = (as.numeric(distancia) != 0),
        true = "Fuera",
        false = "Dentro"
      )),
    mapa_base_car = read_rds("data/mapa_base_toluca.rda"),
    preguntas = read_rds("data/clase_pregunta_toluca.rda")
  ),
  "Ecatepec de Morelos" = list(
    catalogo_seccion_cluster = read_rds("data/catalogo_seccion_cluster_ecatepec.rda"),
    enc_shp_car = readr::read_rds("data/enc_shp_ecatepec.rda") %>%
      mutate(interior_cluster = dplyr::if_else(
        condition = (as.numeric(distancia) != 0),
        true = "Fuera",
        false = "Dentro"
      )),
    mapa_base_car = read_rds("data/mapa_base_ecatepec.rda"),
    preguntas = read_rds("data/clase_pregunta_ecatepec.rda")
  ),
  "Chimalhuacán" = list(
    catalogo_seccion_cluster = read_rds("data/catalogo_seccion_cluster_chimal.rda"),
    enc_shp_car = readr::read_rds("data/enc_shp_chimal.rda") %>%
      mutate(interior_cluster = dplyr::if_else(
        condition = (as.numeric(distancia) != 0),
        true = "Fuera",
        false = "Dentro"
      )),
    mapa_base_car = read_rds("data/mapa_base_chimal.rda"),
    preguntas = read_rds("data/clase_pregunta_chimal.rda")
  ),
  "Cuautitlan Izcalli" = list(
    catalogo_seccion_cluster = read_rds("data/catalogo_seccion_cluster_cuatli.rda"),
    enc_shp_car = readr::read_rds("data/enc_shp_cuatli.rda") %>%
      mutate(interior_cluster = dplyr::if_else(
        condition = (as.numeric(distancia) != 0),
        true = "Fuera",
        false = "Dentro"
      )),
    mapa_base_car = read_rds("data/mapa_base_cuatli.rda"),
    preguntas = read_rds("data/clase_pregunta_cuatli.rda")
  ),
  "Naucalpan De Juarez" = list(
    catalogo_seccion_cluster = read_rds("data/catalogo_seccion_cluster_nauc.rda"),
    enc_shp_car = readr::read_rds("data/enc_shp_nauc.rda") %>%
      mutate(interior_cluster = dplyr::if_else(
        condition = (as.numeric(distancia) != 0),
        true = "Fuera",
        false = "Dentro"
      )),
    mapa_base_car = read_rds("data/mapa_base_nauc.rda"),
    preguntas = read_rds("data/clase_pregunta_nauc.rda")
  ),
  # "Tlalnepantla De Baz" = list(
  #   catalogo_seccion_cluster = read_rds("data/catalogo_seccion_cluster_tlane.rda"),
  #   enc_shp_car = readr::read_rds("data/enc_shp_tlane.rda") %>%
  #     mutate(interior_cluster = dplyr::if_else(
  #       condition = (as.numeric(distancia) != 0),
  #       true = "Fuera",
  #       false = "Dentro"
  #     )),
  #   mapa_base_car = read_rds("data/mapa_base_tlane.rda"),
  #   preguntas = read_rds("data/clase_pregunta_tlane.rda")
  # ),
  "Resto de Edomex" = list(
    catalogo_seccion_cluster = read_rds("data/catalogo_seccion_cluster_rest_edomex.rda"),
    enc_shp_car = readr::read_rds("data/enc_shp_rest_edomex.rda") %>%
      mutate(interior_cluster = dplyr::if_else(
        condition = (as.numeric(distancia) != 0),
        true = "Fuera",
        false = "Dentro"
      )),
    mapa_base_car = read_rds("data/mapa_base_rest_edomex.rda"),
    preguntas = read_rds("data/clase_pregunta_rest_edomex.rda")
  ),
  "Tultitlán" = list(
    catalogo_seccion_cluster = read_rds("data/catalogo_seccion_cluster_tulti.rda"),
    enc_shp_car = readr::read_rds("data/enc_shp_tulti.rda") %>%
      mutate(interior_cluster = dplyr::if_else(
        condition = (as.numeric(distancia) != 0),
        true = "Fuera",
        false = "Dentro"
      )),
    mapa_base_car = read_rds("data/mapa_base_tulti.rda"),
    preguntas = read_rds("data/clase_pregunta_tulti.rda")
  )
)


lista_mun <- data.frame(
  "Municipios" = c('Atizapan De Zaragoza',"Texcoco","Nezahualcoyotl", "Ecatepec de Morelos","Toluca","Chimalhuacán",
                   "Cuautitlan Izcalli","Naucalpan De Juarez","Resto de Edomex","Tultitlán")
) |> arrange(Municipios)



 #datos_municipios[["Atizapan De Zaragoza"]]


# Asigna dinámicamente las variables

catalogo_seccion_cluster<-datos_municipios[["Atizapan De Zaragoza"]]$catalogo_seccion_cluster
preguntas <- datos_municipios[["Atizapan De Zaragoza"]]$preguntas
diseno <- preguntas$encuesta$muestra$muestra
shp <- preguntas$encuesta$shp_completo
bd <- preguntas$encuesta$respuestas$base
eliminadas <- preguntas$encuesta$respuestas$eliminadas
eliminadas_shp <<- eliminadas %>%
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
corregidas_shp <<- preguntas$encuesta$respuestas$cluster_corregido %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
enc_shp <- datos_municipios[["Atizapan De Zaragoza"]]$enc_shp_car
mapa_base <- datos_municipios[["Atizapan De Zaragoza"]]$mapa_base_car





Sys.setlocale(locale = "es_ES.UTF-8")

# Funciones -----------------------------------------------------------------------------------

unidades_app <- function(diseno, u_nivel) {
  unidades <- u_nivel %>% pull(variable)
  u_nivel_tipo <- u_nivel %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
  
  aulr <- shp$shp[[unidades]] %>%
    inner_join(diseno$muestra[[diseno$ultimo_nivel]] %>%
                 unnest(data) %>%
                 distinct(!!rlang::sym(unidades), !!rlang::sym(u_nivel_tipo)))
}

entrevistas <- function(diseno, enc, u_nivel, u_nivel_tipo){
  cortes <- diseno$cuotas %>% count(parse_number(rango)-1) %>% pull(1) %>% append(Inf)
  texto_cortes <- diseno$cuotas %>% distinct(rango) %>% pull(1)
  
  hecho <- enc %>%
    mutate(edad = as.character(cut(as.integer(edad),cortes,
                                   texto_cortes)),
           sexo = if_else(sexo == "F", "Mujer", "Hombre"),
           cluster = as.numeric(!!rlang::sym(u_nivel$variable))) %>%
    count(cluster, edad, sexo, name = "hecho") %>%
    full_join(
      diseno$cuotas %>% mutate(sexo = if_else(sexo == "F", "Mujer", "Hombre")) %>%
        rename(cuota = n, cluster = !!rlang::sym(u_nivel_tipo), edad = rango)
    ) %>% replace_na(list(hecho = 0, faltan = 0)) %>%
    mutate(faltan = cuota - hecho) %>% filter(cluster %in% diseno$cuotas[[u_nivel_tipo]])
  
  por_hacer <- diseno$cuotas %>% mutate(sexo = if_else(sexo == "F", "Mujer", "Hombre")) %>%
    rename(cuota = n, cluster = u_nivel_tipo, edad = rango) %>%
    left_join(
      enc %>%
        mutate(edad = as.character(cut(as.integer(edad),cortes,
                                       texto_cortes)),
               sexo = if_else(sexo == "F", "Mujer", "Hombre"),
               cluster = as.numeric(!!rlang::sym(u_nivel$variable))) %>%
        count(cluster, edad, sexo, name = "hecho")
    ) %>% replace_na(list(hecho = 0)) %>% mutate(por_hacer = cuota-hecho,
                                                 por_hacer2 = if_else(por_hacer < 0, 0, por_hacer)
    )
  return(list(hecho = hecho, por_hacer = por_hacer))
}

graficar_historico <- function(bd_efectivas, bd_eliminadas, bd_corregidas){
  
  hist_efectivas <- bd_efectivas %>%
    as_tibble %>%
    count(fecha = lubridate::as_date(Date)) |>
    rename("tot_hechas" = n)
  
  hist_eliminadas <- bd_eliminadas %>%
    as_tibble %>%
    count(fecha = lubridate::as_date(Date)) |>
    rename("tot_eliminadas" = n)
  
  hist_corregidas <- bd_corregidas %>%
    as_tibble %>%
    count(fecha = lubridate::as_date(Date)) |>
    rename("tot_corregidas" = n)
  
  bd_plot <- hist_efectivas %>%
    left_join(hist_eliminadas, by = "fecha") %>%
    left_join(hist_corregidas, by = "fecha") |>
    mutate(fecha = stringr::str_to_title(string = gsub(pattern = "\\.", replacement = "", x = format(fecha, "%b-%d"))))
  
  g <- highchart() |>
    hc_xAxis(categories = bd_plot$fecha, labels = list(style = list(fontSize = "18px"))) |>
    hc_yAxis(labels = list(style = list(fontSize = "18px"))) |>
    hc_add_series(name = "Efectivas", data = bd_plot$tot_hechas, type = "line", color = "green") |>
    hc_add_series(name = "Efectivas", data = bd_plot$tot_hechas, type = "scatter", color = "green", showInLegend = FALSE) |>
    hc_add_series(name = "Corregidas", data = bd_plot$tot_corregidas, type = "line", color = "orange") |>
    hc_add_series(name = "Corregidas", data = bd_plot$tot_corregidas, type = "scatter", color = "orange", showInLegend = FALSE) |>
    hc_add_series(name = "Eliminadas", data = bd_plot$tot_eliminadas, type = "line", color = "red") |>
    hc_add_series(name = "Eliminadas", data = bd_plot$tot_eliminadas, type = "scatter", color = "red", showInLegend = FALSE) |>
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, format = "{point.y}", style = list(fontSize = "24px")))) |>
    hc_legend(itemStyle = list(fontSize = "24px"))
  
  return(g)
  
}

graficar_barras <- function(bd, color){
  
  g <- highchart() |>
    hc_xAxis(categories = bd$Srvyr, labels = list(style = list(fontSize = "18px"))) |>
    hc_yAxis(labels = list(style = list(fontSize = "18px"))) |>
    hc_add_series(data = bd$n, type = "bar", color = color, showInLegend = FALSE) |>
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, format = "{point.y}", style = list(fontSize = "24px")))) |>
    hc_legend(itemStyle = list(fontSize = "24px"))
  
  return(g)
}

graficar_mapa_clusters <- function(lflt = NULL, bd, nivel, muestra, shp){
  
  nivel <- if(nivel == "MANZANA"){
    "MZA"
  } else nivel
  
  pal <- if(nivel == "MUNICIPIO"){
    colorFactor(topo.colors(n_distinct(muestra$strata_1)), domain = unique(muestra$strata_1))
  } else {
    pal <- colorFactor(palette = c("black", "gray80", "gray80"), domain = c("LOCALIDAD", "SECCION", "Cluster"), levels = c("LOCALIDAD", "SECCION", "Cluster"), ordered = T, na.color = "blue")
  }
  
  mapa <- if(is.null(lflt)){
    shp %>% purrr::pluck(nivel) %>%
      left_join(muestra %>% distinct(MUNICIPIO,strata_1)) %>%
      group_by(strata_1) %>% summarise(n()) %>%
      sf::st_buffer(dist = 0) %>%
      leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = ~pal(strata_1), opacity = 1, fill = F) %>%
      addLegend(pal = pal, values = ~strata_1, position = "bottomleft")
  } else{
    if(nivel == "MUNICIPIO"){
      lflt %>% addPolygons(data = shp %>% purrr::pluck(nivel) %>% inner_join(muestra %>% distinct(across(all_of(nivel)), .keep_all = T)),
                           fillColor = ~pal(strata_1), color = "black", opacity = 1, weight = 1, fillOpacity = 1, label = ~glue::glue("Municipio: {NOMBRE_MUN}"))
    } else{
      if(nivel == "MANZANA"){
        mapear <- shp %>% purrr::pluck(nivel) %>% inner_join(muestra %>% distinct(across(all_of(nivel)), .keep_all = T))
        
        lflt %>%
          addCircleMarkers(data = mapear %>% filter(sf::st_geometry_type(.) == "POINT"),
                           label = ~glue::glue("Localidad: {MANZANA}"), opacity = 1, fillOpacity = 1,
                           fillColor = "#f72585", color = "black", weight = 1) %>%
          addLegend(position = "bottomright", colors = "#f72585", labels = "Localidades rurales")
        
      } else{
        mapear <- shp$SECCION %>%
          inner_join(muestra %>% distinct(across(all_of(nivel)), .keep_all = T) |>
                       select(!c(ENTIDAD, DISTRITO_F, MUNICIPIO)), by = "SECCION")
        
        nivel_muestra <- mapear %>% as_tibble %>% select(contains("cluster")) %>% names %>% parse_number %>% max
        
        popup_cluster <- paste0("cluster_",nivel,": ", as_tibble(mapear)[[paste("cluster",nivel,sep = "_")]])
        popup_mun <- paste("Municipio: ", mapear$NOMBRE_MUN)
        mapa <- lflt %>%
          addPolygons(data = mapear,
                      stroke = T,
                      color = "black",
                      fillColor = ~pal(nivel),
                      fillOpacity = .1,
                      weight = 1,
                      opacity = 1,
                      popup = paste(popup_mun, popup_cluster, sep = "<br>")
          ) %>%
          addLegend(title = "Nivel", na.label = "Sin nivel", data = mapear, pal = pal, values = c("SECCION" = "Cluster"), position = "bottomleft")
      }
    }
  }
  
  return(mapa)
}

graficar_mapa_mza <- function(lflt = NULL, bd, nivel, muestra, shp){
  
  nivel <- if(nivel == "MANZANA"){
    "MANZANA"
  } else nivel
  
  pal <- if(nivel == "MUNICIPIO"){
    colorFactor(topo.colors(n_distinct(muestra$strata_1)), domain = unique(muestra$strata_1))
  } else {
    pal <- colorFactor(palette = c("black", "gray80", "gray80"), domain = c("LOCALIDAD", "SECCION", "Cluster"), levels = c("LOCALIDAD", "SECCION", "Cluster"), ordered = T, na.color = "blue")
  }
  
  mapa <- if(is.null(lflt)){
    shp %>% purrr::pluck(nivel) %>%
      left_join(muestra %>% distinct(MUNICIPIO,strata_1)) %>%
      group_by(strata_1) %>% summarise(n()) %>%
      sf::st_buffer(dist = 0) %>%
      leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = ~pal(strata_1), opacity = 1, fill = F) %>%
      addLegend(pal = pal, values = ~strata_1, position = "bottomleft")
  } else{
    if(nivel == "MUNICIPIO"){
      lflt %>% addPolygons(data = shp %>% purrr::pluck(nivel) %>% inner_join(muestra %>% distinct(across(all_of(nivel)), .keep_all = T)),
                           fillColor = ~pal(strata_1), color = "black", opacity = 1, weight = 1, fillOpacity = 1, label = ~glue::glue("Municipio: {NOMBRE_MUN}"))
    } else{
      if(nivel == "MANZANA"){
        
        mapear <- shp$MANZANA %>%
          inner_join(diseno$muestra$MZA %>%
                       tidyr::unnest(cols = data) |>
                       distinct(.keep_all = T))
        
        mapa <-
          lflt %>%
          # leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
          # addProviderTiles("CartoDB.Positron") %>%
          addPolygons(data = mapear %>% filter(sf::st_geometry_type(.) != "POINT"),
                      # label = ~glue::glue("Localidad: {MANZANA}"),
                      # stroke = TRUE,
                      # opacity = 1,
                      # fillOpacity = 1,
                      fillColor = "red",
                      color = "red"
          ) %>%
          addCircleMarkers(data = mapear %>% filter(sf::st_geometry_type(.) == "POINT"),
                           # label = ~glue::glue("Localidad: {MANZANA}"),
                           opacity = 1,
                           fillOpacity = 1,
                           fillColor = "red",
                           color = "red",
                           weight = 1)
        # addLegend(position = "bottomright", colors = "#f72585", labels = "Localidades rurales")
        
      } else{
        mapear <- shp$SECCION %>%
          inner_join(muestra %>% distinct(across(all_of(nivel)), .keep_all = T) |>
                       select(!c(ENTIDAD, DISTRITO_F, MUNICIPIO)), by = "SECCION")
        
        nivel_muestra <- mapear %>% as_tibble %>% select(contains("cluster")) %>% names %>% parse_number %>% max
        
        popup_cluster <- paste0("cluster_",nivel,": ", as_tibble(mapear)[[paste("cluster",nivel,sep = "_")]])
        popup_mun <- paste("Municipio: ", mapear$NOMBRE_MUN)
        mapa <- lflt %>%
          addPolygons(data = mapear,
                      stroke = T,
                      color = "black",
                      fillColor = ~pal(nivel),
                      fillOpacity = .1,
                      weight = 1,
                      opacity = 1,
                      popup = paste(popup_mun, popup_cluster, sep = "<br>")
          ) %>%
          addLegend(title = "Nivel", na.label = "Sin nivel", data = mapear, pal = pal, values = c("SECCION" = "Cluster"), position = "bottomleft")
      }
    }
  }
  
  return(mapa)
}

asignar_colores <- function(tb_respuestas, partidos = T){
  tb_respuestas |>
    mutate(color = dplyr::case_when(grepl(pattern = "por MORENA, ", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_morena,
                                    grepl(pattern = "Astiazarán", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pri,
                                    grepl(pattern = "por PAN, ", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pan,
                                    grepl(pattern = "por Movimiento Ciudadano, ", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_mc,
                                    grepl(pattern = "MORENA", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_morena,
                                    grepl(pattern = "Morena", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_morena,
                                    grepl(pattern = "PAN", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pan,
                                    grepl(pattern = "PRI", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pri,
                                    grepl(pattern = "PRD", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_prd,
                                    grepl(pattern = "PT", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pt,
                                    grepl(pattern = "PES", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pes,
                                    grepl(pattern = "Partido Verde", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pvem,
                                    grepl(pattern = "PVEM", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pvem,
                                    grepl(pattern = "Movimiento Ciudadano|MC", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_mc,
                                    grepl(pattern = "Fuerza por", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_fuerzaxmexico,
                                    grepl(pattern = "Otro", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_otro,
                                    grepl(pattern = "Ninguno", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_ninguno,
                                    grepl(pattern = "Candidato independiente", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_otro,
                                    grepl(pattern = "no registrado", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_otro,
                                    grepl(pattern = "No iré a votar", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_ninguno,
                                    grepl(pattern = "Anular", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_ninguno,
                                    grepl(pattern = "No sabe", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_nsnc,
                                    grepl(pattern = "No contesta", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_nsnc,
                                    grepl(pattern = "Ns/Nc", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_nsnc,
                                    grepl(pattern = "Chiapas Unido", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_chisunido,
                                    grepl(pattern = "Encuentro Solidario", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pencsolchis,
                                    grepl(pattern = "Mover a Chiapas", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pmchis,
                                    T ~ color_general)) |>
    pull(color) |>
    purrr::set_names(tb_respuestas |>
                       pull())
}

analizar_frecuencias <- function(diseno, pregunta){
  surveySummary_mean <- survey::svymean(survey::make.formula(pregunta),
                                        design = diseno,
                                        na.rm = TRUE)
  
  estimacion <-
    surveySummary_mean |>
    tibble::as_tibble(rownames = "respuesta") %>%
    rename(media = 2, ee = 3) %>%
    mutate(respuesta = stringr::str_replace(pattern = rlang::expr_text(ensym(pregunta)),
                                            replacement = "",
                                            string = respuesta),
           pregunta = rlang::expr_text(ensym(pregunta)),
           respuesta = stringr::str_replace_all(respuesta, " \\(No leer\\)", "")) |>
    left_join(surveySummary_mean |>
                stats::confint() %>%
                tibble::as_tibble(rownames = "respuesta") |>
                mutate(respuesta = stringr::str_replace(pattern = rlang::expr_text(ensym(pregunta)),
                                                        replacement = "",
                                                        string = respuesta),
                       pregunta = rlang::expr_text(ensym(pregunta))) |>
                rename(inf = 2, sup = 3), by = c("respuesta", "pregunta")) |>
    mutate(respuesta = forcats::fct_reorder(.f = respuesta,
                                            .x = media,
                                            .fun = max))
  return(estimacion)
}

calcular_tasa_rechazo <- function(bd_respuestas_efectivas, por_usuario = FALSE){
  bd_respuestas_efectivas %>%
    {
      if(!por_usuario) {
        summarise(.data = .,
                  rechazo = (sum(as.integer(intento_efectivo)) - n())/sum(as.integer(intento_efectivo)))
      }
      else {
        group_by(.data = .,
                 Srvyr) %>%
          summarise(rechazo = (sum(as.integer(intento_efectivo)) - n())/sum(as.integer(intento_efectivo))) |>
          arrange(desc(rechazo))
      }
    }
}

tema_morant<- function(base_family = "Poppins") {
  (ggthemes::theme_foundation(base_size = 15,
                              base_family = base_family) +
     theme(
       line = element_line(colour = "#4C5B61"),
       rect = element_rect(fill = "#FFFFFF", linetype = 0, colour = NA),
       text = element_text(color = "#2C423F"),
       axis.title = element_blank(),
       axis.text = element_text(),
       axis.text.x = element_text(size = 14),
       axis.text.y = element_text(size = 16),
       axis.ticks = element_blank(),
       axis.line.x = element_line(colour = "#E1356D"),
       legend.position = "none",
       legend.direction = "horizontal",
       legend.box = "vertical",
       legend.text = element_text(size = 14),
       panel.grid = element_line(colour = NULL),
       panel.grid.major.y = element_blank(),
       panel.grid.major.x = element_line(colour = "#C5C5C5", linetype = "dotted"),
       panel.grid.minor = element_blank(),
       plot.title = element_text(hjust = 0, size = rel(1.1), colour = "#4C5B61"),
       plot.subtitle = element_text(hjust = 0, size = rel(1), face = "bold", colour = "#C5C5C5", family = base_family),
       plot.caption = element_text(size = 14),
       plot.margin = unit(c(1, 1, 1, 1), "lines"),
       strip.text = element_text(colour ="#2C423F")
     ) 
  )
}

# Constantes ----------------------------------------------------------------------------------

color_general <- "#CF6177"
color_general_complemento <- '#61CF74'

color_morena <- "#A6032F"
color_morena_complemento <- "#F5CD5F"
color_pan <- "#0339a6"
color_mc <- "#F27405"
color_pri <- "#038C33"
color_pvem <- "#98BF5E"
color_prd <- "#F2B705"
color_pt <- "#D91136"
color_panal <- "#03A6A6"
color_pes <- "#632D79"
color_futuro <- "#2b0541"
color_hagamos <- "#8323CD"
color_fuerzaxmexico <- "#ff6392"

color_pmp <- color_panal
color_rsp <- color_prd
color_pencsolchis <- "#AE95BF"
color_chisunido <- "#0396A6"
color_rspchis <-"#D9526B"
color_pmchis <- "#6B3A8C"
color_movpro <- color_rspchis

color_otro <- "gray30"
color_nsnc <- "gray60"
color_ninguno <- "black"

gray70 <- "#B3B3B3"
PRINCIPAL <- color_morena

# vars necesarios para app

# u_nivel <- diseno$niveles %>% filter(nivel == diseno$ultimo_nivel)
# u_nivel_tipo <- u_nivel %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
# aulr <- unidades_app(diseno, u_nivel)
# 
# # cuotas
# 
# n_entrevista <- entrevistas(diseno, bd, u_nivel, u_nivel_tipo)
# hecho <- n_entrevista$hecho |>
#   left_join(preguntas$encuesta$muestra$muestra$muestra$SECCION |>
#               tidyr::unnest(cols = data) |>
#               distinct(cluster_2, region),
#             by = c("cluster" = "cluster_2"))
# 
# por_hacer <- n_entrevista$por_hacer
# 
# faltan_shp <- aulr %>%
#   left_join(hecho %>% count(!!rlang::sym(paste("cluster",u_nivel$nivel, sep = "_")) := cluster,
#                             wt =  faltan))


u_nivel <- diseno$niveles %>% filter(nivel == diseno$ultimo_nivel)
u_nivel_tipo <- u_nivel %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
aulr <- unidades_app(diseno, u_nivel)

# cuotas

n_entrevista <- entrevistas(diseno, bd, u_nivel, u_nivel_tipo)
hecho <- n_entrevista$hecho |>
  left_join(preguntas$encuesta$muestra$muestra$muestra$SECCION |>
              tidyr::unnest(cols = data) |>
              distinct(cluster_2, region),
            by = c("cluster" = "cluster_2"))

por_hacer <- n_entrevista$por_hacer

faltan_shp <- aulr %>%
  left_join(hecho %>% count(!!rlang::sym(paste("cluster",u_nivel$nivel, sep = "_")) := cluster,
                            wt =  faltan))


# Actualizar dinámicamente el rango de fechas en el UI
fechas <- enc_shp |> as_tibble() |> distinct(Date) |> pull()

# Shiny app -----------------------------------------------------------------------------------

## UI -----------------------------------------------------------------------------------------

ui <- bslib::page_navbar(
  useShinyjs(),
  title = "Edomex Tablero general",
  bslib::nav_spacer(),
  selectInput("municipio_select", "Municipio",choices = lista_mun, selected = "Atizapan De Zaragoza"),
  bslib::nav_spacer(),
  bslib::nav_panel(
    title = "Mapa principal",
    bslib::card(
      full_screen = TRUE,
      card_header("Mapa principal"),
      layout_sidebar(
        sidebar = sidebar(
          title = "Menú",
          open = "closed",
          id = "control_mapa",
          # El rango de fechas se inicializa con valores predeterminados; se actualizará dinámicamente en el servidor
          dateRangeInput(
            inputId = "mapa_fecha_input",
            label = "Rango de fechas",
            language = "es",
            separator = "a",
            format = "MM-dd",
            start = NULL,  # Será definido dinámicamente en el servidor
            end = NULL,    # Será definido dinámicamente en el servidor
            min = NULL,    # Será definido dinámicamente en el servidor
            max = NULL     # Será definido dinámicamente en el servidor
          ),
          actionButton(
            inputId = "filtrar_fechas",
            label = "Filtrar fechas"
          ),
          # El selector de cluster se inicializa vacío; se llenará dinámicamente en el servidor
          selectInput(
            inputId = "cluster",
            label = "Cluster",
            choices = NULL
          ),
          h6("Mostrar ubicación"),
          textInput(
            inputId = "coord_input",
            label = "Coordenadas",
            value = ""
          ),
          actionButton(
            inputId = "filtrar",
            label = "Buscar"
          ),
          gt_output(outputId = "faltantes")
        ),
        # El mapa principal se renderizará dinámicamente en función de los datos seleccionados
        leafletOutput(outputId = "mapa_principal")
      )
    ),
    icon = icon("map")
  ),
  
  bslib::nav_panel(
    title = "Progreso",
    bslib::card(
      full_screen = T,
      card_header("Entrevistas"),
      layout_sidebar(
        sidebar = sidebar(
          title = "Menú",
          downloadButton(
            outputId = "descargar_region",
            label = "Descargar resumen por region"),
          selectInput(
            inputId = "estrato",
            label =  "Región",
            choices = c("Todos", sort(unique(preguntas$encuesta$muestra$muestra$muestra$SECCION |>
                                               tidyr::unnest(cols = data) |>
                                               pull(region)))),
            selected = "Todos")),
        bslib::accordion(
          open = c("Progreso", "Rechazo"),
          bslib::accordion_panel(
            title = "Progreso",
            value = "Progreso",
            bslib::card_body(
              max_height = 700,
              shinyWidgets::progressBar(
                id = "enc_hechas",
                value = 0,
                display_pct = T,
                striped = T,
                total = 100,
                status = "success"),
              shinycssloaders::withSpinner(highchartOutput(outputId = "avance_region"))
            )
          ),
          bslib::accordion_panel(
            title = "Rechazo",
            value = "Rechazo",
            bslib::card_body(
              shinycssloaders::withSpinner(highchartOutput(outputId = "tasa_rechazo_global")),
              shinycssloaders::withSpinner(plotOutput("rechazo_distribucion"))
            )
          ),
          bslib::accordion_panel(
            title = "Histórico de entrevistas",
            value = "Histórico de entrevistas",
            shinycssloaders::withSpinner(
              highchartOutput(outputId = "historico")),
            bslib::value_box(
              title = "Entrevistas efectivas",
              value = textOutput(outputId = "hecho_totales"),
              bsicons::bs_icon(name = "check-square-fill"),
              showcase_layout = "top right",
              theme = value_box_theme(bg = "green")),
            bslib::value_box(
              title = "Entrevistas faltantes",
              value = textOutput(outputId = "faltantes_totales"),
              bsicons::bs_icon(name = "clock"),
              showcase_layout = "top right",
              theme = value_box_theme(bg = "yellow")),
            bslib::value_box(
              title = "Entrevistas de más",
              value = textOutput(outputId = "excedentes_totales"),
              bsicons::bs_icon(name = "exclamation-triangle"),
              showcase_layout = "top right",
              theme = value_box_theme(bg = "orange")),
            bslib::value_box(
              title = "Entrevistas eliminadas",
              value = textOutput(outputId = "eliminadas_totales"),
              bsicons::bs_icon(name = "x-octagon"),
              showcase_layout = "top right",
              theme = value_box_theme(bg = "red")),
          ),
          bslib::accordion_panel(
            title = "Balance de entrevistas",
            value = "Balance de entrevistas",
            shinycssloaders::withSpinner(highchartOutput("por_hacer")),
            shinycssloaders::withSpinner(plotOutput("por_hacer_cuotas"))
          ),
          bslib::accordion_panel(
            title = "Distribución por edad y sexo",
            value = "Distribución por edad y sexo",
            shinycssloaders::withSpinner(plotOutput("sexo")),
            shinycssloaders::withSpinner(plotOutput("rango_edad")))
        )
      )
    ),
    icon = icon("bar-chart")
  ),
  bslib::nav_panel(
    title = "Encuestadores",
    value = "Encuestadores",
    full_screen = T,
    bslib::navset_card_tab(
      selected = "General",
      title = "Encuestadores",
      full_screen = T,
      sidebar = sidebar(
        open = "open",
        selectInput(
          inputId = "estrato_encuestadores",
          label = "Region",
          choices = c("Todos", sort(unique(preguntas$encuesta$muestra$muestra$muestra$SECCION |>
                                             tidyr::unnest(cols = data) |>
                                             pull(region)))), selected = "Todos"),
        selectInput(inputId = "encuestador",
                    label = "Encuestador",
                    choices = c("Seleccionar", sort(unique(bd$Srvyr))),
                    selected = "Seleccionar")),
      bslib::nav_panel(
        title = "General",
        value = "General",
        shinycssloaders::withSpinner(highchartOutput("eliminadas_encuestador")),
        shinycssloaders::withSpinner(highchartOutput("corregidas_encuestador")),
        shinycssloaders::withSpinner(plotOutput("prom_tiempo_encuestador")),
        shinycssloaders::withSpinner(highchartOutput("duracion_entrevistas")),
        icon = icon("users")),
      bslib::nav_panel(
        title = "Individual",
        value = "Individual",
        bslib::accordion(
          open = c("Entrevistas del encuestador"),
          bslib::accordion_panel(
            title = "Entrevistas del encuestador",
            value = "Entrevistas del encuestador",
            leafletOutput(outputId = "mapa_auditoria")),
          bslib::accordion_panel(
            title = "Tasa de rechazo",
            value = "rechazo_individual",
            bslib::card_body(
              shinycssloaders::withSpinner(highchartOutput(outputId = "tasa_rechazo_ind")),
              shinycssloaders::withSpinner(plotOutput("rechazo_distribucion_ind"))
            )
          ),
          bslib::accordion_panel(
            title = "Puntaje del encuestador",
            value = "Puntaje del encuestador",
            bslib::value_box(
              title = "Entrevistas eliminadas",
              value = textOutput(outputId = "eliminadas_individual"),
              bsicons::bs_icon(name = "x-octagon"),
              showcase_layout = "top right",
              theme = value_box_theme(bg = "red")),
            bslib::value_box(
              title = "Entrevistas corregidas",
              value = textOutput(outputId = "corregidas_individual"),
              bsicons::bs_icon(name = "clock"),
              showcase_layout = "top right",
              theme = value_box_theme(bg = "orange")),
            bslib::value_box(
              title = "Entrevistas efectivas",
              value = textOutput(outputId = "efectivas_individual"),
              bsicons::bs_icon(name = "check-square-fill"),
              showcase_layout = "top right",
              theme = value_box_theme(bg = "green")))
        ),
        icon = icon("person"))),
    icon = icon("users")
    )
    
  )#panel condicional
  
  
  #,
  # bslib::nav_panel(
  #   title = "Rutas",
  #   bslib::card(
  #     full_screen = T,
  #     card_header("Ruta"),
  #     bslib::layout_sidebar(
  #       sidebar = sidebar(
  #         open = "closed",
  #         textInput(
  #           inputId = "origen",
  #           label = "Origen"),
  #         textInput(
  #           inputId = "destino",
  #           label =  "Destino"),
  #         actionButton(
  #           inputId = "ruta",
  #           label = "Obtener ruta"),
  #         actionButton(
  #           inputId = "borrar",
  #           label = "Borrar"),
  #         textInput(
  #           inputId = "link",
  #           label = "Link")
  #       ),
  #       google_mapOutput("map")
  #     )
  #   ),
  #   icon = icon("map")
  # )



## Server -------------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  observeEvent(input$municipio_select, {
    # Obtén los datos base del municipio seleccionado
    req(input$municipio_select)
    datos_base <- datos_municipios[[input$municipio_select]]
    
    
    # Asigna dinámicamente las variables
    preguntas <<- datos_base$preguntas
    diseno <<- preguntas$encuesta$muestra$muestra
    shp <<- preguntas$encuesta$shp_completo
    bd <<- preguntas$encuesta$respuestas$base
    eliminadas <<- preguntas$encuesta$respuestas$eliminadas
    eliminadas_shp <<- eliminadas %>%
      filter(!is.na(Longitude)) %>%
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
    corregidas_shp <<- preguntas$encuesta$respuestas$cluster_corregido %>%
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
    enc_shp <<- datos_base$enc_shp_car
    mapa_base <<- datos_base$mapa_base_car
    
    u_nivel <- diseno$niveles %>% filter(nivel == diseno$ultimo_nivel)
    u_nivel_tipo <- u_nivel %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
    aulr <- unidades_app(diseno, u_nivel)
    
    # cuotas
    
    n_entrevista <- entrevistas(diseno, bd, u_nivel, u_nivel_tipo)
    hecho <- n_entrevista$hecho |>
      left_join(preguntas$encuesta$muestra$muestra$muestra$SECCION |>
                  tidyr::unnest(cols = data) |>
                  distinct(cluster_2, region),
                by = c("cluster" = "cluster_2"))
    
    por_hacer <- n_entrevista$por_hacer
    
    faltan_shp <- aulr %>%
      left_join(hecho %>% count(!!rlang::sym(paste("cluster",u_nivel$nivel, sep = "_")) := cluster,
                                wt =  faltan))
    
    
    # Actualizar dinámicamente el rango de fechas en el UI
    fechas <- enc_shp |> as_tibble() |> distinct(Date) |> pull()
    updateDateRangeInput(
      session = session,
      inputId = "mapa_fecha_input",
      start = lubridate::as_date(min(fechas)),
      end = lubridate::as_date(max(fechas)),
      min = lubridate::as_date(min(fechas)),
      max = lubridate::as_date(max(fechas))
    )
    
    # Actualizar las opciones del selector de cluster en el UI
    updateSelectInput(
      session = session,
      inputId = "cluster",
      choices = c("Seleccione..." = "",
                  sort(unique(diseno$muestra[[diseno$ultimo_nivel]][[u_nivel_tipo]])))
    )
    
    
    entrevistas_efectivas <- reactive({
      input$filtrar_fechas
      
      if(enc_shp %>% filter(as.numeric(distancia) != 0) %>% nrow() > 0){
        ent_c <- enc_shp %>%
          mutate(label = paste(!!rlang::sym(u_nivel$variable), Srvyr, SbjNum, sep= "-"),
                 color = dplyr::if_else(condition = as.numeric(distancia) == 0,
                                        true = "#7BF739",
                                        false = "purple")
          ) %>%
          arrange(distancia)
      } else {
        ent_c <- enc_shp %>%
          mutate(label = paste(!!rlang::sym(u_nivel$variable), Srvyr, SbjNum, sep= "-"),
                 color = "#7BF739")
      }
      shp_efectivas <- ent_c |>
        mutate(fecha = lubridate::as_date(Date)) |>
        filter(lubridate::as_date(isolate(input$mapa_fecha_input[1])) <= fecha) |>
        filter(fecha <= lubridate::as_date(isolate(input$mapa_fecha_input[2])))
      
      return(list(shp_efectivas))
      
    })
    
    # Renderizar el mapa dinámicamente
    output$mapa_principal <- renderLeaflet({
      
      nombres_region <- diseno$poblacion$marco_muestral |>
        distinct(region, strata_1) |>
        arrange(region) |>
        mutate(nombre_region = paste("Región ", strata_1, sep = ""))
      
      avance_clusters <- hecho |>
        group_by(cluster) |>
        summarise(across(.cols = c(hecho, cuota), .fns = ~ sum(.x, na.rm = T))) |>
        mutate(pct = hecho/cuota,
               cuartil = dplyr::case_when(pct <= 0.25 ~ "<=50%",
                                          0.5 < pct & pct <= 1.0 ~ "50% <= 100%",
                                          T ~ "Excedida"))
      
      faltan_shp <- faltan_shp |>
        left_join(avance_clusters, by = c("cluster_2" = "cluster"))
      
      pal_region <- leaflet::colorFactor(palette = topo.colors(n_distinct(nombres_region$nombre_region)), domain = unique(nombres_region$nombre_region))
      
      pal_efectivas <- leaflet::colorFactor(palette = c("#7BF739", "purple"), domain = c("Dentro", "Fuera"))
      
      pal_faltantes <- leaflet::colorFactor(palette = c("red", "green", "orange"), levels = c("<=50%", "50% <= 100%", "Excedida"), domain = faltan_shp$cuartil, ordered = T)
      
      color_rezagoAlto = "#e7298a"
      color_rezagoMedioalto = "#fb8072"
      color_rezagoMedio = "#e6ab02"
      color_rezagoBajo = "#1b9e77"
      
      pal_rezago <- leaflet::colorFactor(palette = c(color_rezagoAlto, color_rezagoMedioalto, color_rezagoMedio, color_rezagoBajo),
                                         levels = c("Alto rezago", "Medio-alto rezago", "Medio rezago", "Bajo rezago"),
                                         domain = shp_secciones_rezago$rezago, ordered = T, na.color =  "#B3B3B3")
      
      map <-
        mapa_base %>%
        left_join(nombres_region |> select(strata_1, nombre_region), by = "strata_1") |>
        leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=r&x={x}&y={y}&z={z}",
                 attribution = '© Google') |>
        addPolygons(color = ~pal_region(nombre_region),
                    opacity = 1,
                    fill = T,
                    fillOpacity = 0.1) %>%
        # addLegend(pal = pal_region,
        #           values = ~nombre_region,
        #           position = "bottomleft",
        #           title = "Región") %>%
        graficar_mapa_clusters(bd = diseno$muestra,
                               # nivel = u_nivel %>% pull(variable),
                               nivel = 2,
                               muestra = diseno$muestra$SECCION %>% tidyr::unnest(data),
                               shp = shp$shp) %>%
        graficar_mapa_mza(bd = diseno$muestra,
                          # nivel = u_nivel %>% pull(variable),
                          nivel = "MANZANA",
                          muestra = diseno$muestra$SECCION %>% tidyr::unnest(data),
                          shp = shp$shp) %>%
        addPolygons(data = faltan_shp,
                    fillColor = ~ pal_faltantes(cuartil),
                    fillOpacity = 1,
                    stroke = F,
                    label = ~glue::glue("Cuota cubierta: {scales::percent(pct, accuracy = 1.)} Entrevistas faltantes: {n}"),
                    group = "Encuestas faltantes") %>%
        # addLegend(pal = pal_faltantes,
        #           values = faltan_shp$cuartil,
        #           title = "Cuota cubierta",
        #           group = "Encuestas faltantes",
        #           position = "bottomleft") %>%
        addCircleMarkers(data = entrevistas_efectivas()[[1]],
                         color = ~color,
                         stroke = F,
                         label = ~label,
                         group = "Entrevistas") %>%
        # addLegend(position = "bottomleft",
        #           pal = pal_efectivas,
        #           values = entrevistas_efectivas()[[1]]$interior_cluster,
        #           na.label = "Indefinido",
        #           title = "Dentro de cluster") %>%
        addCircleMarkers(data = corregidas_shp,
                         stroke = F,
                         color = "yellow",
                         fillOpacity = 1,
                         popup = ~glue::glue("{SbjNum} - {Srvyr} - {Date} ≤<br> cluster reportado: {anterior} <br> cluster corregido: {nueva}"),
                         group = "Cluster corregido",
                         clusterOptions = markerClusterOptions())
      
      if (nrow(eliminadas_shp) > 0) {
        
        map <- map |>
          addCircleMarkers(data = eliminadas_shp,
                           stroke = F,
                           color = "#FF715B",
                           fillOpacity = 1,
                           label = ~glue::glue("{SbjNum} - {Srvyr}"),
                           group = "Eliminadas",
                           clusterOptions = markerClusterOptions())
      }
      
      map <- map %>%
        addLayersControl(baseGroups = c("Entrevistas", "Eliminadas", "Cluster corregido"),
                         overlayGroups = c("Encuestas faltantes"),
                         options = layersControlOptions(),
                         position = "bottomright") %>%
        hideGroup(c("Rezago", "Encuestas faltantes"))
      
      return(map)
      
    })
    
    cluster_actual <- reactiveVal(value = "")
    
    proxy_mapa_principal <- leafletProxy("mapa_principal")
    
    observeEvent(input$filtrar, {
      
      if(input$cluster != cluster_actual()) {
        
        if(input$cluster != "") {
          
          if(!(is.na(coordenadas()[1]$latitud) & is.na(coordenadas()[2]$longitud))) {
            
            bbox <- aulr %>%
              filter(!!rlang::sym(u_nivel_tipo) == !!input$cluster) %>%
              sf::st_bbox()
            
            proxy_mapa_principal %>%
              flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>%
              addMarkers(lat = coordenadas()[1]$latitud, lng = coordenadas()[2]$longitud)
            
            cluster_actual(input$cluster)
            
          } else {
            
            bbox <- aulr %>%
              filter(!!rlang::sym(u_nivel_tipo) == !!input$cluster) %>%
              sf::st_bbox()
            
            proxy_mapa_principal %>%
              flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
            
            cluster_actual(input$cluster)
            
          }
          
        } else {
          
          if(!(is.na(coordenadas()[1]$latitud) & is.na(coordenadas()[2]$longitud))) {
            
            proxy_mapa_principal %>%
              flyToBounds(bbox_qro[[1]], bbox_qro[[2]], bbox_qro[[3]], bbox_qro[[4]]) %>%
              addMarkers(lat = coordenadas()[1]$latitud, lng = coordenadas()[2]$longitud)
            
            cluster_actual(input$cluster)
            
          } else {
            
            proxy_mapa_principal %>%
              flyToBounds(bbox_qro[[1]], bbox_qro[[2]], bbox_qro[[3]], bbox_qro[[4]])
            
            cluster_actual(input$cluster)
            
          }
          
        }
        
      } else {
        
        if(input$cluster != "") {
          
          if(!(is.na(coordenadas()[1]$latitud) & is.na(coordenadas()[2]$longitud))) {
            
            proxy_mapa_principal %>%
              addMarkers(lat = coordenadas()[1]$latitud, lng = coordenadas()[2]$longitud)
            
            cluster_actual(input$cluster)
            
            
          } else {
            
            proxy_mapa_principal
            
            cluster_actual(input$cluster)
            
          }
          
        } else {
          
          if(!(is.na(coordenadas()[1]$latitud) & is.na(coordenadas()[2]$longitud))) {
            
            proxy_mapa_principal %>%
              addMarkers(lat = coordenadas()[1]$latitud, lng = coordenadas()[2]$longitud)
            
            cluster_actual(input$cluster)
            
            
          } else {
            
            proxy_mapa_principal
            
            cluster_actual(input$cluster)
            
          }
          
        }
        
      }
      
    })
    observeEvent(input$regresar, {
      proxy_mapa_principal %>%
        flyToBounds(bbox_qro[[1]],bbox_qro[[2]],bbox_qro[[3]],bbox_qro[[4]])
    })
    
    balance_cluster <- reactive({
      input$filtrar
      
      balance_cluster <- hecho %>%
        filter(cluster == !!isolate(input$cluster)) |>
        group_by(cluster) |>
        summarise(across(.cols = c(cuota, hecho, faltan), .fns = ~ sum(.x)))
      
      hecho %>%
        filter(cluster == !!isolate(input$cluster)) %>%
        select(sexo, edad, faltan) %>%
        mutate(edad = gsub(pattern = "([0-9])([A-Z])([0-9])", replacement = "\\1 a \\3", x = edad),
               edad = gsub(pattern = "Y", replacement = " y ", x = edad)) |>
        tidyr::pivot_wider(names_from = sexo, values_from = faltan) %>%
        tidyr::replace_na(list(Mujer = 0, Hombre = 0)) %>%
        rename(Edad = edad)
    })
    
    output$faltantes <- render_gt({
      req(nrow(balance_cluster()) > 0)
      balance_cluster() %>%
        gt() %>%
        tab_header(title = md(glue::glue("**Cluster {isolate(input$cluster)}**")),
                   subtitle = glue::glue("Cuota: {balance_cluster()$cuota}   Hecho: {balance_cluster()$hecho}   Faltan: {balance_cluster()$faltan}")) |>
        tab_spanner(label = "Entrevistas faltanes por edad y sexo", columns = c(Edad, Hombre, Mujer))
    })
    
    coordenadas <- reactive({
      input$filtrar
      
      coord <- tidyr::separate(data = tibble(coord = isolate(input$coord_input)), col = coord, into = c("lat", "lon"), sep = ", ")
      
      list(latitud = as.double(coord$lat),
           longitud = as.double(coord$lon))
    })
#####################
    # Pestana "Progreso" --------------------------------------------------------------------------
    
    observe({
      # Simula el cálculo del progreso
      
      value_tots <- nrow(bd) 
      progreso <- ((diseno$niveles %>% filter(nivel == 0) %>% pull(unidades)) * diseno$n_0) 
      
      # Actualiza la barra de progreso
      shinyWidgets::updateProgressBar(
        session = session,
        id = "enc_hechas",
        total = progreso,
        value = value_tots
      )
    })
    
    efectivas_filter <- eventReactive(c(bd, input$estrato),{
      
      bd %>%
        {
          if(input$estrato != "Todos"){
            filter(., region == input$estrato)
          } else{
            .
          }}
    })
    
    eliminadas_filter <- eventReactive(c(eliminadas, input$estrato),{
      
      eliminadas %>%
        mutate(cluster = as.integer(cluster)) |>
        left_join(diseno$poblacion$marco_muestral %>%
                    distinct(cluster_2, region),
                  by = c("cluster" = "cluster_2")) %>%
        {
          if(input$estrato != "Todos"){
            filter(., region == input$estrato)
          } else{
            .
          }}
    })
    
    corregidas_filter <- eventReactive(c(corregidas_shp, input$estrato),{
      
      corregidas_shp %>%
        as_tibble %>%
        left_join(bd %>%
                    distinct(SbjNum, region),
                  by = "SbjNum") %>%
        {
          if(input$estrato != "Todos"){
            filter(., region == input$estrato)
          } else{
            .
          }}
    })
    
    por_hacer_filter <- eventReactive(c(por_hacer, input$estrato),{
      
      por_hacer %>%
        left_join(bd %>%
                    distinct(cluster_2, region),
                  by = c("cluster" = "cluster_2")) %>%
        {
          if(input$estrato != "Todos"){
            filter(., region == input$estrato)
          } else{
            .
          }}
    })
    
    hecho_filter <- eventReactive(c(hecho, input$estrato),{
      
      hecho %>%
        {
          if(input$estrato != "Todos"){
            filter(., region == input$estrato)
          } else{
            .
          }}
    })
    
    ## Avance -------------------------------------------------------------------------------------
    
    output$avance_region <- renderHighchart({
      
      clusters_en_muestra <- diseno$poblacion$marco_muestral |>
        distinct(strata_1, region, cluster_2)
      
      datos_de_levantamiento <- por_hacer |>
        group_by(cluster) |>
        summarise(across(.cols = c(cuota, hecho, por_hacer), .fns = ~ sum(.x)))
      
      bd_plot <- clusters_en_muestra |>
        inner_join(datos_de_levantamiento, by = c("cluster_2" = "cluster")) |>
        group_by(region, strata_1) |>
        summarise(across(.cols = c(cuota, hecho, por_hacer), .fns = ~ sum(.x))) |>
        mutate(pct = hecho/cuota) |>
        arrange(desc(pct)) |>
        mutate(por_hacer = pmax(0, por_hacer),
               region = paste("Región ", strata_1, sep = ""))
      
      g <- highchart() |>
        hc_xAxis(categories = bd_plot$region, labels = list(style = list(fontSize = "18px"))) |>
        hc_yAxis(labels = list(style = list(fontSize = "18px"))) |>
        hc_add_series(name = "Faltante", data = bd_plot$por_hacer, type = "bar", color = gray70, zIndex = 1, stacking = "normal") |>
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, inside = FALSE, format = "{point.y}", style = list(fontSize = "24px"))), align = "right") |>
        hc_add_series(name = "Hecho", data = bd_plot$hecho, type = "bar", color = PRINCIPAL, zIndex = 2, stacking = "normal") |>
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, inside = TRUE, format = "{point.y}", style = list(fontSize = "24px")))) |>
        hc_legend(itemStyle = list(fontSize = "24px", reversed = TRUE))
      
      return(g)
      
    })
    
    output$tasa_rechazo_global <- renderHighchart({
      
      col_stops <-
        data.frame(
          q = c(0.15, 0.4, .8),
          c = c('#55BF3B', '#DDDF0D', '#DF5353'),
          stringsAsFactors = FALSE
        )
      
      highchart() %>%
        hc_chart(type = "solidgauge") %>%
        hc_pane(
          startAngle = -90,
          endAngle = 90,
          background = list(
            outerRadius = '100%',
            innerRadius = '60%',
            shape = "arc")) %>%
        hc_tooltip(enabled = FALSE) %>%
        hc_yAxis(
          stops = list_parse2(col_stops),
          lineWidth = 0,
          minorTickWidth = 0,
          tickAmount = 2,
          min = 0,
          max = 100,
          labels = list(y = 26,
                        style = list(fontSize = "22px"),
                        formatter = JS("function() { return this.value + '%'; }")
          )
        ) %>%
        hc_add_series(
          data = round(calcular_tasa_rechazo(
            preguntas$encuesta$muestra$diseno$variables,
            por_usuario = FALSE) |>
              pull()*100, digits = 0),
          dataLabels = list(
            y = -50,
            borderWidth = 0,
            useHTML = TRUE,
            format = '{y}%',  # Aquí se agrega el símbolo de porcentaje
            style = list(fontSize = "40px")
          )
        )
    })
    
    output$rechazo_distribucion <- renderPlot({
      
      preguntas$encuesta$muestra$diseno$variables |>
        select(SbjNum, intento_efectivo) |>
        mutate(intento_efectivo = as.integer(intento_efectivo)) |>
        count(intento_efectivo) |>
        ggplot(aes(x = intento_efectivo,
                   y = n,
                   fill = color_general)) +
        geom_col() +
        geom_text(aes(label = n), size = 10, nudge_y = 20) +
        tema_morant() +
        scale_x_continuous(breaks = seq.int(from = 0,
                                            to = 15,
                                            by = 1)) +
        labs(x = "Intentos de levantar la encuesta",
             y = "Encuestas efectivas") +
        theme(axis.title.x = element_text(),
              axis.title.y = element_text())
    })
    
    ## Histórico ---------------------------------------------------------------
    
    output$historico <- renderHighchart({
      
      graficar_historico(bd_efectivas = efectivas_filter(),
                         bd_eliminadas = eliminadas_filter(),
                         bd_corregidas = corregidas_filter())
      
    })
    
    output$hecho_totales <- renderText({
      
      hecho_filter() %>%
        summarise(hecho = sum(hecho)) |>
        pull() |>
        scales::comma()
      
    })
    
    output$faltantes_totales <- renderText({
      
      res <- por_hacer_filter() %>%
        summarise(por_hacer = sum(por_hacer)) |>
        pull() |>
        scales::comma()
      
      res <- pmax(0, res)
      
      return(res)
      
    })
    
    output$excedentes_totales <- renderText({
      
      res <- hecho_filter() %>%
        summarise(excedentes = sum(faltan)) |>
        mutate(excedentes = abs(pmin(0, excedentes))) |>
        pull() |>
        scales::comma()
      
      return(res)
      
    })
    
    output$eliminadas_totales <- renderText({
      
      res <- eliminadas_filter() %>%
        nrow() |>
        scales::comma()
      
      return(res)
      
    })
    
    ## Balance de entrevistas -------------------------------------------------
    
    output$por_hacer <- renderHighchart({
      
      bd_inicial <- por_hacer_filter() %>%
        count(cluster, wt = por_hacer, name = "total") |>
        mutate(cluster = as.character(cluster),
               tipo = dplyr::case_when(total < 0 ~ "Faltantes",
                                       total > 0 ~ "Excedidos",
                                       total == 0 ~ "Completos"))
      
      bd_categoricas <- bd_inicial |>
        count(tipo, name = "total") |>
        mutate(tipo = factor(x = tipo, levels = rev(c("Faltantes", "Excedidos", "Completos"))),
               color = case_when(tipo == "Faltantes" ~  "#FF0000",
                                 tipo == "Excedidos" ~ "#FFA500",
                                 tipo == "Completos" ~ "#0000FF"))
      
      bd_drilldown <- bd_inicial |>
        group_nest(tipo) |>
        mutate(tipo = factor(x = tipo, levels = rev(c("Faltantes", "Excedidos", "Completos"))),
               id = tipo,
               type = "bar",
               data = purrr::map(.x = data, .f = mutate, name = cluster, y = total),
               data = map(data, list_parse))
      
      g <- hchart(
        object = bd_categoricas,
        type = "bar",
        hcaes(x = tipo, y = total, name = tipo, color = color, drilldown = tipo),
        name = "Entrevistas",
        colorByPoint = TRUE) |>
        hc_drilldown(
          allowPointDrilldown = TRUE,
          series = list_parse(bd_drilldown)) |>
        hc_xAxis(
          title = ""
        ) |>
        hc_yAxis(
          title = "",
          labels = list(enable = FALSE)) |>
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, inside = FALSE, format = "{point.y}", style = list(fontSize = "24px"))))
      
      return(g)
      
    })
    
    output$por_hacer_cuotas <- renderPlot({
      
      pal_p <- leaflet::colorNumeric("Blues", domain = hecho_filter() %>% filter(faltan>0) %>% pull(faltan) %>% unique %>% sort)
      pal_n <- leaflet::colorNumeric("Reds", domain = hecho_filter() %>% filter(faltan<0) %>% pull(faltan) %>% unique %>% sort)
      uno <- pal_p(hecho_filter() %>% filter(faltan>0) %>% pull(faltan) %>% unique %>% sort)
      dos <- pal_n(hecho_filter() %>% filter(faltan<0) %>% pull(faltan) %>% unique %>% sort %>% rev)
      
      hecho_filter() %>% mutate(grupo = glue::glue("{edad} {sexo}")) %>%
        group_by(cluster) %>% mutate(total = sum(faltan)) %>%
        ungroup %>% mutate(cluster = reorder(cluster, total)) %>%
        ggplot(aes(y = cluster, x = grupo,
                   fill = factor(faltan))) +
        geom_tile() +
        geom_text(aes(label = faltan ), alpha = .5) +
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, 6)) +
        scale_fill_manual(values = c(dos, "white", uno)) +
        labs(fill = "Entrevistas \n por hacer", y = NULL, x = NULL) + theme_minimal()
    })
    
    ## Distribución por edad y sexo -------------------------------------------
    
    output$sexo <- renderPlot({
      preguntas$encuesta$muestra$revisar_sexo()
      
    })
    
    output$rango_edad <- renderPlot({
      preguntas$encuesta$muestra$revisar_rango_edad()
    })
    
    output$descargar_region <- downloadHandler(filename = function(){
      paste("avance_regional_", format(Sys.time(), "%Y_%m_%d-%H_%M"), ".xlsx", sep = "")
    },
    content = function(file){
      
      clusters_en_muestra <-
        diseno$poblacion$marco_muestral |>
        distinct(strata_1, region, cluster_2)
      
      datos_de_levantamiento <-
        por_hacer |>
        group_by(cluster) |>
        summarise(across(.cols = c(cuota, hecho, por_hacer), .fns = ~ sum(.x)))
      
      datos_de_levantamiento_mun <-
        por_hacer |>
        group_by(cluster, Municipio) |>
        summarise(across(.cols = c(cuota, hecho, por_hacer), .fns = ~ sum(.x)))
      
      bd_region <-
        clusters_en_muestra |>
        inner_join(datos_de_levantamiento, by = c("cluster_2" = "cluster")) |>
        group_by(region) |>
        summarise(across(.cols = c(cuota, hecho, por_hacer), .fns = ~ sum(.x)))
      
      bd_region_municipio <-
        clusters_en_muestra |>
        inner_join(datos_de_levantamiento_mun, by = c("cluster_2" = "cluster")) |>
        relocate(Municipio, .after = region) |>
        group_by(region, Municipio) |>
        summarise(across(.cols = c(cuota, hecho, por_hacer), .fns = ~ sum(.x))) |>
        ungroup()
      
      bd_region_municipio_cluster <-
        clusters_en_muestra |>
        inner_join(datos_de_levantamiento_mun, by = c("cluster_2" = "cluster")) |>
        relocate(Municipio, .after = region) |>
        group_by(region, Municipio, cluster_2) |>
        summarise(across(.cols = c(cuota, hecho, por_hacer), .fns = ~ sum(.x))) |>
        ungroup()
      
      wb <- openxlsx::createWorkbook()
      
      openxlsx::addWorksheet(wb, sheetName = "region")
      openxlsx::writeData(wb, bd_region, sheet = "region")
      
      openxlsx::addWorksheet(wb, sheetName = "municipio")
      openxlsx::writeData(wb, bd_region_municipio, sheet = "municipio")
      
      openxlsx::addWorksheet(wb, sheetName = "cluster")
      openxlsx::writeData(wb, bd_region_municipio_cluster, sheet = "cluster")
      
      openxlsx::saveWorkbook(wb, file = file)
      
    },
    contentType = "file/xlsx"
    )
#####################3
    # Pestaña "Encuestadores" -------------------------------------------------
    
    ## Estadisticas colectivas ------------------------------------------------
    
    efectivas_filter_encuestadores <- eventReactive(c(bd, input$estrato_encuestadores),{
      
      bd %>%
        {
          if(input$estrato_encuestadores != "Todos"){
            filter(., region == input$estrato_encuestadores)
          } else{
            .
          }}
    })
    
    eliminadas_filter_encuestadores <- eventReactive(c(eliminadas, input$estrato_encuestadores),{
      
      eliminadas %>%
        mutate(cluster = as.integer(cluster)) |>
        left_join(diseno$poblacion$marco_muestral %>%
                    distinct(cluster_2, region),
                  by = c("cluster" = "cluster_2")) %>%
        {
          if(input$estrato_encuestadores != "Todos"){
            filter(., region == input$estrato_encuestadores)
          } else{
            .
          }}
    })
    
    corregidas_filter_encuestadores <- eventReactive(c(corregidas_shp, input$estrato_encuestadores),{
      
      corregidas_shp %>% as_tibble %>%
        left_join(bd %>% distinct(SbjNum, region), by = "SbjNum") %>%
        {
          if(input$estrato_encuestadores != "Todos"){
            filter(., region == input$estrato_encuestadores)
          } else{
            .
          }}
    })
    
    por_hacer_filter_encuestadores <- eventReactive(c(por_hacer, input$estrato_encuestadores),{
      
      por_hacer %>%
        {
          if(input$estrato_encuestadores != "Todos"){
            filter(., region == input$estrato_encuestadores)
          } else{
            .
          }}
    })
    
    hecho_filter_encuestadores <- eventReactive(c(hecho, input$estrato_encuestadores),{
      
      hecho %>%
        {
          if(input$estrato_encuestadores != "Todos"){
            filter(., region == input$estrato_encuestadores)
          } else{
            .
          }}
    })
    
    indice_eliminadas <- reactiveVal(1)
    indice_corregidas <- reactiveVal(1)
    indice_promedio <- reactiveVal(1)
    
    observeEvent(input$siguiente_eliminadas,{
      indice_eliminadas(indice_eliminadas() + 1)
    })
    
    observeEvent(input$siguiente_corregidas,{
      indice_corregidas(indice_corregidas() + 1)
    })
    
    observeEvent(input$siguiente_promedio,{
      indice_promedio(indice_promedio() + 1)
    })
    
    output$eliminadas_encuestador <- renderHighchart({
      
      lista <-
        eliminadas_filter_encuestadores() %>%
        count(Srvyr) %>%
        arrange(desc(n)) %>%
        tibble::rownames_to_column("id") %>%
        mutate(grupo = ((as.numeric(id)-1) %/% 7)+1) %>%
        split(.$grupo)
      
      pag <- indice_eliminadas() %% (length(lista)+1)
      
      if(pag == 0) {
        indice_eliminadas(indice_eliminadas() + 1)
        pag <- indice_eliminadas() %% (length(lista)+1)
      }
      
      aux <- lista %>% purrr::pluck(pag) %>% select(Srvyr, n)
      
      g <- graficar_barras(bd = aux, color = "red")
      
      return(g)
      
    })
    
    output$corregidas_encuestador <- renderHighchart({
      
      lista <- corregidas_filter_encuestadores() %>%
        count(Srvyr) %>%
        arrange(desc(n)) %>%
        tibble::rownames_to_column("id") %>%
        mutate(grupo = ((as.numeric(id)-1) %/% 7)+1) %>%
        split(.$grupo)
      
      pag <- indice_corregidas() %% (length(lista)+1)
      
      if(pag == 0) {
        indice_corregidas(indice_corregidas() + 1)
        pag <- indice_corregidas() %% (length(lista)+1)
      }
      
      aux <- lista %>% purrr::pluck(pag) %>% select(Srvyr, n)
      
      g <- graficar_barras(bd = aux, color = "orange")
      
      return(g)
      
    })
    
    output$duracion_entrevistas <- renderHighchart({
      
      bd_plot <- efectivas_filter_encuestadores() %>%
        mutate(duracion = as.double(VEnd - VStart))
      
      g <- hchart(density(bd_plot$duracion),
                  color = 'teal',
                  name = 'Distribución de duración de las entrevistas',
                  type = "line") %>%
        hc_plotOptions(series = list(animation = FALSE)) |>
        hc_yAxis(title = "Duración") |>
        hc_yAxis(
          labels = list(
            formatter = JS("function() { return this.value * 100 + '%'; }")
          ),
          tickInterval = 0.05
        )
      
      return(g)
      
    })
    
    output$prom_tiempo_encuestador <- renderPlot({
      
      duracion_promedio <- efectivas_filter_encuestadores() %>% transmute(Srvyr,
                                                                          duracion = as.double(VEnd - VStart)) %>%
        filter(duracion <= 60) %>%
        summarise(duracion_promedio = mean(duracion)) %>% pull(duracion_promedio)
      
      lista <- efectivas_filter_encuestadores() %>%
        transmute(Srvyr,
                  duracion = as.double(VEnd - VStart)) %>%
        filter(duracion <= 60) %>%
        group_by(Srvyr) %>%
        mutate(promedio = mean(duracion),
               min = min(duracion),
               max = max(duracion),
               sd = sd(duracion),
               mediana = median(duracion)) %>%
        select(!duracion) %>%
        ungroup() %>%
        distinct(.keep_all = T) %>%
        arrange(promedio) %>%
        tibble::rownames_to_column("id") %>%
        mutate(grupo = ((as.numeric(id)-1) %/% 7)+1) %>%
        split(.$grupo)
      
      pag <- indice_promedio() %% (length(lista)+1)
      
      if(pag == 0) {
        indice_promedio(indice_promedio() + 1)
        pag <- indice_promedio() %% (length(lista)+1)
      }
      
      aux <- lista %>% purrr::pluck(pag)
      
      g <- aux %>%
        ggplot(aes(x = reorder(str_to_title(Srvyr), -promedio), y = promedio)) +
        geom_point(size = 2) +
        geom_errorbar(aes(ymin = min, ymax = max)) +
        geom_hline(yintercept = duracion_promedio, show.legend = "Duración promedio", na.rm = T, color = "brown") +
        coord_flip() +
        scale_y_continuous(breaks = seq.int(from = 0, to = 60, by = 5)) +
        labs(x = NULL, y = "Minutos", title = "Duración promedio de las entrevistas") +
        theme_minimal() +
        theme(legend.position = "none", axis.text = element_text(size = 14))
      
      return(g)
      
    })
    
    output$eliminadas <- renderDT({
      
      eliminadas_filter_encuestadores() %>% select(SbjNum, Fecha= Date, Encuestador = Srvyr) %>%
        # bind_rows(
        #   bd %>% filter(is.na(Longitude)) %>% select(SbjNum, Fecha = Date, Encuestador = Srvyr) %>%
        #     mutate(Razón = "GPS apagado")
        # ) %>%
        arrange(desc(Fecha))
    }, options = list(dom = "ltpi",
                      language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")))
    
    ## Estadísticas individuales ----------------------------------------------
    
    ### Mapa --------------------------------------------------------------------------------------
    
    output$mapa_auditoria <- renderLeaflet({
      
      req(input$encuestador != "Seleccionar")
      
      nombres_region <- diseno$poblacion$marco_muestral |>
        distinct(region, strata_1) |>
        arrange(region) |>
        mutate(nombre_region = paste(strata_1, region, sep = " "))
      
      pal <- colorFactor(topo.colors(n_distinct(nombres_region$nombre_region)),
                         domain = unique(nombres_region$nombre_region))
      
      if(enc_shp %>% filter(as.numeric(distancia) != 0) %>% nrow() > 0){
        
        efectivas_encuestador <- preguntas$encuesta$muestra$diseno$variables |>
          as_tibble() |>
          filter(Srvyr == input$encuestador) |>
          pull(SbjNum)
        
        ent_c <- enc_shp %>%
          filter(Srvyr == input$encuestador) |>
          mutate(label = paste(!!rlang::sym(u_nivel$variable), Srvyr, SbjNum, sep= " - "),
                 color = case_when(SbjNum %in% efectivas_encuestador ~ "green",
                                   T ~ "sin categoria")
          ) %>%
          arrange(distancia)
        
      } else {
        
        ent_c <- enc_shp %>%
          mutate(label = paste(!!rlang::sym(u_nivel$variable), Srvyr, SbjNum,sep= "-"),
                 color = "green")
        
      }
      
      pal_n <- leaflet::colorNumeric(colorRampPalette(c("red", "white", "blue"))(3),
                                     domain = faltan_shp$n)
      
      mapa_auditoria <- mapa_base %>%
        left_join(nombres_region |>
                    select(strata_1, nombre_region), by = "strata_1") |>
        leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(color = ~pal(strata_1), opacity = 1, fill = T, fillOpacity = 0.1) %>%
        addPolygons(color = ~pal(nombre_region), opacity = 1, fill = T, fillOpacity = 0.1) %>%
        addLegend(pal = pal, values = ~nombre_region, position = "bottomleft", title = "Región") %>%
        shp$graficar_mapa(bd = diseno$muestra, nivel = u_nivel %>% pull(variable)) %>%
        addCircleMarkers(data = ent_c,
                         color = ~color,
                         stroke = F,
                         label = ~label,
                         group = "Entrevistas") %>%
        addCircleMarkers(data = corregidas_shp %>%
                           filter(Srvyr == input$encuestador),
                         stroke = F,
                         color = "yellow",
                         fillOpacity = 1,
                         popup = ~glue::glue("{SbjNum} - {Srvyr} - {Date} ≤<br> cluster reportado: {anterior} <br> cluster corregido: {nueva}"),
                         group = "Cluster corregido")
      
      if(nrow(eliminadas_shp %>% filter(Srvyr == input$encuestador)) != 0) {
        
        mapa_auditoria <- mapa_auditoria %>%
          addCircleMarkers(data = eliminadas_shp %>%
                             filter(Srvyr == input$encuestador),
                           stroke = F, color = "red", fillOpacity = 1,
                           label = ~glue::glue("{SbjNum} - {Srvyr}"),
                           group = "Eliminadas")
        
      }
      
      return(mapa_auditoria)
      
    })
    
    proxy <- leafletProxy("mapa_auditoria")
    
    ### Rechazo -----------------------------------------------------------------------------------
    
    output$tasa_rechazo_ind <- renderHighchart({
      
      req(input$encuestador != "Seleccionar")
      
      col_stops <-
        data.frame(
          q = c(0.15, 0.4, .8),
          c = c('#55BF3B', '#DDDF0D', '#DF5353'),
          stringsAsFactors = FALSE
        )
      
      highchart() %>%
        hc_chart(type = "solidgauge") %>%
        hc_pane(
          startAngle = -90,
          endAngle = 90,
          background = list(
            outerRadius = '100%',
            innerRadius = '60%',
            shape = "arc")) %>%
        hc_tooltip(enabled = FALSE) %>%
        hc_yAxis(
          stops = list_parse2(col_stops),
          lineWidth = 0,
          minorTickWidth = 0,
          tickAmount = 2,
          min = 0,
          max = 100,
          labels = list(y = 26,
                        style = list(fontSize = "22px"),
                        formatter = JS("function() { return this.value + '%'; }")
          )
        ) %>%
        hc_add_series(
          data = round(calcular_tasa_rechazo(
            preguntas$encuesta$muestra$diseno$variables,
            por_usuario = TRUE) |>
              filter(Srvyr == input$encuestador) |>
              pull()*100, digits = 0),
          dataLabels = list(
            y = -50,
            borderWidth = 0,
            useHTML = TRUE,
            format = '{y}%',  # Aquí se agrega el símbolo de porcentaje
            style = list(fontSize = "40px")
          )
        )
    })
    
    output$rechazo_distribucion_ind <- renderPlot({
      
      req(input$encuestador != "Seleccionar")
      
      preguntas$encuesta$muestra$diseno$variables |>
        filter(Srvyr == input$encuestador) |>
        select(SbjNum, intento_efectivo) |>
        mutate(intento_efectivo = as.integer(intento_efectivo)) |>
        count(intento_efectivo) |>
        ggplot(aes(x = intento_efectivo,
                   y = n,
                   fill = color_general)) +
        geom_col() +
        geom_text(aes(label = n), size = 10, nudge_y = 20) +
        tema_morant() +
        scale_x_continuous(breaks = seq.int(from = 0,
                                            to = 15,
                                            by = 1)) +
        labs(x = "Intentos de levantar la encuesta",
             y = "Encuestas efectivas") +
        theme(axis.title.x = element_text(),
              axis.title.y = element_text())
    })
    
    ### Puntaje del encuestador -------------------------------------------------------------------
    
    output$eliminadas_individual <- renderText({
      
      req(input$encuestador != "Seleccionar")
      
      aux <- eliminadas %>%
        count(Srvyr) %>%
        filter(Srvyr == input$encuestador)
      
      if(nrow(aux) != 0) {
        
        res <- aux %>% pull(n)
        
      }
      
      if(nrow(aux) == 0) {
        
        res <- nrow(aux)
        
      }
      
      res <- pmax(0, res)
      
      return(res)
      
    })
    
    output$corregidas_individual <- renderText({
      
      req(input$encuestador != "Seleccionar")
      
      res <- corregidas_shp %>%
        as_tibble %>%
        left_join(bd %>% distinct(SbjNum, region), by = "SbjNum") %>%
        count(Srvyr) %>%
        tidyr::complete(Srvyr = preguntas$encuesta$muestra$diseno$variables |> distinct(Srvyr) |> pull(),
                        fill = list(n = 0)) |>
        filter(Srvyr == input$encuestador) %>%
        pull(n)
      
      res <- pmax(0, res)
      
      return(res)
      
    })
    
    output$efectivas_individual <- renderText({
      
      req(input$encuestador != "Seleccionar")
      
      res <- bd %>%
        count(Srvyr) %>%
        filter(Srvyr == input$encuestador) %>%
        pull(n)
      
      res <- pmax(0, res)
      
      return(res)
      
    })
    
    output$duracion_individual <- renderPlot({
      
      g <- efectivas_filter_encuestadores() %>%
        transmute(duracion = as.double(VEnd - VStart)) %>%
        filter(duracion <= 60) %>%
        ggplot(aes(x = duracion)) +
        geom_histogram(bins = 120, fill = "blue") +
        labs(x = "Duración (minutos)", y = "Entrevistas", title = "Duración de las entrevistas") +
        theme_minimal()
      
      return(g)
      
    })
    
    
    
    
    
  })##############
  
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
#shiny::runApp(port = 3838)
