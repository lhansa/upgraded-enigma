library(tidyverse)
library(RJSONIO)
library(glue)

mes_id <- "201805"

fichero_zip <- glue("data/{mes_id}_Usage_Bicimad.zip")
fichero_json <- str_replace(fichero_zip, ".zip", ".json")

unzip(zipfile = fichero_zip, exdir = "data")

# readJSONStream()
raw <- readLines(fichero_json)
dat <- iconv(raw, "latin1", "utf8")

rm(raw); gc()

dat <- sapply(dat, fromJSON)

write_rds(dat, glue("data/lista_{mes_id}_Usage_Bicimad.rds"))

gc()

## Lectura de coordenadas ---------------------------------------------

library(leaflet)

colores <- c("red", "green", "yellow", "purple", "orange", 
             "black", "blue", "darkblue", "violet")

numero_rutas <- 10000

set.seed(1234)
coordenadas <- map_df(sample(seq_along(dat), numero_rutas), function(i){
  
  if(dat[[i]]$user_type == 0) return(NULL)
  
  # clr <- sample(colores, 1)
  clr <- if_else(dat[[i]]$user_type == 3, "darkblue", "red")
  
  pts <- length(dat[[i]]$track$features) 
  
  fecha <- as.POSIXct(dat[[i]]$unplug_hourTime)
  
  hora <- dat[[i]]$unplug_hourTime %>% 
    str_sub(12, 13) %>%
    as.numeric()
  
  longitud <- map_dbl(seq_along(dat[[i]]$track$features), function(j){
    dat[[i]]$track$features[[j]]$geometry$coordinates[1]
  })

  latitud <- map_dbl(seq_along(dat[[i]]$track$features), function(j){
    dat[[i]]$track$features[[j]]$geometry$coordinates[2]
  })
  
  tiempo <- map_dbl(seq_along(dat[[i]]$track$features), function(j){
    dat[[i]]$track$features[[j]]$properties$secondsfromstart
  })

  # direccion <- map_chr(seq_along(dat[[i]]$track$features), function(j){
  #   dat[[i]]$track$features[[j]]$properties$`var`
  # })
  
  data_frame(
    color = clr, 
    long = longitud, 
    lat = latitud, 
    tiempo = tiempo, 
    # direccion = direccion, 
    fecha = fecha, 
    hora = hora
  ) %>% 
    mutate(
      tiempo = (tiempo - min(tiempo)) / max(tiempo), 
      weekday = format(fecha, format = "%a")
    )
  
})

formato_numero_rutas <- formatC(numero_rutas, format = "f", digits = 0)
write_rds(coordenadas, glue("data/coordenadas_{mes_id}_{formato_numero_rutas}-rutas.rds"))

gc()

## Exploración por fechas sueltas ------------------------------------

fechas_referencia <- sort(unique(coordenadas$fecha))
mapas <- map(fechas_referencia, function(fecha_guay){
  coordenadas %>% 
    filter(fecha == fecha_guay) %>% 
    leaflet() %>% 
    setView(lng = -3.707, lat = 40.415, zoom = 12) %>%
    addTiles(options = providerTileOptions(opacity = 0.35)) %>% 
    addCircleMarkers(
      lng = ~long, lat = ~lat, 
      color = ~color, stroke = FALSE, 
      fillOpacity = ~tiempo, 
      radius = 2, popup = ~direccion)
})

mapas %>% 
  set_names(fechas_referencia)

## Exploración por días de la semana ---------------------------------------

dias_semana <- c("lu.", "ma.", "mi.", "ju.", "vi.", "sá.", "do.")

mapas <- map(dias_semana, function(fecha_guay){
  coordenadas %>% 
    filter(weekday == fecha_guay) %>% 
    leaflet() %>% 
    setView(lng = -3.707, lat = 40.415, zoom = 12) %>%
    addTiles(options = providerTileOptions(opacity = 0.35)) %>% 
    addCircleMarkers(
      lng = ~long, lat = ~lat, 
      color = ~color, stroke = FALSE, 
      fillOpacity = ~tiempo, 
      radius = 2, popup = ~hora)
})

mapas %>% 
  set_names(dias_semana)
# library(htmltools)

# tagList(mapas)

# addGeoJSON(aa, color = "#e31818", noClip = TRUE, dashArray = "-", stroke = TRUE)

