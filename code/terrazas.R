library(tidyverse)
library(leaflet)
library(rgdal)
library(geojsonio)
library(rgeos)

## Lectura de terrazas --------------------------------------------------

df_terrazas <- read_csv2("data/OPEN DATA Terrazas201903.csv")

## Tratamiento de coordenadas -------------------------------------------

df_terrazas <- df_terrazas %>%
  filter(coordenada_x_local > 1000, coordenada_y_local > 3e6)

# UTM a siglo XXI
terrazas_utm <- SpatialPoints(cbind(df_terrazas$coordenada_x_local, 
                                    df_terrazas$coordenada_y_local), 
                              proj4string=CRS("+proj=utm +zone=30"))

terrazas_latlong <- spTransform(terrazas_utm, CRS("+proj=longlat"))

df_terrazas <- df_terrazas %>% 
  mutate(long = terrazas_latlong$coords.x1, 
         lat = terrazas_latlong$coords.x2)

## Lectura de polígonos -------------------------------------------------


geomadrid <- geojson_read("data/madrid.geojson", method = "local", what = "sp")

## Cálculo de terrazas por barrio ------------------------------------------

terrazas <- SpatialPoints(select(df_terrazas, long, lat), 
                          proj4string=CRS(proj4string(geomadrid)))


# comprueba_terraza <- function(sp2){
#   map_lgl(seq_along(geomadrid), function(j){
#     gContains(geomadrid[j, ], sp2)
#   })
# }

comprueba_barrio <- function(sp1){
  map_lgl(seq_along(terrazas), function(i){
    gContains(sp1, terrazas[i, ])
  })
}

num_terrazas_por_barrio <- map_dbl(seq_along(geomadrid), function(j){
  sum(comprueba_barrio(geomadrid[j, ]))
})

geomadrid@data$num_terrazas <- num_terrazas_por_barrio

write_rds(geomadrid, "data/geomadrid_terrazas.rds")


## Mapa -----------------------------------------------------------------

pal <- colorNumeric("viridis", NULL)

leaflet(geomadrid) %>% 
  addTiles() %>% 
  addPolygons(stroke = FALSE, smoothFactor = 0.3, 
              fillOpacity = 0.6,  
              fillColor = ~pal(num_terrazas), 
              color = "black",
              label = ~paste0(name, ": ", formatC(num_terrazas, 
                                                  big.mark = ".", 
                                                  decimal.mark = ",")), 
              group = "Polygons") %>% 
  addLegend(pal = pal, values = ~num_terrazas, opacity = 0.3,
            labFormat = labelFormat(transform = function(x) round(10^x))) %>% 
  # addMarkers(~long, ~lat, 
  #            data = df_terrazas %>% sample_n(100), 
  #            group = "Points") %>% 
  addCircleMarkers(~long, ~lat, 
                   data = df_terrazas %>% sample_n(100),
                   radius = 6, stroke = FALSE, 
                   group = "Points") %>% 
  addLayersControl(
    overlayGroups = c("Polygons", "Points"),
    options = layersControlOptions(collapsed = FALSE)
  )

