library(tidyverse)
library(jsonlite)

url <- "https://datos.madrid.es/egob/catalogo/216619-0-wifi-municipal.json"

datos <- jsonlite::read_json(url)

datos$`@graph`
