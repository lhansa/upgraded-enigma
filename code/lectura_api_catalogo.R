library(httr)

r <- GET("https://datos.madrid.es/egob/catalogo/modified/begin/1990-01-01T02%3A00Z/end/2019-09-20T02%3A00Z.json")

contenido <- content(r)

length(contenido)

contenido$result$items[[1]]
