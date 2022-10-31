library(tidyverse)
library(RJSONIO)
library(glue)

mes_id <- "201808"

fichero_zip <- glue("data/{mes_id}_Usage_Bicimad.zip")
fichero_json <- str_replace(fichero_zip, ".zip", ".json")

# unzip(zipfile = fichero_zip, exdir = "data")
# 
# # readJSONStream()
# raw <- readLines(fichero_json)
# dat <- iconv(raw, "latin1", "utf8")
# 
# rm(raw); gc()
# 
# dat <- sapply(dat, fromJSON)

dat <- read_rds(glue("data/lista_{mes_id}_Usage_Bicimad.rds"))

gc()

## Id diario de usuario ---------------------------------------------

# df_tiempos %>% 
#   dplyr::count(user_id) %>% 
#   filter(n > 4) %>% 
#   arrange(desc(n))

## Estación de salida ---------------------------------------------

#' Para entender cuál es cada estación hará falta un fichero que lo indique. 
#' A partir de la información disponible en los movimientos no parece
#' suficiente entender cuál es cuál.

selecciona_zipcodes <- function(estacion_out){
  df_tiempos %>%
    # filter(!str_replace_all(zip_code, "0", "") %in% c("", " ")) %>% 
    filter(
      str_detect(zip_code, "^28"),
      !zip_code %in% c("28", "28000")
    ) %>% 
    group_by(estacion_salida_id, zip_code) %>% 
    summarise(movimientos = n()) %>% 
    filter(estacion_salida_id == estacion_out, movimientos >= 10) %>% 
    arrange(estacion_salida_id, desc(movimientos)) %>% 
    slice(1:200) %>% 
    return()
}

## Tiempos ----------------------------------------------------------

df_tiempos <- data_frame()

df_tiempos <- map_df(seq_along(dat), function(i){
  
  if (dat[[i]]$user_type %in% c(1,2)){
    data_frame(
      user_id = dat[[i]]$user_day_code,
      mov_id = dat[[i]]$`_id`,
      estacion_salida_id = dat[[i]]$idunplug_station, 
      estacion_llegada_id = dat[[i]]$idplug_station, 
      tiempo = dat[[i]]$travel_time, 
      user_type = dat[[i]]$user_type,
      zip_code = dat[[i]]$zip_code
    ) %>% 
      return()
  } else {
    return(NULL)
  }

})

gc()

write_rds(df_tiempos, glue("data/df_tiempos_{mes_id}.rds"))

# Ranking de estaciones

q1 <- function(x) quantile(x, probs = 0.25, na.rm = TRUE)
q3 <- function(x) quantile(x, probs = 0.75, na.rm = TRUE)

df_tiempos$estacion_salida_id %>% unique %>% length
df_tiempos$estacion_llegada_id %>% unique %>% length

#' Hay del orden de 28000 pares de estaciones posibles

df_tiempos %>%
  mutate(tiempo = tiempo / 60) %>%
  pull(tiempo) %>% 
  quantile(probs = seq(0.9, 1, by = 0.01))


#' El 95% de los movimientos duran menos de 44 minutos. Los movimientos de 
#' menos de una hora suponen el 97% de los movimientos.


df_pares_estaciones <- df_tiempos %>% 
  select(estacion_salida_id, estacion_llegada_id, tiempo) %>% 
  arrange(estacion_salida_id, estacion_llegada_id) %>% 
  mutate(tiempo = tiempo / 60) %>% 
  filter(tiempo <= 45) %>% 
  group_by(estacion_salida_id, estacion_llegada_id) %>% 
  summarise(
    min = min(tiempo), 
    q1 = q1(tiempo), 
    media = mean(tiempo), 
    mediana = median(tiempo), 
    q3 = q3(tiempo), 
    max = max(tiempo), 
    movimientos = n()
  ) %>% 
  ungroup()


ggplot(df_pares_estaciones, aes(x = movimientos)) + 
  geom_histogram(binwidth = 2)

# Ida y vuelta

#' Voy a empezar quitando los movimientos que van de una estación a la misma

df_ida_vuelta <- df_pares_estaciones %>%
  select(
    estacion_1 = estacion_salida_id,
    estacion_2 = estacion_llegada_id,
    mediana_ida = mediana,
    movimientos_ida = movimientos
  ) %>%
  inner_join(
    df_pares_estaciones %>%
      select(
        estacion_2 = estacion_salida_id,
        estacion_1 = estacion_llegada_id,
        mediana_vuelta = mediana,
        movimientos_vuelta = movimientos
      )
  )


# df_pares_ida <- df_pares_estaciones %>%
#   filter(estacion_salida_id != estacion_llegada_id) %>% 
#   select(
#     estacion_1 = estacion_salida_id,
#     estacion_2 = estacion_llegada_id,
#     mediana_ida = mediana,
#     movimientos_ida = movimientos
#   )
# 
# df_pares_vuelta <- df_pares_estaciones %>% 
#   filter(estacion_salida_id != estacion_llegada_id) %>% 
#   select(
#     estacion_2 = estacion_salida_id, 
#     estacion_1 = estacion_llegada_id, 
#     mediana_vuelta = mediana, 
#     movimientos_vuelta = movimientos
#   )

df_ida_vuelta <- df_ida_vuelta %>% 
  filter(estacion_1 != estacion_2) %>% 
  mutate(
    estacion_1_aux = str_pad(estacion_1, 4, pad = 0), 
    estacion_2_aux = str_pad(estacion_2, 4, pad = 0)
  ) %>% 
  rowwise() %>% 
  mutate(
    id_par = paste0(
      min(c(estacion_1_aux, estacion_2_aux)), 
      "-", 
      max(c(estacion_1_aux, estacion_2_aux))
    )
  ) %>% 
  select(-estacion_1_aux, -estacion_2_aux) %>% 
  arrange(id_par) %>% 
  group_by(id_par) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(
    diferencia_tiempos = abs(mediana_ida - mediana_vuelta), 
    diferencia_movimientos = abs(movimientos_ida - movimientos_vuelta)
  ) %>% 
  arrange(diferencia_tiempos, diferencia_movimientos) %>%
  select(starts_with("estacion"), starts_with("mediana"), 
         starts_with("movimientos"), starts_with("diferencia"), 
         id_par)


selecciona_zipcode_estacion <- function(estacion_id){
  df_tiempos %>%
    filter(estacion_salida_id == estacion_id) %>% 
    filter(
      str_detect(zip_code, "^28"),
      !zip_code %in% c("28", "28000")
    ) %>% 
    count(estacion_salida_id, zip_code) %>% 
    arrange(estacion_salida_id, desc(n)) %>% 
    slice(1) %>% 
    magrittr::use_series(zip_code)
}

df_ida_vuelta <- df_ida_vuelta %>% 
  mutate(
    zip_code_1 = map_chr(df_ida_vuelta$estacion_1, function(est){
      selecciona_zipcode_estacion(est)
    }), 
    zip_code_2 = map_chr(df_ida_vuelta$estacion_2, function(est){
      selecciona_zipcode_estacion(est)
    })) %>% 
  select(zip_code_1, zip_code_2, everything())

write_rds(df_ida_vuelta, glue("data/df-ida-vuelta_{mes_id}.rds"))

df_ida_vuelta %>% 
  filter_at(.vars = vars(starts_with("movimientos")), .vars_predicate = all_vars(. >= 10)) %>% 
  filter(diferencia_tiempos > 0.1) %>% 
  # tail(100) %>%
  print(n = 100)

## Tiempos y códigos postales -------------------------------------------

df_zipcode_tiempos <- df_tiempos %>% 
  filter(
    str_detect(zip_code, "^28"),
    !zip_code %in% c("28", "28000"), 
    str_length(zip_code) == 5
  ) %>% 
  mutate(tiempo = tiempo / 60) %>% 
  group_by(zip_code) %>% 
  summarise(
    min = min(tiempo), 
    q1 = q1(tiempo), 
    media = mean(tiempo), 
    mediana = median(tiempo), 
    q3 = q3(tiempo), 
    max = max(tiempo), 
    movimientos = n()
  ) 

#' Hay unos 200 códigos postales de los usuarios, pero son muy pocos
#' los que concentran la inmensa mayoría de los movimientos. 

df_tiempos$zip_code %>% unique %>% length
#' Esta inmensa mayoría (más del 90% de movimientos)
#' está asociada a códigos postales de Madrid ciudad.

df_tiempos %>% 
  filter(
    str_detect(zip_code, "^28"),
    !zip_code %in% c("28", "28000"), 
    str_length(zip_code) == 5
  ) %>% 
  count(zip_code, sort = TRUE) %>% 
  ggplot(aes(x = reorder(zip_code, n), y = n)) + 
  geom_col() + 
  coord_flip()

df_tiempos %>% 
  filter(
    str_detect(zip_code, "^28"),
    !zip_code %in% c("28", "28000"), 
    str_length(zip_code) == 5
  ) %>% 
  count(zip_code, sort = TRUE) %>% 
  mutate(n_rel = n / sum(n), 
         n_rel_cum = cumsum(n_rel)) %>% 
  print(n = 200)

## Duración de movimiento por código postal --------------------------------------

#' Vemos distribución del tiempo mediano en cada trayecto por CP. Forzamos más 
#' de 1000 movimientos en un mes (90% de movimientos se hacen CCPP con más 
#' de 1000 movimientos en un mes)
#' 
#' Se observa que los movimientos de mayor duración mediana se dan entre 
#' usuarios de códigos postales más céntricos. 

df_zipcode_tiempos %>% 
  filter(movimientos > 1000) %>% 
  arrange(mediana) %>% 
  mutate(zip_code = as_factor(zip_code)) %>% 
  ggplot(aes(x = zip_code)) + 
  geom_col(aes(y = mediana, fill = movimientos)) + 
  # geom_errorbar(aes(ymin = q1, ymax = q3)) +
  coord_flip() + 
  theme(axis.text.y = element_text(size = 8))


#' Hago zoom para el caso de más de 100 movimientos (CCPP que
#' engloban el 97% de movimientos)
#' 
#' Los que menos tiempo pasan

df_zipcode_tiempos %>% 
  filter(movimientos > 100) %>% 
  arrange(mediana) %>% 
  slice(1:50) %>%
  mutate(zip_code = as_factor(zip_code)) %>% 
  ggplot(aes(x = zip_code)) + 
  geom_col(aes(y = mediana, fill = movimientos)) + 
  # geom_errorbar(aes(ymin = q1, ymax = q3)) + 
  coord_flip() + 
  theme(axis.text.y = element_text(size = 8))

#' Los que más tiempo pasan:

df_zipcode_tiempos %>% 
  filter(movimientos > 10) %>% 
  arrange(mediana) %>% 
  slice(51:nrow(.)) %>% 
  mutate(zip_code = as_factor(zip_code)) %>% 
  ggplot(aes(x = zip_code)) + 
  geom_col(aes(y = mediana, fill = movimientos)) + 
  # geom_errorbar(aes(ymin = q1, ymax = q3)) + 
  coord_flip() + 
  theme(axis.text.y = element_text(size = 8))

## Tiempo diario en la bici de un usuario por código postal -------------------

df_mediana_usuarios <- df_tiempos %>% 
  filter(
    str_detect(zip_code, "^28"),
    !zip_code %in% c("28", "28000"), 
    str_length(zip_code) == 5
  ) %>% 
  mutate(tiempo = tiempo / 60) %>% 
  group_by(zip_code, user_id) %>% 
  summarise(tiempo = sum(tiempo)) %>% 
  summarise(
    min = min(tiempo), 
    q1 = q1(tiempo), 
    media = mean(tiempo), 
    mediana = median(tiempo), 
    q3 = q3(tiempo), 
    max = max(tiempo), 
    usuarios = n()
  ) %>% 
  arrange(desc(mediana))


quantile(df_mediana_usuarios$usuarios, probs = seq(0,1,by = 0.1))

df_tiempos %>% 
  filter(
    str_detect(zip_code, "^28"),
    !zip_code %in% c("28", "28000"), 
    str_length(zip_code) == 5
  ) %>% 
  group_by(zip_code) %>% 
  summarise(usuarios = n_distinct(user_id)) %>% 
  arrange(desc(usuarios)) %>% 
  mutate(n_rel = usuarios / sum(usuarios),
         n_rel_cum = cumsum(n_rel)) %>% 
  print(n = 200)
  
# mutate(zip_code = as_factor(zip_code)) %>% 
#   ggplot(aes(x = zip_code, y = usuarios))+ 
#   geom_col() +
#   theme(axis.text.x = element_text(angle = 90))

df_mediana_usuarios %>% 
  filter(usuarios >= 100) %>%
  slice(1:50) %>%
  arrange(mediana) %>% 
  mutate(zip_code = as_factor(zip_code)) %>% 
  ggplot() + 
  geom_col(aes(x = zip_code, y = mediana, fill = usuarios)) + 
  coord_flip() + 
  theme(axis.text.y = element_text(size = 8))
