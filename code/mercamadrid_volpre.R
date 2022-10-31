library(tidyverse)

# Fuente:
# https://datos.madrid.es/portal/site/egob/menuitem.c05c1f754a33a9fbe4b2e4b284f1a5a0/?vgnextoid=a4df993ae322b610VgnVCM1000001d4a900aRCRD&vgnextchannel=374512b9ace9f310VgnVCM100000171f5a0aRCRD&vgnextfmt=default

## Funciones -------------------------------------------------

arregla_formato_feo <- function(x){
  # gracias, stackoverflow
  return(iconv(enc2utf8(x), sub = "byte"))
}

## Lectura -----------------------------------------------
df_merca <- read_csv2("data/volpre2019.csv") %>%
  set_names(c("fecha_desde", "fecha_hasta", "cod_variedad", "desc_variedad", 
              "cod_origen", "desc_origen", "kilos", 
              "precio_freq", "precio_max", "precio_min"))

## Tratamiento ------------------------------------------------------

# Creo columna con la primera palabra de la descripción del producto
df_merca <- df_merca %>% 
  mutate_at(.vars = vars(starts_with("desc")), 
            .funs = list(arregla_formato_feo))
  mutate(desc_variedad1 = str_extract(desc_variedad, "^([A-z])+")) %>% 
  mutate_at(.vars = vars(starts_with("fecha")), 
            .funs = list(as.character))

## Datos feos -------------------------------------------

cuantos_na <- function(x) sum(is.na(x))

df_merca %>% 
  summarise_all(.funs = list(cuantos_na))

df_merca <- df_merca %>% 
  filter(!is.na(fecha_desde))

## Exploración evolución ---------------------------------------------------

## Evolución en el tiempo de las variedades

length(unique(df_merca$desc_variedad))
length(unique(df_merca$desc_variedad1))

df_merca_evolucion <- df_merca %>%
  group_by(desc_variedad1) %>% 
  # Me quedo con los que están en 6 periodos
  filter(n_distinct(fecha_desde) == 6) %>% 
  group_by(fecha_desde, desc_variedad1) %>% 
  summarise(kilos = sum(kilos)) %>% 
  ungroup()

orden_descripcion <- df_merca_evolucion %>% 
  group_by(desc_variedad1) %>% 
  summarise(kilos_total = sum(kilos)) %>% 
  arrange(kilos_total) %>% 
  pull(desc_variedad1) %>% 
  as_factor()

# Se ve algo de estacionalidad, afortunadamente...

df_merca_evolucion %>% 
  mutate(desc_variedad1 = factor(desc_variedad1, levels = orden_descripcion)) %>% 
  arrange(fecha_desde, desc(desc_variedad1)) %>% 
  group_by(fecha_desde) %>% 
  slice(1:100) %>% 
  ggplot(aes(x = desc_variedad1, y = kilos)) + 
  geom_col() + 
  coord_flip() + 
  facet_grid(. ~ fecha_desde)

## Exploración origen ------------------------------------------
  
## Comparativa de orígenes de algunos productos
## Código origen >= 54 es extranjero

df_merca %>%
  group_by(desc_variedad1) %>% 
  summarise(kilos_total = sum(kilos)) %>% 
  arrange(desc(kilos_total)) %>% 
  print(n = 100)

distribucion_origen <- function(origen){
  df_merca %>% 
    # mutate(desc_variedad = iconv(enc2utf8(desc_variedad), sub = "byte")) %>% 
    filter(str_detect(tolower(desc_variedad), origen)) %>% 
    mutate(origen_esp = cod_origen < 54) %>% 
    select(fecha_desde, desc_variedad1, origen_esp, desc_origen, kilos) %>% 
    group_by(desc_origen) %>% 
    mutate(kilos_total = sum(kilos)) %>% 
    ggplot(aes(x = reorder(desc_origen, kilos_total), 
               y = kilos, 
               fill = origen_esp)) + 
    geom_col() +
    labs(title = str_to_title(origen), x = "Origen", y = "Kilos") + 
    coord_flip() + 
    facet_grid(. ~ fecha_desde)
  
}

distribucion_origen("aguacate")
distribucion_origen("conejo")
distribucion_origen("naranja")
distribucion_origen("platano")
distribucion_origen("freson")
distribucion_origen("atun")
distribucion_origen("bacalao")

df_merca$desc_variedad1
