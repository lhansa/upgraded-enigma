## Puesta a punto -----------------------------------------------------
packages <- c("dplyr", "purrr", "ggplot2", "tidyr", "forcats", "stringr", "readxl", 
  "janitor", "gganimate","gifski")

no_instalados <- packages[!packages %in% installed.packages()]

if(length(no_instalados)) install.packages(no_instalados)

## Librerías ------------------------------------------------------

# library(tidyverse)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(forcats)
library(stringr)
library(readxl)
library(janitor)
library(gganimate)


## Funciones --------------------------------------

leer_excel <- function(hoja){
  read_excel("data/01_ ACCIDENTES POR TIPO EN  DISTRITOS.xls", 
             sheet = hoja, skip = if_else(hoja == "2013", 4L, 7L)) %>% 
    clean_names() %>% 
    rename_at(.vars = vars(starts_with("x")), 
              .funs = list(function(x) "total")) %>% 
    return()
}

## Lectura --------------------------------------

annos <- excel_sheets("data/01_ ACCIDENTES POR TIPO EN  DISTRITOS.xls")

df_accis <- map_df(set_names(annos, annos), leer_excel, .id = "anno")

## Tratamiento ----------------------------------------------------------

df_accis <- df_accis %>%
  mutate(anno = as.integer(anno)) %>% 
  select(-total, -otras_causas, everything(), otras_causas, total) %>% 
  filter(distrito_accidente != "Total") %>% 
  mutate(distrito_accidente = str_to_title(distrito_accidente))

df_accis_long <- df_accis %>% 
  select(-total) %>% 
  gather("tipo_accidente", "numero", -anno, -distrito_accidente)

orden_distritos <- df_accis_long %>% 
  group_by(distrito_accidente) %>% 
  summarise(total = sum(numero, na.rm = TRUE)) %>% 
  arrange(total) %>% 
  pull(distrito_accidente) %>% 
  as_factor()

orden_tipos <- df_accis_long %>% 
  group_by(tipo_accidente) %>% 
  summarise(total = sum(numero, na.rm = TRUE)) %>% 
  arrange(total) %>% 
  pull(tipo_accidente) %>% 
  as_factor()

df_accis_long <- df_accis_long %>% 
  mutate(distrito_accidente = factor(distrito_accidente, levels = orden_distritos), 
         tipo_accidente = factor(tipo_accidente, levels = orden_tipos))

## Visualizaciones -------------------------------------------------

# Evolución temporal 
ggplot(df_accis_long, aes(x = anno, y = numero, 
                          fill = tipo_accidente)) + 
  geom_col() + 
  facet_wrap(~ distrito_accidente, scales = "free")

# Distribución por tipo y distrito
ggplot(df_accis_long, aes(x = distrito_accidente, y = numero, fill = tipo_accidente)) + 
  geom_col() +
  coord_flip() + 
  facet_wrap(~ anno)

# Evolución
ggplot(df_accis_long, aes(x = distrito_accidente, y = numero, fill = tipo_accidente)) + 
  geom_col() +
  coord_flip() + 
  transition_time(anno) + 
  labs(title = "Año: {frame_time}")


