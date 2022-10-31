library(tidyverse)
library(janitor)
library(arrow)
library(microbenchmark)

caso <- "turismo-por-ccaa"


# READ DATA ---------------------------------------------------------------
df_turismo <- read_csv2(
  sprintf("data/%s.csv", caso)
) %>% 
  clean_names() %>% 
  mutate(total = parse_number(total, 
                              locale = locale(grouping_mark = "."), 
                              na = c("", "NA", ".")))

df_turismo <- df_turismo %>% 
  rename_with(~ "destino", .cols = contains("destino"))


# SAVE FORMATS ------------------------------------------------------------
write_parquet(
  df_turismo, 
  sprintf("data/%s.parquet", caso)
)
write_feather(
  df_turismo, 
  sprintf("data/%s.feather", caso)
)
# write_csv_arrow(df_turismo, "data/turismo-por-provincia_CSV.csv")



# BENCHMARK ---------------------------------------------------------------
parquet <- function() read_parquet("data/turismo-por-provincia.parquet")
feather <- function() read_feather("data/turismo-por-provincia.feather")
# csv <- function() read_csv_arrow("data/turismo-por-provincia_CSV.csv")

# microbenchmark::microbenchmark(
#   parquet(), 
#   feather(), 
#   csv(), 
#   times = 20
# )
# 
# bench::mark(
#   parquet(), 
#   feather(), 
#   # csv(), 
#   iterations = 10
# )

df_parquet <- parquet()
df_feather <- feather()

pryr::object_size(df_parquet)
pryr::object_size(df_feather)
pryr::object_size(df_turismo)

