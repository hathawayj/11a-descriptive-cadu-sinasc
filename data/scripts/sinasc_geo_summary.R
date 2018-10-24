library(kitools)
library(tidyverse)
# devtools::install_github("ki-tools/brazilgeo")
library(brazilgeo)

snsc <- data_use("snsc_2001-2015")

# group_vars <- vars(birth_year, birth_state_code)

geo_summary <- function(group_vars) {
  snsc %>%
    group_by_at(group_vars) %>%
    summarise(
      n_birth = n()
      # ...
    )
}

snsc_geo_state <- geo_summary(vars(birth_year, birth_state_code))
snsc_geo_micro <- geo_summary(vars(birth_year, birth_micro_code))
snsc_geo_meso <- geo_summary(vars(birth_year, birth_meso_code))
snsc_geo_muni <- geo_summary(vars(birth_year, birth_muni_code))

# test

# birth weight less than 2000 grams
# greater than 4000 grams
# proportion less than 37 weeks gestational age. before 2011 it is a category
# two or three categories like delivery method get summary be each
# marital status like percentage of single moms
# one row per year muni and then different summary variables. lots of columns
# 
# 
