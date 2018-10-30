library(kitools)
library(tidyverse)

snsc <- data_use("snsc_2001-2015")
set.seed(102018)
snsc_sample <- snsc %>%
  group_by(birth_year, birth_muni_code, sex, deliv_type) %>%
  sample_n(10, replace = TRUE) %>%
  ungroup() %>%
  distinct()

data_publish(snsc_sample, name = "snsc_sample_1.6m", file_type = "rds", type = "derived")

