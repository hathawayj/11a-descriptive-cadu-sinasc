library(tidyverse)
source("data/scripts/census_dict.R")

census <- list()

for (type in names(census_dict)) {
  message(type)
  # type <- names(census_dict)[[1]]
  ff <- list.files("data/discovered/_raw/datasus/IBGE/censo", full.names = TRUE, pattern = type)
  nms_keep <- names(census_dict[[type]])
  res <- list()
  for (ii in seq_along(ff)) {
    message(ii)
    res[[ii]] <- read_and_transform(ff[ii], census_dict[[type]], nms_keep)
  }
  tmp <- bind_rows(res)

  mc <- brazilgeo::br_muni_codes
  mc$muni_code <- substr(mc$muni_code, 1, 6)
  tmp <- left_join(tmp, mc)

  census[[type]] <- tmp
}

census$RENDABR$situation <- NULL

lapply(census, nrow)

# ALFBR: Rate of illiteracy
# ESCABR: Population education of 15 years and over
# ESCBBR: Education of the population aged 18 to 24
# IDOSOBR: Proportion of elderly living in households in the condition of another relative
# RENDABR: Average household income per capita

names(census) <- c("illit", "educ15", "educ18_24", "elderly", "income")

kitools::data_publish(census$illit, name = "census_illit",
  file_type = "csv",
  desc = "Brazil census data for illiteracy rates across municipality/race/sex/urban/age/ for 1991, 2000, and 2010.")

kitools::data_publish(census$educ15, name = "census_educ15",
  file_type = "csv",
  desc = "Brazil census data for education level of age 15+ across municipality/race/sex/urban/age/ for 1991, 2000, and 2010.")

kitools::data_publish(census$educ18_24, name = "census_educ18_24",
  file_type = "csv",
  desc = "Brazil census data for education level of age 18-24 across municipality/race/sex/urban/age/ for 1991, 2000, and 2010.")

kitools::data_publish(census$elderly, name = "census_elderly",
  file_type = "csv",
  desc = "Brazil census data for dependent elderly rates across municipality/race/sex/urban/age/ for 1991, 2000, and 2010.")

kitools::data_publish(census$income, name = "census_income",
  file_type = "csv",
  desc = "Brazil census data for income across municipality/race/sex/urban/age/ for 1991, 2000, and 2010.")

