# devtools::install_github("ki-tools/brazilgeo")
# devtools::install_github("ki-tools/kitools")
library(kitools)
library(tidyverse)
library(brazilgeo)
library(purrr)

snsc <- data_use("snsc_2001-2015")

snsnsc_sample <- data_use("snsc_sample_1.6m")

snsc_small <- snsc_sample %>%
  sample_n(25)

## Questions
#
# Look at distribution of time of day of birth birth_time?
# We could use the birth_type to maybe find the set of births in the 3 or mor category

# summarize m_age_yrs
# summarize agpar1 and agpar5 scores
# summarize n_live_child and n_dead_child
# summarize gest_weeks
# summarize gest_month_precare  # maybe convert to trimester and group by

geo_summary <- function(group_vars, dat = snsc) {
  
    #n_dead and n_live_child are missing at a pretty high rate often over 10% and sometimes up to 45%.  Not going to use.
    #agpar and gest weeks are the next most missing
    group_by_at(.tbl = dat, group_vars) %>%
    summarise(
      n = n(),
      n_birthweight = sum(!is.na(brthwt_g)), # there are 171975 missing birthwts in the entire data set
      n_apgar1 = sum(!is.na(apgar1)), # 2391567 missing
      n_apgar5 = sum(!is.na(apgar5)), # 2514611 missing
#      n_n_live_child = sum(!is.na(n_live_child)), # 4039480 missing
#      n_n_dead_child = sum(!is.na(n_dead_child)), # 7312813 missing
      n_gest_weeks = sum(!is.na(gest_weeks_cat)), # 804294 missing
      n_m_age_yrs = sum(!is.na(m_age_yrs)), # 39322 missing
      
      prop_lt2k_bwt = length(which(brthwt_g < 2000)) / n_birthweight,
      prop_gt4k_bwt = length(which(brthwt_g > 4000)) / n_birthweight,
      prop_lt37_gest_weeks = length(which(gest_weeks_cat %in% c("Less than 22 weeks", "22 to 27 weeks", "28 to 31 weeks", "32 to 36 weeks")))/ n_gest_weeks,
      mean_bwt = mean(brthwt_g, na.rm = TRUE),
      mean_apgar1 = mean(apgar1, na.rm = TRUE),
      mean_apgar5 = mean(apgar5, na.rm = TRUE),
#      mean_n_live_child = mean(n_live_child, na.rm = TRUE),
#      mean_n_dead_child = mean(n_dead_child, na.rm = TRUE),
      mean_m_age_yrs = mean(m_age_yrs, na.rm = TRUE),
      
      sd_bwt = sd(brthwt_g, na.rm = TRUE),
      sd_apgar1 = sd(apgar1, na.rm = TRUE),
      sd_apgar5 = sd(apgar5, na.rm = TRUE),
#      sd_n_live_child = sd(n_live_child, na.rm = TRUE),
#      sd_n_dead_child = sd(n_dead_child, na.rm = TRUE),
      sd_m_age_yrs = sd(m_age_yrs, na.rm = TRUE),
      
      # mad_bwt = mad(brthwt_g, na.rm = TRUE),
      # mad_apgar1 = mad(apgar1, na.rm = TRUE),
      # mad_apgar5 = mad(apgar5, na.rm = TRUE),
      # mad_n_live_child = mad(n_live_child, na.rm = TRUE),
      # mad_n_dead_child = mad(n_dead_child, na.rm = TRUE),
      # mad_m_age_yrs = mad(m_age_yrs, na.rm = TRUE),
      
      median_bwt = median(brthwt_g, na.rm = TRUE),
      median_apgar1 = median(apgar1, na.rm = TRUE),
      median_apgar5 = median(apgar5, na.rm = TRUE),
#      median_n_live_child = median(n_live_child, na.rm = TRUE),
#      median_n_dead_child = median(n_dead_child, na.rm = TRUE),
      median_m_age_yrs = median(m_age_yrs, na.rm = TRUE),
      
      # q1_bwt = quantile(brthwt_g, 0.25, na.rm = TRUE),
      # q1_apgar1 = quantile(apgar1, 0.25, na.rm = TRUE),
      # q1_apgar5 = quantile(apgar5, 0.25, na.rm = TRUE),
      # q1_n_live_child = quantile(n_live_child, 0.25, na.rm = TRUE),
      # q1_n_dead_child = quantile(n_dead_child, 0.25, na.rm = TRUE),
      # q1_m_age_yrs = quantile(m_age_yrs, 0.25, na.rm = TRUE),
      # 
      # q3_bwt = quantile(brthwt_g, 0.75, na.rm = TRUE),
      # q3_apgar1 = quantile(apgar1, 0.75, na.rm = TRUE),
      # q3_apgar5 = quantile(apgar5, 0.75, na.rm = TRUE),
      # q3_n_live_child = quantile(n_live_child, 0.75, na.rm = TRUE),
      # q3_n_dead_child = quantile(n_dead_child, 0.75, na.rm = TRUE),
      # q3_m_age_yrs = quantile(m_age_yrs, 0.75, na.rm = TRUE)
    )
}

# Calculate proportion allocations
geo_props <- function(group_vars, cat_vars, dat = snsc) {
  
  group_vars_char <- group_vars %>%
    map(~rlang::quo_get_expr(.x)) %>%
    map(~as.character(.x)) %>% 
    unlist()
  cat_vars_char <- cat_vars %>%
    map(~rlang::quo_get_expr(.x)) %>%
    map(~as.character(.x)) %>% 
    unlist()
  
  # build n counts for both group vars and the category grouping. Right now I only think it can handle one category grouping
  all_group <- group_by_at(.tbl = dat, c(group_vars, cat_vars)) %>%
    drop_na(!!group_vars_char, !!cat_vars_char) %>% # I drop all the cases that don't have values for all group and cat vars
    count() %>%
    ungroup() %>%
    rename(paste_var = !!cat_vars_char) %>%
    filter(!paste_var %in% c("null", "Null", "NULL")) %>% # I don't know why there are NULL values
    group_by_at(group_vars) %>%
    mutate(total = sum(n), perc = n / total) %>%
    ungroup() %>%
    mutate(paste_var = str_to_lower(paste_var), 
           spread_var = str_c(cat_vars_char, "__", paste_var,"__p")) %>%
    select(!!group_vars_char, spread_var, perc) %>%
    filter(!is.na(spread_var)) %>% #not sure why there are NAs showing up in the spread var.
    spread(key = spread_var, value = perc, fill = 0)

  return(all_group)

}

# group by sex
# group by deliv type
# group by n_prenat_visit_cat
# group by gest_weeks_cat
# group by marital status
# group by birth_place I may have the hospital code identifiers on laptop.
# group by birth_assist
# group by race  
# group by m_educ
# group by birth_qtr
# group by gest_method
# presentation is only in 2011-forward.  Could be good but don't use.

group_types <- list(vars(sex), vars(deliv_type), vars(marital_status), vars(gest_weeks_cat), vars(race), vars(gest_method), vars(n_prenat_visit_cat), vars(birth_qtr))  
#group_types <- list(vars(sex))

#############################
# State aggregation and write
#############################

snsc_geo_state_props <- group_types %>%
  map(~ geo_props(vars(birth_year, birth_state_code), .x, snsc)) %>%
  reduce(left_join) %>%
  rename(state_code = birth_state_code) %>%
  left_join(brazilgeo::br_state_codes)

snsc_geo_state <- geo_summary(vars(birth_year, birth_state_code), snsc)

state <- snsc_geo_state %>%
  rename(state_code = birth_state_code) %>%
  left_join(snsc_geo_state_props)

data_publish(state, name = "snsc_state_summary", file_type = "csv", type = "derived")

rm(snsc_geo_state, snsc_geo_state_props, state)

#############################
# Micro aggregation and write
#############################

snsc_geo_micro_props <- group_types %>%
  map(~ geo_props(vars(birth_year, birth_micro_code), .x, snsc)) %>%
  reduce(left_join) %>%
  rename(micro_code = birth_micro_code) %>%
  left_join(brazilgeo::br_micro_codes)

snsc_geo_micro <- geo_summary(vars(birth_year, birth_micro_code), snsc)

micro <- snsc_geo_micro %>%
  rename(micro_code = birth_micro_code) %>%
  left_join(snsc_geo_micro_props)

data_publish(micro, name = "snsc_micro_summary", file_type = "csv", type = "derived")

rm(snsc_geo_micro, snsc_geo_micro_props, micro)


#############################
# Meso aggregation and write
#############################


snsc_geo_meso_props <- group_types %>%
  map(~ geo_props(vars(birth_year, birth_meso_code), .x, snsc)) %>%
  reduce(left_join) %>%
  rename(meso_code = birth_meso_code) %>%
  left_join(brazilgeo::br_meso_codes)

snsc_geo_meso <- geo_summary(vars(birth_year, birth_meso_code)  , snsc)

meso <- snsc_geo_meso %>%
  rename(meso_code = birth_meso_code) %>%
  left_join(snsc_geo_meso_props)

data_publish(meso, name = "snsc_meso_summary", file_type = "csv", type = "derived")

rm(snsc_geo_meso, snsc_geo_meso_props, meso)


#############################
# Muni aggregation and write
#############################

snsc_geo_muni_props <- group_types %>%
  map(~ geo_props(vars(birth_year, birth_muni_code), .x, snsc)) %>%
  reduce(left_join) %>%
  rename(muni_code = birth_muni_code) %>%
  left_join(brazilgeo::br_muni_codes)

snsc_geo_muni <- geo_summary(vars(birth_year, birth_muni_code)  , snsc)

muni <- snsc_geo_muni %>%
  rename(muni_code = birth_muni_code) %>%
  left_join(snsc_geo_muni_props)

data_publish(muni, name = "snsc_muni_summary", file_type = "csv", type = "derived")

rm(snsc_geo_muni, snsc_geo_muni_props, muni)


# 
# looking at missing rates
# snsc_geo_state %>%
#   select(birth_year, birth_state_code, starts_with("n")) %>%
#   gather(key = "n_vars", value = "count", -birth_year, -birth_state_code, -n) %>%
#   mutate(prop_less = (count - n) / n) %>%
#   filter(birth_year %in% 2010:2015, !is.na(birth_state_code)) %>%
#   ggplot(aes(x = fct_reorder(n_vars, prop_less), y = prop_less)) +
#   facet_grid(birth_year~birth_state_code) +
#   geom_point() +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90))
# 
# snsc_geo_state %>%
#   select(birth_year, birth_state_code, starts_with("n")) %>%
#   gather(key = "n_vars", value = "count", -birth_year, -birth_state_code, -n) %>%
#   mutate(prop_less = (count - n) / n) %>%
#   filter(birth_year %in% 2005:2010, !is.na(birth_state_code)) %>%
#   ggplot(aes(x = fct_reorder(n_vars, prop_less), y = prop_less)) +
#   facet_grid(birth_year~birth_state_code) +
#   geom_point() +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90))


    
  


# publish data 
# 





