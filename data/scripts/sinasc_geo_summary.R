# devtools::install_github("ki-tools/brazilgeo")
# devtools::install_github("ki-tools/kitools")
library(kitools)
library(tidyverse)
library(brazilgeo)

snsc <- data_use("snsc_2001-2015")

snsc_sample <- data_use("snsc_sample_1.6m")

snsc_small <- snsc_sample %>%
  sample_n(25)

# 
# [1] "m_muni_code"          "birth_muni_code"      "birth_place"          "health_estbl_code"    "m_age_yrs"            "marital_status"      
# [7] "m_educ"               "occ_code"             "n_live_child"         "n_dead_child"         "gest_weeks_cat"       "preg_type"           
# [13] "deliv_type"           "n_prenat_visit_cat"   "birth_date"           "sex"                  "apgar1"               "apgar5"              
# [19] "race"                 "brthwt_g"             "cong_anom"            "cong_icd10"           "birth_year"           "birth_state_code"    
# [25] "birth_micro_code"     "birth_meso_code"      "m_state_code"         "m_micro_code"         "m_meso_code"          "birth_nbhd_code"     
# [31] "res_nbhd_code"        "birth_time"           "m_country_code"       "m_birth_country_code" "m_educ_grade"         "m_birth_date"        
# [37] "m_race"               "n_prev_preg"          "n_vag_deliv"          "n_ces_deliv"          "f_age_yrs"            "menstrual_date_last" 
# [43] "gest_weeks"           "gest_method"          "n_prenat_visit"       "gest_month_precare"   "presentation"         "labor_induced"       
# [49] "ces_pre_labor"        "m_educ_2010"          "m_fu_code"            "birth_assist"         "m_educ_2010agg"  

## Questions
#
# Should we only look at singleton births?
# What is the m_fu_code?
# What is gest_month_precare?
# Look at distribution of time of day of birth birth_time?

# summarize m_age_yrs
# summarize agpar1 and agpar5 scores
# summarize n_live_child and n_dead_child
# summarize gest_weeks

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

# summary grouping
snsc_geo_state <- geo_summary(vars(birth_year, birth_state_code), snsc)
snsc_geo_micro <- geo_summary(vars(birth_year, birth_micro_code), snsc)
snsc_geo_meso <- geo_summary(vars(birth_year, birth_meso_code)  , snsc)
snsc_geo_muni <- geo_summary(vars(birth_year, birth_muni_code)  , snsc)
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
    summarise(
      n = n(),
      n_birthweight = sum(!is.na(brthwt_g)), # there are 171975 missing birthwts in the entire data set
      n_apgar1 = sum(!is.na(apgar1)),
      n_apgar5 = sum(!is.na(apgar5)),
      n_gest_weeks = sum(!is.na(gest_weeks_cat)),
      n_m_age_yrs = sum(!is.na(m_age_yrs))) %>% 
    ungroup() %>%
    rename(paste_var = !!cat_vars_char) %>%
    filter(!paste_var %in% c("null", "Null", "NULL"))  # assumes the category variables have been changed to factors
#    filter(!is.null(paste_var)) %>%# assumes the category variables not factors
    
  
  # after ungrouping above and removing the null values in the cat_vars then total  
  total <- all_group %>%
    group_by_at(group_vars) %>%
    mutate_at(vars(starts_with("n")) , funs(total = sum(.))) %>%
    select_at(c(group_vars, vars(paste_var), vars(contains("_total")))) %>%
    ungroup()

  # now keep the columns for the n counts and the same columns with totals
  perc_all   <- all_group %>% select_at(vars(starts_with("n")))
  perc_total <- total %>% select_at(vars(contains("_total")))     
  # now perc_all / perc_total seen below will return the percentages.
  
  #varname_level_pct (e.g. sex_male_pct, sex_female_pct)
  percents <- all_group %>% 
    select_at(c(group_vars, vars(paste_var))) %>% 
    bind_cols(perc_all/perc_total) %>% # the percentage data.  Assuming the rows haven't changed order
    gather(key = "perc_var", value = "perc", -group_vars_char, -paste_var) %>%
    mutate(paste_var = str_to_lower(paste_var), spread_var = str_c(cat_vars_char, "_", paste_var, "_", perc_var, "_perc")) %>%
    select(!!group_vars_char, spread_var, perc) %>%
    filter(!is.na(spread_var)) %>% #not sure why there are NAs showing up in the spread var.
    spread(key = spread_var, value = perc)

return(percents)
}
# group by sex
# group by deliv type
# group by n_prenat_visit_cat
# group by gest_weeks_cat
# group by marital status
# group by birth_place I may have the hospital code identifiers on laptop.
# group by birth_assist
# group by race  
  
  
# proportions group sex     
  snsc_geo_state_sex <- geo_props(vars(birth_year, birth_state_code), vars(sex), snsc)
  snsc_geo_micro_sex <- geo_props(vars(birth_year, birth_micro_code), vars(sex), snsc)
  snsc_geo_meso_sex <- geo_props(vars(birth_year, birth_meso_code)  , vars(sex), snsc)
  snsc_geo_muni_sex <- geo_props(vars(birth_year, birth_muni_code)  , vars(sex), snsc)
  

## Still working on this. May add to function above ###
###########################################
snsc_geo_state_sex %>%
  gather(key = "var", value = "perc", -birth_year, -birth_state_code) %>%
  separate(var, into = c("group_var", "n_type"), sep = "_n_", remove = FALSE) %>%
  separate(group_var, into = c("variable", "variable_level")) %>%
  
  filter(birth_year %in% 2008:2009, !is.na(birth_state_code)) %>%
  ggplot(aes(x = birth_year, y = perc, color = n_type, fill = n_type)) +
  facet_grid(variable_level~birth_state_code, scales = "free_y") +
  geom_jitter(height = 0)
###########################################    
  
#proportions group deliv_type        
  snsc_geo_state_deliv <- geo_props(vars(birth_year, birth_state_code), vars(deliv_type), snsc)
  snsc_geo_micro_deliv <- geo_props(vars(birth_year, birth_micro_code), vars(deliv_type), snsc)
  snsc_geo_meso_deliv <- geo_props(vars(birth_year, birth_meso_code)  , vars(deliv_type), snsc)
  snsc_geo_muni_deliv <- geo_props(vars(birth_year, birth_muni_code)  , vars(deliv_type), snsc)
  
      
# merge code and name labels
brazilgeo::br_meso_codes

state <- snsc_geo_state %>%
  left_join(snsc_geo_state_sex) %>%
  left_join(snsc_geo_state_deliv) %>%
  rename(state_code = birth_state_code) %>%
  left_join(brazilgeo::br_state_codes) 

# publish data 
# 

data_publish(state, name = "snsc_state", file_type = "csv", type = "derived")
