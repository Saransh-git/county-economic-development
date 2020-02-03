############################################################################################################################
# trimming data

import::here(grwth_county, multiplot, .from="data_preparation.R")

# Based on univariate analysis across all the predictors, the observations are trimmed at values as below
# Note: This is based on histogram, boxplot analysis

grwth_county = grwth_county %>% dplyr::filter(sex_ratio >=0.55 && sex_ratio <= 1.30) %>% 
  dplyr::filter(pct_empd >=20 && pct_empd <= 65) %>% dplyr::filter(pct_bpl <= 37) %>%
  dplyr::filter(pct_occ01 <= 0.3) %>% dplyr::filter(pct_occ02 <= 0.16) %>% 
  dplyr::filter(pct_occ03 <= 0.15) %>% dplyr::filter(pct_occ04 >= 0.014 && pct_occ04 <= 0.13) %>%
  dplyr::filter(pct_occ05 <= 0.17) %>% dplyr::filter(pct_cow01 >= 9 && pct_cow01 <= 51) %>% 
  dplyr::filter(pct_cow02 >= 2 && pct_cow02 <= 18) %>% dplyr::filter(pct_cow03 <= 12) %>%
  dplyr::filter(pct_cow04 <= 1.2) %>% dplyr::filter(pct_lessthan_highgrad <= 28) %>%
  dplyr::filter(pct_highschool_grad >= 10 && pct_highschool_grad <= 42) %>% 
  dplyr::filter(pct_bachdeg_or_highr <= 42) %>% dplyr::filter(tax_in <= 7000) %>%
  dplyr::filter(pct_wrk_class >= 53 && pct_wrk_class <= 78) %>% 
  dplyr::filter(pct_dep_class >= 14 && pct_dep_class <= 33)

