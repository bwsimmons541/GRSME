# Table 7 - age summary -------------------------------------------------------
# Age composition, by return year, based on known-age carcasses

# this is NOT how GRSME estimates age composition of return years or brood years
# we DO NOT want to use these proportions for GRSME run-reconstruction work
# this is a summary of carcasses that were aged...nothing more
# it is a means to compare age of carcasses across populations (for what that's worth)
# we know "known-age" carcasses are not a random sample for the whole Lostine return
# surveyors haven't necessarily collected scales on all hatchery fish for this very reason
# also, in some years ODFW selectively read scales (non-random)
# including length age would give us a better sample but then there is still
#    a bias against recovering jacks to consider

car_dat_tmp <- car_dat %>%
  filter(Species == 'Chinook salmon' & Run == 'Spring/summer') %>%
  mutate(Best_Age = case_when(!is.na(CWT_Age) ~ as.character(CWT_Age),
                             !is.na(PIT_Age) ~ as.character(PIT_Age),
                             !is.na(VIE_Age) ~ as.character(VIE_Age),
                             !is.na(Fin_Age) ~ as.character(Fin_Age),
                             !is.na(Scale_Age) ~ as.character(Scale_Age)))

# estimate age proportions w/CIs
pAge_carc <- car_dat_tmp %>%
  filter(!is.na(Best_Age)) %>%
  sum_groups(.summary_var = Best_Age, .cnt_var = Count, SurveyYear,MPG,POP_NAME) %>%
  pivot_wider(names_from = Best_Age, names_prefix = 'Age_', values_from = n) %>%
  mutate(across(where(is.numeric), replace_na, 0),
         tots = Age_3 + Age_4 + Age_5) %>%
  bind_cols(est_proportion(.$Age_3,.$tots, method = 'score')) %>%
  rename(pAge_3 = p, pAge_3_SE = SE, pAge_3_lwr = lwr, pAge_3_upr = upr) %>%
  select(-c(Age_2,Age_3,pAge_3_SE)) %>%
  bind_cols(est_proportion(.$Age_4,.$tots, method = 'score')) %>%
  rename(pAge_4 = p, pAge_4_SE = SE, pAge_4_lwr = lwr, pAge_4_upr = upr) %>%
  select(-c(Age_4,pAge_4_SE)) %>%
  bind_cols(est_proportion(.$Age_5,.$tots, method = 'score')) %>%
  rename(pAge_5 = p, pAge_5_SE = SE, pAge_5_lwr = lwr, pAge_5_upr = upr, n_age = tots) %>%
  select(-c(Age_5,pAge_5_SE)) %>%
  mutate(POP_NAME=case_when(POP_NAME == 'Lostine River' ~ 'Wallowa/Lostine River',
                            TRUE ~ POP_NAME)) %>%
  relocate(n_age, .after = last_col())

rm(car_dat_tmp)
# could use apply() approach...
# also saw that there is an est_proportions function after doing this...

# could leave in summary counts used to estimate proportions
# at least as inputs for the report template spreadsheets
# also a good qa-qc check to at least review those numbers
# or just do the qa-qc here

# writexl::write_xlsx(pAge_carc, path = './data/outputs/T7_carc_ages.xlsx')
