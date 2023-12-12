# Table 5 - sgs stuff ---------------------------------------------------------


# total redds by ESA  ---------------------------------------------------------
redds_total <- redd_dat %>%
  filter(Species == 'Chinook salmon', Run == 'Spring/summer') %>%
  group_by(SurveyYear,MPG,POP_NAME,StreamName,ActivityId) %>%
  summarize(NewRedds = mean(NewRedds)) %>% #could we use aggregate function instead
  group_by(SurveyYear,MPG,POP_NAME) %>%
  summarize(NewRedds = sum(NewRedds))
#if there is one record for each redd
#?could just count a different field instead of using the newredds field/grouping


# phos - carcass-based, grouped by pop, all sizes ------------------------------
pHOS_carc <- car_dat %>%
  filter(Origin != 'Unknown') %>%
  sum_groups(.summary_var = Origin, .cnt_var = Count, SurveyYear,MPG,POP_NAME) %>%
  pivot_wider(names_from = Origin, values_from = n) %>%
  mutate(across(where(is.numeric), replace_na, 0)) %>%
  bind_cols(est_proportion(.$Hatchery, .$Hatchery + .$Natural, method = 'score')) %>%
  rename(pHOS = p, pHOS_SE = SE, pHOS_lwr = lwr, pHOS_upr = upr) %>%
  select(-c(Hatchery,Natural,pHOS_SE))


# female proportion - carcass-based, grouped by pop, all sizes ----------------
pFemale_carc <- car_dat %>%
  filter(Sex != 'Unknown') %>%
  filter(Sex != 'Jack') %>% # database error - should be unknown, not Jack
  sum_groups(.summary_var = Sex, .cnt_var = Count, SurveyYear, MPG, POP_NAME) %>%
  pivot_wider(names_from = Sex, values_from = n) %>%
  mutate(across(where(is.numeric), replace_na, 0)) %>%
  bind_cols(est_proportion(.$Female, .$Female + .$Male, method = 'score')) %>%
  rename(pFemale = p, pFemale_SE = SE, pFemale_lwr = lwr, pFemale_upr = upr) %>%
  select(-c(Female,Male,pFemale_SE))


# prespawn mort, grouped by pop, all sizes ------------------------------------
psm <- car_dat %>%
  #might be worth double-checking spawned-out criteria in cleaning script
  filter(Sex == 'Female' & SpawnedOut != 'NA') %>%
  sum_groups(.summary_var = SpawnedOut,
             .cnt_var = Count, SurveyYear,MPG,POP_NAME) %>%
  pivot_wider(names_from = SpawnedOut, values_from = n) %>%
  mutate(across(where(is.numeric), replace_na, 0)) %>%
  bind_cols(est_proportion(.$No, .$No + .$Yes, method = 'score')) %>%
  rename(psm = p, psm_SE = SE, psm_lwr = lwr, psm_upr = upr) %>%
  select(-c(Yes,No,psm_SE))

# table 5 - pulling the pieces together  --------------------------------------
sgs_stuff <- full_join(redds_total,pHOS_carc) %>%
  full_join(pFemale_carc) %>% full_join(psm) %>%
  mutate(POP_NAME = case_when(
    POP_NAME == 'Lostine River' ~ 'Wallowa/Lostine River',
    TRUE ~ POP_NAME))

writexl::write_xlsx(T5_sgs_stuff, path = './data/outputs/sgs_stuff.xlsx')
