# in progress...really messy...using lots of pieces from other scripts

# need return to trib pieces by strata Hat/Nat A/J -------------------------------------------
# above weir esc
tmp1 <- left_join(esc_aboveweir, MR_preferred, by = c('SurveyYear','strata')) %>% # T3 data
  filter(MR_method == MR_preferred) %>%
  select(SurveyYear, StreamName, strata, N_U)

# weir removals
grouping = c('trap_year','stream', 'age_designation', 'origin')
weir_removals_stratified = est_final_dispositions(trap_dat, grouping) %>%
  filter(disp_final == 'Brood Stock' |
           disp_final == 'Distribution' |
           disp_final == 'Nutrient Enhancement' |
           disp_final == 'Outplant') %>%
  group_by(trap_year, stream, age_designation, origin) %>%
  summarize(weir_removals = sum(n)) %>%
  ungroup() %>%
  mutate(strata = case_when(
    age_designation == "Adult" & origin == 'Hatchery'  ~ 'A_Hat',
    age_designation == "Adult" & origin == 'Natural'  ~ 'A_Nat',
    age_designation == "Jack/Jill" & origin == 'Hatchery' ~ 'J_Hat',
    age_designation == "Jack/Jill" & origin == 'Natural' ~ 'J_Nat',
    TRUE ~ NA_character_)) %>%
  filter(!is.na(strata)) %>%
  select(-c(age_designation, origin, stream))

tmp2 <- weir_removals_stratified %>%
  rename(SurveyYear = trap_year, weir_removals_tmp = weir_removals) %>%
  left_join(bs_returned) %>%
  mutate(across(where(is.numeric), replace_na, 0)) %>%
  mutate(weir_removals = weir_removals_tmp - BS_returned) %>%
  select(-c(StreamName, weir_removals_tmp, BS_returned))
  

# harvest data
tmp3 <- readxl::read_xlsx('./data/inputs/trib_harvest.xlsx') %>%
  filter(ReturnYear %in% yr_range) %>%
  pivot_longer(!c(ReturnYear, Entity), names_to = 'strata', values_to = 'trib_harvest') %>%
  group_by(ReturnYear, strata) %>%
  summarize(trib_harvest = sum(trib_harvest)) %>%
  rename(SurveyYear = ReturnYear)

# recycled fish - final disp (since they aren't included in weir removal)
# needed to adjust ND to include origin and age strata
grouping = c('trap_year','stream', 'age_designation', 'origin')
tmp_recycled <- est_final_dispositions(trap_dat, grouping) %>%
  filter(disp_final == 'Recycled') %>%
  group_by(trap_year, stream, age_designation, origin) %>%
  summarize(recycled = sum(n)) %>%
  ungroup() %>%
  mutate(strata = case_when(
    age_designation == "Adult" & origin == 'Hatchery'  ~ 'A_Hat',
    age_designation == "Adult" & origin == 'Natural'  ~ 'A_Nat',
    age_designation == "Jack/Jill" & origin == 'Hatchery' ~ 'J_Hat',
    age_designation == "Jack/Jill" & origin == 'Natural' ~ 'J_Nat',
    TRUE ~ NA_character_)) %>%
  filter(!is.na(strata)) %>%
  select(-c(age_designation, origin, stream)) %>%
  rename(SurveyYear = trap_year)

weir_pNat_tmp <- left_join(tmp1, tmp2) %>%
  left_join(tmp3) %>%
  left_join(tmp_recycled) %>%
  mutate(across(where(is.numeric), replace_na, 0)) %>%
  mutate(weir_sum = N_U + weir_removals + recycled) %>%
  select(SurveyYear, StreamName, strata, weir_sum) %>%
  pivot_wider(values_from = weir_sum, names_from = strata) %>%
  mutate(A_pNat = A_Nat / (A_Nat + A_Hat),
         J_pNat = J_Nat / (J_Nat + J_Hat))

N_D_tmp <- full_join(tmp_esc, tmp_weir) %>%
  pivot_wider(names_from = tmp_strata,
              values_from = c(N_U, N_U_upr, N_U_lwr, N_weir)) %>%
  full_join(adults_D[,c('stream','trap_year','N_D_A','N_D_A_lwr','N_D_A_upr')]) %>%
  mutate(N_D_J = (N_U_J + N_weir_J) / (N_U_A + N_weir_A) * N_D_A,
         N_D_J_upr = (N_U_upr_J + N_weir_J) / (N_U_upr_A + N_weir_A) * N_D_A_upr,
         N_D_J_lwr = (N_U_lwr_J + N_weir_J) / (N_U_lwr_A + N_weir_A) * N_D_A_lwr,
         N_D = N_D_J + N_D_A,
         N_D_lwr = N_D_J_lwr + N_D_A_lwr,
         N_D_upr = N_D_J_upr + N_D_A_upr) %>%
  mutate(across(where(is.numeric), round, digits=0)) %>%
  select(stream, trap_year, N_D_A, N_D_J) %>%
  rename(StreamName = stream, SurveyYear = trap_year)

N_D_strata <- left_join(N_D_tmp, weir_pNat_tmp) %>%
  mutate(N_D_A_Nat = N_D_A * A_pNat,
         N_D_A_Hat = N_D_A - N_D_A_Nat,
         N_D_J_Nat = N_D_J * J_pNat,
         N_D_J_Hat = N_D_J - N_D_J_Nat) %>%
  mutate(across(where(is.numeric), round, digits=0)) %>%
  ungroup() %>%
  select(SurveyYear, N_D_A_Nat, N_D_A_Hat, N_D_J_Nat, N_D_J_Hat) %>%
  pivot_longer(cols = starts_with('N_D_'),
               names_prefix = 'N_D_',
               names_to = 'strata',
               values_to = 'N_D')
  
esc_strata = left_join(tmp1, tmp2) %>%
  left_join(tmp3) %>%
  left_join(N_D_strata) %>%
  mutate(across(where(is.numeric), replace_na, 0)) %>%
  mutate(trib_esc = N_U + weir_removals + trib_harvest + N_D)

tmp_4Brian <- esc_strata %>%
  select(SurveyYear, StreamName, strata, trib_esc) %>%
  pivot_wider(names_from = strata, names_glue = '{strata}_{.value}', values_from = trib_esc) %>%
  mutate(sumcheck = rowSums(.[,3:6]))
write_xlsx(tmp_4Brian, path = './tmp/trib_esc_stratified_12-21.xlsx')

tmp_4Brian2 <- esc_strata %>%
  select(SurveyYear, StreamName, strata, N_U, N_D) %>%
  mutate(N_inriver = N_U + N_D) %>%
  select(-c(N_U, N_D)) %>%
  pivot_wider(names_from = strata, names_glue = '{strata}_{.value}', values_from = N_inriver) %>%
  mutate(sumcheck = rowSums(.[,3:6]))
write_xlsx(tmp_4Brian2, path = './tmp/esc_stratified_12-21.xlsx')

tmp_checkesc <- left_join(tmp_4Brian, T3_T6, c('SurveyYear' = 'trap_year')) %>%
  select(SurveyYear, trib_esc, sumcheck) # looks good
#2014 discrepancy is b/c a minijack is included in the total trib #s
#2019 discrepancy is b/c two hatchery adult recaps with 2rops were repurposed...
#...but there were no hatchery adults outplants to begin with...probably a few on the cusp of
#...adult/jack size, those fish were accounted for in the coarser grouping

# if keeping the above approach, add necessary CI info

# adding AgeKey and BestAge from NEO dataset to  the 'cleaned' CarcsData -----------------------

# temporary fix - update missing adult/jack data
load('./data/inputs/CarcsData.rda')
carcs_updated <- CarcsData %>%
  filter(River == 'Lostine River') %>%
  select(c(CarcassID, Adult_or_Jack, BestAge, AgeKey)) %>%
  rename(Best_Age = BestAge, Age_Key = AgeKey) %>%
  mutate(CarcassID = as.character(.$CarcassID))

car_dat_tmp <- car_dat %>%
   filter(Species == 'Chinook salmon' & Run == 'Spring/summer') %>%
   filter(StreamName == 'Lostine River') %>%
   left_join(carcs_updated, by = c('CarcassWPT' = 'CarcassID'))

# for some reason NPT is using CarcassWPT as the unique ID???
# may need to change because we are now taking waypoints for each carcass in NE Oregon
# doesn't make sense to me....maybe we add an NEOCarcassID field in cleaning script
# BestAge and AgeKey could also be added by an edit to the cleaning script

# define strata to match esc #s, include all available best age data
best_age_strata <- car_dat_tmp %>%
  mutate(strata = case_when(
    Adult_or_Jack == 'Adult' & Origin == 'Hatchery' ~ 'A_Hat',  
    Adult_or_Jack == 'Adult' & Origin == 'Natural' ~ 'A_Nat',
    Adult_or_Jack == 'Jack' & Origin == 'Hatchery' ~ 'J_Hat',
    Adult_or_Jack == 'Jack' & Origin == 'Natural' ~ 'J_Nat',
    TRUE ~ NA_character_ )) %>%
  filter(!is.na(strata)) %>%
  group_by(SurveyYear, strata, Best_Age) %>%
  summarise(n = n()) %>%
  filter(!is.na(Best_Age)) %>%
  pivot_wider(names_from = Best_Age, names_prefix = 'Age_', values_from = n) %>%
  mutate(across(where(is.numeric), replace_na, 0)) %>%
  #pt estimates for now - add CIs later
  mutate(total_aged = Age_3 + Age_4 + Age_5,
         p3 = Age_3 / total_aged,
         p4 = Age_4 / total_aged,
         p5 = Age_5 / total_aged)
                           
#use age props above (and jack 20 criteria) to partition esc #s by strata to esc #s by age
tmp <- left_join(esc_strata, best_age_strata) %>%
  select(SurveyYear, strata, trib_esc, total_aged, p3, p4, p5) %>%
  #if H
  #esc_H3 = trib_esc * p3
  #esc_H4 = trib_esc * p4
  #esc_H5 = trib_esc * p5
  #if N
  #esc_H3 = trib_esc * p3
  #esc_H4 = trib_esc * p4
  #esc_H5 = trib_esc * p5
  #then aggregate stratas by return year, sum esc_XX should give us run recon
  #add/adjust to BY output
  
  #add these caveats
  #if all zero p's for year/strata - if all zeros and jack, jack = 3, if adult, adult =4
  #if total_aged and jack - p3 = 1 OR esc_X3 = trib_esc
  # will need to add age 2s...6? into this when doing full time series.
  

  
  

