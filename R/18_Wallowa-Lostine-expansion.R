# work-in-progress...some of the inputs from other scripts are messy...
# need to first run working script and T8 script to use this...
# messy :)

# Wallowa/Lostine escapement and spawner abundance

# grab esc to Lostine by adult/jack hat/nat from T8
LOS_esc <- tmp_4Brian %>% select(-sumcheck) %>% rename(return_year = SurveyYear, stream = StreamName)

# below weir script - adults_D tibble -  has adult esc above weir, redds above weir, and fish per redd above weir
LOS_AperRedd <- adults_D %>% select(stream, trap_year, N_U_A, Redds_U, Fish_per_redd_U_A) %>%
  rename(return_year = trap_year)

df <- left_join(LOS_esc, LOS_AperRedd) %>%
  mutate(Hat_JperA = J_Hat_trib_esc / A_Hat_trib_esc,
         Nat_JperA = J_Nat_trib_esc / A_Nat_trib_esc)

# get total redds for Bear, Wallowa, Hurricane
other_redds <- redd_dat %>%
  filter(Species == 'Chinook salmon',
         Run == 'Spring/summer',
         POP_NAME == 'Lostine River',
         StreamName != 'Lostine River') %>%
  group_by(SurveyYear, StreamName, ActivityId) %>%
  summarize(NewRedds = mean(NewRedds)) %>%
  group_by(SurveyYear, StreamName) %>%
  summarize(NewRedds = sum(NewRedds)) %>% #awkward work-around - could we sum counts instead?
  pivot_wider(values_from = NewRedds, names_from = StreamName, names_prefix = 'redds_') %>%
  rename(return_year = SurveyYear)

df <- df %>% left_join(other_redds) %>%
  mutate(esc_nonLOS_A = (.$'redds_Bear Creek' + .$'redds_Hurricane Creek' + .$'redds_Wallowa River')
         * Fish_per_redd_U_A) %>%
  mutate(esc_nonLOS_A = round(esc_nonLOS_A, 0))

other_carcs <- car_dat %>%
  filter(Species == 'Chinook salmon',
         Run == 'Spring/summer',
         POP_NAME == 'Lostine River',
         StreamName != 'Lostine River',
         Origin != 'Unknown',
         ForkLength > 630) %>%
  group_by(SurveyYear, Origin) %>%
  summarize(n_carc_A = sum(Count)) %>%
  pivot_wider(names_from = Origin, names_prefix = 'n_carc_A_', values_from = n_carc_A) %>%
  mutate(hatfrac_nonLOS_A = n_carc_A_Hatchery / (n_carc_A_Hatchery + n_carc_A_Natural)) %>%
  rename(return_year = SurveyYear)
  
df <- df %>% left_join(other_carcs) %>%
  mutate(esc_nonLOS_A_Hat_withoutplants = esc_nonLOS_A * hatfrac_nonLOS_A) %>%
  mutate(esc_nonLOS_A_Hat_withoutplants = round(esc_nonLOS_A_Hat_withoutplants, 0)) 

grouping = c('trap_year','stream', 'age_designation')
outplants = est_final_dispositions(trap_dat, grouping) %>%
  filter(disp_final == 'Outplant',
         age_designation != 'Mini-Jack') %>%
  ungroup() %>%
  select(trap_year, age_designation, n) %>%
  pivot_wider(names_from = age_designation, values_from = n, names_prefix = 'outplants_') %>%
  mutate(across(where(is.numeric), replace_na, 0)) %>%
  rename(return_year = trap_year)

df <- df %>% left_join(outplants) %>%
  mutate(esc_nonLOS_A_Hat_without_outplants = esc_nonLOS_A_Hat_withoutplants - outplants_Adult) %>%
  mutate(esc_nonLOS_A_Hat_without_outplants = case_when(
    esc_nonLOS_A_Hat_without_outplants < 0 ~ 0, TRUE ~ esc_nonLOS_A_Hat_without_outplants)) %>%
  mutate(esc_nonLOS_A_Nat = esc_nonLOS_A - esc_nonLOS_A_Hat_withoutplants) %>%
  mutate(esc_WL_J_Hat = 1,
         esc_WL_A_Hat = 1,
         esc_WL_Hat = 1,
         esc_WL_J_Nat = 1,
         esc_WL_A_Nat = 1,
         esc_WL_Nat = 1,
         esc_WL_all = 1,
         esc_WL_all = 1) #up through escapement, next up, W-L spawner abundance estimates by strata

