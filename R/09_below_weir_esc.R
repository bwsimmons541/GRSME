# below weir escapement ----------------------------------------------------------------

# adults below weir = adults/redd upstream * redds below 

# Summarize downstream and upstream redds ---------------------------------

  redds <- redd_dat %>%
    filter(Species == 'Chinook salmon' & Run == 'Spring/summer') %>%
    filter(StreamName == 'Lostine River') %>%
    group_by(SurveyYear, AboveWeir, ActivityId) %>%
    summarize(NewRedds = mean(NewRedds)) %>%
    group_by(SurveyYear, AboveWeir) %>%
    summarize(NewRedds = sum(NewRedds)) %>%
    pivot_wider(names_from = AboveWeir, values_from = NewRedds) %>%
    rename(Redds_D = No, Redds_U = Yes)

# Adults upstream from T3_trib_esc ----------------------------------------

  adults_U <- left_join(esc_aboveweir,MR_preferred, by = c('SurveyYear','strata')) %>%
    filter(MR_method == MR_preferred) %>%
    filter(strata %in% c("A", "A_Hat", "A_Nat"))

# Calculate Downstream Adult Estimates ------------------------------------

  adults_D <- left_join(adults_U,redds,by='SurveyYear') %>%
    mutate(Fish_per_redd_U_A = N_U / Redds_U,
           Fish_per_redd_U_A_lwr = N_U_lwr / Redds_U,
           Fish_per_redd_U_A_upr = N_U_upr / Redds_U,
           N_D_A = round(Fish_per_redd_U_A * Redds_D, 0),
           N_D_A_lwr = round(Fish_per_redd_U_A_lwr * Redds_D, 0),
           N_D_A_upr = round(Fish_per_redd_U_A_upr * Redds_D, 0)) %>%
    rename(trap_year = SurveyYear, stream = StreamName) %>%
    mutate(origin = case_when(
      strata == "A_Hat" ~ "Hat",
      strata == "A_Nat" ~ "Nat",
      strata == "A" ~ "All",
      TRUE ~ strata),
      N_U_A = N_U,
      N_U_lwr_A = N_U_lwr,
      N_U_upr_A = N_U_upr) %>%
    select(-strata, -N_U, -N_U_lwr, -N_U_upr) #Whyd did I get rid of strata? not needed when joining by origin later.


# Temporary upstream escapement data.frame --------------------------------

  tmp_esc <- left_join(esc_aboveweir, MR_preferred, by = c('SurveyYear','strata')) %>%
    filter(MR_method == MR_preferred) %>%
    filter(!is.na(strata)) %>%
    group_by(StreamName, SurveyYear, strata) %>%
    summarise(N_U = sum(N_U),
              N_U_lwr = sum(N_U_lwr),
              N_U_upr = sum(N_U_upr)) %>%
    rename(trap_year = SurveyYear, stream = StreamName)


# Calculate Weir Removals -----------------------------------------------------------

# Grouping for est_final_disposition

  grouping <- c('trap_year','stream', 'age_designation', 'origin')

# Weir removals strata A J

  tmp_weir <- est_final_dispositions(trap_dat, grouping) %>%
    filter(disp_final != 'Natural Spawning') %>%
    mutate(strata = case_when(
      age_designation == 'Adult' ~ 'A',
      age_designation == 'Jack/Jill' ~'J',
      TRUE ~ NA_character_)) %>%
    filter(!is.na(strata)) %>%
    group_by(trap_year, stream, strata) %>%
    summarize(N_weir = sum(n))

# Weir removals for A_Hat, A_Nat, J_Hat, J_Nat

  tmp_weir2 <- est_final_dispositions(trap_dat, grouping) %>%
    filter(disp_final != 'Natural Spawning') %>%
    mutate(strata = case_when(
      age_designation == 'Adult' & origin == 'Hatchery' ~ 'A_Hat',
      age_designation == 'Adult' & origin == 'Natural' ~ 'A_Nat',
      age_designation == 'Jack/Jill' & origin == 'Hatchery' ~ 'J_Hat',
      age_designation == 'Jack/Jill' & origin == 'Natural' ~ 'J_Nat',
        TRUE ~ NA_character_)) %>%
    filter(!is.na(strata)) %>%
    group_by(trap_year, stream, strata) %>%
    summarize(N_weir = sum(n))

# Combine all strata to single dataframe

  tmp_weir <- bind_rows(tmp_weir, tmp_weir2)
  rm(tmp_weir2)

# Create N_Weir_A for jack calculations
  
  tmp_weir3 <- tmp_weir %>%
    mutate(origin = case_when(
      strata == "A_Hat" ~ "Hat",
      strata == "A_Nat" ~ "Nat",
      strata == "A" ~ "All",
      TRUE ~ strata),
      N_weir_A = N_weir) %>%
    filter(origin %in% c("Hat", "Nat", "All")) %>%
    select(-strata, -N_weir)
  

# Join upstream escapement and weir add origin to append other Adult fields
  

  N_D <- full_join(tmp_esc, tmp_weir, by = c("stream", "trap_year", "strata")) %>%
    mutate(origin = case_when(
      strata == "A_Hat" ~ "Hat",
      strata == "J_Hat" ~ "Hat",
      strata == "A_Nat" ~ "Nat",
      strata == "J_Nat" ~ "Nat",
      strata == "A" ~ "All",
      strata == "J" ~ "All",
      TRUE ~ strata),
      N_weir = case_when(
        is.na(N_weir) ~ 0, # In a lot of years there are not natural jacks removed
        TRUE ~ N_weir
      ))

# Add adults removed at weir (N_weir_A) for jack calculations
  
  N_D <- full_join(N_D, tmp_weir3, by = c("stream", "trap_year", "origin"))

# Join N_D and Adults_D 
  
  N_D <- left_join(N_D, adults_D, by = c('stream', 'trap_year', 'origin'))
  
# Calculate Downstream Juveniles ------------------------------------------

  N_D <- N_D %>%
    mutate(N_D = case_when(
              strata == "J_Hat" ~ (N_U + N_weir) / (N_U_A + N_weir_A) * N_D_A,
              strata == "J_Nat" ~ (N_U + N_weir) / (N_U_A + N_weir_A) * N_D_A,
              strata == "J" ~ (N_U + N_weir) / (N_U_A + N_weir_A) * N_D_A, 
              TRUE ~ N_D_A),
           N_D_lwr = case_when(
              strata == "J_Hat" ~ (N_U_lwr + N_weir) / (N_U_lwr_A + N_weir_A) * N_D_A_lwr,
              strata == "J_Nat" ~ (N_U_lwr + N_weir) / (N_U_lwr_A + N_weir_A) * N_D_A_lwr,
              strata == "J" ~ (N_U_lwr + N_weir) / (N_U_lwr_A + N_weir_A) * N_D_A_lwr,
              TRUE ~ N_D_A_lwr),
           N_D_upr = case_when(
               strata == "J_Hat" ~ (N_U_lwr + N_weir) / (N_U_upr_A + N_weir_A) * N_D_A_upr,
               strata == "J_Nat" ~ (N_U_lwr + N_weir) / (N_U_upr_A + N_weir_A) * N_D_A_upr,
               strata == "J" ~ (N_U_lwr + N_weir) / (N_U_upr_A + N_weir_A) * N_D_A_upr,
               TRUE ~ N_D_A_upr),
           across(where(is.numeric), round, digits=0)) %>%
    select(stream, trap_year, strata, N_D, N_D_lwr, N_D_upr, N_weir)


# includes recycles in N_weir which differs from removals methods
# this may not be consistent with comanager spreadsheet
# including the unique recycled fish here seems prudent

  
# to split adults by origin - ?not necessary for report? (?but necessary for project?)
# .... incomplete below here ....
# must be a simpler way...?multiply by hat frac of carcs below weir...but same issues as above weir?



