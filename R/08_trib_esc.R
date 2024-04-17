
# Table 3 - escapement to trib ------------------------------------------------

# Load Packages -------------------------------------------------------------

  # library(cuyem)
  # library(tidyverse)

# Set Year Range -------------------------------------------------------------

# yr_range = 2013:2022

#Load Data -------------------------------------------------------------------

  # load('./data/inputs/ReddsData.rda')
  # redd_dat <- clean_reddData_NEOR(ReddsData) %>%
  #   filter(SurveyYear %in% yr_range & ReddSpecies == 'S_CHN')
  # rm(ReddsData)
  # 
  # load('./data/inputs/WeirData.rda')
  # trap_dat <- clean_weirData(WeirData) %>%
  #   filter(facility == 'NPT GRSME Program') %>%
  #   filter(trap_year %in% yr_range)
  # rm(WeirData)
  # 
  # load('./data/inputs/CarcsData.rda')
  # car_dat <- clean_carcassData_NEOR(CarcsData) %>%
  #   filter(SurveyYear %in% yr_range)
  # rm(CarcsData)

# look up preferred mark-recap method - preliminary approach ------------------
MR_preferred = read_xlsx('./data/inputs/MR_preferred_method.xlsx') %>% 
  select(-Notes) %>%
  pivot_longer(!SurveyYear,names_to = 'strata',values_to='MR_preferred')
  
# this was an afterthought ... which made the script extra messy
# but should allow us to compare a few different M/R methods
# a table like this could document metadata on method choices
# e.g., switching to size/origin MR strata because of carcass recovery bias
# e.g., only summary data available as size strata (2001-2008)
# e.g., limited weir op's, low trap eff, ..., etc. etc.

# mark-recap above weir - weir data work --------------------------------------

# n1 = live, marked fish passed upstream of weir

# get last survey date above weir to filter n1 ====
# this assumes last carcass recovery date = last opportunity to recover marked fish
last_sgs <- redd_dat %>%
  filter(StreamName == 'Lostine River') %>%
  filter(AboveWeir == 'Yes') %>%
  filter(Species == 'Chinook salmon' & Run == 'Spring/summer') %>%
  group_by(SurveyYear, StreamName) %>%
  summarise(last_sgs = max(SurveyDate))


# n1 upstream fish Adults and Jacks ====
n1 <- trap_dat %>%
  filter(species == 'Chinook') %>%
  filter(marked) %>%  # derived field from cleaning script
  # fish punched and passed upstream, should exclude existing LOP recaps
  mutate(count = as.numeric(count)) %>%
  mutate(strata = case_when( # define first strata for mark-recapture
    age_designation == 'Adult' ~ 'A',  
    age_designation == 'Jack/Jill' ~ 'J',  
    TRUE ~ NA_character_ )) %>%
  filter(!is.na(strata)) %>%
  left_join(last_sgs, by = c('trap_year' = 'SurveyYear')) %>%
  mutate(count_late = case_when(
    trapped_date > last_sgs ~ 1,
    TRUE ~ 0)) %>%
  group_by(trap_year, stream, strata) %>%
  summarize(mark_released = sum(count, na.rm = TRUE),
            released_late = sum(count_late, na.rm = TRUE)) %>%
  as.data.frame() %>%
  rename(SurveyYear = trap_year, StreamName = stream)

# adding additional strata for consideration
# this approach will be repeated for n2 and m2
n1tmp <- trap_dat %>%
  filter(species == 'Chinook') %>%
  filter(marked) %>%  # derived field from cleaning script
  # fish punched and passed upstream, should exclude existing LOP recaps
  mutate(count = as.numeric(count)) %>%
  mutate(strata = case_when( # define second level of strata for mark-recapture
    age_designation == 'Adult' & origin == 'Hatchery' ~ 'A_Hat',  
    age_designation == 'Adult' & origin == 'Natural' ~ 'A_Nat',
    age_designation == 'Jack/Jill' & origin == 'Hatchery' ~ 'J_Hat',
    age_designation == 'Jack/Jill' & origin == 'Natural' ~ 'J_Nat',  
    TRUE ~ NA_character_ )) %>%
  filter(!is.na(strata)) %>%
  left_join(last_sgs, by = c('trap_year' = 'SurveyYear')) %>%
  mutate(count_late = case_when(
      trapped_date > last_sgs ~ 1,
    TRUE ~ 0)) %>%
  group_by(trap_year, stream, strata) %>%
  summarize(mark_released = sum(count, na.rm = TRUE),
            released_late = sum(count_late, na.rm = TRUE)) %>%
  as.data.frame() %>%
  rename(SurveyYear = trap_year, StreamName = stream)

n1 <- rbind(n1,n1tmp)
rm(n1tmp, last_sgs)

# 2009 is a problem - missing age designations, and 1997-2000?
# will need to add another method to use summarized data for 2001-2009

# unspawned brood returned to the Lostine River above the weir
# prior to the last SGS survey
# would be better to have a file source for this (and eventually in CDMS)
# need to also split this out by origin in order to do MR by size/origin strata
# should document any known info about these fish, sex, marking, etc.
# 2018 - 12 ? - need to do some digging
# 2019 - 4 NAT A ?sex
# 2020 - 2 NAT A F
# 2021 - ?Brian?

# Enter Brootdstock Return Data ====
bs_returned <- tribble(
  ~SurveyYear, ~ StreamName, ~ strata, ~ BS_returned,
  2018, 'Lostine River', 'A', 12,
  2018, 'Lostine River', 'A_Hat', 12,
  2018, 'Lostine River', 'J', 1,
  2018, 'Lostine River', 'J_Hat', 1,
  2019, 'Lostine River', 'A', 4,
  2019, 'Lostine River', 'A_Nat', 4,
  2020, 'Lostine River', 'A', 2,
  2020, 'Lostine River', 'A_Nat', 2,
  2021, 'Lostine River', 'A', 2,
  2021, 'Lostine River', 'A_Hat', 2,
  2023, 'Lostine River', 'A', 10,
  2023, 'Lostine River', 'A_Hat', 10)

  # No broodstock returned in 2022

# Add Broodstock Return Data to N1 ====
n1 <- left_join(n1, bs_returned) %>%
  replace_na(replace = list(BS_returned = 0)) %>%
  mutate(n1 = mark_released - released_late + BS_returned)

# if fish returned after last survey - should be treated like fish passed after
# last survey -- does this even happen? might not be worth worrying about

# BS_returned should also be subtracted from weir removals, so they are not
# double-counted


# mark-recap above weir - carcass data wrangling ====
car_up <- car_dat %>%
  filter(StreamName == 'Lostine River') %>%
  filter(AboveWeir == 'Yes') %>%
  filter(Species == 'Chinook salmon' & Run == 'Spring/summer') %>% # This species filter is unnecessary
  mutate(MR_strata = case_when( # This changes the FL of -999 to MR_Strata NA
    ForkLength == '-999' ~ "NA", # 15 Jacks and 1 Adult in 2022 with FL -999
    TRUE ~ MR_strata)) %>%
  mutate(strata = case_when( # renames strata to match weir strata names
    MR_strata == 'Adult' ~ 'A', 
    MR_strata == 'Jack' ~ 'J',
    TRUE ~ NA_character_)) %>%
  filter(!is.na(strata))


# n2 = total carcasses recovered above weir with discernable mark/no mark ====

n2 <- car_up %>%
  filter(Mark_Discernible == TRUE) %>% # We're not Actually applying any filter here only 2 FALSE for 2022. No Opercle Punches
  sum_groups(.summary_var = strata, .cnt_var = Count, SurveyYear, ReportingGroup) %>%
  rename(StreamName = ReportingGroup, n2 = n)

# adding additional Strata to N2
car_uptmp <- car_dat %>%
  filter(StreamName == 'Lostine River') %>%
  filter(AboveWeir == 'Yes') %>%
  filter(Species == 'Chinook salmon' & Run == 'Spring/summer') %>%
  mutate(MR_strata = case_when( # This changes the FL of -999 to MR_Strata = NA
    ForkLength == '-999' ~ "NA", # 15 Jacks and 1 Adult in 2022 with FL -999
    TRUE ~ MR_strata)) %>%
  mutate(strata = case_when(
    MR_strata == 'Adult' & Origin == 'Hatchery' ~ 'A_Hat',  
    MR_strata == 'Adult' & Origin == 'Natural' ~ 'A_Nat',
    MR_strata == 'Jack' & Origin == 'Hatchery' ~ 'J_Hat',
    MR_strata == 'Jack' & Origin == 'Natural' ~ 'J_Nat',  
    TRUE ~ NA_character_ )) %>%
  filter(!is.na(strata))

# filter new strata for discernable marks
n2tmp <- car_uptmp %>%
  filter(Mark_Discernible == TRUE) %>% #changed to include only OP marks discernable
  sum_groups(.summary_var = strata, .cnt_var = Count, SurveyYear, ReportingGroup) %>%
  rename(StreamName = ReportingGroup, n2 = n)

# combine with n2
n2 <- rbind(n2,n2tmp)
rm(n2tmp)

# m2 = recovered carcasses with marks ====
m2 <- car_up %>%
  filter(Recapture) %>%
  sum_groups(.summary_var = strata, .cnt_var = Count, SurveyYear, ReportingGroup) %>%
  rename(StreamName = ReportingGroup, m2 = n)

m2tmp <- car_uptmp %>%
  filter(Recapture) %>%
  sum_groups(.summary_var = strata, .cnt_var = Count, SurveyYear, ReportingGroup) %>%
  rename(StreamName = ReportingGroup, m2 = n)

m2 <- rbind(m2,m2tmp)
rm(m2tmp)
rm(car_uptmp)


# mark-recap above weir - estimates ----------------------------------------

# join n1, n2, m2 estimate abundance with adjusted peterson ====
MR_aboveweir <- full_join(n1, n2) %>%
  full_join(m2) %>%
  mutate(across(where(is.numeric), replace_na, 0)) %>%
  bind_cols(est_abundance(.$n1, .$n2, .$m2, method = 'adjusted Peterson', alpha = 0.05) %>%
              rename(N_U = N, SE_N_U = SE, N_U_lwr = lwr, N_U_upr = upr)) %>%
  mutate(across(where(is.numeric), round, digits=0)) %>% # animals are always whole numbers
  mutate(MR_method = case_when(
    strata %in% c('A', 'J') ~ '1 - size strata', # I specified these two because the other four fall into method 2
    TRUE ~ '2 - size/origin strata'),
  # mutate(MR_method = '2 - size/origin strata',
    N_U = N_U + released_late,
    N_U_lwr = N_U_lwr + released_late,
    N_U_upr = N_U_upr + released_late)


rm(n1,n2,m2)

# method 3 - size strata * carcass origin --------------------------------------

# Adult and Jack Hatchery Proportion (pHAT) passed upstream
c_hfrac_up <- car_up %>% # car_up only includes A/J size strata (method 1)
  filter(Origin != 'Unknown') %>%
  sum_groups(.summary_var = strata, Origin, .cnt_var = Count, SurveyYear, StreamName) %>%
  pivot_wider(names_from = Origin, values_from = n) %>%
  mutate(across(where(is.numeric), replace_na, 0)) %>%
  bind_cols(est_proportion(.$Hatchery, .$Hatchery + .$Natural, method = 'score')) %>%
  rename(pHat = p, pHat_SE = SE, pHat_lwr = lwr, pHat_upr = upr)

rm(car_up)

# method 3 hatchery estimates
method3_hat <- left_join(c_hfrac_up,MR_aboveweir) %>%
  mutate(strata = paste0(strata,'_Hat')) %>%
  mutate(N_U = N_U * pHat,
         N_U_lwr = N_U_lwr * pHat_lwr,
         N_U_upr = N_U_upr * pHat_upr,
         MR_method = '3 - size strata * carcass origin') %>%
  mutate(across(where(is.numeric), round, digits=0)) %>%
  select(SurveyYear, StreamName, strata, N_U, N_U_lwr, N_U_upr, MR_method)

# method 3 natural estimates
method3_nat <- left_join(c_hfrac_up,MR_aboveweir) %>%
  mutate(strata = paste0(strata,'_Nat')) %>%
  mutate(N_U = N_U * (1-pHat),
         N_U_lwr = N_U_lwr * (1-pHat_upr),
         N_U_upr = N_U_upr * (1-pHat_lwr),
         MR_method = '3 - size strata * carcass origin') %>%
  mutate(across(where(is.numeric), round, digits=0)) %>%
  select(SurveyYear, StreamName, strata, N_U, N_U_lwr, N_U_upr, MR_method)

# combine hatchery and natural estimates
method3_all <- rbind(method3_hat, method3_nat) %>%
  mutate(strata = case_when(
    strata %in% c("A_Hat", "A_Nat") ~ "A",
    strata %in% c("J_Hat", "J_Nat") ~ "J",
    TRUE ~ strata
  )) %>%
  group_by(SurveyYear, StreamName, strata, MR_method) %>%
  summarise("N_U" =  sum(N_U),
            "N_U_lwr" = sum(N_U_lwr),
            "N_U_upr" = sum(N_U_upr))

rm(c_hfrac_up)

# Add method 2 size /strata estimates of A and J strata ------------------------------------

tmp <- MR_aboveweir %>%
  filter(strata != 'A', # "strata != c('A', 'J')" quit working?
         strata != 'J') %>%
  mutate(strata = case_when(
    strata %in% c('A_Hat', 'A_Nat') ~ 'A',
    strata %in% c('J_Hat', 'J_Nat') ~ 'J',
    TRUE ~ strata
  )) %>%
  group_by(SurveyYear,
           StreamName,
           strata) %>%
  summarize(mark_released = sum(mark_released),
            released_late = sum(released_late),
            BS_returned = sum(BS_returned),
            n1 = sum(n1),
            n2 = sum(n2),
            m2 = sum(m2),
            N_U = sum(N_U),
            SE_N_U = sum(SE_N_U),
            N_U_lwr = sum(N_U_lwr),
            N_U_upr = sum(N_U_upr)) %>%
  mutate(MR_method = '2 - size/origin strata')


# add to MR abov weir data frame
MR_aboveweir <- bind_rows(MR_aboveweir, tmp)

# add to other code to make more efficient
MR_aboveweir <- MR_aboveweir %>%
  mutate(umarked = n2 - m2)

writexl::write_xlsx(MR_aboveweir, path = './data/outputs/escapement/MR_aboveweir.xlsx')

rm(tmp)

# Compile estimates ---------------------------------------
esc_aboveweir <- MR_aboveweir %>%
  select(SurveyYear, StreamName, strata, N_U, N_U_lwr, N_U_upr, MR_method) %>%
  bind_rows(method3_hat, method3_nat, method3_all)

rm(method3_hat, method3_nat, method3_all)

writexl::write_xlsx(esc_aboveweir, path = './data/outputs/escapement/esc_aboveweir.xlsx')

# REMOVE if Unecessary weir removals for escapement estimate ====

    # grouping by trap_year and stream b/c that's all that's needed for report
    # need to add the size/origin strata at some point
    # just add 'origin' to grouping and join via a tmp data file
    
    # grab brood returns to subtract from weir removals
    # this assumes these are all returns to the Lostine
    # there are some returns to the Wallowa that need to be accounted for as
    # outplants, ugh, sigh

# bs_returned_tmp <- bs_returned %>%
#   filter(strata == 'A' | strata == 'J') %>%
#   group_by(SurveyYear,StreamName) %>%
#   summarize(BS_returned = sum(BS_returned))

# grouping = c('trap_year','stream', "origin", "age_designation")
# 
#       weir_removals_esc = est_final_dispositions(trap_dat, grouping) %>%
#         filter(disp_final == 'Brood Stock' |
#                  disp_final == 'Distribution' |
#                  disp_final == 'Nutrient Enhancement' |
#                  disp_final == 'Outplant') %>%
#         group_by(trap_year,
#                  stream,
#                  origin,
#                  age_designation) %>%
#         # mutate(weir_removals = n) %>%
#         summarize(weir_removals = sum(n)) %>%
#         left_join(bs_returned_tmp,
#                   by = c('trap_year' = 'SurveyYear',
#                          'stream' = 'StreamName'),
#                           "origin" = "origin",
#                           "age" = "age") %>%
#         mutate(across(where(is.numeric), replace_na, 0)) %>%
#         mutate(weir_removals = weir_removals - BS_returned) %>%
#         select(-BS_returned)

# below weir estimates --- in progress elsewhere ----------------------------------------

#Jack numbers are off because of the downstream portion of this      

source('./R/11_GRSME_functions.R')
source('./R/09_below_weir_esc.R') # J and J_Hat + J_Nat don't add up

rm(redds, tmp_esc, tmp_weir, tmp_weir3, adults_D, adults_U, MR_aboveweir)

# account for broodstock returned in weir removals ----------------------------------------
      
  N_D  <-  N_D %>%
    left_join(bs_returned,
              by = c('trap_year' = 'SurveyYear',
                     'stream' = 'StreamName',
                      'strata' = 'strata')) %>%
    mutate(across(where(is.numeric), replace_na, 0)) %>%
    mutate(N_weir = N_weir - BS_returned) %>%
    select(-BS_returned)

rm(bs_returned)

writexl::write_xlsx(N_D, path = './data/outputs/escapement/esc_belowweir.xlsx')
      
# tributary harvest--------------------------------------------------
# copied data from comanager spreadsheet initially - updated annually from ODFW, NPT, CTUIR

  trib_harvest = readxl::read_xlsx('./data/inputs/trib_harvest.xlsx') %>% # I need to modify the format of trib_harvest
    filter(ReturnYear %in% yr_range) %>%
    group_by(ReturnYear) %>%
    # summarize(trib_harvest = sum(c(A_Hat, J_Hat, A_Nat, J_Nat))) %>%
    rename (trap_year = ReturnYear) %>%
    pivot_longer("J_Hat":"A", names_to = "strata", values_to = "harvest") %>%
    group_by(trap_year, 
             strata) %>%
    summarise("harvest" = sum(harvest))


# summary of escapement pieces----------------------------------------
# above weir, below weir, weir removals, harvest, trib esc, with CIs
trib_esc <- left_join(esc_aboveweir,MR_preferred, by = c('SurveyYear','strata')) %>%
  filter(MR_method == MR_preferred) %>%
  # group_by(StreamName, SurveyYear, strata) %>%
  # summarise(N_U = sum(N_U),
  #           N_U_lwr = sum(N_U_lwr),
  #           N_U_upr = sum(N_U_upr)) %>%
  rename(trap_year = SurveyYear, stream = StreamName) %>%
  full_join(N_D, by = c('trap_year','stream', 'strata')) %>%
  # full_join(weir_removals_esc, by = c('trap_year','stream')) %>%
  full_join(trib_harvest, by = c('trap_year', 'strata')) %>%
  mutate(trib_esc = N_U + N_D + N_weir + harvest,
         trib_esc_lwr = N_U_lwr + N_D_lwr + N_weir + harvest,
         trib_esc_upr = N_U_upr + N_D_upr + N_weir + harvest) %>%
  rename(weir_removals = N_weir) %>%
  arrange(trap_year, strata)

rm(esc_aboveweir, MR_preferred, N_D, trib_harvest)

# Sum A + J -------------
tmp <- trib_esc %>%
  filter(strata %in% c('A', 'J')) %>%
  group_by(trap_year) %>%
  summarize(across(where(is.numeric), sum)) %>%
  mutate(
    stream = "Lostine River",
    strata = "A+J",
    MR_method = "2 - size/origin strata & 3 - size strata * carcass origin",
    MR_preferred = "2 - size/origin strata & 3 - size strata * carcass origin"
  ) 

tmp2 <- trib_esc %>%
  filter(strata %in% c('A_Hat', 'J_Hat')) %>%
  group_by(trap_year) %>%
  summarize(across(where(is.numeric), sum)) %>%
  mutate(
    stream = "Lostine River",
    strata = "A+J_Hat",
    MR_method = "2 - size/origin strata & 3 - size strata * carcass origin",
    MR_preferred = "2 - size/origin strata & 3 - size strata * carcass origin"
  ) 

tmp3 <- trib_esc %>%
  filter(strata %in% c('A_Nat', 'J_Nat')) %>%
  group_by(trap_year) %>%
  summarize(across(where(is.numeric), sum)) %>%
  mutate(
    stream = "Lostine River",
    strata = "A+J_Nat",
    MR_method = "2 - size/origin strata & 3 - size strata * carcass origin",
    MR_preferred = "2 - size/origin strata & 3 - size strata * carcass origin"
  ) 

# Add new strata to table ---------
trib_esc <- trib_esc %>%
  bind_rows(tmp, tmp2, tmp3) %>%
  arrange(trap_year)

# Add Jack/Adult and Origin -----

trib_esc <- trib_esc %>%
  mutate(Origin = case_when(
    grepl("Hat", strata) ~ "Hatchery",
    grepl("Nat", strata) ~ "Natural",
    strata == "A" | strata == "J" | strata == "A+J" ~ "All"),
    jack_adult = case_when(
      strata == "A+J_Nat" | strata == "A+J_Hat" | strata == "A+J" ~ "All",
      grepl('A', strata) ~ 'Adult',
      grepl("J", strata) ~ 'Jack'
    ))

# Spawners ----

# prespawn mort, grouped by pop, all sizes ------------------------------------
psm <- car_dat %>%
  filter(POP_NAME == 'Lostine River') %>%
  #might be worth double-checking spawned-out criteria in cleaning script
  filter(Sex == 'Female' & SpawnedOut != 'NA') %>%
  sum_groups(.summary_var = SpawnedOut,
             .cnt_var = Count, SurveyYear,MPG,POP_NAME) %>%
  pivot_wider(names_from = SpawnedOut, values_from = n) %>%
  mutate(across(where(is.numeric), replace_na, 0)) %>%
  bind_cols(est_proportion(.$No, .$No + .$Yes, method = 'score')) %>%
  rename(trap_year = SurveyYear) %>%
  select(trap_year, p)


trib_esc <- trib_esc %>%
  left_join(psm, by = 'trap_year') %>%
  mutate(spawners = round((N_U + N_D) * (1-p), 0)) %>%
  select(trap_year,
         stream,
         strata,
         Origin,
         jack_adult,
         MR_method,
         MR_preferred,
         N_U,
         N_U_lwr,
         N_U_upr,
         N_D,
         N_D_lwr,
         N_D_upr,
         weir_removals,
         harvest,
         trib_esc,
         trib_esc_lwr,
         trib_esc_upr,
         spawners
         )


# Create ATT table ---------
trib_esc_att <- tmp %>%
select(
  strata,
  stream,
  trap_year,
  N_U,
  N_U_lwr,
  N_U_upr,
  N_D,
  N_D_lwr,
  N_D_upr,
  weir_removals,
  harvest,
  trib_esc,
  trib_esc_lwr,
  trib_esc_upr
)

rm(tmp, tmp2, tmp3)

# fix inconsistent joining of with/without stream in data set, and upr/lwr naming ---------

writexl::write_xlsx(trib_esc, path = './data/outputs/escapement/trib_esc.xlsx')

# glimpse(trib_esc)

