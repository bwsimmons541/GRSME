
# Install Packages ----
    library(cuyem)
    library(cdmsR)
    library(tidyverse)
    library(readxl)

# CDMS login ----

  # if necessary

  # source('./r/cdms_login.R')

# CDMS data load and clean ----

  # add data later.

#load data ----

    load('./data/inputs/car_dat.rda')
    load('./data/inputs/redd_dat.rda')
    load('./data/inputs/trap_dat.rda')

# set year ----
    
    yr = 2023
    yr_range = c((yr-9):yr) 

# Load functions ----
    
    source('./R/11_GRSME_functions.R') # GRSME functions
    
# Last Above Weir SGS Survey ------
# this assumes last carcass recovery date = last opportunity to recover marked fish

  last_sgs <- redd_dat %>%
    filter(StreamName == 'Lostine River') %>%
    filter(AboveWeir == 'Yes') %>%
    filter(Species == 'Chinook salmon' & Run == 'Spring/summer') %>%
    group_by(SurveyYear, StreamName) %>%
    summarise(last_sgs = max(SurveyDate))    
    
# NOR + HOR Est Above - Below Weir ----
    
    source('./r/08_trib_esc.R')

    # trib_esc.xlsx, MR_aboveweir.xlsx
    
# Total Redds Above and Below Weir----
    
    redds_total_above_below <- redd_dat %>%
      filter(Species == 'Chinook salmon', Run == 'Spring/summer') %>%
      group_by(SurveyYear,MPG,StreamName, AboveWeir, ActivityId) %>%
      summarize(NewRedds = mean(NewRedds)) %>% #could we use aggregate function instead
      group_by(SurveyYear,MPG,StreamName, AboveWeir) %>%
      summarize(NewRedds = sum(NewRedds)) %>%
      arrange(StreamName) %>%
      filter(StreamName == "Lostine River")    
    
# Outplant Data ----

  ouplant_dat <- trap_dat %>%
    filter(purpose == "Outplant",
           species == "Chinook",
           run == "Summer") %>%
    group_by(trap_year,
             species,
             run,
             release_site,
             origin,
             age_designation) %>%
    summarize(n = sum(count))

#Harvest Data ----
  
  trib_harvest = readxl::read_xlsx('./data/inputs/trib_harvest.xlsx') %>% # I need to modify the format of trib_harvest
    filter(ReturnYear %in% yr_range) %>%
    group_by(ReturnYear) %>%
    # summarize(trib_harvest = sum(c(A_Hat, J_Hat, A_Nat, J_Nat))) %>%
    rename (trap_year = ReturnYear) %>%
    pivot_longer("J_Hat":"A", names_to = "strata", values_to = "harvest") %>%
    group_by(trap_year,
             Entity,
             strata) %>%
    summarise("harvest" = sum(harvest))    

# Removal of fish at Weir ----
    
    
# Total Esc & Others - Prespawn_Mortality ----
    
    # prespawn mort, grouped by pop, all sizes 
    psm_lostine <- car_dat %>%
      #might be worth double-checking spawned-out criteria in cleaning script
      filter(Sex == 'Female' & SpawnedOut != 'NA',
             StreamName == "Lostine River") %>%
      sum_groups(.summary_var = SpawnedOut,
                 .cnt_var = Count, SurveyYear,MPG,POP_NAME) %>%
      pivot_wider(names_from = SpawnedOut, values_from = n) %>%
      mutate(across(where(is.numeric), replace_na, 0)) %>%
      bind_cols(est_proportion(.$No, .$No + .$Yes, method = 'score')) %>%
      rename(psm = p, psm_SE = SE, psm_lwr = lwr, psm_upr = upr) %>%
      mutate(n_psm = Yes + No) #added row for n_psm
    
# Weir Efficiency ----
    
    # Build a weir efficiency script
    
    
# Run Reconstruction ----
    

    # set up age data like shane did for report
      tmp <- car_dat %>%
        filter(Species == 'Chinook salmon' & Run == 'Spring/summer') %>%
        mutate(Best_Age = case_when(!is.na(CWT_Age) ~ as.character(CWT_Age), #add length age? need to go back to cleaning script.
                                    !is.na(PIT_Age) ~ as.character(PIT_Age),
                                    !is.na(VIE_Age) ~ as.character(VIE_Age),
                                    !is.na(Fin_Age) ~ as.character(Fin_Age),
                                    !is.na(Scale_Age) ~ as.character(Scale_Age)))
      
      inf_age <- tmp %>%
        filter(!is.na(Best_Age), 
               ForkLength != -999)
  
    # age by pop_year, origin, doesn't quite math co-mgr spreadsheet
      
      inf_age_p <- inf_age %>%
        mutate('age_length' = case_when(
          ForkLength < 630 ~ "J",
          ForkLength >= 630 ~ "A")) %>%
        filter(StreamName == "Lostine River") %>%
        est_group_p(Best_Age, alpha = 0.05,StreamName, SurveyYear, Origin, age_length)
    
      write_xlsx(inf_age_p, path = './data/outputs/inferred_age_co_mgr.xlsx')
      
    # group by year, origin, < 630 mm | >= 630 mm, and BestAge
    
      best_age_p <- car_dat %>%
        mutate('age_length' = case_when(
          ForkLength < 630 ~ "J",
          ForkLength >= 630 ~ "A")) %>%
        filter(StreamName == "Lostine River") %>%
        est_group_p(Best_Age, alpha = 0.05, StreamName, SurveyYear, Origin, age_length)

      write_xlsx(best_age_p, path = './data/outputs/best_age_co_mgr.xlsx')
            
      
# Non Lostine Redds

redds_total_non_los <- redd_dat %>%
  filter(Species == 'Chinook salmon', Run == 'Spring/summer') %>%
  group_by(SurveyYear,MPG,StreamName, ActivityId) %>%
  summarize(NewRedds = mean(NewRedds)) %>% #could we use aggregate function instead
  group_by(SurveyYear,MPG,StreamName) %>%
  summarize(NewRedds = sum(NewRedds)) %>%
  arrange(StreamName)



# Table 4 - weir final disposition summary ------------------------------------

# this doesn't account for broodstock returns
# need to get the origin/sex info to do that
# note that where fish were returned also matters
# the bs_returned table in T3 is just for fishr eturned to the Lostine
# but some fish were also returned to the Wallowa in 2020? and other yrs?

grouping = c('trap_year','stream', 'origin', 'age_designation')

getwd()
source('./R/11_GRSME_functions.R')

weir_removal_co_mgr <- est_final_dispositions(trap_dat, grouping) %>%
  select(-c(sum_tmp,sub_outplant,sub_recycle)) 

trap_sums <- est_final_dispositions(trap_dat, grouping) %>%
  group_by(trap_year, stream, origin, age_designation) %>%
  summarize(n = sum(n)) %>%
  mutate(disp_final = 'total')

tmp <- weir_removal_co_mgr %>%
  bind_rows(trap_sums) %>%
  pivot_wider(names_from = c(origin,age_designation), values_from = n) %>%
  mutate(across(where(is.numeric), replace_na, 0)) %>%
  arrange(trap_year)



writexl::write_xlsx(tmp, path = './data/outputs/weir_removal_co_mgr.xlsx')

rm(grouping, tmp)



#Redd Expansion ----

# non-lostine carcasses

glimpse(car_dat)



wal_pop_carcs <- car_dat %>%
    filter(StreamName == "Hurricane Creek" |
            StreamName == "Wallowa River" |
            StreamName == "Bear Creek",
           SurveyYear %in% yr_range,
           Species == "Chinook salmon",
           Run == "Spring/summer",
           ForkLength > 630) %>%
  group_by(SurveyYear,
           Origin) %>%
  summarize(n_carc = sum(Count, na.rm = TRUE))


# total redds by trib
redds_total <- redd_dat %>%
  filter(Species == 'Chinook salmon', Run == 'Spring/summer') %>%
  group_by(SurveyYear,MPG,POP_NAME,StreamName,ActivityId) %>%
  summarize(NewRedds = mean(NewRedds)) %>% #could we use aggregate function instead
  group_by(SurveyYear,MPG,POP_NAME, StreamName) %>%
  summarize(NewRedds = sum(NewRedds))

#if there is one record for each redd
#?could just count a different field instead of using the newredds field/grouping






























































