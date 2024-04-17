
# Load packages ----


  librarian::shelf(tidyverse,
                   ryankinzer/cdmsR,
                   ryankinzer/cuyem,
                   readxl,
                   writexl,
                   plotrix)


# Load Data ---

  load('./data/inputs/trap_dat.rda')
  load('./data/inputs/car_dat.rda')
  load('./data/inputs/redd_dat.rda')
  # Load smolt data
  
# set year ----
  
  yr = year(Sys.Date())
  yr_range = 2009:yr # do we have data back to 1995?
  
# Escapment ----  

  source('./R/08_trib_esc.R')
  rm(trib_esc_att, psm)  

  
# age composition ----


  # I need to make sure this is doing what I want if Best_age is filled keep it, if not fill with best value
  
  car_dat_tmp <- car_dat %>%
    filter(Species == 'Chinook salmon' & Run == 'Spring/summer',
           ForkLength != -999) %>%
    mutate(Best_Age = case_when(
            !is.na(Best_Age) ~ as.character(Best_Age), #First row trumps lower rows.
            !is.na(PIT_Age) ~ as.character(PIT_Age),
            !is.na(CWT_Age) ~ as.character(CWT_Age),
            !is.na(Fin_Age) ~ as.character(Fin_Age),
            !is.na(Scale_Age) ~ as.character(Scale_Age),
            !is.na(VIE_Age) ~ as.character(VIE_Age),
            !is.na(Length_Age) ~ as.character(Length_Age)
            ))
  
  # Estimate age proportions & account for low proportions of jacks
  pAge_carc <- car_dat_tmp %>%
    filter(!is.na(Best_Age),
           Best_Age != '2',
           StreamName == "Lostine River") %>%
    mutate(jack_adult = case_when( # what is length age called in the ODFW Database?
              ForkLength < 630 ~ "Jack",
              ForkLength >= 630 ~ "Adult"
              ),
           Best_Age = case_when(
             Origin == 'Hatchery' & SurveyYear >=2015 & ForkLength < 630 ~ '3', #Fewer than 20 jack ages, assume jacks are jacks.
             Origin == 'Natural' & SurveyYear >=2012 & ForkLength < 630 ~ '3', #Fewer than 20 jack ages, assume jacks are jacks.
             TRUE ~ Best_Age
             )
           ) %>%
    est_group_p(.summary_var = Best_Age, alpha = .05, SurveyYear, MPG, POP_NAME, StreamName, Origin, jack_adult) %>%
    rename(return_year = SurveyYear) %>% 
    mutate(brood_year = case_when(
            Best_Age == 2 ~ return_year - 2,
            Best_Age == 3 ~ return_year - 3,
            Best_Age == 4 ~ return_year - 4,
            Best_Age == 5 ~ return_year - 5
            )
           )
  
      #import rows for years missing any jack carcasses
      run_rec_jacks <- readxl::read_xlsx('./data/inputs/run_rec_jacks.xlsx')
  
      # Bind Rows 
      pAge_carc <-  pAge_carc %>%
        bind_rows(run_rec_jacks) %>%
        arrange(return_year,
               Origin,
               Best_Age)
      
      rm(run_rec_jacks)
      
# Brood Year escapement ----

        # setup - I think i want to go with Jack or Adult, not ALL
        # Also Hat or Nat Not all
        tmp <- trib_esc %>%
          rename(return_year = trap_year) %>%
          filter(strata == "A+J_Hat" | 
                 strata == "A+J_Nat") %>%
          select(return_year,
                 Origin,
                 weir_removals,
                 harvest,
                 trib_esc, # add 22 jacks to 2023
                 spawners) # THIS DOESN'T WORK. FIX SPAWNERS. NEED TO INCLUDE PRESPAWN MORTALITY.
  
        # get nat_esc & hat_esc
        esc <- tmp %>% 
          select(return_year,
                 Origin,
                 trib_esc) %>%
          pivot_wider(names_from = 'Origin',
                      values_from = 'trib_esc') %>%
          rename(brood_year = return_year,
                 nat_esc = Natural,
                 hat_esc = Hatchery)
      
        #get spawners - do spawners include jacks? or only age 4 - 5?
        spawners <- tmp %>% 
          select(return_year,
                 Origin,
                 spawners) %>%
          pivot_wider(names_from = Origin,
                      values_from = spawners) %>%
          rename(brood_year = return_year,
                 nat_spawners = Natural,
                 hat_spawners = Hatchery)
    
        rm(tmp)
        
        # run reconstruction ----
        
          tmp <- trib_esc %>%
            rename(return_year = trap_year) %>%
            filter(Origin != 'All',
                   jack_adult != 'All') %>%
          select(return_year,
                 Origin,
                 jack_adult,
                 weir_removals,
                 harvest,
                 trib_esc, # add 22 jacks to 2023
                 spawners)
            
          
          # THE NA COLUMN STARTS HERE IS IT MUST BE COMING FROM pAge_carc
          # NO 2023 NATURAL ADULT IN pAge_Carc causes the NA column. why isn't there one.
          tmp2 <- pAge_carc %>%
            filter(Origin != 'Unknown') %>%
            right_join(tmp, by = c('return_year', 'Origin', 'jack_adult'))
          
          tmp3 <- tmp2 %>%
            mutate(by_esc = round(p * trib_esc, 0)) %>%
            group_by(StreamName,
                     Origin,
                     brood_year,
                     Best_Age) %>%
            summarize(by_esc = sum(by_esc)) %>%
            ungroup()
        
        # Brood year escapement by age - Natural Origin - nat_2, nat_3, nat_4, Nat_5, nat_6
        nat_age_esc <- tmp3 %>%
          filter(Origin == 'Natural') %>%
          select(brood_year,
                 Best_Age,
                 by_esc) %>%
          pivot_wider(names_from = Best_Age,
                      values_from = by_esc) %>%
          relocate(c('3','4','5'), .after = brood_year) %>%
          rename(nat_3 = '3',
                 nat_4 = '4',
                 nat_5 = '5')
  
        # Brood year escapement by age - Hatchery Origin - hat_2, hat_3, hat_4, hat_5
        hat_age_esc <- tmp3 %>%
          filter(Origin == 'Hatchery') %>%
          select(brood_year,
                 Best_Age,
                 by_esc) %>%
          pivot_wider(names_from = Best_Age,
                      values_from = by_esc) %>%
          relocate(c('3','4','5'), .after = brood_year) %>%
          rename(hat_3 = '3',
                 hat_4 = '4',
                 hat_5 = '5')  
  

        
        # Total brood year returns
        by_return <- tmp3 %>% # i think I can move this down and eliminate tmp3 ??WHAT IS THIS COMMENT??  
          group_by(brood_year,
                   Origin) %>%
          summarize(by_return = sum(by_esc)) %>%
          filter(Origin != 'Unknown') %>%
          pivot_wider(names_from = Origin,
                      values_from = by_return) %>%
          rename(nat_by_return = Natural,
                 hat_by_return = Hatchery)
        
        rm(tmp, tmp2, tmp3)
              
# Broodstock  ---
   
        # summarize and format broodstock data     
        bs_spawners <- est_final_dispositions(trap_dat, c('trap_year', 'stream')) %>%
          filter(disp_final == 'Brood Stock') %>%
          rename(brood_year = trap_year,
                 bs_spawners = n) %>%
          select(brood_year,
                 bs_spawners)
        
# Juvenile Abundance and Smolt Equivalents ----
        
        # Load hatchery surival data
          # source('./R/cdms_login.R')
          # juv_survival <- get_JuvSurvival()
          # save(juv_survival, file = './data/inputs/juv_survival.rda')
          # rm(juv_survival)
        
        load('./data/inputs/juv_survival.rda')
        
        
        # filter for Lostine only survival
        los_survival <- juv_survival %>%
          filter(StreamName == "Lostine River") %>%
          mutate(release_group = case_when(
            ReleaseGroup == "Lostine Early/Late Combined" | ReleaseGroup == "Lostine Early/Late" ~ 'combined',
            ReleaseGroup == "Lostine Early " | ReleaseGroup == "Lostine Early" ~ 'early',
            ReleaseGroup == "Lostine Late" ~ 'late'
          )) %>%
          rename(brood_year = BroodYear,
                 survival = Survival)
        
        
        # load hatchery smolt data -  queried from RMIS and
        # MISSING 2004, 2005, 2010
        los_smolts_hat <- readxl::read_xlsx('./data/inputs/lostine_smolts_hat.xlsx')
        
        tmp <- los_smolts_hat %>%
          group_by(brood_year,
                   release_group) %>%
          summarize(hat_emig = sum(hat_emig))
        
        hat_smolts <- tmp %>%
          left_join(los_survival, by = c('brood_year', 'release_group')) %>%
          select(brood_year,
                 release_group,
                 hat_emig,
                 survival) %>%
          mutate(hat_smolts = round(hat_emig * survival,0)) %>%
          group_by(brood_year) %>%
          summarize(hat_emig = sum(hat_emig),
                    hat_smolts = sum(hat_smolts))
        
        rm(los_smolts_hat, tmp, juv_survival, los_survival)
        
        # load natural smolt data -from ODFW 
        los_smolts_nat <- readxl::read_xlsx('./data/inputs/lostine_smolts_nat.xlsx')
        
        
        # summarize RST data for Juvenile Redd Expansion
        rst_redds <- redd_dat %>%
          filter(Species == 'Chinook salmon', 
                 Run == 'Spring/summer',
                 StreamName == 'Lostine River') %>%
          group_by(SurveyYear, StreamName, AboveRST, ActivityId) %>%
          summarize(NewRedds = mean(NewRedds)) %>% #could we use aggregate function instead
          group_by(SurveyYear, AboveRST) %>%
          summarize(NewRedds = sum(NewRedds)) %>%
          pivot_wider(names_from = AboveRST, values_from = NewRedds) %>%
          rename(below_rst = No,
                 above_rst = Yes,
                 brood_year = SurveyYear)
        
        # Redd expansion
        tmp <- los_smolts_nat %>%
          left_join(rst_redds) %>%
          mutate(nat_emig = (nat_emig + (nat_emig * (below_rst/above_rst))),
                 nat_smolts = (nat_smolts + (nat_smolts * (below_rst/above_rst)))) %>%
          select(-below_rst,
                 -above_rst)
        
        
        los_smolts <- tmp %>%
          select(-migration_year) %>%
          full_join(hat_smolts, by = 'brood_year')

        rm(los_smolts_nat, hat_smolts, tmp)

# Lifecycle- inputs ----
        
        life_cycle <- esc %>%
          full_join(spawners, by = 'brood_year') %>%
          full_join(by_return, by = 'brood_year') %>%
          full_join(bs_spawners, by = 'brood_year') %>%
          full_join(los_smolts, by = 'brood_year') %>%
          full_join(hat_age_esc, by = 'brood_year') %>%
          full_join(nat_age_esc, by = 'brood_year') %>%
          select(-stream) %>%
          arrange(brood_year)
        
        writexl::write_xlsx(life_cycle, path = './data/outputs/lifecycle.xlsx')
        
  