
# Load packages ----


  librarian::shelf(tidyverse,
                   ryankinzer/cdmsR,
                   ryankinzer/cuyem,
                   readxl,
                   writexl,
                   plotrix)

# set year ----
  
  # Should I remove this? it's only necessary if I'm calculating back to 1995?
    yr = year(Sys.Date())
    yr_range = 2010:yr # Data back to 1995, but not necessarily reliable.

# Load Data ---
    
  # Trapping Data
    
    # load('./data/inputs/WeirData.rda')
    #
    # trap_dat <- clean_weirData(WeirData) %>%
    #   filter(facility == 'NPT GRSME Program') %>%
    #   filter(trap_year %in% yr_range)
    # 
    # rm(WeirData)
    # 
    # save(trap_dat, file = './data/inputs/trap_dat.rda')

    load('./data/inputs/trap_dat.rda')
    
  # Carcass Data
    
    
    
    # load('./data/inputs/CarcsData.rda')
    # 
    # car_dat <- clean_carcassData_NEOR(CarcsData) %>%
    #   filter(SurveyYear %in% yr_range)
    # 
    # rm(CarcsData)
    # 
    # save(car_dat, file = './data/inputs/car_dat.rda')
    
    load('./data/inputs/car_dat.rda')
    
  # Redd Data
    
    # load('./data/inputs/ReddsData.rda')
    # 
    # redd_dat <- clean_reddData_NEOR(ReddsData) %>%
    #   filter(SurveyYear %in% yr_range & ReddSpecies == 'S_CHN')
    # 
    # rm(ReddsData)
    # 
    # save(redd_dat, file = './data/inputs/redd_dat.rda')

    # load('./data/inputs/redd_dat.rda')

  # Load hatchery surival data
  # ONLY BACK TO 1999 - GET FULL DATA SET IN CDMS
    # source('./R/cdms_login.R')
    # juv_survival <- get_JuvSurvival()
    # save(juv_survival, file = './data/inputs/juv_survival.rda')
    # rm(juv_survival)
    load('./data/inputs/juv_survival.rda')
  
        
  # load hatchery smolt data -  queried from RMIS and
    # MISSING 2004, 2005, 2010
    los_smolts_hat <- readxl::read_xlsx('./data/inputs/lostine_smolts_hat.xlsx')
  
  # load natural smolt data -from ODFW 
    los_smolts_nat <- readxl::read_xlsx('./data/inputs/lostine_smolts_nat.xlsx')
    
  
# Escapement ----  

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
              !is.na(Age_Key) ~ as.character(Age_Key), # Age Length Key analysis values
              !is.na(Length_Age) ~ as.character(Length_Age) # "Visually" assigned age based on length
              ))
  
    rm(car_dat)
  
  # Estimate age proportions & account for low proportions of jacks
  # NO NATURAL DATA FOR 2023
    pAge_carc <- car_dat_tmp %>%
      filter(!is.na(Best_Age),
             Best_Age != '2',
             StreamName == "Lostine River") %>%
      mutate(age_designation = case_when( # what is JACK/ADULTS in ODFW database?
                ForkLength < 630 ~ "Jack",
                ForkLength >= 630 ~ "Adult"
                ),
             Best_Age = case_when(
               Origin == 'Hatchery' & SurveyYear >=2015 & ForkLength < 630 ~ '3', #Fewer than 20 jack ages, assume jacks are jacks.
               Origin == 'Natural' & SurveyYear >=2012 & ForkLength < 630 ~ '3', #Fewer than 20 jack ages, assume jacks are jacks.
               TRUE ~ Best_Age
               )
             ) %>%
      est_group_p(.summary_var = Best_Age, alpha = .05, SurveyYear, MPG, POP_NAME, StreamName, Origin, age_designation) %>%
      rename(return_year = SurveyYear) %>% 
      mutate(brood_year = case_when(
              Best_Age == 2 ~ return_year - 2,
              Best_Age == 3 ~ return_year - 3,
              Best_Age == 4 ~ return_year - 4,
              Best_Age == 5 ~ return_year - 5
              )
             )
      
      rm(car_dat_tmp)
    
      #import rows for years missing any jack carcasses & OTHER MISSING RECORDS
      # I NEED TO DO AN AGE-LENGTH KEY
      # We still need 2023 data updated.
        run_rec_jacks <- readxl::read_xlsx('./data/inputs/run_rec_jacks.xlsx')
  
      # Bind Rows 
        pAge_carc <-  pAge_carc %>%
          bind_rows(run_rec_jacks) %>%
          arrange(return_year,
                 Origin,
                 Best_Age)
        
        rm(run_rec_jacks)
      
# Brood Year Escapement & Spawners ----

        # Set up tributary escapement data for escapement and spawners by origin
          tmp <- trib_esc %>%
            rename(return_year = trap_year) %>%
            filter(strata == "A+J_Hat" | 
                   strata == "A+J_Nat") %>%
            select(return_year,
                   Origin,
                   weir_removals,
                   harvest,
                   trib_esc, 
                   spawners)
  
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
      
        #get spawners by origin - (Includes all ages)
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
                   age_designation != 'All') %>%
          select(return_year,
                 Origin,
                 age_designation,
                 weir_removals,
                 harvest,
                 trib_esc, # add 22 jacks to 2023
                 spawners)
            
          rm(trib_esc)
          
          # THE NA COLUMN STARTS HERE IS IT MUST BE COMING FROM pAge_carc
          # NO 2023 NATURAL ADULT IN pAge_Carc causes the NA column. why isn't there one.
            tmp2 <- pAge_carc %>%
              filter(Origin != 'Unknown') %>%
              right_join(tmp, by = c('return_year', 'Origin', 'age_designation'))
            
            tmp3 <- tmp2 %>%
              mutate(by_esc = round(p * trib_esc, 0)) %>%
              group_by(StreamName,
                       Origin,
                       brood_year,
                       Best_Age) %>%
              summarize(by_esc = sum(by_esc)) %>%
              ungroup()
        
            rm(pAge_carc)
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
          
          rm(trap_dat)
        
# Juvenile Abundance and Smolt Equivalents ----
        
        
      # HATCHERY ORIGIN 
      # Missing 2010 in final
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
            
            rm(juv_survival)
          
        # Calculate smolt equivalents (hat_smolts)
          hat_smolts <- los_smolts_hat %>%
            group_by(brood_year,
                     release_group) %>%
            summarize(hat_emig = sum(hat_emig)) %>%
            ungroup() %>%
            left_join(los_survival, by = c('brood_year', 'release_group')) %>%
            select(brood_year,
                   release_group,
                   hat_emig,
                   survival) %>%
            mutate(hat_smolts = round(hat_emig * survival,0)) %>%
            group_by(brood_year) %>%
            summarize(hat_emig = sum(hat_emig),
                      hat_smolts = sum(hat_smolts)) %>%
            ungroup()
          
          
          rm(los_smolts_hat, los_survival)
        
        
      # NATURAL ORIGIN
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
        
          rm(redd_dat)
          
        # Natural Origin redd expansion & for juveniles (nat_emig) & smolt equivalents (nat_smolts)
          tmp <- los_smolts_nat %>%
            left_join(rst_redds) %>%
            mutate(nat_emig = (nat_emig + (nat_emig * (below_rst/above_rst))),
                   nat_smolts = (nat_smolts + (nat_smolts * (below_rst/above_rst)))) %>%
            select(-below_rst,
                   -above_rst)
          
          rm(rst_redds)
          
        # Join 
          los_smolts <- tmp %>%
            select(-migration_year) %>%
            full_join(hat_smolts, by = 'brood_year')
  
          rm(los_smolts_nat, hat_smolts, tmp)

# Lifecycle- inputs ----
        
        # join lifecycle inputs into a single dataframe
        lc <- esc %>%
          full_join(spawners, by = 'brood_year') %>%
          full_join(by_return, by = 'brood_year') %>%
          full_join(bs_spawners, by = 'brood_year') %>%
          full_join(los_smolts, by = 'brood_year') %>%
          full_join(hat_age_esc, by = 'brood_year') %>%
          full_join(nat_age_esc, by = 'brood_year') %>%
          select(-stream, -NA.x, - NA.y) %>%
          filter(brood_year > 2009)
            
          
        # Data is messy or incomplete in CDMS prior to 2010
        # Import and bind past data 
        lc09 <- readxl::read_xlsx('./data/inputs/lifecycle_lostine_95-09.xlsx')
        
        # Bind data together
        life_cycle <- lc %>%
          bind_rows(lc09) %>%
          arrange(brood_year)
          
          
        
        writexl::write_xlsx(life_cycle, path = './data/outputs/lifecycle.xlsx')
        
        rm(esc,
           spawners, 
           by_return, 
           bs_spawners, 
           los_smolts, 
           hat_age_esc,
           nat_age_esc,
           lc,
           lc09)
        
        
        
  