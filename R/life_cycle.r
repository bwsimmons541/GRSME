
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
  yr_range = 2009:yr
  
# Escapment ----  

  source('./R/08_trib_esc.R')
  rm(trib_esc_att)  
  
  
# age composition ----

  # I need to make sure this is doing what I want if Best_age is filled keep it, if not fill with best value
  car_dat_tmp <- car_dat %>%
    filter(Species == 'Chinook salmon' & Run == 'Spring/summer') %>%
    mutate(Best_Age = case_when(
            !is.na(Best_Age) ~ as.character(Best_Age), #First row trumps lower rows.
            !is.na(PIT_Age) ~ as.character(PIT_Age),
            !is.na(CWT_Age) ~ as.character(CWT_Age),
            !is.na(Fin_Age) ~ as.character(Fin_Age),
            !is.na(Scale_Age) ~ as.character(Scale_Age),
            !is.na(VIE_Age) ~ as.character(VIE_Age),
            !is.na(Length_Age) ~ as.character(Length_Age)
            ))
  
  
  # Age composition to the stream all age groups and origin.
    
  # this is going to have some issues. in some years there are not jacks carcasses recovered.
  # REVIEW AND APPLY CO MANAGER METHODS TO SCRIPT 
  
  pAge_carc <- car_dat_tmp %>%
    filter(!is.na(Best_Age)) %>%
    # mutate(length_age = case_when( # what is length age called in the ODFW Database?
    #   ForkLength < 630 ~ "Jack",
    #   ForkLength >= 630 ~ "Adult"
    # )) %>%
    est_group_p(.summary_var = Best_Age, alpha = .05, SurveyYear, MPG, POP_NAME, StreamName, Origin) %>%
    filter(StreamName == "Lostine River") %>%
    rename(return_year = SurveyYear,
           origin = Origin) %>%
    mutate(
      # strata = case_when(
      #   Best_Age == 2 | Best_Age == 3 ~ 'J',
      #   Best_Age == 4 | Best_Age == 5 ~ 'A'
      #   ),
      brood_year = case_when(
        Best_Age == 2 ~ return_year - 2,
        Best_Age == 3 ~ return_year - 3,
        Best_Age == 4 ~ return_year - 4,
        Best_Age == 5 ~ return_year - 5
        )
      # p = case_when(
      #   Best_Age == 3 & n == 0 ~ 1, # this isn't quite right. 
      #   TRUE ~ p
      )
      

# Brood Year escapement ----

        # setup 
        tmp <- trib_esc %>%
          rename(return_year = trap_year) %>%
          filter(strata == "A+J_Hat" |
                 strata == "A+J_Nat") %>%
          mutate(origin = case_when(
            strata == 'A+J_Hat' ~ 'Hatchery',
            strata == 'A+J_Nat'~ 'Natural'
          )) %>%
          select(return_year,
                 origin,
                 weir_removals,
                 harvest,
                 trib_esc, # add 22 jacks to 2023
                 spawners) # THIS DOESN'T WORK. FIX SPAWNERS. NEED TO INCLUDE PRESPAWN MORTALITY.
  
        # get nat_esc & hat_esc
        esc <- tmp %>% 
          select(return_year,
                 origin,
                 trib_esc) %>%
          pivot_wider(names_from = 'origin',
                      values_from = 'trib_esc') %>%
          rename(brood_year = return_year,
                 nat_esc = Natural,
                 hat_esc = Hatchery)
      
        #get spawners - do spawners include jacks? or only age 4 - 5?
        spawners <- tmp %>% 
          select(return_year,
                 origin,
                 spawners) %>%
          pivot_wider(names_from = origin,
                      values_from = spawners) %>%
          rename(brood_year = return_year,
                 nat_spawners = Natural,
                 hat_spawners = Hatchery)
    
          tmp2 <- pAge_carc %>%
            left_join(tmp, by = c('return_year', 'origin'))
          
          tmp3 <- tmp2 %>%
            mutate(by_esc = round(p * trib_esc, 0))
        
        # Brood year escapement by age - Natural Origin - nat_2, nat_3, nat_4, Nat_5, nat_6
        nat_age_esc <- tmp3 %>%
          filter(origin == 'Natural') %>%
          select(brood_year,
                 Best_Age,
                 by_esc) %>%
          pivot_wider(names_from = Best_Age,
                      values_from = by_esc) %>%
          rename(nat_2 = '2',
                 nat_3 = '3',
                 nat_4 = '4',
                 nat_5 = '5')
  
        # Brood year escapement by age - Hatchery Origin - hat_2, hat_3, hat_4, hat_5
        hat_age_esc <- tmp3 %>%
          filter(origin == 'Hatchery') %>%
          select(brood_year,
                 Best_Age,
                 by_esc) %>%
          pivot_wider(names_from = Best_Age,
                      values_from = by_esc) %>%
          rename(hat_2 = '2',
                 hat_3 = '3',
                 hat_4 = '4',
                 hat_5 = '5')  
  
  
      #      
      # tmp4 <- tmp3 %>% # i think I can move this down and eliminate tmp3    
      #   group_by(brood_year,
      #            origin) %>%
      #   summarize(by_return = sum(by_esc))
      # 
      #   #pivot wider for hat_by_return, nat_by_return
      # 
      #   by_return <- tmp4 %>%
      #     filter(origin != 'Unknown') %>%
      #     pivot_wider(names_from = origin,
      #                 values_from = by_return) %>%
      #     rename(nat_by_return = Natural,
      #            hat_by_return = Hatchery)
        
        
        by_return <- tmp3 %>% # i think I can move this down and eliminate tmp3    
          group_by(brood_year,
                   origin) %>%
          summarize(by_return = sum(by_esc)) %>%
          filter(origin != 'Unknown') %>%
          pivot_wider(names_from = origin,
                      values_from = by_return) %>%
          rename(nat_by_return = Natural,
                 hat_by_return = Hatchery)
        
        rm(tmp, tmp2, tmp3)
              
#import broodstock data ---
        
        bs_spawners <- est_final_dispositions(trap_dat, c('trap_year', 'stream')) %>%
          filter(disp_final == 'Brood Stock') %>%
          rename(brood_year = trap_year,
                 bs_spawners = n) %>%
          select(brood_year,
                 bs_spawners)
        
# import smolt data ----
        
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
        
        rm(lost_smolts_hat, tmp)
        
        # load natural smolt data -from ODFW
        los_smolts_nat <- readxl::read_xlsx('./data/inputs/lostine_smolts_nat.xlsx')
        
        los_smolts <- los_smolts_nat %>%
          select(-migration_year) %>%
          full_join(hat_smolts, by = 'brood_year')


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
        
      
# cuyem packages ----
  
  lsf.str("package:cuyem")
  ls("package:cuyem")

  
  
?case_when
  