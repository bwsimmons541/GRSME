

# This is a start to code SAR I want to figure out how to incorporate uncertainty in composite metrics.

# load Packages ----

  librarian::shelf(ryankinzer/cdmsR, 
                 tidyverse)


# load data

  source('./r/getCDMSdata.r')

# Run life cycle script for inputs


  source('./r/life_cycle.r')


# Smolt to Adult Return Rates ----

  sar <- life_cycle %>% 
    mutate(nat_emig_sar = nat_by_return / nat_emig,
           nat_smolts_sar = nat_by_return / nat_smolts,
           hat_emig_sar = hat_by_return / hat_emig,
           hat_smolts_sar = hat_by_return / hat_smolts)
  
  
  
  # Smolt to adult return rate for Adult Tech Team report.
  # Clean this up for easier copy and paste
  sar_att <- sar %>%
  select(brood_year,
         hat_emig,
         hat_by_return,
         hat_emig_sar,
         nat_emig,
         nat_by_return,
         nat_emig_sar) %>%
    mutate(hat_emig_sar = round(hat_emig_sar*100,3),
           nat_emig_sar = round(nat_emig_sar*100,3))

  writexl::write_xlsx(sar_att, path = './data/outputs/adult-report/sar_att.xlsx')

  # Recruits per Spawner ----  
  # DO I need to recombine in case HAT and NAT have a different PSM?
  # Double check how I'm calculating spawners in trib_esc. is PSM split by origin
    
  
# smolts per spawner _ ADULTS ONLY
  
  # Natural Spawners ----

    source('./r/trib_esc.r')
    rm(trib_esc_att)
  
  # Spawners from tributary escapment data
    spawners <- trib_esc %>%
      filter(age_designation == 'Adult',
             Origin == 'All') %>%
      rename(brood_year = trap_year) %>%
      select(brood_year,
             # Origin,
             age_designation,
             spawners)
  
  # pFemale by Origin
    pfemale <- car_dat %>%
    filter(StreamName == 'Lostine River',
           Sex != 'Unknown',) %>%
    est_group_p(.summary_var = Sex, alpha = 0.5, SurveyYear) %>% #, Origin
      filter(Sex == 'Female') %>%
      rename(brood_year = SurveyYear)
  
    spawners_f <- spawners %>%
      left_join(pfemale, by = 'brood_year') %>% # add back in origin if sex ratio different.
      mutate(spawners_f = spawners * p,
             Origin = 'Natural') %>%
      select(brood_year,
             Origin,
             spawners,
             spawners_f)
    
     rm(spawners)

    # hatchery broodstock (spawners) ----
     

     grouping = c('trap_year', 'stream', 'sex')
     
    los_brood <-  trap_dat %>%
       filter(trap == "Lostine River Weir") %>%
     est_final_dispositions(grouping = grouping) %>%
      filter(disp_final == 'Brood Stock') %>%
      select(-c(sum_tmp,sub_outplant,sub_recycle)) %>%
      group_by(trap_year,
               sex,
               disp_final) %>%
      summarize(n = sum(n)) %>%
      ungroup() %>%
      group_by(trap_year) %>%
      mutate(spawners = sum(n)) %>%
      ungroup() %>%
      filter(sex == 'Female') %>%
      rename(spawners_f = n,
             brood_year = trap_year) %>%
      mutate(Origin = 'Hatchery') %>%
      select(brood_year,
             Origin,
             spawners,
             spawners_f)
    # Import boodstock Returned
      # ADD SEX INFORMATION TO BROOD STOCK RETURNED FILE.
    
    brood_returned <- readxl::read_xlsx('./data/inputs/brood_returned.xlsx') %>%
      filter(age_designation == 'Adult',
             Origin != 'All') %>%
      rename(brood_year = SurveyYear) %>%
      select(brood_year,
             Origin,
             BS_returned)
    
    los_brood <- los_brood %>%
      left_join(brood_returned, by = c('brood_year' = 'brood_year', 'Origin' = 'Origin')) %>%
      mutate(spawners = case_when(
        !is.na(BS_returned) ~ spawners - BS_returned,
        TRUE ~ spawners)) %>%
      select(-BS_returned)
    
    rm(grouping, brood_returned)
    
    # combine natural spawners and brood
    
    los_spawners <- spawners_f %>%
      bind_rows(los_brood)
     
    # Emigrants from life_cycle
      emigrants <- life_cycle %>%
        select(brood_year,
               nat_emig,
               hat_emig) %>%
        pivot_longer(cols = -'brood_year', names_to = 'Origin', values_to = 'juv_emig') %>%
        mutate(Origin = case_when(
          grepl('nat_', Origin) ~ 'Natural',
          grepl('hat_', Origin) ~ 'Hatchery'
        ))

    # Smolts from life_cycle
      smolts <- life_cycle %>%
        select(brood_year,
               nat_smolts,
               hat_smolts) %>%
        pivot_longer(cols = -'brood_year', names_to = 'Origin', values_to = 'juv_smolts') %>%
        mutate(Origin = case_when(
          grepl('nat_', Origin) ~ 'Natural',
          grepl('hat_', Origin) ~ 'Hatchery'
        ))
    
    # Combine into recruits
    
      recruits <- emigrants %>%
        full_join(smolts, by = c('brood_year' = 'brood_year', 'Origin' = 'Origin'))
      
      rm(emigrants, smolts)

    
    # Join with spawners
    
      rps <- los_spawners %>%
        full_join(recruits, by = c('brood_year' = 'brood_year', 'Origin' = 'Origin')) %>%
        mutate(
          emig_ps = juv_emig/spawners,
          emig_pfs = juv_emig/spawners_f,
          smolts_ps = juv_smolts/spawners,
          smolts_pfs = juv_smolts/spawners_f
        )
      
    # ATT output
      
      rps_att <- rps %>%
        mutate(stream_name = "Lostine River") %>%
        select(brood_year,
               stream_name,
               Origin,
               emig_pfs,
               spawners_f,
               spawners,
               juv_emig
               ) %>%
          rename(origin = Origin,
                 rps_f = emig_pfs,
                 spawners_female = spawners_f,
                 spawners_trib = spawners,
                 juv_trib = juv_emig) %>%
        arrange(origin,
               brood_year)

      
      writexl::write_xlsx(rps_att, path = './data/outputs/adult-report/rps_att.xlsx')        
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    