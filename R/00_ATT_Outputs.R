# Summary ---------------------------------------------------------------------
# SV learning/testing scripts for GRSME analyses and annual report prep
# general approach - load packages and CDMS data sets here
# source scripts for each piece of report


# load/install packages -------------------------------------------------------
# library(tidyverse)
# library(cdmsR)
# library(cuyem)
# library(readxl)
# library(writexl)
# library(plotrix)

librarian::shelf(tidyverse,
                 ryankinzer/cdmsR,
                 ryankinzer/cuyem,
                 readxl,
                 writexl,
                 plotrix)

# load/clean CDMS datasets for redds, carcasses, and trapping -----------------

#source('./R/getCDMSdata.R') # un-comment to update data sets from CDMS

  yr = 2023
  yr_range = c((yr-9):yr)

  #Load and CLean Wier Data----
  
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

  
  # Load and clean Carcass Data----  
    
      # load('./data/inputs/CarcsData.rda')
      # 
      # source('./R/01_clean_carcassData_NEOR_v2.R') # this still needs some work
      # 
      # car_dat <- clean_carcassData_NEOR_v2(CarcsData) %>%
      #   filter(SurveyYear %in% yr_range)
      # 
      # rm(CarcsData)
      # 
      # save(car_dat, file = './data/inputs/car_dat.rda')
  
  
      load('./data/inputs/car_dat.rda')
          
  # Load and clean Redd Data----
      
      # load('./data/inputs/ReddsData.rda')
      # 
      # redd_dat <- clean_reddData_NEOR(ReddsData) %>%
      #   filter(SurveyYear %in% yr_range & ReddSpecies == 'S_CHN')
      # 
      # rm(ReddsData)
      # 
      # save(redd_dat, file = './data/inputs/redd_dat.rda')

      load('./data/inputs/redd_dat.rda')

# inputs for annual report tables and figures ---------------------------------
source('./R/11_GRSME_functions.R') # GRSME functions
source('./R/08_trib_esc.R') # Table 3 - tributary escapement
source('./R/12_weir_disp.R') # Table 4 - final weir disposition
source('./R/13_sgs_stuff.R') # Table 5 - sgs stuff
source('./R/14_weir_props.R') # Table 6 - weir-based proportions
source('./R/15_carc_ages.R') # Table 7 - carcass ages by return year - REVIEW AND UPDATE
# Table 8 - R/S, SAR, PP
source('./R/17_steelhead.R')# Tables 9 & 11 - sth trapping and escapement

# combine/reformat tables to fit ATT report templates
T6_tmp <- T6_weir_proportions %>%
  rename(stream = trap) %>%
  mutate(stream = replace(stream, stream == 'Lostine River Weir', 'Lostine River'))

T3_T6 <- full_join(trib_esc_att, T6_tmp)
write_xlsx(T3_T6, path = './data/outputs/T3-T6-GRSME.xlsx')

write_xlsx(T4_disp, path = './data/outputs/T4-GRSME.xlsx')

T5_T7 <- full_join(sgs_stuff, pAge_carc)
write_xlsx(T5_T7, path = './data/outputs/T5-T7-GRSME.xlsx')

T9_T11 <- full_join(T9_sth_disp, T11_sth_weirstuff) %>%
  .[, c(2, 1, 3:ncol(.))]
write_xlsx(T9_T11, path = './data/outputs/T9-T11-GRSME.xlsx')


# figures ---------------------------------------------------------------------
# source('./R/graphing.R') #or just produce graphs within the above scripts#

# 2010/11 - carcass origin oddities...HON...
# will need to make assumption about hat/nat as in comanager spdsht

# next step(s): ignore the report's organization and --------------------------
# start a new project to produce PMs for all strata of interest
# e.g., jack/adult/origin/all


nat_trib <- trib_esc %>%
  filter(strata == "A_Nat" | strata == "J_Nat") %>%
  mutate(pDOWN = N_D/trib_esc)

glimpse(nat_trib)

writexl::write_xlsx(nat_trib, path = "./data/outputs/nat_trib.xlsx")
