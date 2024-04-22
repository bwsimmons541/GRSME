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

# devtools::install_github('RyanKinzer/cuyem')

librarian::shelf(tidyverse,
                 ryankinzer/cdmsR,
                 ryankinzer/cuyem,
                 readxl,
                 writexl,
                 plotrix)

# load datasets for redds, carcasses, and trapping ----

    source('./R/getCDMSdata.R') # un-comment to update data sets from CDMS

# inputs for annual report tables and figures ----

    source('./R/GRSME_functions.R') # GRSME functions
    source('./R/trib_esc.R') # Table 3 - tributary escapement
    source('./R/weir_disp.R') # Table 4 - final weir disposition
    source('./R/sgs_stuff.R') # Table 5 - sgs stuff - Wallowa/Lostine River Population
    source('./R/weir_props.R') # Table 6 - weir-based proportions
    source('./R/carc_ages.R') # Table 7 - carcass ages by return year - REVIEW AND UPDATE
    # Table 8 - R/S, SAR, PP
    source('./R/steelhead.R')# Tables 9 & 11 - sth trapping and escapement


# Tributary escapement fromatted for ATT ----

    trib_esc_att <- trib_esc %>%
      filter(strata == "A+J") %>%
      select(
        strata,
        stream,
        trap_year,
        n_unique,
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

# fix inconsistent joining of with/without stream in data set, and upr/lwr naming ---------

# combine/reformat tables to fit ATT report templates ----

    T6_tmp <- T6_weir_proportions %>%
      rename(stream = trap) %>%
      mutate(stream = replace(stream, stream == 'Lostine River Weir', 'Lostine River'))
    
    T3_T6 <- full_join(trib_esc_att, T6_tmp)
    write_xlsx(T3_T6, path = './data/outputs/adult-report/T3-T6-GRSME.xlsx')
    
    write_xlsx(T4_disp, path = './data/outputs/adult-report/T4-GRSME.xlsx')
    
    T5_T7 <- full_join(sgs_stuff, pAge_carc)
    write_xlsx(T5_T7, path = './data/outputs/adult-report/T5-T7-GRSME.xlsx')
    
    T9_T11 <- full_join(T9_sth_disp, T11_sth_weirstuff) %>%
      .[, c(2, 1, 3:ncol(.))]
    write_xlsx(T9_T11, path = './data/outputs/adult-report/T9-T11-GRSME.xlsx')
  

# figures ---------------------------------------------------------------------
# source('./R/graphing.R') #or just produce graphs within the above scripts#

# 2010/11 - carcass origin oddities...HON...
# will need to make assumption about hat/nat as in comanager spdsht

# next step(s): ignore the report's organization and --------------------------
# start a new project to produce PMs for all strata of interest
# e.g., jack/adult/origin/all


# Natural escapement ----

  nat_trib <- trib_esc %>%
    filter(strata == "A_Nat" | strata == "J_Nat") %>%
    mutate(pDOWN = N_D/trib_esc)
  
  writexl::write_xlsx(nat_trib, path = "./data/outputs/nat_trib.xlsx")
