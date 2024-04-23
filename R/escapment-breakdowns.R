
# Escapement breakdowns

librarian::shelf(tidyverse)

# Load data ---- 

    source('./R/getCDMSdata.R') # un-comment to update data sets from CDMS

# Escapement script ----
  
    source('./R/trib_esc.R') 
    # also depends on below_weir_esc & GRSME functions?

# Natural escapement ----

    nat_trib <- trib_esc %>%
      filter(strata == "A_Nat" | strata == "J_Nat") %>%
      mutate(pDOWN = N_D/trib_esc)
    
    writexl::write_xlsx(nat_trib, path = "./data/outputs/nat_trib.xlsx")


# Tributary escapement formatted for ATT ----

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