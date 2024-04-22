#Table 6 - weir-based proportions ---------------------------------------------

# hatchery fraction, weir-based -----------------------------------------------

hat_frac_weir <- trap_dat %>%
  filter(species == 'Chinook' & recap == FALSE) %>%
  sum_groups(.summary_var = origin, .cnt_var = count, trap_year, trap) %>%
  pivot_wider(names_from = origin, values_from = n) %>%
  bind_cols(est_proportion(.$Hatchery, .$Hatchery + .$Natural, method = 'score')) %>%
  rename(pHat = p, pHat_SE = SE, pHat_lwr = lwr, pHat_upr = upr) %>%
  select(-c(Hatchery,Natural,pHat_SE))
  
# this is not GRSME would prefer to estimate hat fraction
# weir data doesn't account for the hatchery-biased below-weir harvest that occurs in the Lostine
# we may have just used the weir data for this table in last year's report
# that's OK as long as we clearly label/describe what we're reporting
# a better approach would be to use our escapement to tributary data
# i.e., #hats / #total return
# that would include all sources of data - weir, carcs, redds, harvest
# could pull esc#s from spreadsheet or escapement script

# female proportion, weir-based -----------------------------------------------

pFemale_weir <- trap_dat %>%
  filter(sex != 'Unknown') %>%
  sum_groups(.summary_var = sex, .cnt_var = count, trap_year, trap) %>%
  pivot_wider(names_from = sex, values_from = n) %>%
  bind_cols(est_proportion(.$Female, .$Female + .$Male, method = 'score')) %>%
  rename(pFemale = p, pFemale_SE = SE, pFemale_lwr = lwr, pFemale_upr = upr) %>%
  select(-c(Female,Male,pFemale_SE))

#weir based - probably our most reliable data since we have female-biased carc recoveries
#we discussed that at length the last couple of years...maybe Kinzer has ATT notes
#we should document some of these method decisions to prevent rehashing them annually

# table 6 - combining the pieces ----------------------------------------------

T6_weir_proportions <- full_join(hat_frac_weir,pFemale_weir)
  
writexl::write_xlsx(T6_weir_proportions, path = './data/outputs/los_weir_proportions.xlsx')

