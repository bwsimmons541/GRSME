# Tables 9 & 11 - inputs - GRSME data -----------------------------------------
# filter to just steelhead data, already filtered to GRSME and yr_range--------
trap_dat_sth <- trap_dat %>%
  filter(species == 'Steelhead')


# Table 9 - weir trapping disposition summary  --------------------------------
# weir, ponded, unique fish passed, disposed, transferred
T9_sth_disp <- trap_dat_sth %>%
  filter(recap == FALSE) %>% # we want all unique fish
  group_by(trap,trap_year,disposition) %>%
  summarize(n = sum(count)) %>%
  pivot_wider(names_from = disposition, values_from = n)

writexl::write_xlsx(T9_sth_disp, path = './data/outputs/sth_disp.xlsx')


# Table 11 - weir summary - esc, etc ------------------------------------------
# n1 = fish passed upstream with mark -----------------------------------------
n1 <- trap_dat_sth %>%
  filter(marked == TRUE) %>%  # derived field from cleaning script,
                              # fish punched and passed upstream
  group_by(trap_year, trap) %>%
  summarize(n1 = sum(count, na.rm = TRUE))

# n2 = total recoveries upstream of weir (or in downstream trap) --------------
#      with discernable mark/no mark
# GRSME - some limited kelt data entered into FINS but ignored here
#n2_FINS <- trap_dat_sth %>%
  #filter(current_location == 'Downstream') %>%  
  # this assumes all have discernable mark/no mark
  # are marks always discernable? what if they are not? how is that entered into FINS?
  # could filter by existing mark contains 'UNK'?
  #group_by(trap_year, trap) %>%
  #summarize(n2 = sum(count, na.rm = TRUE))

# Lostine - no downstream trap, all recaps are carcass or kelt recoveries on/above the weir
# 2021 data in a temporary spreadsheet
carcs_los_sth <- read_xlsx(path = './data/inputs/Lostine_weir_carcasses.xlsx') %>%
  filter(Species == 'STS') %>%
  rename(OP_Mark = 'OP Mark') %>%
  filter(OP_Mark != 'Unknown') %>% #don't want unknowns
  mutate(trap_year = as.numeric(format(Date, format = '%Y')), 
         recapped = case_when(grepl('LOP',OP_Mark) ~ TRUE, #will need to change to include ROP marks in past
                              TRUE ~ FALSE), #or just use a recap field from the spreadsheet
         count = 1, trap = 'Lostine River Weir')

n2 <- carcs_los_sth %>%
  group_by(trap_year, trap) %>%
  summarize(n2 = sum(count, na.rm = TRUE))


# m2 = recoveries upstream of weir (or in downstream trap) with marks ---------
m2 <- carcs_los_sth %>%
  filter(recapped == TRUE) %>%
  group_by(trap_year, trap) %>%
  summarize(m2 = sum(count, na.rm = TRUE))

# load summary n2, m2 data from a spreadsheet
# temporary fix until we can get the past raw data management standardized
carc_n2m2_summ <- read_xlsx(path = './data/inputs/steelhead_summary_n2m2.xlsx')

# above weir escapement estimates ---------------------------------------------
weir_esc_sth <- full_join(n1, n2) %>%
  full_join(m2) %>%
  left_join(carc_n2m2_summ, by = c('trap_year','trap')) %>% # temporary work-around
  mutate(n2 = coalesce(n2.x, n2.y),
         m2 = coalesce(m2.x, m2.y)) %>% 
  select(-n2.x, -n2.y, -m2.x, -m2.y) %>% 
  bind_cols(est_abundance(.$n1, .$n2, .$m2,
                          method = 'adjusted Peterson',
                          alpha = 0.05) %>%
              rename(N_U = N, SE_N_U = SE, N_U_lwr = lwr, N_U_upr = upr)) %>%
  mutate(across(where(is.numeric), round, digits=0)) # animals are whole numbers


# hatchery fraction - FINS data only ------------------------------------------
# for Lostine could add unmarked carcasses - but it's probably not necessary
hat_frac_sth <- trap_dat_sth %>%
  filter(recap == FALSE) %>%
  filter(origin != 'Unknown') %>%
  group_by(trap,trap_year,origin) %>%
  summarize(n = sum(count)) %>%
  pivot_wider(names_from = origin, values_from = n) %>%
  mutate(across(where(is.numeric), replace_na, 0)) %>%
  bind_cols(est_proportion(.$Hatchery,.$Hatchery+.$Natural)) %>%
  rename(pHat = p, pHat_SE = SE, pHat_lwr = lwr, pHat_upr = upr)


# female proportion - FINS data only ------------------------------------------
# for Lostine could add unmarked carcasses - but it's probably not necessary
fem_prop_sth <- trap_dat_sth %>%
  filter(recap == FALSE) %>%
  filter(sex != 'Unknown') %>%
  group_by(trap,trap_year,sex) %>%
  summarize(n = sum(count)) %>%
  pivot_wider(names_from = sex, values_from = n) %>%
  mutate(across(where(is.numeric), replace_na, 0)) %>%
  bind_cols(est_proportion(.$Female,.$Female+.$Male)) %>%
  rename(pFem = p, pFem_SE = SE, pFem_lwr = lwr, pFem_upr = upr)


# T11 - pulling the pieces together -------------------------------------------
T11_sth_weirstuff <- full_join(weir_esc_sth, hat_frac_sth) %>%
  full_join(fem_prop_sth) %>%
  select(trap_year, trap,
         N_U, N_U_lwr, N_U_upr,
         pHat, pHat_lwr, pHat_upr,
         pFem, pFem_lwr, pFem_upr)
writexl::write_xlsx(T11_sth_weirstuff, path = './data/outputs/sth_weirstuff.xlsx')
