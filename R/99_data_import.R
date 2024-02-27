#### This is just to save old code for importing data from xlsx or csv####


```{r get-iptds-data}
load('./data/ry21_iptds.rda') #dabom_esc, iptds_prod?, stadem_dat

dabom_esc <- dabom_esc %>%
  filter(Name != 'Not Observed') %>%
  mutate(Name = case_when(
    location == 'MFMAR' ~ 'Marsh Creek on the Middle Fork Salmon River',
    location == 'SNHCT-s' ~ 'Snake River Hells Canyon Trap',
    TRUE ~ Name #If the conditions above don't anywhere else use the name column.
  ))

iptds_age <- dabom_esc %>% filter(variable == 'Age Proportion')
pop_esc <- dabom_esc %>% filter(variable == 'Population Escapement')
iptds_fem <- dabom_esc %>% filter(variable == 'Female Proportion')
site_esc <- dabom_esc %>% filter(variable == 'Site Escapement')
node_det <- dabom_esc %>% filter(variable == 'Detection Efficiency')

rm(dabom_esc)
```

```{r get-Carcass-data}
# car_df <- get_CarcassData()
# save(car_df, file = './data/car_df.rda') # had to change '../data/' to './data/'
# load('./data/car_df.rda')
# car_dat <- clean_carcassData(car_df)

# rm(car_df) #remove car_df

# grsme_carcass <- get_CarcassData_NEOR()
# save(grsme_carcass, file = './data/grsme_carcass.rda') # hat to change '../data/' to '.data/'

# rm(grsme_carcass)
# load('./data/grsme_carcass.rda')

# IS THIS JUST A CHECK TO SEE IF WE GOT THE DATA? #

# grsme_carcass %>%
# ggplot(aes(x = ForkLength)) +
#   geom_histogram() +
#   facet_wrap(~MarkRecapSizeCategory)

# grsme_carcass <- clean_carcassData_NEOR(grsme_carcass)

# car_dat <- bind_rows(car_dat,grsme_carcass) %>%
#   filter(SurveyYear %in% yr_range)

# rm(grsme_carcass)

# save(car_dat, file = './data/car_dat.rda')
# rm(car_dat)

load('./data/car_dat.rda')

```

```{r get_weir_data}
#Get Data

# using CDMS get trapping data
#    trap_df <- get_WeirData()
#    save('trap_df', file = './data/trap_df.rda')
#    rm(trap_df)
#    load('./data/trap_df.rda')


# spwn_df <-get_SpawningData()
# save('trap_df', 'spwn_df', file = './data/fins_df.rda')
# load('./data/fins_df.rda')


# trap_dat <- clean_weirData(trap_df) %>%
#   filter(grepl('NPT', facility)) %>%
#   filter(trap_year %in% yr_range)

# rm(trap_df)


# save('trap_dat', file = './data/trap_dat.rda')
# rm(trap_dat)
load('./data/trap_dat.rda')

```

```{r get_Fall_Chinook_data}

# CHF_run_rec_df <- read_csv('./inputs/CHF_run_rec.csv')
# glimpse(CHF_run_rec_df)
# save(CHF_run_rec_df, file = './data/CHF_run_rec_df.rda')
# rm(CHF_run_rec_df)
load('./data/CHF_run_rec_df.rda')

# CHF_ppp_df <- read_csv('./inputs/CHF_ppp.csv')
# glimpse(CHF_ppp_df)
# save(CHF_ppp_df, file = './data/CHF_ppp_df.rda')
# rm(CHF_ppp_df)
load('./data/CHF_ppp_df.rda')


# CHF_sgs_df <- read_csv('./inputs/CHF_t1_sgs.csv')
# glimpse(CHF_sgs_df)
# save(CHF_sgs_df, file = './data/CHF_sgs_df.rda')
# rm(CHF_sgs_df)
load('./data/CHF_sgs_df.rda')

# CHF_sar_df <- read_csv('./inputs/CHF_t2_sar.csv')
# glimpse(CHF_sar_df)
# save(CHF_sar_df, file = './data/CHF_sar_df.rda')
# rm(CHF_sar_df)
load('./data/CHF_sar_df.rda')

# fcrr <- get_FallRR()
# glimpse(fcrr)
# save('fcrr', file = './data/fcrr.rda')
# rm(fcrr)
# load('./data/fcrr.rda')

# fcrr_clean <- clean_FallRR(fcrr)
# rm(fcrr)
# glimpse(fcrr_clean)
# save('fcrr_clean', file = ('./data/fcrr_clean.rda'))
# rm(fcrr_clean)
load('./data/fcrr_clean.rda')  

```

```{r get_Spring/Summer_Chinook_data}

# CHS_esc_weir_df <- read_csv('./inputs/CHS_t3t6_trib_esc_weir_proportions.csv')
# glimpse(CHS_esc_weir_df)
# save(CHS_esc_weir_df, file = './data/CHS_esc_weir_df.rda')
# rm(CHF_esc_weir_df)
load('./data/CHS_esc_weir_df.rda')

# CHS_final_disp_df <- read_csv('./inputs/CHS_t4_final_disp.csv')
# glimpse(CHS_final_disp_df)
# save(CHS_final_disp_df, file = './data/CHS_final_disp_df.rda')
# rm(CHS_final_disp_df)
load('./data/CHS_final_disp_df.rda')


# KEEP FORE REFERENCE CLEAN UP FIELDS FROM .xlsx IMPORT
# CHS_sgs_age_df <- readxl::read_excel('./data/CHS_t5t7_sgs_measures_carc_ages.xlsx') %>%
#   mutate(across(.cols = c(starts_with('pAge'), starts_with('psm'), starts_with('pHOS'), starts_with('pFem')), .fns = as.double))

# CHS_sgs_age_df <- read_csv('./inputs/CHS_t5t7_sgs_measures_carc_ages.csv')
# glimpse(CHS_sgs_age_df)
# save(CHS_sgs_age_df, file = './data/CHS_sgs_age_df.rda')
# rm(CHS_sgs_age_df)
load('./data/CHS_sgs_age_df.rda')


# Map SharePoint Folder to a network drive, then read_excel
# CHS_productivity_df <- readxl::read_excel('Y:/CHS_productivity.xlsx')
# glimpse(CHS_productivity_df)
# save(CHS_productivity_df, file = './data/CHS_productivity_df.rda')
load('./data/CHS_productivity_df.rda')

# old method    
# CHS_productivity_df <- read_csv('./inputs/CHS_productivity.csv')
# glimpse(CHS_productivity_df)
# save(CHS_productivity_df, file = './data/CHS_productivity_df.rda')
# rm(CHS_productivity_df)
# load('./data/CHS_productivity_df.rda')