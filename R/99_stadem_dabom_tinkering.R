apD <- node_det %>%
  filter(spawn_year == yr) %>%
  filter(estimate != 0) %>%
  mutate(cv = round(as.numeric(cv), 2)) %>%
  mutate(across(.cols = c('estimate', 'sd', 'lower_ci', 'upper_ci'), 
                as.numeric)) %>%
  mutate(across(.cols = c('estimate', 'sd', 'lower_ci', 'upper_ci'), 
                round, 2)) %>%
  select(species, site_id, node, n_tags, estimate, sd, cv, lower_ci, 
         upper_ci)


tmp <- node_det %>%
  filter(spawn_year == 2021)


glimpse(dabom_esc)

glimpse(dabom21)

 dabom21 <- dabom_esc %>%
   filter(spawn_year == 2021,
          node != is.na(node),
          estimate != 0) %>%
   mutate(cv = round(as.numeric(cv), 2)) %>%
   mutate(across(.cols = c('estimate', 'sd', 'lower_ci', 'upper_ci'), 
                 as.numeric)) %>%
   mutate(across(.cols = c('estimate', 'sd', 'lower_ci', 'upper_ci'), 
                 round, 2)) %>%
   select(variable, species, site_id, node, n_tags, estimate, sd, cv, lower_ci, 
          upper_ci)
   
   
   
  
 
unique(dabom_esc$variable)


unique(dabom21$node)


load('./data/ry21_iptds.rda') #dabom_esc, iptds_prod?, stadem_dat

dabom_esc <- dabom_esc %>%
  filter(Name != 'Not Observed') %>%
  mutate(Name = case_when(
    location == 'MFMAR' ~ 'Marsh Creek on the Middle Fork Salmon River',
    location == 'SNHCT-s' ~ 'Snake River Hells Canyon Trap',
    TRUE ~ Name #If the conditions above don't anywhere else use the name column.
  ))

node_det <- dabom_esc %>% filter(variable == 'Detection Efficiency')


load('./data/ry21_iptds.rda') #dabom_esc, iptds_prod?, stadem_dat
load('./data/ry21iptds052522.rda')

glimpse(dabom_esc)

glimpse(ry21_iptds)



dabom_esc <- dabom_esc %>%
  filter(Name != 'Not Observed') %>%
  mutate(Name = case_when(
    location == 'MFMAR' ~ 'Marsh Creek on the Middle Fork Salmon River',
    location == 'SNHCT-s' ~ 'Snake River Hells Canyon Trap',
    TRUE ~ Name #If the conditions above don't anywhere else use the name column.
  ))

glimpse(dabom_esc)

iptds_age <- dabom_esc %>% filter(variable == 'Age Proportion')
pop_esc <- dabom_esc %>% filter(variable == 'Population Escapement')
iptds_fem <- dabom_esc %>% filter(variable == 'Female Proportion')
site_esc <- dabom_esc %>% filter(variable == 'Site Escapement')
node_det <- dabom_esc %>% filter(variable == 'Detection Efficiency')

rm(dabom_esc)

cdms_host <- 'https://npt-cdms.nezperce.org'# REMOVE?
api_key = 'api_user'# REMOVE?
cdmsLogin('api_user', api_key, cdms_host = cdms_host)# REMOVE?

cdmsR::cdmsLogin('brians', 'bs2018')

iptds_locs <- get_Project(11069)$Locations

glimpse(ry21_iptds)
glimpse(iptds_locs)

tmp <- iptds_locs %>%
  select(TRT)

?left_join

tmp <- left_join(ry21_iptds, by
                       iptds_locs %>%
                         select(Name, LocationLabel = Label))

