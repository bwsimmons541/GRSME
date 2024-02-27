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

glimpse(pop_esc_tmp)
unique(pop_esc_tmp$species)

pop_esc_sth <- pop_esc %>%
  filter(spawn_year == yr) %>%
  filter(species == "Steelhead")

pop_esc_chs <- pop_esc %>%
  filter(spawn_year == yr) %>%
  filter(species == "Chinook salmon")


node_det_tmp <- node_det %>%
  filter(spawn_year == yr)

unique(pop_esc_tmp$TRT_POPID)

sth_trt_popid <- read.csv('./inputs/sth_trt_popid.csv')

chs_trt_popid <- read.csv('./inputs/chs_trt_popid.csv')


subset(sth_trt_popid, !(TRT_POPID %in% pop_esc_sth$TRT_POPID ))

subset(chs_trt_popid, !(TRT_POPID %in% pop_esc_chs$TRT_POPID ))
