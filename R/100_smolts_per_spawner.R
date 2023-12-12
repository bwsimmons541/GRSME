librarian::shelf(tidyverse,
                 ryankinzer/cuyem,
                 ryankinzer/cdmsR,
                 readxl,
                 viridis)


# Import Juvenile Data data without altering Polly's workbook ----------------

  # myCols <- as.character(read_xlsx(path = "./data/odfw_smolts.xlsx", 
  #                                  sheet = "A7 CHS Smolt equivalents",
  #                                  skip = 1, 
  #                                  n_max = 1, 
  #                                  col_names = FALSE))
  # 
  # odfw_smolts <- read_xlsx(path = "./data/odfw_smolts.xlsx",
  #                          skip = 4,
  #                          col_names = myCols)
  # 
  # save(odfw_smolts, file = "./data/inputs/odfw_smolts.rda")
  # 
  # rm(odfw_smolts)

  load("./data/inputs/odfw_smolts.rda")
    
rm(myCols)

# load/clean CDMS datasets for redds, carcasses, and trapping -----------------

#source('./R/getCDMSdata.R') # un-comment to update data sets from CDMS

yr_range = c(2013:2022)

load('./data/inputs/WeirData.rda')
trap_dat <- clean_weirData(WeirData) %>%
  filter(facility == 'NPT GRSME Program') %>%
  filter(trap_year %in% yr_range)
rm(WeirData)

load('./data/inputs/CarcsData.rda')
car_dat <- clean_carcassData_NEOR(CarcsData) %>%
  filter(SurveyYear %in% yr_range)
rm(CarcsData)

load('./data/inputs/ReddsData.rda')
redd_dat <- clean_reddData_NEOR(ReddsData) %>%
  filter(SurveyYear %in% yr_range & ReddSpecies == 'S_CHN')
rm(ReddsData)



# Get adult data and estimates

source("./R/08_trib_esc.R")
source("./R/13_sgs_stuff.R")

# Join tables and calcualte trib spawners 

tmp <- trib_esc %>%
  filter(strata == "A") %>%
  left_join(psm, by = c("stream" = "POP_NAME", "trap_year" = "SurveyYear")) %>%
  mutate(trib_spawners = (N_U + N_D) * (1 - psm),
         trib_spawners_lwr = (N_U_lwr + N_D_lwr) * (1 - psm),
         trib_spawners_upr = (N_U_upr + N_D_upr) * (1 - psm))

odfw_smolts_2 <- odfw_smolts %>%
  mutate(trap_year = byear)

tmp2 <- tmp %>%
  left_join(odfw_smolts_2, by = c("trap_year" = "trap_year", "stream" = "river"))

glimpse(tmp2)

# Calculate smolts per spawner

tmp3 <- tmp2 %>%
  mutate(sm_eq_lgr = as.double(sm_eq_lgr),
         smolts_per_spawner = sm_eq_lgr / trib_spawners,
         stream = as_factor(stream)) %>%
  select(stream,
         byear,
         sm_eq_lgr,
         N_U,
         N_D,
         psm,
         trib_spawners,
         smolts_per_spawner)

write_xlsx(tmp3, path = "./data/outputs/LR_smolts_p_spwnr.xlsx")

plt <- tmp3 %>%
ggplot(aes(x = byear, y = smolts_per_spawner, color = stream))

plt +
  geom_point(size = 1) +
  geom_line(size = 1) +
  geom_hline(yintercept = 100, color = "red", linetype = 2, show.legend = TRUE) +
  labs(y = "Smolts Equivalents at LGR per Tributary Spawner",
       x = "Year",
       title = "Lostine River, OR Spring/Summer Chinook Smolts per Spawner") +
  scale_colour_viridis(discrete = TRUE, option = "D") +
  theme_bw() +
  theme(panel.grid  = element_blank(),
        panel.background = element_blank(),
        legend.position = 'bottom',
        text = element_text(family = 'sans'))

ggsave("smolts_spawner.png",
       path = "./plots/")


# Coordinated assessment data -----------------------

librarian::shelf(nwfsc-math-bio/rCAX,
                 tidyverse)

rcax_termsofuse()

rcax_datasets()

nosa <- rcax_hli("NOSA")
RperS <- rcax_hli("RperS")

glimpse(nosa)

unique(nosa$esu_dps)

sr_nosa <- nosa %>%
  filter(esu_dps == "Salmon, Chinook (Snake River spring/summer-run ESU)")

sr_RperS <- RperS %>%
  filter(esu_dps == "Salmon, Chinook (Snake River spring/summer-run ESU)")

sort(unique(sr_nosa$spawningyear))

plt_sr_nosa %>% sr_nosa 

nosa_cols <- rcax_hli("NOSA", type = "colnames")
RperS_cols <- rcax_hli("RperS", type = "colnames")

glimpse(sr_RperS)

plt_sr_RperS<- sr_RperS %>%
  filter(broodyear > 1999,
         locationname %in% c("Catherine Creek",
                             "Grande Ronde River Upper Mainstem",
                             "Imnaha River Mainstem",
                             "Wallowa/Lostine Rivers",
                             "Minam River",
                             "wenaha River"),
         !is.na(rpers)) #%>%
  mutate(locationname = as_factor(locationname),
         rpers = as.double(rpers),
         rperslowerlimit = as.double(rperslowerlimit),
         rpersupperlimit = as.double(rpersupperlimit)) %>%
  ggplot(aes(x = broodyear, y = rpers, colour = locationname))

plt_sr_RperS +
  geom_point( size = 2) +
  geom_line(size = 1) +
  # geom_ribbon(aes(ymin = rperslowerlimit, ymax = rpersupperlimit, colour = locationname), alpha = 0.5) +
  scale_color_viridis_d() +
  theme_bw() +
  theme(panel.grid  = element_blank(),
        panel.background = element_blank(),
        legend.position = 'bottom',
        text = element_text(family = 'sans')) +
  labs(title = "CAX Recruits Per Spawner NEOR",
       x = "Brood Year",
       y = "Juvenile Recruits Per Spawner")


ggsave("plt_CAX_RperS.png",
       path = "./plots/",
       height = 7.5,
       width = 10,
       )


unique(sr_RperS$locationname)

devtools::install_github()
