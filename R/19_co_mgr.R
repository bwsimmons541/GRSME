
# Total Redds Above and Below Weir

redds_total_above_below <- redd_dat %>%
  filter(Species == 'Chinook salmon', Run == 'Spring/summer') %>%
  group_by(SurveyYear,MPG,StreamName, AboveWeir, ActivityId) %>%
  summarize(NewRedds = mean(NewRedds)) %>% #could we use aggregate function instead
  group_by(SurveyYear,MPG,StreamName, AboveWeir) %>%
  summarize(NewRedds = sum(NewRedds)) %>%
  arrange(StreamName) %>%
  filter(StreamName == "Lostine River")


redds_total_non_los <- redd_dat %>%
  filter(Species == 'Chinook salmon', Run == 'Spring/summer') %>%
  group_by(SurveyYear,MPG,StreamName, ActivityId) %>%
  summarize(NewRedds = mean(NewRedds)) %>% #could we use aggregate function instead
  group_by(SurveyYear,MPG,StreamName) %>%
  summarize(NewRedds = sum(NewRedds)) %>%
  arrange(StreamName)

#Harvest Data

trib_harvest = readxl::read_xlsx('./data/inputs/trib_harvest.xlsx') %>% # I need to modify the format of trib_harvest
  filter(ReturnYear %in% yr_range) %>%
  group_by(ReturnYear) %>%
  # summarize(trib_harvest = sum(c(A_Hat, J_Hat, A_Nat, J_Nat))) %>%
  rename (trap_year = ReturnYear) %>%
  pivot_longer("J_Hat":"A", names_to = "strata", values_to = "harvest") %>%
  group_by(trap_year, 
           strata) %>%
  summarise("harvest" = sum(harvest))

#Redd Expansion

# non-lostine carcasses

glimpse(car_dat)

unique(car_dat$StreamName)
unique(car_dat$Species)
unique(car_dat$Run)
unique(car_dat$CWT_Age)


wal_pop_carcs <- car_dat %>%
    filter(StreamName == "Hurricane Creek" |
            StreamName == "Wallowa River" |
            StreamName == "Bear Creek",
           SurveyYear %in% yr_range,
           Species == "Chinook salmon",
           Run == "Spring/summer",
           ForkLength > 630) %>%
  group_by(SurveyYear,
           Origin) %>%
  summarize(n_carc = sum(Count, na.rm = TRUE))
