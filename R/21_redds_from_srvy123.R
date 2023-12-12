library(readxl)

sgs <- read_xlsx(path = './data/inputs/SGS_GDB.xlsx', sheet = "Chinook_Spawning_Ground_Sur_0")
new_redd <- read_xlsx(path = './data/inputs/SGS_GDB.xlsx', sheet = "NewRedds_1")

glimpse(new_redd)
glimpse(sgs)

?full_join

redd_dat <- new_redd %>%
  full_join(sgs, by = join_by(ParentGlobalID == GlobalID)) %>%
  mutate(StreamName = `River:`,
         section = `Site ID:`,
         survey_date = date(Date),
         New_Redd_Num = as.double(New_Redd_Num))

glimpse(redd_dat)

los_redd_2023 <- redd_dat %>%
  filter(StreamName == "Lostine River",
         `Is there a new redd?` == 'Yes') %>%
  mutate(SurveyYear = year(Date)) %>%
  group_by(StreamName,
           SurveyYear,
           section,
           survey_date) %>%
  summarize(new_redds = sum(New_Redd_Num)) %>%
  ungroup() %>%
  arrange(survey_date,
          section)

total_redds_2023 <- los_redd_2023 %>%
  group_by(StreamName, SurveyYear) %>%
  summarize(total_redds = sum(new_redds))

unique(los_redd$New_Redd_Num)

library(cdmsR)

cdmsLogin(username = "brians", api_key = "bs2018")
neor_redd_dat <- get_ReddData_NEOR()


neor_redd_dat <- clean_reddData_NEOR(neor_redd_dat)

glimpse(los_redd)

los_redd_yr <- neor_redd_dat %>%
  filter(StreamName == "Lostine River",
         SurveyYear %in% yr_range) %>%
  mutate(count = 1) %>%
  group_by(StreamName,
           SurveyYear) %>%
  summarize(total_redds = sum(count))

los_redd_yr <- los_redd_yr %>%
  bind_rows(total_redds_2023)
  
los_redd_yr %>% 
  mutate(stream = as.factor(StreamName)) %>%
  ggplot(aes(x = SurveyYear, y = total_redds, fill = stream)) +
  geom_bar(stat = "identity", color = "black", size = 1) +
  scale_fill_viridis(begin = 0.75,
                     discrete = TRUE,
                     option = "A",
                     alpha = 0.9) +
  labs(x = 'Year',
       y = "Total Redds") +
  theme_ridges() +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

ggsave(paste('./plots/los_redds',
             '.tiff', sep = ""), scale = 1, dpi = 150)


?scale_fill_viridis()
