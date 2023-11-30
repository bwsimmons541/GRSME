install.packages("ggridges")

library(writexl)
library(readxl)
library(tidyverse)
library(cuyem)
library(ggridges)
library(viridis)

trap_dat <- read.csv('./data/TrappingData.csv') %>%
  mutate(trapped_date = as.Date(Trapped.Date, "%m/%d/%Y"),
         trap_year = year(trapped_date),
         trap_day = yday(trapped_date),
         Length = as.double(Length),
         trap = Trap,
         count = as.double(Count),
         year_rev = fct_rev(as.factor(trap_year)))

glimpse(trap_dat)
unique(trap_dat$count)

yr_range = c(2014:2023)

df <- trap_dat %>%
  filter(trap_year %in% yr_range) %>%
  filter(Species == 'Chinook') %>%
  filter(Run == 'Summer') %>% # run == 'Spring' | 
  filter(Recap == "False")
#filter(age_designation == 'Adult')
#decided to include all fish b/c Dworshak, SF Salmon had several fish with
#no adult/jack status


# descriptive stats - all traps ------------------------------------------------
arrival_timing <- df %>%
  mutate(trap_day = yday(trapped_date)) %>%
  group_by(trap, trap_year) %>%
  summarise(unique_fish = sum(count),
            first_fish = min(trap_day),
            perc_10 = quantile(trap_day, 0.1),
            median = quantile(trap_day, 0.5),
            perc_90 = quantile(trap_day, 0.9),
            last_fish = max(trap_day)) %>%
  mutate(across(c(first_fish:last_fish),
                as.Date, origin = '1900-01-01')) %>%
  mutate(across(c(first_fish:last_fish),
                format,'%b %d'))
write_xlsx(arrival_timing, path = ('./data/outputs/arrival_timing_FINS.xlsx'))



# ridgeline plot year by year --------------------------------------------------
graph_trap_timing <- function(df, trap_name) {
  graph_df <- df %>%
    filter(trap == trap_name) %>%
    mutate(trap_day = yday(trapped_date)) %>%
    mutate(trap_day = as.Date(trap_day, origin = '1900-01-01')) %>%
    # mutate(year_rev = fct_rev(as.factor(trap_year))) %>%
    ggplot(aes(x = trap_day, y = year_rev, colour = year_rev, fill = year_rev)) +
    geom_density_ridges(rel_min_height = 0.001, color = "black", size = 1) +
    scale_fill_viridis(discrete = TRUE,
                       direction = -1,
                       alpha = 0.8,
                       option = "A") +
    labs(x = 'Date of trapping',
         y = "Return year"#,
         # title = trap_name,
         # subtitle = 'Spring/Summer Chinook'
         ) +
    theme_ridges() + 
    theme(legend.position = "none",
          axis.title.x = element_text(hjust = 0.5),
          axis.title.y = element_text(hjust = 0.5))
  ggsave(paste('./plots/',trap_name,
               '.tiff', sep = ""), scale = 1, dpi = 150)
  return(graph_df)
}


#trap_name = unique(df$trap) # to plot all traps


#' trap_name = c('Rapid River Fish Trap',
#'               #'Hells Canyon Dam Ladder Trap', # graph looked odd - didn't explore
#'               'Dworshak National Fish Hatchery Ladder Trap', # several fish do not have jack/adult designation
#'               'Kooskia National Fish Hatchery Weir',
#'               'South Fork Salmon River', # several fish do not have jack/adult designation
#'               'Johnson Creek Weir - Upstream Trap',
#'               'Salmon River Trap Sawtooth FH ',
#'               'Imnaha River Weir',
#'               'Lostine River Weir',
#'               'Pahsimeroi Trap',
#'               #'Lookingglass Lower Trap', # don't think this data is relevant to purpose
#'               'Lookingglass  Upper Trap',
#'               'Lostine River Weir')

trap_name = 'Lostine River Weir'

lapply(trap_name, graph_trap_timing, df = df)
#I don't think the geom_density_ridges account for records with counts > 1 ...
#but that effects relatively few fish...shouldn't effect key patterns
#these are smoothed graphs anyways
#the desc stats do account for counts > 1

# descriptive stats - average run timing for traps -----------------------------
arrival_timing_avg <- df %>%
  # mutate(trap_day = yday(trapped_date)) %>%
  group_by(trap, trap_year) %>% # get stats for each year and trap
  summarise(unique_fish = sum(count),
            first_fish = min(trap_day),
            perc_10 = quantile(trap_day, 0.1),
            median = quantile(trap_day, 0.5),
            perc_90 = quantile(trap_day, 0.9),
            last_fish = max(trap_day)) %>%
  group_by(trap) %>% # average stats across years for each trap
  summarise(unique_fish_avg = mean(unique_fish),
            first_fish_avg = mean(first_fish),
            perc_10_avg = mean(perc_10),
            median_avg = mean(median),
            perc_90_avg = mean(perc_90),
            last_fish_avg = mean(last_fish)) %>%
  filter(trap %in% trap_name) %>%
  .[order(.$perc_10_avg),] %>%
  mutate(across(where(is.numeric), round, 0)) %>%
  mutate(across(c(first_fish_avg, perc_10_avg,
                  median_avg, perc_90_avg, last_fish_avg),
                as.Date, origin = '1900-01-01')) %>%
  mutate(across(c(first_fish_avg, perc_10_avg,
                  median_avg, perc_90_avg, last_fish_avg),
                format,'%b %d'))

write_xlsx(arrival_timing_avg, path = ('./data/outputs/arrival_timing_FINS_avg.xlsx'))


# ridgeline plot - comparing avg run timing, using weekly data -----------------
sum_names = paste(rep('prop',length(yr_range)), yr_range, sep='')
tmp <- df %>%
  filter(trap %in% trap_name) %>%
  mutate(trap_week = week(trapped_date)) %>%
  group_by(trap, trap_year, trap_week) %>%
  summarise(n = sum(count)) %>%
  arrange(trap_week, .by_group = TRUE) %>%
  mutate(n_prop = (n)/sum(n)) %>% 
  # pivot and fill in NA with zeros so avg's are correct
  select(-n) %>%
  pivot_wider(names_from = trap_year, names_prefix = 'prop', values_from = n_prop) %>%
  mutate(across(where(is.numeric), replace_na, 0)) %>%
  as.data.frame() %>%
  mutate(n_prop_avg = rowMeans(.[,c(sum_names)])) %>%
  mutate(trap = case_when(
    trap == 'Rapid River Fish Trap' ~ 'Rapid R',
    trap == 'Dworshak National Fish Hatchery Ladder Trap' ~ 'Dworshak NFH',
    trap == 'Kooskia National Fish Hatchery Weir' ~ 'Kooskia NFH',
    trap == 'South Fork Salmon River' ~ 'SF Salmon R',
    trap == 'Johnson Creek Weir - Upstream Trap' ~ 'Johnson Cr',
    trap == 'Salmon River Trap Sawtooth FH ' ~ 'Sawtooth FH',
    trap == 'Imnaha River Weir' ~ 'Imnaha R',
    trap == 'Lostine River Weir' ~ 'Lostine R',
    trap == 'Pahsimeroi Trap' ~ 'Tucannon R',
    #trap == 'Lookingglass Lower Trap' ~ 'Lookingglass Lwr',
    trap == 'Lookingglass  Upper Trap' ~ 'Lookingglass Upr'))

tmp %>%
  mutate(trap_week_start = ymd('1900-01-01') + weeks(trap_week - 1)) %>%
  ggplot(aes(x = trap_week_start, y = trap, group = trap, height = n_prop_avg)) +
  geom_ridgeline(fill = 'dodgerblue1', scale = 5) +
  labs(x = 'Start of Trap Week',
       y = "Trap",
       title = 'Average Weekly Trapping',
       subtitle = 'Spring/Summer Chinook') +
  theme_ridges() + 
  theme(legend.position = "none")

ggsave('./avg_timing.tiff',
       scale = 1.5, dpi = 150)


