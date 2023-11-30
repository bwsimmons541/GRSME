
library(cdmsR)
library(cuyem)
library(tidyverse)
library(lubridate)
library(viridis) # Don't need Viridis unless using for color package in plot
library(scales)


catch <- read.csv(file = './data/TrappingData12_21.csv') %>%
  mutate(date = as.Date(Trapped.Date, "%m/%d/%Y"),
         year = year(date),
         month = month(date),
         doy = yday(date)) %>%
  filter(Recap == "False") %>%
  mutate(month = as.character(month),
         month_label = case_when(
           month == 1 ~ 'Jan',
           month == 2 ~ 'Feb',
           month == 3 ~ 'Mar',
           month == 4 ~ 'Apr',
           month == 5 ~ 'May',
           month == 6 ~ 'Jun',
           month == 7 ~ 'Jul',
           month == 8 ~ 'Aug',
           month == 9 ~ 'Sep',
           month == 10 ~ 'Oct',
           month == 11 ~ 'Nov',
           month == 12 ~ 'Dec',
           TRUE ~ month
         ),
         month = as.double(month))

glimpse(catch)

#by month 10-year ####
ggplot(catch, aes(x = month, y = Count, fill = Species)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  scale_y_continuous(name = 'Count', 
                     breaks = breaks_pretty(n = 3)) +
  scale_x_continuous(name = 'Month',
                     breaks = breaks_pretty(12),
                     limits = c(1, 12)) +
  theme_bw() +
  theme(panel.grid  = element_blank(),
        panel.background = element_blank(),
        legend.position = 'bottom',
        text = element_text(family = 'sans')) +
  # facet_wrap( ~ year, ncol = 2, scales = 'free_y')
  facet_wrap( ~ year, ncol = 2)


ggsave(
  'lrw_catch_2012-2021.png',
  plot = last_plot(),
  device = "png",
  path = './figures/',
  scale = 1,
  width = 8,
  height = 10.5,
  units = "in",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

ggsave(
  'lrw_catch_2012-2021_free_y.pdf',
  plot = last_plot(),
  device = "pdf",
  path = './figures/',
  scale = 1,
  width = 8,
  height = 10.5,
  units = "in",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

#by day 5-year ####

catch %>%
  filter(year %in% 2016:2021) %>%
ggplot(aes(x = doy, y = Count, fill = Species)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  scale_y_continuous(name = 'Count', 
                     breaks = breaks_pretty(n = 4)) +
  scale_x_continuous(name = 'Day of Year',
                     breaks = breaks_pretty(),
                     limits = c(1, 365)) +
  theme_bw() +
  theme(panel.grid  = element_blank(),
        panel.background = element_blank(),
        legend.position = 'bottom',
        text = element_text(family = 'sans')) +
  # facet_wrap( ~ year, ncol = 1, scales = 'free_y')
  facet_wrap( ~ year, ncol = 1)


ggsave(
  'lrw_catch_day_16-2021.png',
  plot = last_plot(),
  device = "png",
  path = './figures/',
  scale = 1,
  width = 8,
  height = 10.5,
  units = "in",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

ggsave(
  'lrw_catch_day_16-2021_free_y.pdf',
  plot = last_plot(),
  device = "pdf",
  path = './figures/',
  scale = 1,
  width = 8,
  height = 10.5,
  units = "in",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

#by day 2020 ####

catch %>%
  filter(year == 2020) %>%
  ggplot(aes(x = doy, y = Count, fill = Species)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  scale_y_continuous(name = 'Count', 
                     breaks = breaks_pretty(n = 4)) +
  scale_x_continuous(name = 'Day of Year',
                     breaks = breaks_pretty(12),
                     limits = c(1, 365)) +
  theme_bw() +
  theme(panel.grid  = element_blank(),
        panel.background = element_blank(),
        legend.position = 'bottom',
        text = element_text(family = 'sans')) +
  facet_wrap( ~ year, ncol = 1)

ggsave(
  'lrw_catch_day_2020.png',
  plot = last_plot(),
  device = "png",
  path = './figures/',
  scale = 1,
  width = 10.5,
  height = 8,
  units = "in",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

ggsave(
  'lrw_catch_day_2020.pdf',
  plot = last_plot(),
  device = "pdf",
  path = './figures/',
  scale = 1,
  width = 10.5,
  height = 8,
  units = "in",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

