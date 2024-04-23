#' @title sumGRSMEbrood
#'
#' @description takes prepared FINS Data and generates summaries for weir report.
#'
#' @param data FINS data filtered for desired year (GRSME_df)
#' @author Tyler Stright
#'
#' @examples 
#'
#' @import tidyverse, lubridate, ggplot2, viridis
#' @export
#' @return NULL


sumSTHweekly <- function(data, trap.year) {

t3_df <- data %>% #change back to data
  filter(trap_year == trap.year) %>%
  mutate(EpiWeek = epiweek(trapped_date),
         FloorDate = floor_date(trapped_date, unit = 'week'),
         Week = format(FloorDate, "%b %d"))

# Steelhead dates for table.
sth_df <- t3_df %>%
  filter(species == 'Steelhead')

w1_sth <- min(sth_df$EpiWeek)  # first week of trapping
wf_sth <- max(sth_df$EpiWeek)  # 'last' week of trapping (most recent)

w1_sth_date <- sth_df %>%   # Date of first catch
  ungroup() %>%
  filter(EpiWeek == w1_sth) %>%
  distinct(Week) %>%
  pull(Week)

# Create Groups by Week
t3_tmp <- t3_df %>%
  ungroup() %>%
  mutate(`Week Start` = case_when(
    EpiWeek < w1_sth ~ paste0('< ', w1_sth_date),
    EpiWeek >= w1_sth & EpiWeek != wf_sth ~ Week,
    EpiWeek == wf_sth ~ paste0(Week, '*')),
    EpiWeek = case_when(
      grepl(pattern = '<', `Week Start`) ~ 1,
      !grepl(pattern = '<', `Week Start`) ~ EpiWeek
    ))

# Tally Broodstock - DON't NEED THis
# broodstock_df <- t3_tmp %>%
#   filter(species == 'Chinook',
#          age_designation == 'Adult',
#          moved_to == "Lookingglass Fish Hatchery Inbox") %>%
#   group_by(origin, EpiWeek, `Week Start`) %>%
#   summarize(Brood = sum(count)) %>%
#   mutate(Cohort = case_when(
#     origin == 'Hatchery' ~ 'H Chinook Brood',
#     origin == 'Natural' ~ 'N Chinook Brood'
#   )) %>% ungroup()
# 
# b_hat <- broodstock_df %>% filter(origin == 'Hatchery') %>%
#   spread(key= Cohort, value = Brood, fill = 0) %>%
#   select(-origin)
# b_nat <- broodstock_df %>% filter(origin == 'Natural') %>%
#   spread(key= Cohort, value = Brood, fill = 0) %>%
#   select(-origin) 

# Tally Captures
captures_df <- t3_tmp %>%
  filter(species %in% c('Steelhead', 'Rainbow Trout', 'Bull Trout'),
         recap == 'FALSE',
         age_designation %in% c(NA, 'Adult')) %>%
  group_by(species, origin, EpiWeek, `Week Start`) %>%
  summarize(Captured = sum(count)) %>%
  mutate(Cohort = case_when(
    origin == 'Hatchery' & species == 'Steelhead' ~ 'H Steelhead Captures',
    origin == 'Natural' & species == 'Steelhead' ~ 'N Steelhead Captures',
    species == 'Rainbow Trout' ~ 'Rainbow Trout',
    species == 'Bull Trout' ~ 'Bull Trout'
  )) %>% ungroup()

c_hat <- captures_df %>%
  filter(Cohort == 'H Steelhead Captures') %>%
  spread(key=Cohort, value = Captured, fill = 0) %>%
  select(-species, -origin) 

c_nat <- captures_df %>%
  filter(Cohort == 'N Steelhead Captures') %>%
  spread(key=Cohort, value = Captured, fill = 0) %>%
  select(-species, -origin) 

c_rbt <- captures_df %>%
  filter(Cohort == 'Rainbow Trout') %>%
  spread(key = Cohort, value = Captured, fill = 0) %>%
  select(-species, -origin)

c_bt <- captures_df %>%
  filter(Cohort == 'Bull Trout') %>%
  spread(key=Cohort, value = Captured, fill = 0) %>%
  select(-species, -origin) 

# Table
table3_raw <- full_join(c_hat, c_nat, by = c('Week Start', 'EpiWeek')) %>%
  full_join(c_rbt, by = c('Week Start', 'EpiWeek')) %>%
  full_join(c_bt, by = c('Week Start', 'EpiWeek')) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  arrange(EpiWeek) %>% 
  select(-EpiWeek)

t3_totals <- apply(table3_raw[,c(2:4)], 2, sum) #change back to 2:4 if it doesn't work

table3_final <- table3_raw %>%
  add_row(`Week Start` = 'Total',
          `H Steelhead Captures`= t3_totals[1], 
          `N Steelhead Captures` = t3_totals[2],
          `Rainbow Trout` = t3_totals[3]) %>% # I removed the bull trout row.
  mutate(`Natural Steelhead` = `N Steelhead Captures`,
         `Hatchery Steelhead` = `H Steelhead Captures`) %>%
  select(`Week Start`, `Natural Steelhead`, `Hatchery Steelhead`, `Rainbow Trout`) # removed , `Bull Trout`

  return(table3_final)
}
