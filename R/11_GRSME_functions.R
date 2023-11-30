# final disposition function ------------------------------------------------------------------------

# goal: develop a function that will assign "final" weir disposition from the Lostine River Weir
# for use in escapement estimates (weir removals) and in disposition summaries (annual reporting)

# notes on "final" disposition accounting based on recent data entry for purpose (>2014)
# Nat spawning = exclude recaps with existing 1LOP or 2LOP
# Outplant = exclude recaps with existing 2ROP, subtract 2ROP recaps sent elsewhere
# Recycled = exclude recaps with existing 1ROP, subtract existing 1ROP recaps sent elsewhere
# Brood = keep all ??but what about returned to river fishes???if unspawned part of nat-spawning pop?
# Distribution = keep all

# disposition information has not been entered consistently for the whole time series
# so we need to use a combination of fields (disposition, purpose, and moved_to)
# to best assign dispositions and account for the various 
# probably easier than trying to edit all the past data entry in FINS

# not going to be able to accurately account for recaps from outplants/recycled to fishery 
#   for some years because there were no unique marks, but will have little effect on results

# tested with dat = FINS data, cleaned, filtered to Lostine
# tested group_by list = trap_year, stream, origin, sex
# tested using all available GRSME data in FINS

est_final_dispositions <- function(dat, grouping) {
  
tmp <- dat %>%
  filter(species == 'Chinook') %>%
  # define final disposition for analyses, should account for data entry caveats in time series
  mutate(disp_final = case_when(
      purpose == 'Brood Stock' ~ 'Brood Stock',
      purpose == 'Within FINS Facility' ~ 'Brood Stock',
      purpose == 'Distribution' ~ 'Distribution',
      purpose == 'Natural Spawning' & grepl('Lostine River',moved_to) ~ 'Natural Spawning',
      purpose == 'Nutrient Enhancement' ~ 'Nutrient Enhancement',
      purpose == 'Natural Spawning' & grepl('Outplant',moved_to) ~ 'Outplant',
      purpose == 'Outplant' ~ 'Outplant',
      purpose == 'Recycled' ~ 'Recycled',
      TRUE ~ NA_character_)) %>%
  # prep to account for recaps and recaps that get sent elsewhere
  mutate(
    sum_tmp = case_when(
      disp_final == 'Natural Spawning' & grepl('LOP',existing_marks) ~ 0,
      disp_final == 'Outplant' &
        trap_year > 2014 & 
        grepl('ROP',existing_marks) &
        grepl('^.*?, 2',existing_marks_quantity) ~ 0, 
      disp_final == 'Recycled' &
        trap_year > 2014 &
        grepl('ROP',existing_marks) &
        grepl('^.*?, 1',existing_marks_quantity) ~ 0,
      disp_final == 'Recycled' &
        trap_year == 2011 &
        recap == TRUE ~ 0,
      TRUE ~ 1),
    sub_from_outplants = case_when(
      disp_final != 'Outplant' & trap_year > 2014 &
        grepl('ROP',existing_marks) & grepl('^.*?, 2',existing_marks_quantity) ~ 1,
      TRUE ~ 0),
    sub_from_recycled = case_when(
      disp_final != 'Recycled' & trap_year > 2014 &
        grepl('ROP',existing_marks) & grepl('^.*?, 1',existing_marks_quantity) ~ 1,
      TRUE ~ 0))

df_tmp1 <- tmp %>%
  group_by_at(append(grouping,'disp_final')) %>%
  summarise(sum_tmp = sum(sum_tmp))

df_tmp2 <- tmp %>%
  group_by_at(grouping) %>%
  summarise(sub_outplant = sum(sub_from_outplants), .groups = 'keep') %>%
  mutate(disp_final = 'Outplant')

df_tmp3 <- tmp %>%
  group_by_at(grouping) %>%
  summarise(sub_recycle = sum(sub_from_recycled), .groups = 'keep') %>%
  mutate(disp_final = 'Recycled')

df <- left_join(df_tmp1,df_tmp2) %>%
  left_join(df_tmp3) %>%
  mutate(across(where(is.numeric), replace_na, 0),
         n = sum_tmp - sub_outplant - sub_recycle)

return(df)
}
