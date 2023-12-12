# Table 4 - weir final disposition summary ------------------------------------

# this doesn't account for broodstock returns
# need to get the origin/sex info to do that
# note that where fish were returned also matters
# the bs_returned table in T3 is just for fishr eturned to the Lostine
# but some fish were also returned to the Wallowa in 2020? and other yrs?

library(cuyem)
library(cdmsR)
library(tidyverse)

grouping = c('trap_year','stream', 'origin', 'age_designation')

weir_removal_co_mgr <- est_final_dispositions(trap_dat, grouping) %>%
  select(-c(sum_tmp,sub_outplant,sub_recycle)) 

trap_sums <- est_final_dispositions(trap_dat, grouping) %>%
  group_by(trap_year, stream, origin, age_designation) %>%
  summarize(n = sum(n)) %>%
  mutate(disp_final = 'total')

tmp <- weir_removal_co_mgr %>%
  bind_rows(trap_sums) %>%
  pivot_wider(names_from = c(origin,age_designation), values_from = n) %>%
  mutate(across(where(is.numeric), replace_na, 0)) %>%
  arrange(trap_year)



writexl::write_xlsx(tmp, path = './data/outputs/weir_removal_co_mgr.xlsx')

rm(grouping, tmp)
