# Table 4 - weir final disposition summary ------------------------------------

# this doesn't account for broodstock returns
# need to get the origin/sex info to do that
# note that where fish were returned also matters
# the bs_returned table in T3 is just for fishr eturned to the Lostine
# but some fish were also returned to the Wallowa in 2020? and other yrs?

library(cuyem)
library(cdmsR)
library(tidyverse)

grouping = c('trap_year','stream', 'origin', 'sex')

T4_disp <- est_final_dispositions(trap_dat, grouping) %>%
  select(-c(sum_tmp,sub_outplant,sub_recycle)) %>%
  pivot_wider(names_from = c(origin,sex), values_from = n) %>%
  mutate(across(where(is.numeric), replace_na, 0))

rm(grouping)

writexl::write_xlsx(T4_disp, path = './data/outputs/los-weir-dispositions.xlsx')
