#Adult Report Re-write

librarian::shelf(tidyverse, writexl, readxl)

# ls(package:readxl)

# list.files("./data/")

load('./data/CHS_sgs_age_df.rda')

# age

  p_age <- CHS_sgs_age_df %>%
    select(survey_year, MPG, POP_NAME, 17:25) %>%
    pivot_longer(cols = !c(survey_year, MPG, POP_NAME),
                 names_to = c('age', 'type'),
                 names_sep = '_',
                 names_prefix = 'pAge_',
                 values_to = 'estimate') %>%
    replace_na(list(type = 'estimate'))  %>%
    pivot_wider(names_from = type, values_from = estimate)
  
  p_age <- p_age %>%
    mutate(performance_measure = "age_proportion")
  
  n_age <- CHS_sgs_age_df %>% 
    select(survey_year, MPG, POP_NAME, n_age)
  
  p_age <- p_age %>%
    left_join(n_age, by = c('survey_year', 'MPG', 'POP_NAME'))
  
  p_age <- rename(p_age, 'n' = 'n_age')
  
  p_age <-p_age %>% 
      relocate(performance_measure, .after = POP_NAME)

# pHOS

  pHOS <- CHS_sgs_age_df %>%
    select(survey_year, MPG, POP_NAME, 5:8) %>%
    mutate(performance_measure = "pHOS",
           age = "all") %>%
    rename('n' = 'n_pHOS',
           'estimate'= 'pHOS',
           'lwr' = 'pHOS_lwr',
           'upr' = 'pHOS_upr')

#p_hos
  
  pFemale <- CHS_sgs_age_df %>%
    select(survey_year, MPG, POP_NAME, 9:12) %>%
    mutate(performance_measure = "female_proportion",
           age = "all") %>%
    rename('estimate'= 'pFemale_Carc',
           'lwr' = 'pFemale_Carc_lwr',
           'upr' = 'pFemale_Carc_upr',
           'n' = 'n_pFemale')
#psm
  

  psm <- CHS_sgs_age_df %>%
    select(survey_year, MPG, POP_NAME, 13:16) %>%
    mutate(performance_measure = "prespawn_mortality",
           age = "all") %>%
    rename('estimate'= 'psm',
           'lwr' = 'psm_lwr',
           'upr' = 'psm_upr',
           'n' = 'n_psm')

# redds
  
  redds <- CHS_sgs_age_df %>%
    select(survey_year, MPG, POP_NAME, total_redds) %>%
    mutate(performance_measure = "total_redds",
           age = "all") %>%
    rename('estimate'= 'total_redds')
  
# all
  
  all <- p_age %>%
    bind_rows(pHOS,
              pFemale,
              psm,
              redds) %>%
    mutate(Species = "Chinook",
           Run = "Summer") %>%
    relocate(c('Species', 'Run'), .after = POP_NAME) %>%
    drop_na(estimate)

  
write_xlsx(all, path = "./outputs/SGS_performance_measures.xlsx")



