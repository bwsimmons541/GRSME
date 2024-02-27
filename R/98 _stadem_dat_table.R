library

stadem_dat %>%
  select(species, spawn_year, origin, estimate, sd, lower_ci, upper_ci) %>%
  mutate(across(.cols = c('estimate', 'sd', 'lower_ci', 'upper_ci'), as.numeric)) %>%
  mutate(across(.cols = c('estimate', 'sd', 'lower_ci', 'upper_ci'), round, 0)) %>%
  arrange(species, spawn_year) %>%
  flextable() %>%
  #align(align = "center", j = c("Pass1","Pass2","Pass3","Pass4","Total"), part = "all" ) %>%
  merge_v(j = c('species', 'spawn_year')) %>%
  valign(valign = "top") %>%
  # set_formatter(Escapement = function(x) round(x,0),
  #               SD = function(x) round(x, 2)) %>%
  #colformat_num(j = c('estimate', 'sd', 'lower_ci', 'upper_ci'), round, digits = 5) %>%
  set_header_labels(species = 'Species', spawn_year = 'Spawn Year', origin = 'Origin-Clip', estimate = 'Escapement',
                    sd = 'SD', lower_ci = 'Lower 95% CI', upper_ci = 'Upper 95% CI') %>%
  set_caption(
    caption = paste0('STADEM estimates from 2010-',yr,'.'),
    style = "Table Caption",
    autonum = run_autonum(seq_id = "app", bkm = "app4",
                          pre_label = 'Appendix ')) %>%
  theme_booktabs() %>%
  autofit() %>%
  set_table_properties(layout = "autofit")

??set_table_properties
