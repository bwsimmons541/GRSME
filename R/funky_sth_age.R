# This first script is old and didn't seem to be calculating proportions correctly within geom_bar.

# steelhead_age %>%
#    # mutate(spawn_year = case_when(
#    #   spawn_year == 'SY2010' ~ '2010',
#    #   spawn_year == 'SY2011' ~ '2011',
#    #   spawn_year == 'SY2012' ~ '2012',
#    #   spawn_year == 'SY2013' ~ '2013',
#    #   spawn_year == 'SY2014' ~ '2014',
#    #   spawn_year == 'SY2015' ~ '2015',
#    #   spawn_year == 'SY2016' ~ '2016',
#    #   spawn_year == 'SY2017' ~ '2017',
#    #   spawn_year == 'SY2018' ~ '2018',
#    #   spawn_year == 'SY2019' ~ '2019',
#    #   spawn_year == 'SY2020' ~ '2020',
#    #   TRUE ~ spawn_year
#    #   )) %>%
#      # mutate(spawn_year = as.numeric(spawn_year)) %>%
#      filter(!is.na(swAge)) %>%
#      filter(spawn_year %in% yr_range) %>%
#    ggplot(aes(x = POP_NAME, y = swAge, fill = as.factor(swAge))) +
#      geom_bar(stat = 'identity', position = position_fill(reverse = TRUE)) + 
#      scale_fill_viridis_d(direction = 1, option = "D") +
#      # scale_fill_futurama() +
#      scale_y_continuous(breaks = breaks_pretty(n=3)) +
#      facet_wrap( ~ spawn_year, ncol = 5, labeller = label_wrap_gen(width = 20)) +
#      coord_flip() +
#      labs(x = '',
#           y = 'Age Proportion',
#           fill = 'Ocean Age') +
#      theme(strip.text.y = element_text(angle = 0),
#            axis.text.y = element_text(size = 8, vjust = 0),
#            panel.spacing.x = unit(1,"lines"))


# calculate proportions and force feed geom_bar

st_swAge <- steelhead_age %>%
  filter(!is.na(swAge)) %>%
  est_group_p(.summary_var = swAge, alpha = 0.5, spawn_year, POP_NAME)

writexl::write_xlsx(st_swAge, path = '../outputs/st_swAge.xlsx')



st_swAge %>%
  filter(spawn_year %in% yr_range) %>%
  ggplot(aes(x = POP_NAME, y = p, fill = as.factor(swAge))) +
  geom_bar(stat = 'identity', position = position_fill(reverse = TRUE)) +
  scale_fill_viridis_d(direction = 1, option = "D") +
  # scale_fill_futurama() +
  scale_y_continuous(breaks = breaks_pretty(n=3)) +
  facet_wrap( ~ spawn_year, ncol = 5, labeller = label_wrap_gen(width = 20)) +
  coord_flip() +
  labs(x = '',
       y = 'Age Proportion',
       fill = 'Ocean Age') +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.y = element_text(size = 8, vjust = 0),
        panel.spacing.x = unit(1,"lines"))




#lemon::gtable_show_names(std_ocean_age)
#lemon::reposition_legend(std_ocean_age, 'center', panel = c('panel-1-3', 'panel-4-3'))