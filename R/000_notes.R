

# Check out the Janitor Package and see what it can do for you ----
librarian::shelf(janitor)

?janitor

?adorn_totals

# genetic samples from weir ----

tmp <- trap_dat %>%
  filter(grepl("GEN", samples),
         trap_year == 2023,
         species == "Chinook",
         run == "Summer")


