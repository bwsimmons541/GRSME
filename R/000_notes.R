

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


# Cleaning script check for opercle punches data transcrption and loss ----

load('./data/inputs/opercles.rda')


  checking %>%
    select(OPPunch, LeftOperclePunchType, OpercleLeft, RightOperclePunchType, OpercleRight) %>%
    group_by(OPPunch, LeftOperclePunchType, RightOperclePunchType, OpercleLeft, OpercleRight) %>%
    distinct(OPPunch)
   
# So - this is passing in the data, post-cleaning, but without the select statement in the cleaning function.   
# Then i group them all and do a distinct to get a look at how every potential value is being dealt with

