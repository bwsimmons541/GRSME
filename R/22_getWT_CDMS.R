### gets project specific water temp data from CDMS for a range of years -------

## dependencies ----------------------------------------------------------------
library(tidyverse)
library(cdmsR)

## GET CDMS DATA ---------------------------------------------------------------

# login to CDMS
source("./r/cdms_login.r")

ds <- get_Datastores() # look up DataStores IDs
# water temp = 122

proj <- get_Projects() # look up Project IDs
# GRMSE = 11059

locs <- cdmsR::get_ProjectLocations(
  DatastoreID = 122,
  ProjectID = 11059
) #gets GRSME water temp locations

# Set years of interest
years <- 2024

# Generate all combinations of years and project locations
combinations <- expand.grid(years, locs$LocationID)

# Get water temp data for each combination using map2
tmp1 <- map2(combinations$Var1, combinations$Var2, get_WaterTempData)

tmp2 <- bind_rows(tmp1) #combine list of results

# add in lat long locations
locs_xy <- locs %>% select(LocationID, Latitude, Longitude)
tmp3 <- left_join(tmp2, locs_xy, by = c('LocationId' = 'LocationID'))

# add useful date time vars
WaterTemps <- tmp3 %>%
  mutate(WTDateTime = ymd_hms(ReadingDateTime),
         WTYear = year(WTDateTime),
         WTMonth = month(WTDateTime),
         WTDOY = yday(WTDateTime))

glimpse(WaterTemps)

saveRDS(WaterTemps, file = './data/WaterTemps.RDS') #save to data folder
rm(list = ls()) # clean up environment

# load cleaned data
WT <- readRDS('./data/WaterTemps.RDS')

glimpse(WT)

# quick summary by year, site, # of days with data
Tsummary <- WT %>%
  group_by(LocationLabel, WTYear) %>%
  summarize(n = n_distinct(WTDOY)) %>%
  pivot_wider(values_from = n, names_from = WTYear, names_sort = T)

Tsummary

# in future would be good to look up NHD comIDs, basin info, streamname, etc
# pulling in comIDs would enable access to all the NHDPlus attributes
