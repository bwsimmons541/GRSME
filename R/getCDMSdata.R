#install.packages('devtools')

# Download most recent FINS data from CDMS

#load packages ----

librarian::shelf(ryankinzer/cdmsR,
                 ryankinzer/cuyem,
                 tidyverse)


# Login to CDMS ----

# source('./r/cdms_login.R')

# set years----

yr = year(Sys.Date())
yr_range = 2010:yr # Data prior to 2010 is kind of sloppy I need more clarification



# get, save, load dataset(s) ----
# get_Datastores() will show list of available datasets

# Weir Data ----

    # WeirData <- get_WeirData()
    # 
    # save(WeirData, file = './data/inputs/WeirData.rda')
    # 
    # rm(WeirData)
    # 
    # load('./data/inputs/WeirData.rda')
    # 
    # trap_dat <- clean_weirData(WeirData) %>%
    #   filter(facility == 'NPT GRSME Program') %>%
    #   filter(trap_year %in% yr_range)
    # 
    # rm(WeirData)
    # 
    # save(trap_dat, file = './data/inputs/trap_dat.rda')
    
    load('./data/inputs/trap_dat.rda')


        # unique(sort(WeirData$CalendarYear, decreasing = TRUE))

# NEOR Redd data ----

    # ReddsData <- get_ReddData_NEOR()
    #
    # save(ReddsData, file = './data/inputs/ReddsData.rda')
    # 
    # rm(ReddsData)
    #
    # load('./data/inputs/ReddsData.rda')
    # 
    # redd_dat <- clean_reddData_NEOR(ReddsData) %>%
    #   filter(SurveyYear %in% yr_range & ReddSpecies == 'S_CHN')
    # 
    # rm(ReddsData)
    # 
    # save(redd_dat, file = './data/inputs/redd_dat.rda')
    
    load('./data/inputs/redd_dat.rda')
    
        # unique(sort(ReddsData$Year, decreasing = TRUE))

# NEOR Carcass data ----

    # Download data from CDMS and save to local Drive
    
    # CarcsData <- get_CarcassData_NEOR()
    #
    # save(CarcsData, file = './data/inputs/CarcsData.rda')
    #
    # rm(CarcsData)
    
    
    # Load and clean data from Local Drive, then save cleaned data
    
    # load('./data/inputs/CarcsData.rda')
    # source('./R/clean_carcassData_NEOR_v2.R') # this still needs some work
    # 
    # car_dat <- clean_carcassData_NEOR_v2(CarcsData) %>%
    #   filter(SurveyYear %in% yr_range)
    # 
    # rm(CarcsData)
    # 
    # save(car_dat, file = './data/inputs/car_dat.rda')
    
    # Load Cleaned data
    
    load('./data/inputs/car_dat.rda')
        
        
        # unique(sort(CarcsData$Year, decreasing = TRUE))

