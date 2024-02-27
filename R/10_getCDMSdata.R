#install.packages('devtools')

# Download most recent FINS data from CDMS

librarian::shelf(ryankinzer/cdmsR,
                 tidyverse)

source('./r/cdms_api_keys.R')

# get, save, load dataset(s)
# get_Datastores() will show list of available datasets

# Weir Data

    # WeirData <- get_WeirData(Facility = 'NPT GRSME Program') # Change facility filter if working with other streams

    # save(WeirData, file = './data/inputs/WeirData.rda')

    # rm(WeirData)

    load('./data/inputs/WeirData.rda')

        # unique(sort(WeirData$CalendarYear, decreasing = TRUE))
    
# NEOR Redd data

    # ReddsData <- get_ReddData_NEOR()

    # save(ReddsData, file = './data/inputs/ReddsData.rda')
    
    # rm(ReddsData)
        
    load('./data/inputs/ReddsData.rda')
    
        # unique(sort(ReddsData$Year, decreasing = TRUE))

# NEOR Carcass data

    # CarcsData <- get_CarcassData_NEOR()

    # save(CarcsData, file = './data/inputs/CarcsData.rda')
   
    # rm(CarcsData)
    
    load('./data/inputs/CarcsData.rda')
    
    
        # unique(sort(CarcsData$Year, decreasing = TRUE))
    
