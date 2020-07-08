# Download most recent FINS data from CDMS

library(cdmsR)
source('./R/cdms_api_keys.R')

keys <- cdmsKeys()
cdms_host <- keys[1]
username <- keys[2]
api_key <- keys[3]

cdmsLogin(username, api_key, cdms_host)


# Download fins data.
AdultWeirData <- getDatasetView(99)

# Save to the data folder as an R data file
save(AdultWeirData, file = './data/AdultWeirData.rda')

# load the file you just saved
load('./data/AdultWeirData.rda')
