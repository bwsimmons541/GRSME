# Download most recent FINS data from CDMS

library(cdmsR)
# enter your CDMS credentials here, password = api_key
cdmsLogin(username = , api_key = )


# Download fins data.
AdultWeirData <- getDatasetView(99)

# Save to the data folder as an R data file
save(AdultWeirData, file = './data/AdultWeirData.rda')