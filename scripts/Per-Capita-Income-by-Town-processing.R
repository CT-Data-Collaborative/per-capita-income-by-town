library(dplyr)
library(acs)
library(datapkg)
library(tidyr)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Per Capita Income by Town
# Created by Jenna Daly
# On 11/27/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw_data <- (paste0(getwd(), "/", raw_location))
x2016_files <- dir(path_to_raw_data, recursive=T, pattern = "ACS")

#Get state data
geography=geo.make(state=09)
yearlist=c(2009:2016)
span = 5
col.names="pretty" 
key="ed0e58d2538fb239f51e01643745e83f380582d7"
options(scipen=999)

tables <- c("", "A", "B", "C", "D", "E", "F", "G", "H", "I")
races <- c("All", "White Alone", "Black or African American Alone", "American Indian and Alaska Native Alone", 
           "Asian Alone", "Native Hawaiian and Other Pacific Islander", "Some Other Race Alone", 
           "Two or More Races", "White Alone Not Hispanic or Latino", "Hispanic or Latino")

state_data <- data.table()
for (i in seq_along(yearlist)) {
  endyear = yearlist[i]
  inter_data <- data.table()
  for (j in seq_along(tables)) {
    tbl <- tables[j]
    race <- races[j]
    data <- acs.fetch(geography=geography, endyear=endyear, span=span, 
                         table.number=paste0("B19301", tbl), col.names=col.names, key=key)
    year <- data@endyear
    print(paste("Processing: ", year, race))
    year <- paste(year-4, year, sep="-")
    geo <- data@geography
    geo$NAME <- NULL
    pc_income <- acsSum(data, 1, "Per Capita Income")
    estimates <- data.table(
            geo, 
            estimate(pc_income),
            year,
            race,
            "Number",
            "Per Capita Income"
        )
    moes <- data.table(
            geo,
            standard.error(pc_income) * 1.645,
            year,
            race,
            "Number",
            "Margins of Error"
        )
    numberNames <- c(
            "FIPS",
            "Total",
            "Year",
            "Race/Ethnicity",
            "Measure Type",
            "Variable"
         )
    setnames(estimates, numberNames)
    setnames(moes, numberNames)
    numbersData.melt <- melt(
            rbind(estimates, moes),
            id.vars=c("FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name="Per Capita Income",
            variable.factor = F,
            value.name="Value",
            value.factor = F
    )
    inter_data <- rbind(inter_data, numbersData.melt)
  }
  state_data <- rbind(state_data, inter_data)
}

#Get town data
geography=geo.make(state=09, county="*", county.subdivision = "*")   

town_data <- data.table()
for (i in seq_along(yearlist)) {
  endyear = yearlist[i]
  inter_data <- data.table()
  for (j in seq_along(tables)) {
    tbl <- tables[j]
    race <- races[j]
    data <- acs.fetch(geography=geography, endyear=endyear, span=span, 
                         table.number=paste0("B19301", tbl), col.names=col.names, key=key)
    year <- data@endyear
    print(paste("Processing: ", year, race))
    year <- paste(year-4, year, sep="-")
    geo <- data@geography
    geo$county <- sprintf("%02d", geo$county)
    geo$county <- gsub("^", "090", geo$county)
    geo$FIPS <- paste0(geo$county, geo$countysubdivision)
    geo$state <- NULL
    geo$NAME <- NULL
    geo$countysubdivision <- NULL
    geo$county <- NULL 
    pc_income <- acsSum(data, 1, "Per Capita Income")
    estimates <- data.table(
            geo, 
            estimate(pc_income),
            year,
            race,
            "Number",
            "Per Capita Income"
        )
    moes <- data.table(
            geo,
            standard.error(pc_income) * 1.645,
            year,
            race,
            "Number",
            "Margins of Error"
        )
    numberNames <- c(
            "FIPS",
            "Total",
            "Year",
            "Race/Ethnicity",
            "Measure Type",
            "Variable"
         )
    setnames(estimates, numberNames)
    setnames(moes, numberNames)
    numbersData.melt <- melt(
            rbind(estimates, moes),
            id.vars=c("FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name="Per Capita Income",
            variable.factor = F,
            value.name="Value",
            value.factor = F
         )
    
    inter_data <- rbind(inter_data, numbersData.melt)
  }
  town_data <- rbind(town_data, inter_data)
}

pcap_income <- rbind(town_data, state_data)

pcap_income$`Per Capita Income` <- NULL

#Merge in towns by FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])
towns <- as.data.table(towns)

pcap_income <- merge(pcap_income, towns, by = "FIPS", all.y=T)

pcap_income$Value <- round(pcap_income$Value, 2)

pcap_income <- pcap_income %>% 
  select(Town, FIPS, Year, `Race/Ethnicity`, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `Race/Ethnicity`, `Measure Type`)

#estimates that were "-" coded to -666666666
pcap_income$Value[pcap_income$Value == -666666666] <- NA

#moes that were "***" coded to -222222222
pcap_income$Value[pcap_income$Value == -222222222] <- NA


write.table (
  pcap_income,
  file.path(getwd(), "data", "per_capita_income_town_2016.csv"),
  sep = ",",
  row.names = F,
  na = "-9999"
)
