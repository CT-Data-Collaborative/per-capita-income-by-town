library(acs)
library(datapkg)
source('./scripts/acsHelpers.R')

data <- acs.fetch(geography=geo.make(state=09, county="*", county.subdivision = "*"),
                  endyear=2015, span = 5, 
                  table.number="B19301", col.names="pretty", 
                  key="ed0e58d2538fb239f51e01643745e83f380582d7")

# ACS B01001
# Get geography object for CT and subcounty divisions
# acsdata <- getACSData(
#     getCTGeos("town"),
#     yearList = 2015,
#     table = "B01001"
# )

pops <- data.table()
for (data in acsdata) {
    year <- data@endyear
    geo <- data@geography$countysubdivision
    pop.total <- acsSum(data, 1, "Total")
    pop.over9 <- acsSum(data, c(5:25, 29:49), "Over 9 years")
    # pop.1020 <- acsSum(data, c(5:8, 29:32), "10 to 20 years")	
    # pop.over20 <- acsSum(data, c(9:25, 33:49), "Over 20 years") 
    # pop.1824 <- acsSum(data, c(7:10, 31:34), "18 to 24 years") 
    
    dataT <- data.table(Year = year,
                        geo = geo,
                        pop = estimate(pop.total),
                        pop2 = estimate(pop.over9),
                        moe = standard.error(pop.total) * 1.645 )
    
    datafips <- data.table(fips = getACSFips(data))

    estimates <- data.table(
        FIPS = datafips$fips,
        Year = year,
        estimate(pop.total) 
        # estimate(pop.over9), 
        # estimate(pop.1020),
        # estimate(pop.over20),
        # estimate(pop.1824)
    )
    
    names(estimates)[names(estimates) == "HD01_VD01.Estimate; Total:"] <- "Total"

    estimates <- melt(
        estimates,
        id.vars = c("FIPS", "Year"),
        variable.name = "Age Range",
        variable.factor = F,
        value.name = "Pop",
        value.factor = F
    )

    moes <- data.table(
        FIPS = datafips$fips,
        Year = year,
        standard.error(pop.total) * 1.645 
        # standard.error(pop.over9) * 1.645, 
        # standard.error(pop.1020) * 1.645,
        # standard.error(pop.over20) * 1.645,
        # standard.error(pop.1824) * 1.645
    )
    
    names(moes)[names(moes) == "HD01_VD01.Estimate; Total:"] <- "Total"
    

    moes <- melt(
        moes,
        id.vars = c("FIPS", "Year"),
        variable.name = "Age Range",
        variable.factor = F,
        value.name = "MOE",
        value.factor = F
    )

    setkey(estimates, FIPS, Year, `Age Range`)
    setkey(moes, FIPS, Year, `Age Range`)

    pops <- rbind(pops, estimates[moes])
}

pops <- pops[pops$FIPS != "0900100000",]

# Write to File
write.table(
    pops,
    file.path(getwd(), "raw", "populations.csv"),
    sep = ",",
    row.names = F
)