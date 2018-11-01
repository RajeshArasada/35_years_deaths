county_deseases_1 <- county_diseases[ , ':='(long = NULL,
                                             lat = NULL,
                                             group = NULL,
                                             order = NULL,
                                             `X..Change.in.Mortality.Rate..1980.2014` = NULL)]
# now we have only state, county, disease and all other columns are mRate columns for each 
# year
# sum by disease, state, county (use whatever column names are appropriate)
county_deseases_1 <- county_diseases[ , lapply(.SD, sum),
                                      by = .(disease, region, subregion)]
                                      
# incase if you get new column names then first select all those columns along with state, county and disease and rename them back with the same names like (mrRate_1980Mean)
# now get the unique rows
county_deseases_1 <- unique(county_deseases_1, by = c("disease", "region", "subregion"))
                                      
# now in the counties map data see what are the names for state and county and rename them to "state" and "county"
# for example
counties_map_data <- as.data.table(map_data("county"))

save(county_diseases_1, file = "./data/county_diseases_1.Rdata")
                                      
# now State Diseases
state_diseases_1 <- state_diseases[ , ':='(long = NULL,
                                            lat = NULL,
                                            group = NULL,
                                            order = NULL,
                                            `% Change in Mortality Rate, 1980-2014` = NULL,
                                            FIPS= NULL,
                                            subregion = NULL)]
                                      
# sum by disease, state, county (use whatever column names are appropriate)
state_diseases_1 <- state_diseases_1[ , lapply(.SD, sum),
                                      by = .(disease, region)]

state_diseases_2 <- melt(state_diseases_1, measure.vars = c("mRate_1980_mean", "mRate_1980_lower", 
                                                            "mRate_1980_upper", "mRate_1985_mean", "mRate_1985_lower", "mRate_1985_upper", 
                                                            "mRate_1990_mean", "mRate_1990_lower", "mRate_1990_upper", "mRate_1995_mean", 
                                                            "mRate_1995_lower", "mRate_1995_upper", "mRate_2000_mean", "mRate_2000_lower", 
                                                            "mRate_2000_upper", "mRate_2005_mean", "mRate_2005_lower", "mRate_2005_upper", 
                                                            "mRate_2010_mean", "mRate_2010_lower", "mRate_2010_upper", "mRate_2014_mean", 
                                                            "mRate_2014_lower", "mRate_2014_upper"),
                         id.vars = c("disease", "region"))

state_diseases_2[, variable := as.character(variable)]
state_diseases_2[, variable := gsub(pattern = "_", replacement = " ", x = variable)]
year <- sapply(state_diseases_2$variable, function(x) strsplit(x, split = " ")[[1]][2])
type <- sapply(state_diseases_2$variable, function(x) strsplit(x, split = " ")[[1]][3])
state_diseases_2[, variable := gsub(pattern = "_", replacement = " ", x = variable)]
state_diseases_2 <- state_diseases_2[, ':='(year = year,
                                            type = type)]

state_diseases_2[, variable := NULL]
setnames(state_diseases_2, "value", "deaths")

state_diseases_2 <- dcast(state_diseases_2, disease + region + year ~ type, value.var = "deaths")

save(state_diseases_2, file = "./data/state_diseases_2.Rdata")

state_diseases_2 <- state_diseases_2[, ':='(year = strsplit(variable, split = " ")[[1]][2],
                                            variable = ifelse(strsplit(variable,split = " ")[[1]][3] == "mean", "mean", 
                                                              ifelse(strsplit(variable,split = " ")[[1]][3] == "lower", "lower", "upper")))]
save(state_diseases_1, file = "./data/state_diseases_1.Rdata")
# the above data should have rows equal or little more than county_diseases_1
                                      
# Now use this for plotting the county map and state map
                                      
# in stead of using two data frames you are using one in both places in the plot code.
                                      