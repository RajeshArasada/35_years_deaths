#install.packages('pacman')
library(pacman)
library(shiny)
library(shinythemes)
library(flexdashboard)
library(shinydashboard)
library(RColorBrewer)
library(ggplot2)
library(ggmap)
library(dplyr)
library(maps)
library(mapdata)
library(data.table)
library(htmltools)



# Add an html dependency, without overwriting existing ones
appendDependencies <- function(x, value) {
  if (inherits(value, "html_dependency"))
    value <- list(value)
  
  old <- attr(x, "html_dependencies", TRUE)
  
  htmlDependencies(x) <- c(old, value)
  x
}

# Add dashboard dependencies to a tag object
addDeps <- function(x) {
  if (getOption("shiny.minified", TRUE)) {
    adminLTE_js <- "app.min.js"
    shinydashboard_js <- "shinydashboard.min.js"
    adminLTE_css <- c("AdminLTE.min.css", "_all-skins.min.css")
  } else {
    adminLTE_js <- "app.js"
    shinydashboard_js <- "shinydashboard.js"
    adminLTE_css <- c("AdminLTE.css", "_all-skins.css")
  }
  
  dashboardDeps <- list(
    htmlDependency("AdminLTE", "2.0.6",
                   c(file = system.file("AdminLTE", package = "shinydashboard")),
                   script = adminLTE_js,
                   stylesheet = adminLTE_css
    ),
    htmlDependency("shinydashboard",
                   as.character(utils::packageVersion("shinydashboard")),
                   c(file = system.file(package = "shinydashboard")),
                   script = shinydashboard_js,
                   stylesheet = "shinydashboard.css"
    )
  )
  
  appendDependencies(x, dashboardDeps)
}

# load the data
# county_diseases <- read.fst("./data/county_diseases.fst", as.data.table = TRUE)
# state_diseases <- read.fst("./data/states_diseases_df.fst", as.data.table = TRUE)
load("./data/county_diseases_1.Rdata")
load("./data/state_diseases_2.Rdata")
load("./data/state_diseases_1.Rdata")
# 
# county_diseases <- unique(county_diseases, by = c("lat", "long", "group", "disease"))
# state_diseases <- unique(state_diseases, by = c("lat", "long", "group", "disease"))
# summarizedData <- read.fst("./data/summarizedData.fst", as.data.table = TRUE)
# summarizedData[, year := substr(year, 8, 11)]
# write.fst(state_diseases, "./data/state_disease_2.fst")
# write.fst(county_diseases, "./data/county_disease_2.fst")

# all states list
allStates <- unique(county_diseases_1$region)

# all Diseases List
allDiseases <- unique(county_diseases_1$disease)

# all states list
states <- map_data("state")
# all counties list
counties <- map_data("county")

states <- as.data.table(states)
counties <- as.data.table(counties)



ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

plotMap <- function(state, year, diseases){
  
  data <- county_diseases_1%>%
    filter(region %in% state & disease %in% diseases)%>%
    select(region, subregion, paste("mRate", year, "mean", sep = "_"))
  
  # change the column mean rate to meanRate
  colnames(data)[3] <- "meanRate"
  # state_df = subset(states, region == state)
  state_county <- subset(counties, region == state)
  data <- as.data.table(data)
  data <- merge(state_county, data, all.x = TRUE, by = c("region", "subregion"))
  
  base <- ggplot(data = data, mapping = aes(x = long, y = lat, group = group))+
    coord_fixed(1.3) +
    geom_polygon(color = 'black', fill = 'gray')
  
  # base + theme_nothing() +
  #   geom_polygon(data = state_county, fill = NA, color = "white") +
  #   geom_polygon(color = "black", fill = NA)
  data <- data[!is.na(meanRate), ]
  base <-base + 
    labs(title = paste("Countywise Death Rates in ", state),
         subtitle = diseases) +
    geom_polygon(data = data, aes(fill = meanRate), color = "white") +
    scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B",
                         midpoint=median(data$meanRate)) +
    geom_polygon(color = "black", fill = NA) +
    # scale_fill_gradient(trans = "log10") +
    theme_bw() +
    theme(plot.title = element_text(color="black", face="bold", size=18, hjust=0))+
    coord_fixed(ratio=1) +
    ditch_the_axes
  
  return(base)
}

# us map

plotUSMap <- function(diseases, year){
  data <- state_diseases_1%>%
    filter(disease %in% diseases)%>%
    select(region, paste("mRate", year, "mean", sep = "_"))
  # change the column mean rate to meanRate
  colnames(data)[2] <- "meanRate"
  data <- as.data.table(data)
  data <- merge(states, data, all.x = TRUE, by = "region")
  
  us_base <- ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) + 
    geom_polygon(color = "grey", fill = "gray") +
    theme_nothing()
  data <- data[!is.na(meanRate), ]
  
  us1 <- us_base + 
    geom_polygon(data = data, aes(fill = meanRate), color = "white") +
    scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B",
                         midpoint=median(data$meanRate)) +
    
    labs(title = paste("Death Rates in US by"),
         subtitle = diseases) +
    geom_polygon(color = "black", fill = NA) +
    theme_bw() +
    theme(plot.title = element_text(color="black", face="bold", size=18, hjust=0))+
    ditch_the_axes 
  # scale_colour_gradient(high = "#132B43", low = "#56B1F7")
  # scale_fill_gradient(trans = "log10")
  
  return(us1)
}

plotCountyMap <- function(diseases, year){
  data <- county_diseases_1%>%
    filter(disease %in% diseases)%>%
    select(region, subregion, paste("mRate", year, "mean", sep = "_"))
  # change the column mean rate to meanRate
  colnames(data)[3] <- "meanRate"
  
  data <- merge(counties, data, all.x = TRUE, by = c("region", "subregion"))
  us_base <- ggplot(data = counties, mapping = aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) + 
    geom_polygon(color = "grey") +
    theme_nothing()
  
  data <- data[!is.na(meanRate), ]
  us1 <- us_base + 
    geom_polygon(data = data, aes(fill = meanRate), color = "white") +
    scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B",
                         midpoint=median(data$meanRate)) +
    
    labs(title = "Countywise Death Rates in US",
         subtitle = diseases) +
    geom_polygon(color = "black", fill = NA) +
    theme_bw() +
    theme(plot.title = element_text(color="black", face="bold", size=18, hjust=0))+ 
    ditch_the_axes
  # scale_colour_gradient(high = "#132B43", low = "#56B1F7")
  # scale_fill_gradient(trans = "log10")
  
  return(us1)
}

plotTopTenCounties <- function(state, diseases, year){
  data <- county_diseases_1%>%
    filter(region %in% state & diseases %in% disease)%>%
    select(subregion, paste("mRate", year, "mean", sep = "_"))
  # change the column mean rate to meanRate
  colnames(data)[2] <- "meanRate"
  data <- data %>%
    group_by(subregion) %>%
    summarize(meanRatebySubregion = mean(meanRate))%>%
    arrange(meanRatebySubregion)%>%
    mutate(subregion = factor(subregion, subregion))%>%
    slice(1:10)%>%
    ggplot(aes(x = subregion, y = meanRatebySubregion), meanRatebySubregion)+
    geom_bar(stat = "identity") +  coord_flip() +
    xlab("Counties") + ylab("Number of Deaths/100,000 people") +
    labs(title = "Top 10 Counties",
         subtitle = diseases) +
    theme(plot.title = element_text(color="black", face="bold", size=18, hjust=0))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "none")
  return(data)
}

# summarizedData <- state_diseases%>%
#   select(disease, region, mRate_1980_mean,mRate_1985_mean, mRate_1990_mean,mRate_1995_mean,mRate_2000_mean,mRate_2005_mean,mRate_2010_mean,mRate_2014_mean)%>%
#   distinct(disease, region, mRate_1980_mean,mRate_1985_mean, mRate_1990_mean,mRate_1995_mean,mRate_2000_mean,mRate_2005_mean,mRate_2010_mean,mRate_2014_mean)%>%
#   group_by(region, disease)%>%
#   summarise(deaths_1980 = sum(mRate_1980_mean),
#             deaths_1985 = sum(mRate_1985_mean),
#             deaths_1990 = sum(mRate_1990_mean),
#             deaths_1995 = sum(mRate_1995_mean),
#             deaths_2000 = sum(mRate_2000_mean),
#             deaths_2005 = sum(mRate_2005_mean),
#             deaths_2010 = sum(mRate_2010_mean),
#             deaths_2014 = sum(mRate_2014_mean))%>%
#   gather(year, deaths, -c(region,disease))
# 
# write.fst(summarizedData, "./data/summarizedData.fst")
plotStatewiseDeathRate <- function(diseases, state){
  data <- state_diseases_2[disease %in% diseases & region %in% state, ]
  ggplot(data = data, aes(x=year, y=mean, group = 1)) +
    geom_point(color = 'black', size = 3) +
    geom_ribbon(aes(ymin=lower, ymax=upper),
                alpha=0.2, color = "lightblue") +
    geom_line(color="blue", size=1.2) + 
    labs(title = paste("Death Rates Over the Years by \n", diseases),
         subtitle = paste("in ", state)) +
    theme(plot.title = element_text(color="black", face="bold", size=18, hjust=0))
}


