library(shiny)
library(leaflet)
library(ggplot2)
library(ggiraph)
library(sp)
library(raster)
library(xgboost)
library(geojsonio)
library(ggplot2)
library(maps)
library(ggmap)
library(SHAPforxgboost)
library(leaflet.extras)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(leaflet.extras)
library(dashboardthemes)
library(RColorBrewer)
library(shinyWidgets)
library(sf)
library(shinyjs)
library(fresh)
library(spData)
setwd("~/CMDA_Capstone/RShinyApp")
# Encoding in UTF-8 to load images properly
options(encoding = "UTF-8")
# Loading in 100k sample points and associated data
dfMerged <- read.csv("sampledPoints.csv")
dfMerged <- dfMerged[,-c(1)]
# Loading in model file
modelCTBoost <- xgb.load("xgboost.model")
states <- geojson_read('us-states.geojson', what = 'sp')
set.seed(123)
plt <- plot(states)
rm(plt)
gc()
names(dfMerged) <- c("Longitude", "Latitude", "Annual Mean Temp",
                     "Mean Diurnal Range", "Isothermality", "Temp Seasonality",
                     "Max Temp of Warmest Month", "Min Temp of Coldest Month",
                     "Temp Annual Range", "Mean Temp of Wettest Quarter", "Mean Temp of Driest Quarter",
                     "Mean Temp of Warmest Quarter", "Mean Temp of Coldest Quarter", "Annual Precipitation",
                     "Precipitation of Wettest Month", "Precipitation of Driest Month",
                     "Precipitation Seasonality", "Precipitation of Wettest Quarter",
                     "Precipitation of Driest Quarter", "Precipitation of Warmest Quarter",
                     "Precipitation of Coldest Quarter", "Altitude", "State")

# Function to run the plots with the user selections
runProcess <- function(dfMergedAdjusted, modelCTBoost) { #dfMerged should be adjusted to the user params
  # This data should now be ready to put into model
  # Specifically for xgboost
  predictionsCTBoost <- predict(modelCTBoost, as.matrix(dfMergedAdjusted[,3:22]))
  predictionsCTBoost_probs <- predictionsCTBoost
  predictionsCTBoost <- as.numeric(predictionsCTBoost > 0.5)
  
  
  dfMergedAdjusted$predictionProb <- predictionsCTBoost_probs
  dfMergedAdjusted$predictionClass <- predictionsCTBoost
  # Feature importance plots
  
  # get importance
  col_names <- names(dfMergedAdjusted[,3:22])
  imp = xgb.importance(col_names, modelCTBoost)
  
  importance_plot <- ggplot(data=imp, aes(x=imp$Gain, y=imp$Feature)) + geom_col(aes(fill=imp$Gain)) +
    scale_fill_gradient2(low="light green", high="dark green",
                         title("Gain")) + theme_minimal() +
    labs(x="Gain", y="Feature",
         title="Most Important Features to the Tree of Heaven") +
    theme(plot.title = element_text(hjust = 0.5))
  
  shap_plot <- shap.plot.summary.wrap1(modelCTBoost, dfMergedAdjusted[,3:22], dilute=10) #dilute=25 is plotting 1/25 or quarter of data
  # Good explanantion of SHAP https://towardsdatascience.com/interpretable-machine-learning-with-xgboost-9ec80d148d27
  plotList <- list("dfMerged" = dfMergedAdjusted, "Importance" = importance_plot, "Shap" = shap_plot)
  return(plotList)
}


plotList <- runProcess(dfMerged, modelCTBoost) #Can call plotList$Heatmap to show heatmap map etc





#############################-----------------Finding the differences------------------#############################

# Comment this section out if you are trying to compile and run the app

# Reading in data
setwd("C:/Users/Akshay/Downloads")
require(tidyverse)
occdata <- readr::read_csv("CSVDownload_map.csv")
# Splitting into states column
occdata$State <- gsub("^.+?, |, United States", "", occdata$Location)
# Filtering to only Virginia, NC
library(dplyr)
#occdata <- occdata %>% filter(State=="Virginia" | State=="North Carolina" | State="South Carolina")
# Decide which columns to keep
keep_cols = c("ObsDate", "Latitude", "Longitude", "State")
occdata <- occdata[keep_cols]
print(length(occdata$ObsDate))
# Filtering out NAs and Blanks
occdata <- occdata[!is.na(occdata$Latitude), ]


#1 decimal place is 6 miles, if you round between two locations, there will be duplicates, take the higher probability of prevalence between them

# 2 sd above the mean of probabilities, as long as the states are included that will be a good mark, don't want too many areas.

threshold <- mean(plotList$dfMerged$predictionProb) + 2*sd(plotList$dfMerged$predictionProb)

occdata$occurence <- 1
newdf <- plotList$dfMerged
newdf <- newdf[newdf$predictionProb > threshold,]
newdf$Latitude <- round(newdf$Latitude, 1)
newdf$Longitude <- round(newdf$Longitude, 1)
occdata$Latitude <- round(occdata$Latitude, 1)
occdata$Longitude <- round(occdata$Longitude, 1)


library(dplyr)
newdf <- newdf %>% 
  group_by(Longitude, Latitude) %>% 
  filter(predictionProb==max(predictionProb))


occdata$comb <- paste(occdata$Longitude, occdata$Latitude, sep="")
newdf$comb <- paste(newdf$Longitude, newdf$Latitude, sep="")


diff_df <- newdf[!(newdf$comb %in% occdata$comb),]

texas_data <- na.omit(texas_data)
texas_data <- diff_df[diff_df$State=="Virginia",]
unique(diff_df$State)
library(ggplot2)
library(maps)
library(ggmap)

library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
# Using Akshay's api key, can register own if you want to
register_google(key="AIzaSyDPKTFEPBsrtlqMBI1nmFvfvskrNQ9Rlw8")
ll_means <- sapply(texas_data[1:2], mean)
# Each state was done one at a time. You can change the location and region for another state
sq_map2 <- get_map(location = 'dallas',  maptype = "hybrid", source = "google", zoom = 7)
counties <- map_data("county")
tx_county <- subset(counties, region == 'texas')
colnames(texas_data) <- c("lon", "lat", "Annual Mean Temp",
                          "Mean Diurnal Range", "Isothermality", "Temp Seasonality",
                          "Max Temp of Warmest Month", "Min Temp of Coldest Month",
                          "Temp Annual Range", "Mean Temp of Wettest Quarter", "Mean Temp of Driest Quarter",
                          "Mean Temp of Warmest Quarter", "Mean Temp of Coldest Quarter", "Annual Precipitation",
                          "Precipitation of Wettest Month", "Precipitation of Driest Month",
                          "Precipitation Seasonality", "Precipitation of Wettest Quarter",
                          "Precipitation of Driest Quarter", "Precipitation of Warmest Quarter",
                          "Precipitation of Coldest Quarter", "Altitude", "State", "predictionProb", "predictionClass", "comb")
ggmap(sq_map2) + 
  geom_point(data = texas_data[,1:2], size = 1, aes(lon, lat, color=texas_data$predictionProb)) +
  geom_polygon(data = tx_county, aes(x=long, y=lat, group = group), fill = NA, color="black")+
  scale_color_gradient(low="yellow", high="red", name='Probability of Occurence')
  



#########################--------------------------Building App----------------####################################

# Reading in data
setwd("C:/Users/Akshay/Downloads")
require(tidyverse)
occdata <- readr::read_csv("CSVDownload_map.csv")
# Splitting into states column
occdata$State <- gsub("^.+?, |, United States", "", occdata$Location)
# Filtering to only Virginia, NC
library(dplyr)
#occdata <- occdata %>% filter(State=="Virginia" | State=="North Carolina" | State="South Carolina")
# Decide which columns to keep
keep_cols = c("ObsDate", "Latitude", "Longitude", "State")
occdata <- occdata[keep_cols]
print(length(occdata$ObsDate))
# Filtering out NAs and Blanks
occdata <- occdata[!is.na(occdata$Latitude), ]
texas_data_occ <- occdata[occdata$State=="Virginia",]
ll_means <- sapply(texas_data_occ[2:3], mean)
sq_map2 <- get_map(location = 'charlottesville',  maptype = "hybrid", source = "google", zoom = 7)
#> Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=34.753117,-119.751324&zoom=15&size=640x640&scale=2&maptype=satellite&language=en-EN&sensor=false
texas_lon_occ <- texas_data_occ[,3:2]
colnames(texas_lon_occ) <- c('lon','lat')

ggmap(sq_map2) + 
  geom_point(data = texas_lon_occ, color = "red", size = 1) +
  geom_polygon(data = tx_county, aes(x=long, y=lat, group = group), fill = NA, color="black")


USA <- map_data('state')
g <- ggplot(USA, aes(long, lat)) + 
  geom_polygon(aes(group=group),fill="white",colour="black",size=0.5) +
  coord_equal() + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) +
  labs(x='Longitude', y='Latitude') +
  theme_bw()
g <- g + 
  geom_point(data=diff_df,aes(Longitude, Latitude, color=diff_df$predictionProb)) +
  theme(
    legend.position = 'bottom',
    legend.key.size = unit(1, "cm")
  ) + ggtitle("Tree of Heaven Model with XGBoost") + theme(plot.title = element_text(hjust = 0.5))
g


USA <- map_data('usa')
g1 <- ggplot(USA, aes(long, lat)) + 
  geom_polygon(aes(group=group),fill="white",colour="black",size=0.5) +
  coord_equal() + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) +
  labs(x='Longitude', y='Latitude') +
  theme_bw()
g1 <- g1 + 
  geom_point(data=occdata,aes(Longitude, Latitude, color=as.factor(occdata$occurence))) +
  theme(
    legend.position = 'bottom',
    legend.key.size = unit(1, "cm")
  ) + ggtitle("Tree of Heaven Actual Points") + theme(plot.title = element_text(hjust = 0.5)) + labs(color="Occurence")
g1

###########################################################################
setwd("~/CMDA_Capstone/RShinyApp")
usa <- geojson_read(
  "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_500k.json",
  what = "sp")

#gets the state based on an input lat/long
#source: https://stackoverflow.com/questions/51266193/how-do-i-automatically-determine-a-state-given-the-latitude-and-longitude-coor
getState <- function(pointsDF){
  coords <- c(pointsDF$lng[1], pointsDF$lat[1])
  pnt <- SpatialPoints(matrix(coords, nrow = 1))
  proj4string(pnt) <- proj4string(usa)
  polygonCheck <- over(pnt, usa)
  return(as.character(polygonCheck$NAME))
  
}

# Read in data that provides the geographical center for each state
stateCenters <- read.csv("state_centers.csv", sep="\t")

# R Shiny Render
# ----------------------------------------------------------------------------------------------------------------------------------------

data_subitems <- state.name[state.name != "Alaska" & state.name != "Hawaii"]

source('tabItems.r', encoding = "UTF-8")
gc()
convertData <- function(userInput, dfMerged) { #user Input is a list of the inputs
  dfMergedAdjusted <- dfMerged
  dfMergedAdjusted$`Annual Mean Temp` = dfMerged$`Annual Mean Temp` + (userInput[1]*10) # data is using C * 10 for temps
  dfMergedAdjusted$`Mean Diurnal Range` = dfMerged$`Mean Diurnal Range` + (userInput[2]*10)
  dfMergedAdjusted$`Isothermality` = dfMerged$`Isothermality` + (userInput[3]*10)
  dfMergedAdjusted$`Temp Seasonality` = dfMerged$`Temp Seasonality` + (userInput[4]*10)
  dfMergedAdjusted$`Max Temp of Warmest Month` = dfMerged$`Max Temp of Warmest Month` + (userInput[5]*10)
  dfMergedAdjusted$`Min Temp of Coldest Month` = dfMerged$`Min Temp of Coldest Month` + (userInput[6]*10)
  dfMergedAdjusted$`Temp Annual Range` = dfMerged$`Temp Annual Range` + (userInput[7]*10)
  dfMergedAdjusted$`Mean Temp of Wettest Quarter` = dfMerged$`Mean Temp of Wettest Quarter` + (userInput[8]*10)
  dfMergedAdjusted$`Mean Temp of Driest Quarter` = dfMerged$`Mean Temp of Driest Quarter` + (userInput[9]*10)
  dfMergedAdjusted$`Mean Temp of Warmest Quarter` = dfMerged$`Mean Temp of Warmest Quarter` + (userInput[10]*10)
  dfMergedAdjusted$`Mean Temp of Coldest Quarter` = dfMerged$`Mean Temp of Coldest Quarter` + (userInput[11]*10)
  dfMergedAdjusted$`Annual Precipitation` = dfMerged$`Annual Precipitation` + (userInput[12]*10) # using cm from user, data is mm for precip
  dfMergedAdjusted$`Precipitation of Wettest Month` = dfMerged$`Precipitation of Wettest Month` + (userInput[13]*10)
  dfMergedAdjusted$`Precipitation of Driest Month` = dfMerged$`Precipitation of Driest Month` + (userInput[14]*10)
  dfMergedAdjusted$`Precipitation Seasonality` = dfMerged$`Precipitation Seasonality` + (userInput[15]*10)
  dfMergedAdjusted$`Precipitation of Wettest Quarter` = dfMerged$`Precipitation of Wettest Quarter` + (userInput[16]*10)
  dfMergedAdjusted$`Precipitation of Driest Quarter` = dfMerged$`Precipitation of Driest Quarter` + (userInput[17]*10)
  dfMergedAdjusted$`Precipitation of Warmest Quarter` = dfMerged$`Precipitation of Warmest Quarter` + (userInput[18]*10)
  dfMergedAdjusted$`Precipitation of Coldest Quarter` = dfMerged$`Precipitation of Coldest Quarter` + (userInput[19]*10)
  #return new plotList adjusted with user selections after model is rerun
  plotList <- runProcess(dfMergedAdjusted, modelCTBoost)
  gc()
  return(plotList)
}

############## THEME ###################
logo_onenote <- function(boldText = "Shiny", mainText = "App", badgeText = "v1.1") {
  logo <- dashboardthemes::shinyDashboardLogoDIY(
    boldText = boldText,
    mainText = mainText,
    textSize = 18,
    badgeText = badgeText,
    badgeTextColor = "purple",
    badgeTextSize = 2,
    badgeBackColor = "rgb(255,255,255)",
    badgeBorderRadius = 3
  )
  
  return(logo)
}


gc()
custom <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Crimson Text"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(255,255,255)"
  
  ### header
  ,logoBackColor = "rgb(134, 31, 65)"
  
  ,headerButtonBackColor = "rgb(134, 31, 65)"
  ,headerButtonIconColor = "rgb(255,255,255)"
  ,headerButtonBackColorHover = "rgb(134, 31, 65)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(134, 31, 65)"
  ,headerBoxShadowColor = "rgb(220,220,220)"
  ,headerBoxShadowSize = "2px 3px 2px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(241,241,241)"
    ,colorMiddle = "rgb(237,237,237)"
    ,colorEnd = "rgb(210,210,210)"
    ,colorStartPos = 0
    ,colorMiddlePos = 97
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "0px 0px 0px"
  ,sidebarShadowColor = ""
  
  ,sidebarUserTextColor = "rgb(0,0,0)"
  
  ,sidebarSearchBackColor = "rgb(255,255,255)"
  ,sidebarSearchIconColor = "rgb(134, 31, 65)"
  ,sidebarSearchBorderColor = "rgb(210,210,210)"
  
  ,sidebarTabTextColor = "rgb(0,0,0)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = ""
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(193,193,193)"
    ,colorMiddle = "rgb(216,216,216)"
    ,colorEnd = "rgb(218,218,218)"
    ,colorStartPos = 0
    ,colorMiddlePos = 5
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(134, 31, 65)"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(230,230,230)"
    ,colorMiddle = "rgb(225,225,225)"
    ,colorEnd = "rgb(210,210,210)"
    ,colorStartPos = 0
    ,colorMiddlePos = 97
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(0,0,0)"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = ""
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 0
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = 18
  ,boxDefaultColor = "rgb(225,225,225)"
  ,boxPrimaryColor = "rgb(134, 31, 65)"
  ,boxInfoColor = "rgb(235,235,235)"
  ,boxSuccessColor = "rgb(134, 31, 65)"
  ,boxWarningColor = "rgb(237,125,49)"
  ,boxDangerColor = "rgb(232,76,34)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(134, 31, 65)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgb(210,210,210)"
  ,tabBoxBorderRadius = 0
  
  ### inputs
  ,buttonBackColor = "rgb(240,240,240)"
  ,buttonTextColor = "rgb(134, 31, 65)"
  ,buttonBorderColor = "rgb(185,185,185)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(227,227,227)"
  ,buttonTextColorHover = "rgb(134, 31, 65)"
  ,buttonBorderColorHover = "rgb(210,210,210)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(210,210,210)"
  ,textboxBorderRadius = 0
  ,textboxBackColorSelect = "rgb(255,255,255)"
  ,textboxBorderColorSelect = "rgb(210,210,210)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(235,235,235)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

ui <- dashboardPage(title = "Tree of Heaven App",
                    dashboardHeader(title = span("Tree of Heaven App", style = 'font-family: Crimson Text;'),
                                    tags$li(a(href = 'https://biostat.centers.vt.edu/',
                                              target="_blank",
                                              tags$img(src = 'CBHDS_logo.jpg',
                                                       title = "CBHDS Website", height = "30px"),
                                              style = "padding-top:10px; padding-bottom:10px;"),
                                            class = "dropdown"),
                                    tags$li(a(href = 'https://www.ais.science.vt.edu/academics/cmda.html',
                                              target="_blank",
                                              tags$img(src = 'cmdaLogo.jpg',
                                                       title = "CMDA Website", height = "30px"),
                                              style = "padding-top:10px; padding-bottom:10px;"),
                                            class = "dropdown"),
                                    tags$li(a(href = 'https://treeofheavendashboard.shinyapps.io/rshinyapp/?_ga=2.264598095.1398619840.1606772610-441448454.1605921502',
                                              target="_self",
                                              icon("sync"),
                                              title = "Restart App"),
                                            class = "dropdown")),
                    # Sidebar to the left of window
                    dashboardSidebar(
                      # Menu items
                      sidebarMenu(
                        id = "tabs",
                        style="max-height: 95vh; overflow-y:auto; font-size:18px",
                        menuItem(
                          "USA Map",
                          tabName = "worldMap",
                          icon = icon("globe")
                        ),
                        uiOutput("stateSidebar"),
                        menuItem(
                          "Gain Impact",
                          tabName = "featureImportance",
                          icon = icon("table")
                        ),
                        menuItem(
                          "SHAP Impact",
                          tabName = "shapImpact",
                          icon = icon("table")
                        ),
                        menuItem(
                          "Modeling",
                          tabName = "refs",
                          icon = icon("info-circle")
                        ),
                        menuItem(
                          "About Species",
                          tabName = "aboutSpecies",
                          icon = icon("info-circle")
                        ),
                        menuItem(
                          "References",
                          tabName = "references",
                          icon = icon("info-circle")
                        ),
                        downloadButton("downloadData", label = "Download Data (.csv)"),
                        actionButton("Rerender", "Rerender Views (~15 secs)", icon("redo")),
                        # Slider inputs where the user can tweak the model
                        sliderInput("Annual Mean Temp", "Change in Annual Mean Temp (\u00B0C)",
                                    min = -10, max = 10,
                                    value = 0),
                        sliderInput("Mean Diurnal Range", "Change in Mean Diurnal Range (\u00B0C)",
                                    min = -10, max = 10,
                                    value = 0),
                        sliderInput("Isothermality", "Change in Isothermality (\u00B0C)",
                                    min = -10, max = 10,
                                    value = 0),
                        sliderInput("Temp Seasonality", "Change in Temp Seasonality (\u00B0C)",
                                    min = -10, max = 10,
                                    value = 0),
                        sliderInput("Max Temp of Warmest Month", "Change in Max Temp of Warmest Month (\u00B0C)",
                                    min = -10, max = 10,
                                    value = 0),
                        sliderInput("Min Temp of Coldest Month", "Change in Min Temp of Coldest Month (\u00B0C)",
                                    min = -10, max = 10,
                                    value = 0),
                        sliderInput("Temp Annual Range", "Change in Temp Annual Range (\u00B0C)",
                                    min = -10, max = 10,
                                    value = 0),
                        sliderInput("Mean Temp of Wettest Quarter", "Change in Mean Temp of Wettest Quarter (\u00B0C)",
                                    min = -10, max = 10,
                                    value = 0),
                        sliderInput("Mean Temp of Driest Quarter", "Change in Mean Temp of Driest Quarter (\u00B0C)",
                                    min = -10, max = 10,
                                    value = 0),
                        sliderInput("Mean Temp of Warmest Quarter", "Change in Mean Temp of Warmest Quarter (\u00B0C)",
                                    min = -10, max = 10,
                                    value = 0),
                        sliderInput("Mean Temp of Coldest Quarter", "Change in Mean Temp of Coldest Quarter (\u00B0C)",
                                    min = -10, max = 10,
                                    value = 0),
                        sliderInput("Annual Precipitation", "Change in Annual Precipitation (cm)",
                                    min = -100, max = 100,
                                    value = 0),
                        sliderInput("Precipitation of Wettest Month", "Change in Precipitation of Wettest Month (cm)",
                                    min = -100, max = 100,
                                    value = 0),
                        sliderInput("Precipitation of Driest Month", "Change in Precipitation of Driest Month (cm)",
                                    min = -100, max = 100,
                                    value = 0),
                        sliderInput("Precipitation Seasonality", "Change in Precipitation Seasonality (cm)",
                                    min = -100, max = 100,
                                    value = 0),
                        sliderInput("Precipitation of Wettest Quarter", "Change in Precipitation of Wettest Quarter (cm)",
                                    min = -100, max = 100,
                                    value = 0),
                        sliderInput("Precipitation of Driest Quarter", "Change in Precipitation of Driest Quarter (cm)",
                                    min = -100, max = 100,
                                    value = 0),
                        sliderInput("Precipitation of Warmest Quarter", "Change in Precipitation of Warmest Quarter (cm)",
                                    min = -100, max = 100,
                                    value = 0),
                        sliderInput("Precipitation of Coldest Quarter", "Change in Precipitation of Coldest Quarter (cm)",
                                    min = -100, max = 100,
                                    value = 0)
                      )
                    ),
                    dashboardBody(
                      tags$head(tags$style(
                        HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}'))),
                      
                      tags$head(tags$style(HTML(".box {margin-bottom: 0;}"))),
                      tags$head(tags$style(HTML("#downloadData {width: 100%; text-align: left; margin-left: 3px;}"))),
                      tags$head(
                        tags$style(HTML(".main-sidebar { font-size: 18px; }")),
                      ),
                      tags$head(
                        tags$style(HTML(".sidebar-menu li a { font-size: 18px !important; }"))
                      ),
                      tags$head(
                        tags$style(HTML("#downloadData { font-size: 18px !important; }"))
                      ),
                      tabItemsUI,
                      custom,
                      use_googlefont("Crimson Text")
                    )
)

server <- function(input, output, session) {
  gc()
  
  # Colors for color bar in heatmap legend
  palTemp <- brewer.pal(9, "YlOrRd")
  pal_rev <- colorNumeric(palTemp, plotList$dfMerged$predictionProb*100, reverse = T)
  
  # Heatmap for country
  output$map <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = TRUE, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap, layerId = "usa",
                       options = providerTileOptions(minZoom=5, maxZoom=5)) %>%
      setView(lat = stateCenters[stateCenters$name == "Kansas",]$lat - 1,
              lng = stateCenters[stateCenters$name == "Kansas",]$long + 0.5,
              zoom = 5) %>%
      addHeatmap(plotList$dfMerged$Longitude, plotList$dfMerged$Latitude, intensity=plotList$dfMerged$predictionProb,
                 max=32, radius=12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = FALSE
                  ))
  })
  
  # Gain impact plot
  output$importance <- renderPlot({plotList$Importance})
  
  # SHAP impact plot
  output$shap <- renderPlot({plotList$Shap})
  
  # Handle downloading data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("tree_of_heaven_predictions", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(plotList$dfMerged, file, row.names = FALSE)
    }
  )
  
  # Handle making predictions based on inputted lat long
  observeEvent(input$predButton, {
    longIn <- as.numeric(input$'longInPred')
    latIn <- as.numeric(input$'latInPred')
    top = 49.3457868 # north lat
    left = -124.7844079 # west long
    right = -66.9513812 # east long
    bottom =  24.7433195 # south lat
    if ((longIn >= right | longIn <= left) || (latIn >= top | latIn <= bottom))
    {
      output$probText <- renderText("Invalid location: Enter a coordinate within the United States")
    }
    else
    {
      showModal(modalDialog(div(style = "text-align:center; height:14px; font-size:20px;font-family:Crimson Text",
                                "Calculating prediction probability..."), footer=NULL, size="l"))
      xy = data.frame("x" = longIn, "y" = latIn)
      spd <- SpatialPoints(xy, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      bioDat = stackOpen("bioStack")
      bioDF <- extract(bioDat, spd)
      dfPred <- cbind.data.frame(coordinates(spd),bioDF)
      names(dfPred) <- c("Longitude", "Latitude", "Annual Mean Temp",
                         "Mean Diurnal Range", "Isothermality", "Temp Seasonality",
                         "Max Temp of Warmest Month", "Min Temp of Coldest Month",
                         "Temp Annual Range", "Mean Temp of Wettest Quarter", "Mean Temp of Driest Quarter",
                         "Mean Temp of Warmest Quarter", "Mean Temp of Coldest Quarter", "Annual Precipitation",
                         "Precipitation of Wettest Month", "Precipitation of Driest Month",
                         "Precipitation Seasonality", "Precipitation of Wettest Quarter",
                         "Precipitation of Driest Quarter", "Precipitation of Warmest Quarter",
                         "Precipitation of Coldest Quarter", "Altitude")
      
      userInput <- c(input$`Annual Mean Temp`, input$`Mean Diurnal Range`, input$`Isothermality`, input$`Temp Seasonality`,input$`Max Temp of Warmest Month`, input$`Min Temp of Coldest Month`, input$`Temp Annual Range`, input$`Mean Temp of Wettest Quarter`, input$`Mean Temp of Driest Quarter`, input$`Mean Temp of Warmest Quarter`, input$`Mean Temp of Coldest Quarter`, input$`Annual Precipitation`,
                     input$`Precipitation of Wettest Month`, input$`Precipitation of Driest Month`, input$`Precipitation Seasonality`, input$`Precipitation of Wettest Quarter`, input$`Precipitation of Driest Quarter`, input$`Precipitation of Warmest Quarter`, input$`Precipitation of Coldest Quarter`)
      dfPred$`Annual Mean Temp` = dfPred$`Annual Mean Temp` + (userInput[1]*10) # data is using C * 10 for temps
      dfPred$`Mean Diurnal Range` = dfPred$`Mean Diurnal Range` + (userInput[2]*10)
      dfPred$`Isothermality` = dfPred$`Isothermality` + (userInput[3]*10)
      dfPred$`Temp Seasonality` = dfPred$`Temp Seasonality` + (userInput[4]*10)
      dfPred$`Max Temp of Warmest Month` = dfPred$`Max Temp of Warmest Month` + (userInput[5]*10)
      dfPred$`Min Temp of Coldest Month` = dfPred$`Min Temp of Coldest Month` + (userInput[6]*10)
      dfPred$`Temp Annual Range` = dfPred$`Temp Annual Range` + (userInput[7]*10)
      dfPred$`Mean Temp of Wettest Quarter` = dfPred$`Mean Temp of Wettest Quarter` + (userInput[8]*10)
      dfPred$`Mean Temp of Driest Quarter` = dfPred$`Mean Temp of Driest Quarter` + (userInput[9]*10)
      dfPred$`Mean Temp of Warmest Quarter` = dfPred$`Mean Temp of Warmest Quarter` + (userInput[10]*10)
      dfPred$`Mean Temp of Coldest Quarter` = dfPred$`Mean Temp of Coldest Quarter` + (userInput[11]*10)
      dfPred$`Annual Precipitation` = dfPred$`Annual Precipitation` + (userInput[12]*10) # using cm from user, data is mm for precip
      dfPred$`Precipitation of Wettest Month` = dfPred$`Precipitation of Wettest Month` + (userInput[13]*10)
      dfPred$`Precipitation of Driest Month` = dfPred$`Precipitation of Driest Month` + (userInput[14]*10)
      dfPred$`Precipitation Seasonality` = dfPred$`Precipitation Seasonality` + (userInput[15]*10)
      dfPred$`Precipitation of Wettest Quarter` = dfPred$`Precipitation of Wettest Quarter` + (userInput[16]*10)
      dfPred$`Precipitation of Driest Quarter` = dfPred$`Precipitation of Driest Quarter` + (userInput[17]*10)
      dfPred$`Precipitation of Warmest Quarter` = dfPred$`Precipitation of Warmest Quarter` + (userInput[18]*10)
      dfPred$`Precipitation of Coldest Quarter` = dfPred$`Precipitation of Coldest Quarter` + (userInput[19]*10)
      modelCTBoost <- xgb.load("xgboost.model")
      pred_result <- predict(modelCTBoost, as.matrix(dfPred[,3:22]))
      leafletProxy('map') %>%
        clearMarkers() %>%
        addMarkers(lng=longIn, lat=latIn,
                   label = paste0(round(latIn,5),", ",round(longIn,5)))
      pred_result = round(as.numeric(pred_result), 3)
      output$probText <- renderText({paste("Probability of occurrence: ", pred_result*100, "%")})
      rm(bioDat)
      rm(bioDF)
      rm(dfPred)
      rm(userInput)
      removeModal()
    }
    gc()
  })
  
  # Handle rerendering plots when Rerender button is clicked
  observeEvent(input$Rerender, {
    showModal(modalDialog(div(style = "text-align:center;height:14px; font-size:20px; font-family:Crimson Text",
                              "Simulating new data..."), footer=NULL, size="l"))
    gc()
    userSelections <- c(input$`Annual Mean Temp`, input$`Mean Diurnal Range`, input$`Isothermality`, input$`Temp Seasonality`,input$`Max Temp of Warmest Month`, input$`Min Temp of Coldest Month`, input$`Temp Annual Range`, input$`Mean Temp of Wettest Quarter`, input$`Mean Temp of Driest Quarter`, input$`Mean Temp of Warmest Quarter`, input$`Mean Temp of Coldest Quarter`, input$`Annual Precipitation`,
                        input$`Precipitation of Wettest Month`, input$`Precipitation of Driest Month`, input$`Precipitation Seasonality`, input$`Precipitation of Wettest Quarter`, input$`Precipitation of Driest Quarter`, input$`Precipitation of Warmest Quarter`, input$`Precipitation of Coldest Quarter`)
    plotList <- convertData(userSelections, dfMerged)
    output$map <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = TRUE, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap, layerId = "usa",
                         options = providerTileOptions(minZoom=5, maxZoom=5)) %>%
        setView(lat = stateCenters[stateCenters$name == "Kansas",]$lat - 1,
                lng = stateCenters[stateCenters$name == "Kansas",]$long + 0.5,
                zoom = 5) %>%
        addHeatmap(plotList$dfMerged$Longitude, plotList$dfMerged$Latitude, intensity=plotList$dfMerged$predictionProb,
                   max=32, radius=12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = FALSE
                    ))
    })
    
    # Rerender Gain impact plot
    output$importance <- renderPlot({plotList$Importance})
    output$shap <- renderPlot({plotList$Shap})
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("tree_of_heaven_predictions", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(plotList$dfMerged, file, row.names = FALSE)
      }
    )
    dfAlabama <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "alabama",]
    dfAlabama <- na.omit(dfAlabama)
    output$alabama <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Alabama",]$lat,
                lng = stateCenters[stateCenters$name == "Alabama",]$long,
                zoom = 7) %>%
        addHeatmap(dfAlabama$Longitude,
                   dfAlabama$Latitude,
                   intensity = dfAlabama$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfArkansas <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "arkansas",]
    dfArkansas <- na.omit(dfArkansas)
    output$arkansas <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Arkansas",]$lat,
                lng = stateCenters[stateCenters$name == "Arkansas",]$long,
                zoom = 7) %>%
        addHeatmap(dfArkansas$Longitude,
                   dfArkansas$Latitude,
                   intensity = dfArkansas$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfArizona <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "arizona",]
    dfArizona <- na.omit(dfArizona)
    output$arizona <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Arizona",]$lat,
                lng = stateCenters[stateCenters$name == "Arizona",]$long - 1,
                zoom = 7) %>%
        addHeatmap(dfArizona$Longitude,
                   dfArizona$Latitude,
                   intensity = dfArizona$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfCalifornia <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "california",]
    dfCalifornia <- na.omit(dfCalifornia)
    output$california <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "California",]$lat,
                lng = stateCenters[stateCenters$name == "California",]$long - 1,
                zoom = 7) %>%
        addHeatmap(dfCalifornia$Longitude,
                   dfCalifornia$Latitude,
                   intensity = dfCalifornia$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfColorado <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "colorado",]
    dfColorado <- na.omit(dfColorado)
    output$colorado <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Colorado",]$lat,
                lng = stateCenters[stateCenters$name == "Colorado",]$long,
                zoom = 7) %>%
        addHeatmap(dfColorado$Longitude,
                   dfColorado$Latitude,
                   intensity = dfColorado$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfConnecticut <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "connecticut",]
    dfConnecticut <- na.omit(dfConnecticut)
    output$connecticut <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Connecticut",]$lat,
                lng = stateCenters[stateCenters$name == "Connecticut",]$long,
                zoom = 7) %>%
        addHeatmap(dfConnecticut$Longitude,
                   dfConnecticut$Latitude,
                   intensity = dfConnecticut$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfDelaware <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "delaware",]
    dfDelaware <- na.omit(dfDelaware)
    output$delaware <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Delaware",]$lat,
                lng = stateCenters[stateCenters$name == "Delaware",]$long,
                zoom = 7) %>%
        addHeatmap(dfDelaware$Longitude,
                   dfDelaware$Latitude,
                   intensity = dfDelaware$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfFlorida <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "florida",]
    dfFlorida <- na.omit(dfFlorida)
    output$florida <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Florida",]$lat + 0.5,
                lng = stateCenters[stateCenters$name == "Florida",]$long - 2,
                zoom = 7) %>%
        addHeatmap(dfFlorida$Longitude,
                   dfFlorida$Latitude,
                   intensity = dfFlorida$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfGeorgia <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "georgia",]
    dfGeorgia <- na.omit(dfGeorgia)
    output$georgia <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Georgia",]$lat,
                lng = stateCenters[stateCenters$name == "Georgia",]$long,
                zoom = 7) %>%
        addHeatmap(dfGeorgia$Longitude,
                   dfGeorgia$Latitude,
                   intensity = dfGeorgia$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfIowa <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "iowa",]
    dfIowa <- na.omit(dfIowa)
    output$iowa <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Iowa",]$lat,
                lng = stateCenters[stateCenters$name == "Iowa",]$long - 0.5,
                zoom = 7) %>%
        addHeatmap(dfIowa$Longitude,
                   dfIowa$Latitude,
                   intensity = dfIowa$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfIdaho <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "idaho",]
    dfIdaho <- na.omit(dfIdaho)
    output$idaho <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Idaho",]$lat + 1,
                lng = stateCenters[stateCenters$name == "Idaho",]$long,
                zoom = 7) %>%
        addHeatmap(dfIdaho$Longitude,
                   dfIdaho$Latitude,
                   intensity = dfIdaho$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfIllinois <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "illinois",]
    dfIllinois <- na.omit(dfIllinois)
    output$illinois <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Illinois",]$lat - 0.75,
                lng = stateCenters[stateCenters$name == "Illinois",]$long,
                zoom = 7) %>%
        addHeatmap(dfIllinois$Longitude,
                   dfIllinois$Latitude,
                   intensity = dfIllinois$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfIndiana <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "indiana",]
    dfIndiana <- na.omit(dfIndiana)
    output$indiana <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Indiana",]$lat - 0.5,
                lng = stateCenters[stateCenters$name == "Indiana",]$long - 0.5,
                zoom = 7) %>%
        addHeatmap(dfIndiana$Longitude,
                   dfIndiana$Latitude,
                   intensity = dfIndiana$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfKansas <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "kansas",]
    dfKansas <- na.omit(dfKansas)
    output$kansas <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Kansas",]$lat,
                lng = stateCenters[stateCenters$name == "Kansas",]$long,
                zoom = 7) %>%
        addHeatmap(dfKansas$Longitude,
                   dfKansas$Latitude,
                   intensity = dfKansas$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfKentucky <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "kentucky",]
    dfKentucky <- na.omit(dfKentucky)
    output$kentucky <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Kentucky",]$lat,
                lng = stateCenters[stateCenters$name == "Kentucky",]$long - 1.5,
                zoom = 7) %>%
        addHeatmap(dfKentucky$Longitude,
                   dfKentucky$Latitude,
                   intensity = dfKentucky$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfLouisiana <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "louisiana",]
    dfLouisiana <- na.omit(dfLouisiana)
    output$louisiana <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Louisiana",]$lat,
                lng = stateCenters[stateCenters$name == "Louisiana",]$long,
                zoom = 7) %>%
        addHeatmap(dfLouisiana$Longitude,
                   dfLouisiana$Latitude,
                   intensity = dfLouisiana$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfMaine <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "maine",]
    dfMaine <- na.omit(dfMaine)
    output$maine <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Maine",]$lat,
                lng = stateCenters[stateCenters$name == "Maine",]$long,
                zoom = 7) %>%
        addHeatmap(dfMaine$Longitude,
                   dfMaine$Latitude,
                   intensity = dfMaine$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfMaryland <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "maryland",]
    dfMaryland <- na.omit(dfMaryland)
    output$maryland <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Maryland",]$lat,
                lng = stateCenters[stateCenters$name == "Maryland",]$long,
                zoom = 7.0) %>%
        addHeatmap(dfMaryland$Longitude,
                   dfMaryland$Latitude,
                   intensity = dfMaryland$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfMassachusetts <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "massachusetts",]
    dfMassachusetts <- na.omit(dfMassachusetts)
    output$massachusetts <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Massachusetts",]$lat,
                lng = stateCenters[stateCenters$name == "Massachusetts",]$long,
                zoom = 7.0) %>%
        addHeatmap(dfMassachusetts$Longitude,
                   dfMassachusetts$Latitude,
                   intensity = dfMassachusetts$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfMichigan <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "michigan",]
    dfMichigan <- na.omit(dfMichigan)
    output$michigan <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Michigan",]$lat-1,
                lng = stateCenters[stateCenters$name == "Michigan",]$long,
                zoom = 7.0) %>%
        addHeatmap(dfMichigan$Longitude,
                   dfMichigan$Latitude,
                   intensity = dfMichigan$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfMinnesota <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "minnesota",]
    dfMinnesota <- na.omit(dfMinnesota)
    output$minnesota <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Minnesota",]$lat-0.5,
                lng = stateCenters[stateCenters$name == "Minnesota",]$long,
                zoom = 7.0) %>%
        addHeatmap(dfMinnesota$Longitude,
                   dfMinnesota$Latitude,
                   intensity = dfMinnesota$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfMississippi <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "mississippi",]
    dfMississippi <- na.omit(dfMississippi)
    output$mississippi <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Mississippi",]$lat,
                lng = stateCenters[stateCenters$name == "Mississippi",]$long,
                zoom = 7.0) %>%
        addHeatmap(dfMississippi$Longitude,
                   dfMississippi$Latitude,
                   intensity = dfMississippi$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfMissouri <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "missouri",]
    dfMissouri <- na.omit(dfMissouri)
    output$missouri <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Missouri",]$lat+0.5,
                lng = stateCenters[stateCenters$name == "Missouri",]$long,
                zoom = 7.0) %>%
        addHeatmap(dfMissouri$Longitude,
                   dfMissouri$Latitude,
                   intensity = dfMissouri$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfMontana <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "montana",]
    dfMontana <- na.omit(dfMontana)
    output$montana <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Montana",]$lat,
                lng = stateCenters[stateCenters$name == "Montana",]$long,
                zoom = 7.0) %>%
        addHeatmap(dfMontana$Longitude,
                   dfMontana$Latitude,
                   intensity = dfMontana$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfNebraska <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "nebraska",]
    dfNebraska <- na.omit(dfNebraska)
    output$nebraska <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Nebraska",]$lat,
                lng = stateCenters[stateCenters$name == "Nebraska",]$long,
                zoom = 7.0) %>%
        addHeatmap(dfNebraska$Longitude,
                   dfNebraska$Latitude,
                   intensity = dfNebraska$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfNevada <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "nevada",]
    dfNevada <- na.omit(dfNevada)
    output$nevada <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Nevada",]$lat,
                lng = stateCenters[stateCenters$name == "Nevada",]$long,
                zoom = 7.0) %>%
        addHeatmap(dfNevada$Longitude,
                   dfNevada$Latitude,
                   intensity = dfNevada$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfNewHampshire <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "new_hampshire",]
    dfNewHampshire <- na.omit(dfNewHampshire)
    output$new_hampshire <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "New Hampshire",]$lat,
                lng = stateCenters[stateCenters$name == "New Hampshire",]$long,
                zoom = 7.0) %>%
        addHeatmap(dfNewHampshire$Longitude,
                   dfNewHampshire$Latitude,
                   intensity = dfNewHampshire$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfNewJersey <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "new_jersey",]
    dfNewJersey <- na.omit(dfNewJersey)
    output$new_jersey <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "New Jersey",]$lat,
                lng = stateCenters[stateCenters$name == "New Jersey",]$long,
                zoom = 7.0) %>%
        addHeatmap(dfNewJersey$Longitude,
                   dfNewJersey$Latitude,
                   intensity = dfNewJersey$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    dfNewMexico <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "new_mexico",]
    dfNewMexico <- na.omit(dfNewMexico)
    output$new_mexico <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "New Mexico",]$lat-0.5,
                lng = stateCenters[stateCenters$name == "New Mexico",]$long-0.5,
                zoom = 7.0) %>%
        addHeatmap(dfNewMexico$Longitude,
                   dfNewMexico$Latitude,
                   intensity = dfNewMexico$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    
    dfNewYork <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "new_york",]
    dfNewYork <- na.omit(dfNewYork)
    output$new_york <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "New York",]$lat-0.3,
                lng = stateCenters[stateCenters$name == "New York",]$long-0.55,
                zoom = 7.0) %>%
        addHeatmap(dfNewYork$Longitude,
                   dfNewYork$Latitude,
                   intensity = dfNewYork$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    
    
    dfNorthCarolina <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "north_carolina",]
    dfNorthCarolina <- na.omit(dfNorthCarolina)
    output$north_carolina <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "North Carolina",]$lat,
                lng = stateCenters[stateCenters$name == "North Carolina",]$long,
                zoom = 7.0) %>%
        addHeatmap(dfNorthCarolina$Longitude,
                   dfNorthCarolina$Latitude,
                   intensity = dfNorthCarolina$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    # North Dakota Plot
    dfNorthDakota <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "north_dakota",]
    dfNorthDakota <- na.omit(dfNorthDakota)
    output$north_dakota <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "North Dakota",]$lat,
                lng = stateCenters[stateCenters$name == "North Dakota",]$long,
                zoom = 7.0) %>%
        addHeatmap(dfNorthDakota$Longitude,
                   dfNorthDakota$Latitude,
                   intensity = dfNorthDakota$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    # Ohio Plot
    dfOhio <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "ohio",]
    dfOhio <- na.omit(dfOhio)
    output$ohio <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Ohio",]$lat,
                lng = stateCenters[stateCenters$name == "Ohio",]$long,
                zoom = 7) %>%
        addHeatmap(dfOhio$Longitude,
                   dfOhio$Latitude,
                   intensity = dfOhio$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    # Oklahoma Plot
    dfOklahoma <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "oklahoma",]
    dfOklahoma <- na.omit(dfOklahoma)
    output$oklahoma <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Oklahoma",]$lat,
                lng = stateCenters[stateCenters$name == "Oklahoma",]$long - 1.2,
                zoom = 7) %>%
        addHeatmap(dfOklahoma$Longitude,
                   dfOklahoma$Latitude,
                   intensity = dfOklahoma$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    # Oregon Plot
    dfOregon <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "oregon",]
    dfOregon <- na.omit(dfOregon)
    output$oregon <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Oregon",]$lat + 0.3,
                lng = stateCenters[stateCenters$name == "Oregon",]$long,
                zoom = 7) %>%
        addHeatmap(dfOregon$Longitude,
                   dfOregon$Latitude,
                   intensity = dfOregon$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence", position = "bottomright",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    # Pennsylvania Plot
    dfPenn <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "pennsylvania",]
    dfPenn <- na.omit(dfPenn)
    output$pennsylvania <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Pennsylvania",]$lat - 0.3,
                lng = stateCenters[stateCenters$name == "Pennsylvania",]$long - 0.8,
                zoom = 7) %>%
        addHeatmap(dfPenn$Longitude,
                   dfPenn$Latitude,
                   intensity = dfPenn$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    # Rhode Island Plot
    dfRI <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "rhode_island",]
    dfRI <- na.omit(dfRI)
    output$rhode_island <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$code == "RI",]$lat,
                lng = stateCenters[stateCenters$code == "RI",]$long,
                zoom = 7) %>%
        addHeatmap(dfRI$Longitude,
                   dfRI$Latitude,
                   intensity = dfRI$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    # South Carolina Plot
    dfSC <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "south_carolina",]
    dfSC <- na.omit(dfSC)
    output$south_carolina <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$code == "SC",]$lat,
                lng = stateCenters[stateCenters$code == "SC",]$long,
                zoom = 7) %>%
        addHeatmap(dfSC$Longitude,
                   dfSC$Latitude,
                   intensity = dfSC$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    # South Dakota Plot
    dfSD <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "south_dakota",]
    dfSD <- na.omit(dfSD)
    output$south_dakota <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$code == "SD",]$lat + 0.4,
                lng = stateCenters[stateCenters$code == "SD",]$long - 0.4,
                zoom = 7) %>%
        addHeatmap(dfSD$Longitude,
                   dfSD$Latitude,
                   intensity = dfSD$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    # Tennessee Plot
    dfTN <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "tennessee",]
    dfTN <- na.omit(dfTN)
    output$tennessee <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$code == "TN",]$lat + 0.1,
                lng = stateCenters[stateCenters$code == "TN",]$long + 0.3,
                zoom = 7) %>%
        addHeatmap(dfTN$Longitude,
                   dfTN$Latitude,
                   intensity = dfTN$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    # Texas Plot
    dfTexas <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "texas",]
    dfTexas <- na.omit(dfTexas)
    output$texas <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=6, maxZoom=6)) %>%
        setView(lat = stateCenters[stateCenters$name == "Texas",]$lat - 0.5,
                lng = stateCenters[stateCenters$name == "Texas",]$long,
                zoom = 6) %>%
        addHeatmap(dfTexas$Longitude,
                   dfTexas$Latitude,
                   intensity = dfTexas$predictionProb,
                   max = 5, radius = 10, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    # Utah Plot
    dfUtah <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "utah",]
    dfUtah <- na.omit(dfUtah)
    output$utah <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Utah",]$lat + 0.22,
                lng = stateCenters[stateCenters$name == "Utah",]$long - 0.6,
                zoom = 7) %>%
        addHeatmap(dfUtah$Longitude,
                   dfUtah$Latitude,
                   intensity = dfUtah$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    # Vermont Plot
    dfVermont <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "vermont",]
    dfVermont <- na.omit(dfVermont)
    output$vermont <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Vermont",]$lat - 1,
                lng = stateCenters[stateCenters$name == "Vermont",]$long - 0.2,
                zoom = 7) %>%
        addHeatmap(dfVermont$Longitude,
                   dfVermont$Latitude,
                   intensity = dfVermont$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    # Virginia Plot
    dfVirginia <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "virginia",]
    dfVirginia <- na.omit(dfVirginia)
    output$virginia <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Virginia",]$lat + 0.2,
                lng = stateCenters[stateCenters$name == "Virginia",]$long - 1.1,
                zoom = 7) %>%
        addHeatmap(dfVirginia$Longitude,
                   dfVirginia$Latitude,
                   intensity = dfVirginia$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    # Washington Plot
    dfWashington <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "washington",]
    dfWashington <- na.omit(dfWashington)
    output$washington <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Washington",]$lat - 0.3,
                lng = stateCenters[stateCenters$name == "Washington",]$long,
                zoom = 7) %>%
        addHeatmap(dfWashington$Longitude,
                   dfWashington$Latitude,
                   intensity = dfWashington$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    # Wisconsin Plot
    dfWisconsin <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "wisconsin",]
    dfWisconsin <- na.omit(dfWisconsin)
    output$wisconsin <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Wisconsin",]$lat + 0.8,
                lng = stateCenters[stateCenters$name == "Wisconsin",]$long - 0.8,
                zoom = 7) %>%
        addHeatmap(dfWisconsin$Longitude,
                   dfWisconsin$Latitude,
                   intensity = dfWisconsin$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    # West Virginia Plot
    dfWV <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "west_virginia",]
    dfWV <- na.omit(dfWV)
    output$west_virginia <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$code == "WV",]$lat,
                lng = stateCenters[stateCenters$code == "WV",]$long,
                zoom = 7) %>%
        addHeatmap(dfWV$Longitude,
                   dfWV$Latitude,
                   intensity = dfWV$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    
    # Wyoming Plot
    dfWyoming <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "wyoming",]
    dfWyoming <- na.omit(dfWyoming)
    output$wyoming <- renderLeaflet({
      leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
        setView(lat = stateCenters[stateCenters$name == "Wyoming",]$lat,
                lng = stateCenters[stateCenters$name == "Wyoming",]$long - 0.2,
                zoom = 7) %>%
        addHeatmap(dfWyoming$Longitude,
                   dfWyoming$Latitude,
                   intensity = dfWyoming$predictionProb,
                   max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
        addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                  title = "Probability of Occurence %", position = "bottomleft",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
        addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#000000",
                      bringToFront = F
                    ))
    })
    removeModal()
  })
  
  gc()
  
  # The submenus for the states
  subitems <- reactiveVal(value = data_subitems)
  output$stateSidebar <- renderUI({
    sidebarMenu(id="states", menuItem("State Maps", tabName = "stateMaps",  icon = icon("map"),
                                      startExpanded = F,
                                      style="max-height: 30vh; overflow-y:auto",
                                      lapply(subitems(), function(x) {
                                        menuSubItem(x, tabName = sub(" ", "_", tolower(x)) )} ))
    )
  })
  
  dfAlabama <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "alabama",]
  dfAlabama <- na.omit(dfAlabama)
  output$alabama <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Alabama",]$lat,
              lng = stateCenters[stateCenters$name == "Alabama",]$long,
              zoom = 7) %>%
      addHeatmap(dfAlabama$Longitude,
                 dfAlabama$Latitude,
                 intensity = dfAlabama$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfArkansas <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "arkansas",]
  dfArkansas <- na.omit(dfArkansas)
  output$arkansas <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Arkansas",]$lat,
              lng = stateCenters[stateCenters$name == "Arkansas",]$long,
              zoom = 7) %>%
      addHeatmap(dfArkansas$Longitude,
                 dfArkansas$Latitude,
                 intensity = dfArkansas$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfArizona <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "arizona",]
  dfArizona <- na.omit(dfArizona)
  output$arizona <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Arizona",]$lat,
              lng = stateCenters[stateCenters$name == "Arizona",]$long - 1,
              zoom = 7) %>%
      addHeatmap(dfArizona$Longitude,
                 dfArizona$Latitude,
                 intensity = dfArizona$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfCalifornia <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "california",]
  dfCalifornia <- na.omit(dfCalifornia)
  output$california <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "California",]$lat,
              lng = stateCenters[stateCenters$name == "California",]$long - 1,
              zoom = 7) %>%
      addHeatmap(dfCalifornia$Longitude,
                 dfCalifornia$Latitude,
                 intensity = dfCalifornia$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfColorado <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "colorado",]
  dfColorado <- na.omit(dfColorado)
  output$colorado <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Colorado",]$lat,
              lng = stateCenters[stateCenters$name == "Colorado",]$long,
              zoom = 7) %>%
      addHeatmap(dfColorado$Longitude,
                 dfColorado$Latitude,
                 intensity = dfColorado$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfConnecticut <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "connecticut",]
  dfConnecticut <- na.omit(dfConnecticut)
  output$connecticut <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Connecticut",]$lat,
              lng = stateCenters[stateCenters$name == "Connecticut",]$long,
              zoom = 7) %>%
      addHeatmap(dfConnecticut$Longitude,
                 dfConnecticut$Latitude,
                 intensity = dfConnecticut$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfDelaware <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "delaware",]
  dfDelaware <- na.omit(dfDelaware)
  output$delaware <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Delaware",]$lat,
              lng = stateCenters[stateCenters$name == "Delaware",]$long,
              zoom = 7) %>%
      addHeatmap(dfDelaware$Longitude,
                 dfDelaware$Latitude,
                 intensity = dfDelaware$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfFlorida <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "florida",]
  dfFlorida <- na.omit(dfFlorida)
  output$florida <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Florida",]$lat + 0.5,
              lng = stateCenters[stateCenters$name == "Florida",]$long - 2,
              zoom = 7) %>%
      addHeatmap(dfFlorida$Longitude,
                 dfFlorida$Latitude,
                 intensity = dfFlorida$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfGeorgia <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "georgia",]
  dfGeorgia <- na.omit(dfGeorgia)
  output$georgia <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Georgia",]$lat,
              lng = stateCenters[stateCenters$name == "Georgia",]$long,
              zoom = 7) %>%
      addHeatmap(dfGeorgia$Longitude,
                 dfGeorgia$Latitude,
                 intensity = dfGeorgia$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfIowa <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "iowa",]
  dfIowa <- na.omit(dfIowa)
  output$iowa <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Iowa",]$lat,
              lng = stateCenters[stateCenters$name == "Iowa",]$long - 0.5,
              zoom = 7) %>%
      addHeatmap(dfIowa$Longitude,
                 dfIowa$Latitude,
                 intensity = dfIowa$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfIdaho <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "idaho",]
  dfIdaho <- na.omit(dfIdaho)
  output$idaho <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Idaho",]$lat + 1,
              lng = stateCenters[stateCenters$name == "Idaho",]$long,
              zoom = 7) %>%
      addHeatmap(dfIdaho$Longitude,
                 dfIdaho$Latitude,
                 intensity = dfIdaho$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfIllinois <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "illinois",]
  dfIllinois <- na.omit(dfIllinois)
  output$illinois <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Illinois",]$lat - 0.75,
              lng = stateCenters[stateCenters$name == "Illinois",]$long,
              zoom = 7) %>%
      addHeatmap(dfIllinois$Longitude,
                 dfIllinois$Latitude,
                 intensity = dfIllinois$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfIndiana <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "indiana",]
  dfIndiana <- na.omit(dfIndiana)
  output$indiana <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Indiana",]$lat - 0.5,
              lng = stateCenters[stateCenters$name == "Indiana",]$long - 0.5,
              zoom = 7) %>%
      addHeatmap(dfIndiana$Longitude,
                 dfIndiana$Latitude,
                 intensity = dfIndiana$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfKansas <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "kansas",]
  dfKansas <- na.omit(dfKansas)
  output$kansas <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Kansas",]$lat,
              lng = stateCenters[stateCenters$name == "Kansas",]$long,
              zoom = 7) %>%
      addHeatmap(dfKansas$Longitude,
                 dfKansas$Latitude,
                 intensity = dfKansas$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfKentucky <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "kentucky",]
  dfKentucky <- na.omit(dfKentucky)
  output$kentucky <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Kentucky",]$lat,
              lng = stateCenters[stateCenters$name == "Kentucky",]$long - 1.5,
              zoom = 7) %>%
      addHeatmap(dfKentucky$Longitude,
                 dfKentucky$Latitude,
                 intensity = dfKentucky$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfLouisiana <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "louisiana",]
  dfLouisiana <- na.omit(dfLouisiana)
  output$louisiana <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Louisiana",]$lat,
              lng = stateCenters[stateCenters$name == "Louisiana",]$long,
              zoom = 7) %>%
      addHeatmap(dfLouisiana$Longitude,
                 dfLouisiana$Latitude,
                 intensity = dfLouisiana$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfMaine <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "maine",]
  dfMaine <- na.omit(dfMaine)
  output$maine <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Maine",]$lat,
              lng = stateCenters[stateCenters$name == "Maine",]$long,
              zoom = 7) %>%
      addHeatmap(dfMaine$Longitude,
                 dfMaine$Latitude,
                 intensity = dfMaine$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfMaryland <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "maryland",]
  dfMaryland <- na.omit(dfMaryland)
  output$maryland <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Maryland",]$lat,
              lng = stateCenters[stateCenters$name == "Maryland",]$long,
              zoom = 7.0) %>%
      addHeatmap(dfMaryland$Longitude,
                 dfMaryland$Latitude,
                 intensity = dfMaryland$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfMassachusetts <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "massachusetts",]
  dfMassachusetts <- na.omit(dfMassachusetts)
  output$massachusetts <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Massachusetts",]$lat,
              lng = stateCenters[stateCenters$name == "Massachusetts",]$long,
              zoom = 7.0) %>%
      addHeatmap(dfMassachusetts$Longitude,
                 dfMassachusetts$Latitude,
                 intensity = dfMassachusetts$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfMichigan <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "michigan",]
  dfMichigan <- na.omit(dfMichigan)
  output$michigan <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Michigan",]$lat-1,
              lng = stateCenters[stateCenters$name == "Michigan",]$long,
              zoom = 7.0) %>%
      addHeatmap(dfMichigan$Longitude,
                 dfMichigan$Latitude,
                 intensity = dfMichigan$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfMinnesota <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "minnesota",]
  dfMinnesota <- na.omit(dfMinnesota)
  output$minnesota <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Minnesota",]$lat-0.5,
              lng = stateCenters[stateCenters$name == "Minnesota",]$long,
              zoom = 7.0) %>%
      addHeatmap(dfMinnesota$Longitude,
                 dfMinnesota$Latitude,
                 intensity = dfMinnesota$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfMississippi <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "mississippi",]
  dfMississippi <- na.omit(dfMississippi)
  output$mississippi <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Mississippi",]$lat,
              lng = stateCenters[stateCenters$name == "Mississippi",]$long,
              zoom = 7.0) %>%
      addHeatmap(dfMississippi$Longitude,
                 dfMississippi$Latitude,
                 intensity = dfMississippi$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfMissouri <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "missouri",]
  dfMissouri <- na.omit(dfMissouri)
  output$missouri <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Missouri",]$lat+0.5,
              lng = stateCenters[stateCenters$name == "Missouri",]$long,
              zoom = 7.0) %>%
      addHeatmap(dfMissouri$Longitude,
                 dfMissouri$Latitude,
                 intensity = dfMissouri$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfMontana <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "montana",]
  dfMontana <- na.omit(dfMontana)
  output$montana <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Montana",]$lat,
              lng = stateCenters[stateCenters$name == "Montana",]$long,
              zoom = 7.0) %>%
      addHeatmap(dfMontana$Longitude,
                 dfMontana$Latitude,
                 intensity = dfMontana$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfNebraska <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "nebraska",]
  dfNebraska <- na.omit(dfNebraska)
  output$nebraska <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Nebraska",]$lat,
              lng = stateCenters[stateCenters$name == "Nebraska",]$long,
              zoom = 7.0) %>%
      addHeatmap(dfNebraska$Longitude,
                 dfNebraska$Latitude,
                 intensity = dfNebraska$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfNevada <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "nevada",]
  dfNevada <- na.omit(dfNevada)
  output$nevada <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Nevada",]$lat,
              lng = stateCenters[stateCenters$name == "Nevada",]$long,
              zoom = 7.0) %>%
      addHeatmap(dfNevada$Longitude,
                 dfNevada$Latitude,
                 intensity = dfNevada$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft", labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfNewHampshire <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "new_hampshire",]
  dfNewHampshire <- na.omit(dfNewHampshire)
  output$new_hampshire <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "New Hampshire",]$lat,
              lng = stateCenters[stateCenters$name == "New Hampshire",]$long,
              zoom = 7.0) %>%
      addHeatmap(dfNewHampshire$Longitude,
                 dfNewHampshire$Latitude,
                 intensity = dfNewHampshire$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfNewJersey <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "new_jersey",]
  dfNewJersey <- na.omit(dfNewJersey)
  output$new_jersey <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "New Jersey",]$lat,
              lng = stateCenters[stateCenters$name == "New Jersey",]$long,
              zoom = 7.0) %>%
      addHeatmap(dfNewJersey$Longitude,
                 dfNewJersey$Latitude,
                 intensity = dfNewJersey$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  dfNewMexico <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "new_mexico",]
  dfNewMexico <- na.omit(dfNewMexico)
  output$new_mexico <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "New Mexico",]$lat-0.5,
              lng = stateCenters[stateCenters$name == "New Mexico",]$long-0.5,
              zoom = 7.0) %>%
      addHeatmap(dfNewMexico$Longitude,
                 dfNewMexico$Latitude,
                 intensity = dfNewMexico$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  
  dfNewYork <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "new_york",]
  dfNewYork <- na.omit(dfNewYork)
  output$new_york <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "New York",]$lat-0.3,
              lng = stateCenters[stateCenters$name == "New York",]$long-0.55,
              zoom = 7.0) %>%
      addHeatmap(dfNewYork$Longitude,
                 dfNewYork$Latitude,
                 intensity = dfNewYork$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  
  
  dfNorthCarolina <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "north_carolina",]
  dfNorthCarolina <- na.omit(dfNorthCarolina)
  output$north_carolina <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "North Carolina",]$lat,
              lng = stateCenters[stateCenters$name == "North Carolina",]$long,
              zoom = 7.0) %>%
      addHeatmap(dfNorthCarolina$Longitude,
                 dfNorthCarolina$Latitude,
                 intensity = dfNorthCarolina$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  # North Dakota Plot
  dfNorthDakota <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "north_dakota",]
  dfNorthDakota <- na.omit(dfNorthDakota)
  output$north_dakota <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "North Dakota",]$lat,
              lng = stateCenters[stateCenters$name == "North Dakota",]$long,
              zoom = 7.0) %>%
      addHeatmap(dfNorthDakota$Longitude,
                 dfNorthDakota$Latitude,
                 intensity = dfNorthDakota$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  # Ohio Plot
  dfOhio <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "ohio",]
  dfOhio <- na.omit(dfOhio)
  output$ohio <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Ohio",]$lat,
              lng = stateCenters[stateCenters$name == "Ohio",]$long,
              zoom = 7) %>%
      addHeatmap(dfOhio$Longitude,
                 dfOhio$Latitude,
                 intensity = dfOhio$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  # Oklahoma Plot
  dfOklahoma <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "oklahoma",]
  dfOklahoma <- na.omit(dfOklahoma)
  output$oklahoma <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Oklahoma",]$lat,
              lng = stateCenters[stateCenters$name == "Oklahoma",]$long - 1.2,
              zoom = 7) %>%
      addHeatmap(dfOklahoma$Longitude,
                 dfOklahoma$Latitude,
                 intensity = dfOklahoma$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  # Oregon Plot
  dfOregon <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "oregon",]
  dfOregon <- na.omit(dfOregon)
  output$oregon <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Oregon",]$lat + 0.3,
              lng = stateCenters[stateCenters$name == "Oregon",]$long,
              zoom = 7) %>%
      addHeatmap(dfOregon$Longitude,
                 dfOregon$Latitude,
                 intensity = dfOregon$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence", position = "bottomright",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  # Pennsylvania Plot
  dfPenn <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "pennsylvania",]
  dfPenn <- na.omit(dfPenn)
  output$pennsylvania <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Pennsylvania",]$lat - 0.3,
              lng = stateCenters[stateCenters$name == "Pennsylvania",]$long - 0.8,
              zoom = 7) %>%
      addHeatmap(dfPenn$Longitude,
                 dfPenn$Latitude,
                 intensity = dfPenn$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  # Rhode Island Plot
  dfRI <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "rhode_island",]
  dfRI <- na.omit(dfRI)
  output$rhode_island <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$code == "RI",]$lat,
              lng = stateCenters[stateCenters$code == "RI",]$long,
              zoom = 7) %>%
      addHeatmap(dfRI$Longitude,
                 dfRI$Latitude,
                 intensity = dfRI$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  # South Carolina Plot
  dfSC <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "south_carolina",]
  dfSC <- na.omit(dfSC)
  output$south_carolina <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$code == "SC",]$lat,
              lng = stateCenters[stateCenters$code == "SC",]$long,
              zoom = 7) %>%
      addHeatmap(dfSC$Longitude,
                 dfSC$Latitude,
                 intensity = dfSC$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  # South Dakota Plot
  dfSD <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "south_dakota",]
  dfSD <- na.omit(dfSD)
  output$south_dakota <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$code == "SD",]$lat + 0.4,
              lng = stateCenters[stateCenters$code == "SD",]$long - 0.4,
              zoom = 7) %>%
      addHeatmap(dfSD$Longitude,
                 dfSD$Latitude,
                 intensity = dfSD$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  # Tennessee Plot
  dfTN <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "tennessee",]
  dfTN <- na.omit(dfTN)
  output$tennessee <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$code == "TN",]$lat + 0.1,
              lng = stateCenters[stateCenters$code == "TN",]$long + 0.3,
              zoom = 7) %>%
      addHeatmap(dfTN$Longitude,
                 dfTN$Latitude,
                 intensity = dfTN$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  # Texas Plot
  dfTexas <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "texas",]
  dfTexas <- na.omit(dfTexas)
  output$texas <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=6, maxZoom=6)) %>%
      setView(lat = stateCenters[stateCenters$name == "Texas",]$lat - 0.5,
              lng = stateCenters[stateCenters$name == "Texas",]$long,
              zoom = 6) %>%
      addHeatmap(dfTexas$Longitude,
                 dfTexas$Latitude,
                 intensity = dfTexas$predictionProb,
                 max = 5, radius = 10, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  # Utah Plot
  dfUtah <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "utah",]
  dfUtah <- na.omit(dfUtah)
  output$utah <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Utah",]$lat + 0.22,
              lng = stateCenters[stateCenters$name == "Utah",]$long - 0.6,
              zoom = 7) %>%
      addHeatmap(dfUtah$Longitude,
                 dfUtah$Latitude,
                 intensity = dfUtah$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  # Vermont Plot
  dfVermont <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "vermont",]
  dfVermont <- na.omit(dfVermont)
  output$vermont <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Vermont",]$lat - 1,
              lng = stateCenters[stateCenters$name == "Vermont",]$long - 0.2,
              zoom = 7) %>%
      addHeatmap(dfVermont$Longitude,
                 dfVermont$Latitude,
                 intensity = dfVermont$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  # Virginia Plot
  dfVirginia <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "virginia",]
  dfVirginia <- na.omit(dfVirginia)
  output$virginia <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Virginia",]$lat + 0.2,
              lng = stateCenters[stateCenters$name == "Virginia",]$long - 1.1,
              zoom = 7) %>%
      addHeatmap(dfVirginia$Longitude,
                 dfVirginia$Latitude,
                 intensity = dfVirginia$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  # Washington Plot
  dfWashington <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "washington",]
  dfWashington <- na.omit(dfWashington)
  output$washington <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Washington",]$lat - 0.3,
              lng = stateCenters[stateCenters$name == "Washington",]$long,
              zoom = 7) %>%
      addHeatmap(dfWashington$Longitude,
                 dfWashington$Latitude,
                 intensity = dfWashington$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  # Wisconsin Plot
  dfWisconsin <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "wisconsin",]
  dfWisconsin <- na.omit(dfWisconsin)
  output$wisconsin <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Wisconsin",]$lat + 0.8,
              lng = stateCenters[stateCenters$name == "Wisconsin",]$long - 0.8,
              zoom = 7) %>%
      addHeatmap(dfWisconsin$Longitude,
                 dfWisconsin$Latitude,
                 intensity = dfWisconsin$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  # West Virginia Plot
  dfWV <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "west_virginia",]
  dfWV <- na.omit(dfWV)
  output$west_virginia <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$code == "WV",]$lat,
              lng = stateCenters[stateCenters$code == "WV",]$long,
              zoom = 7) %>%
      addHeatmap(dfWV$Longitude,
                 dfWV$Latitude,
                 intensity = dfWV$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  # Wyoming Plot
  dfWyoming <- plotList$dfMerged[sub(" ", "_", tolower(plotList$dfMerged$State)) == "wyoming",]
  dfWyoming <- na.omit(dfWyoming)
  output$wyoming <- renderLeaflet({
    leaflet(states, width = 800, height = 800, options = leafletOptions(preferCanvas = T, zoomControl = F)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom=7, maxZoom=7)) %>%
      setView(lat = stateCenters[stateCenters$name == "Wyoming",]$lat,
              lng = stateCenters[stateCenters$name == "Wyoming",]$long - 0.2,
              zoom = 7) %>%
      addHeatmap(dfWyoming$Longitude,
                 dfWyoming$Latitude,
                 intensity = dfWyoming$predictionProb,
                 max = 2.5, radius = 12, blur=0, gradient = palTemp) %>%
      addLegend(pal = pal_rev, values = plotList$dfMerged$predictionProb*100,
                title = "Probability of Occurence %", position = "bottomleft",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addPolygons(data = states, fillOpacity = 0, weight = 1, color = "#8c8c8c",
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#000000",
                    bringToFront = F
                  ))
  })
  
  
  observeEvent(input$map_shape_click, {
    gc()
    click <- input$map_shape_click
    if (is.null(click)) {
      return()
    }
    
    dfClick <- data.frame("lat" = c(click$lat), "lng" = c(click$lng))
    
    selectedState <- tolower(getState(dfClick))
    selectedState <- tolower(sub(" ", "_", selectedState))
    
    updateTabItems(session, "tabs", selectedState)
    
    
  })
  
}

shinyApp(ui, server)
