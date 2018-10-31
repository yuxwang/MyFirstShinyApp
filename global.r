library(shiny)
if (!require("tidyverse")){
  install.packages("tidyverse")
  library("tidyverse")
}

if (!require("usmap")){
  install.packages("usmap")
  library("usmap")
}

if (!require("shinythemes")){
  install.packages("shinythemes")
  library("shinythemes")
}

if (!require("DT")){
  install.packages("DT")
  library("DT")
}

if (!require("Cairo")){
  install.packages("Cairo")
  library("Cairo")
}
##global variables
#read csv data online
#https://catalog.data.gov/dataset/drug-poisoning-mortality-by-state-united-states
drugMtly<-read_csv(file="https://data.cdc.gov/api/views/xbxb-epbu/rows.csv?accessType=DOWNLOAD")%>% tbl_df() %>% mutate_at(6:19,as.numeric)

#subdivide data
#US data
USdata<-drugMtly %>% filter(State=="United States")
#Other data with States specified
StateData<-drugMtly %>% filter(State!="United States")

#new data set with average crude death rate from 1999 to 2016
usstates<-us_map("states")
StateCDR<-StateData%>%group_by(State)%>% summarise(AvgCDR=mean(`Crude Death Rate`,na.rm = TRUE))
StateCDR$full <- StateCDR$State
Total.meanCDR<- merge(usstates, StateCDR, by="full")
#map
usmap.meanCDR <- ggplot() + 
  geom_polygon(data=Total.meanCDR, aes(x=long, y=lat, group = group, fill=AvgCDR),colour="white") + 
  scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar",name="Crude Death Rate")+
  theme(panel.grid = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())


######plot themes
theme.bw<-scale_fill_grey() #Use grey scale

# # define a function which finds which state a point is in. This is the function 
# # that takes input from click and give the name of the state being clicked
# which_state <- function(mapData, long, lat) {
#   # This function decide the state being clicked. 
#   #
#   # Args:
#   #   mapData: The map data has a column "long" and a column "lat" to determine
#   #       state borders. 
#   #   long, lat: longitude and latitude of the clicked point. They are actually
#   #       input$clickMap$x and input$clickMap$y assuming click = "clickMap".
#   #
#   # Returns: 
#   #   The name of the state containing the point (long, lat).
#   
#   # calculate the difference in long and lat of the border with respect to this point
#   mapData$long_diff <- mapData$long - long
#   mapData$lat_diff <- mapData$lat - lat
#   
#   # only compare borders near the clicked point to save computing time
#   mapData <- mapData[abs(mapData$long_diff) < 20 & abs(mapData$lat_diff) < 15, ]
#   
#   # calculate the angle between the vector from this clicked point to border and c(1, 0)
#   vLong <- mapData$long_diff
#   vLat <- mapData$lat_diff
#   mapData$angle <- acos(vLong / sqrt(vLong^2 + vLat^2))
#   
#   # calculate range of the angle and select the state with largest range
#   rangeAngle <- tapply(mapData$angle, mapData$State, function(x) max(x) - min(x))
#   return(names(sort(rangeAngle, decreasing = TRUE))[1])
# }