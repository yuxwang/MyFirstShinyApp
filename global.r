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

#to fit tree model
if (!require("rpart")){
  install.packages("rpart")
  library("rpart")
}
if (!require("visNetwork")){
  install.packages("visNetwork")
  library("visNetwork")
}
if (!require("sparkline")){
  install.packages("sparkline")
  library("sparkline")
}
#cluster 
if (!require("cluster")){
  install.packages("cluster")
  library("cluster")
}
#easy to extract and visualize the output of exploratory multivariate data analyses
if (!require("FactoMineR")){
  install.packages("FactoMineR")
  library("FactoMineR")
}
if (!require("factoextra")){
  install.packages("factoextra")
  library("factoextra")
}

#prediction ribbon on regression line
if (!require("ciTools")){
  install.packages("ciTools")
  library("ciTools")
}

#loading data
if (!require("shinycssloaders")){
  install.packages("shinycssloaders")
  library("shinycssloaders")
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

#for tab2 us data with all categorical varibles classified
#National data categorical variables classified
USsub<-USdata %>% filter(Sex!="Both Sexes") %>% filter(`Race and Hispanic Origin`!="All Races-All Origins") %>% filter(`Age Group`!="All Ages") %>% select(c(2:8,17))
colnames(USsub) <-c("Year","Sex","Age","Race","Deaths","Population","CDrate","UScr")
USsub<-USsub %>% mutate(CDlevel=ifelse(CDrate >30,">30",ifelse(CDrate >20,"20-30",ifelse(CDrate >10,"10-20","0-10"))))%>% mutate_at(9, as.factor)


######plot themes
theme.bw<-scale_fill_grey() #Use grey scale

####loading data
load_data <- function() {
  Sys.sleep(2)
  hide("loading_page")
  show("main_content")
}

