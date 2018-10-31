
shinyServer(function(input, output, session) {
  
####tab 1
  ##About this App
    #text output App info
    output$aboutapp<-renderText({
    paste0("This app is an interactive shiny App. Figures presented in this App shows drug poisoning deaths at the national and state levels.","\n","\n","Data is analyzed on both national and state level. Results can be found under each navigation tabs on top.")
  })
   #plot map
    output$avgCDR<-renderPlot({
      usmap.meanCDR+
        theme(plot.title = element_text(size=20, face="bold.italic"),
              panel.grid = element_blank(),
              axis.title = element_blank(),axis.text = element_blank())+
        ggtitle("Average Crude Death Rate by Drug Poisioning in United State \n from year 1999 to 2016") 
    })
    #download map
    output$downMap<- downloadHandler(
      filename <- function() {
        paste('USmapCrudeDeathRate','png', sep = ".")
      },
      content <- function(file) {
       png(file)
        plot <-  usmap.meanCDR+
          theme(plot.title = element_text(size=20, face="bold.italic"),
                panel.grid = element_blank(),
                axis.title = element_blank(),axis.text = element_blank())+
          ggtitle("Average Crude Death Rate by Drug Poisioning in United State \n from year 1999 to 2016") 
        
        print(plot)
        dev.off()
      },
      contentType = "image/png"
    )
  
  ##About Data source
    #text output Data Source
    output$datasource<-renderText({
     paste("This dataset describes drug poisoning deaths at the U.S. and state level by selected demographic characteristics, and includes age-adjusted death rates for drug poisoning.")
     })
    #hyper links 
   datainfo.url<- a("Click here", href="https://catalog.data.gov/dataset/drug-poisoning-mortality-by-state-united-states")
   dataDownload.url<-a("Click here", href="https://data.cdc.gov/api/views/xbxb-epbu/rows.csv")
   output$datainfo <- renderUI({
     tagList("More information about the dataset", datainfo.url)
   })
   output$dataDownload <- renderUI({
     tagList("Download data", dataDownload.url)
   })
  
  
####tab 2
   ##top row
   #US DATA
  getData <- reactive({
    newData <- USdata %>% filter(Year == input$year)
  })
  
  ##create plot sexDeath and sexCruderate
  output$sexDeath <- renderPlot({
    #get filtered data
    sexDeath <- getData() %>% filter(`Race and Hispanic Origin`=="All Races-All Origins"
                                     & `Age Group`!="All Ages"& Sex!="Both Sexes")
    #create plot
    gsexDeath<- ggplot(data=sexDeath,aes(x=`Age Group`,y=Deaths,fill=Sex))+
      geom_bar(stat="identity")+ggtitle("Barplot of Deaths by Sex and Age Group in Year ",input$year)
    
    if(input$gray){
           #update plot color
           gsexDeath+theme.bw}  
    else{gsexDeath}
  })
  
  #create plot raceDeath
  output$raceDeath <- renderPlot({
    #get filtered data
    raceDeath<- getData() %>% filter(`Race and Hispanic Origin`!="All Races-All Origins"
                                      & `Age Group`!="All Ages"& Sex=="Both Sexes")
    #create plot
    graceDeath <-ggplot(data=raceDeath,aes(x=`Age Group`,y=Deaths,fill=`Race and Hispanic Origin`))+
      geom_bar(stat="identity")+
      ggtitle("Barplot of Deaths by Race and Hispanic Origin and Age Group in Year ",input$year)
      
    if(input$gray){
      #update plot color
      graceDeath+theme.bw}  
    else{graceDeath}
  })
  
  output$downPlotYear1<-downloadHandler(
    filename <- function() {
      paste('sexDeath','png', sep = ".")
    },
    content <- function(file) {
      png(file)
       sexDeath <- getData() %>% filter(`Race and Hispanic Origin`=="All Races-All Origins"
                                       & `Age Group`!="All Ages"& Sex!="Both Sexes")
      #create plot
       gsexDeath<- ggplot(data=sexDeath,aes(x=`Age Group`,y=Deaths,fill=Sex))+
        geom_bar(stat="identity")+ggtitle("Barplot of Deaths by Sex and Age Group in Year ",input$year)
       ifelse(input$gray,print(gsexDeath+theme.bw),print(gsexDeath))
      dev.off()
    },
    contentType = "image/png"
  )
  
  output$downPlotYear2<-downloadHandler(
    filename <- function() {
      paste('raceDeath','png', sep = ".")
    },
    content <- function(file) {
      png(file)
      raceDeath<- getData() %>% filter(`Race and Hispanic Origin`!="All Races-All Origins"
                                       & `Age Group`!="All Ages"& Sex=="Both Sexes")
      #create plot
      graceDeath <-ggplot(data=raceDeath,aes(x=`Age Group`,y=Deaths,fill=`Race and Hispanic Origin`))+
        geom_bar(stat="identity")+
        ggtitle("Barplot of Deaths by Race and Hispanic Origin and Age Group in Year ",input$year)
      
      ifelse(input$gray,print(graceDeath+theme.bw),print(graceDeath))
      dev.off()
    },
    contentType = "image/png"
  )
  #tab 2 bottom left plot
  getData2 <- reactive({
    newData <- USdata %>% filter(`Age Group` == input$ageGroup)
  })
  output$PopvsDeathAge <- renderPlot({
    #get filtered data
    newData <- getData2()
    g <- ggplot(newData, aes(x = Population, y = Deaths))+ geom_point(size = input$size)
    #create plot
    if(input$regline){
      g+geom_smooth(method='lm',formula=y~x) #add a regression line
      
    }else{
      g
    }
  })
  
  output$downPlotAge<-downloadHandler(
    filename <- function() {
      paste('PopvsDeath','png', sep = ".")
    },
    content <- function(file) {
      png(file)
      newData <- getData2()
      g <- ggplot(newData, aes(x = Population, y = Deaths))+ geom_point(size = input$size)
      
      ifelse(input$regline,print(g+geom_smooth(method='lm',formula=y~x)),print(g))
      
      dev.off()
    },
    contentType = "image/png"
  )
  
  
  #tab 2 bottom right table
  output$lmEq <- renderUI({
    withMathJax(helpText('Simple Linear Regression Equation  $$Death=\\beta_0+(\\beta_1)Population$$'))
  })
  
    
  
  #Click to download data as csv
  output$stateDataDwld<-downloadHandler(
    filename = function() {
      paste("US Drug Poisioning Mortality",input$stateYear, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(getDataState(), file, row.names = FALSE)
    }
  )
  
  #brush and click
  output$a <- renderPrint({
      newdata<-getData2()
    brushedPoints(newdata, input$plot_brush)
  })
  output$b <- renderPrint({
    newdata<-getData2()
    nearPoints(newdata, input$plot_click, addDist = TRUE)
  })
  
  
####tab3
  #row1
  getStateData.State<- reactive({
    newData <- StateData %>% filter(State == input$stateName)
  })
   #plot CDR trend by year
   output$deathTrend.State<-renderPlot({
     #get filtered data
     trenddata <- getStateData.State()
     #create plot
     g <- ggplot(trenddata, aes(x = Year, y = Deaths))+geom_line(colour="#FF9999")
    
     if(input$showPoint){
       g + geom_point(size = input$sizeDeath,colour="#FF9999")
     } else {g}
   })
  
  
   
   #Click to download plot
   output$deathTrend<-downloadHandler(
     filename <- function() {
       paste('DeathTrend',input$stateName,'png', sep = ".")
     },
     content <- function(file) {
       png(file)
       trenddata <- getStateData.State()
       plot <- ggplot(trenddata, aes(x = Year, y = Deaths))+geom_line(colour="#FF9999")+ geom_point(size = input$sizeDeath,colour="#FF9999")
       print(plot)
       dev.off()
     },
     contentType = "image/png"
   )
   
  
  
  # #top- CDR map
  # plotMap <- ggplot(usstates, aes(x = long, y = lat, group = group)) + 
  #   geom_polygon(fill = "white", color = "black")
  # plottrend <- ggplot(StateData, aes(x = Year, y = 'Crude Death Rate',group=State),xlim=c(1998,2020)) +geom_line()  
  #   
  
 
  # output$USmap<-renderPlot({
  #   
  #   StateData.year<-getDataState() 
  #   StateData.year$full <- StateData.year$State
  #   total.year<- merge(usstates, StateData.year, by="full")
  # 
  #   #map
  #   usmap.CDR.year <- 
  #     ggplot() + 
  #     geom_polygon(data=total.year,
  #                  aes(x=long, y=lat, group = group,
  #                      fill = factor(as.factor(total.year$'Crude Death Rate')),color="white"))+
  #     theme(panel.grid = element_blank(),axis.title = element_blank(),
  #           axis.text = element_blank(),legend.position="none")
  #   
  #   usmap.CDR.year
  # })

  #Bottom table
  
  #State data
  getDataState <- reactive({
    newData <- StateData %>% filter(Year == input$stateYear)
  })

   #table
  output$stateTable <- DT::renderDataTable({
    DT::datatable(getDataState(),
                  caption=paste0("Drug Poisioning Mortality Data in Year ",
                                 input$stateYear, " at State-level"),
                  options = list(scrollX = TRUE))
  })
  
  
  
  #Click to download data as csv
  output$stateDataDwld<-downloadHandler(
    filename = function() {
      paste("USDrugPoisioningMortality",input$stateYear, ".csv")
    },
    content = function(file) {
      write.csv(getDataState(), file, row.names = FALSE)
    }
  )
  
  # # plot after click
  # observeEvent(input$clickMap, {
  #   xClick <- input$clickMap$x
  #   yClick <- input$clickMap$y
  #   state <- which_state(Total.meanCDR, xClick, yClick)
  #   output$USmap <- renderPlot(
  #     usmap.meanCDR + 
  #       geom_polygon(data = Total.meanCDR[Total.meanCDR$full == state,], fill = "yellow") +
  #       annotate("text", x = xClick, y = yClick, label = state, color = "red")
  #   )
  #   
  # })
  
  ##Bottom information
  output$bottom1<-renderText({
    "This app is developed on Oct. 2018."
  })
  output$bottom2<-renderText({
    "Email: wyuxi@ncsu.edu"
  })
  

  
})