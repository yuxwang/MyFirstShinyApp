
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
     paste0("This dataset describes drug poisoning deaths at the U.S. and state level by selected demographic characteristics, and includes age-adjusted death rates for drug poisoning.","There are 18 columns and 2,862 rows in this dataset","\n","\n","Deaths are classified using the International Classification of Diseases, Tenth Revision (ICD–10). Drug-poisoning deaths are defined as having ICD–10 underlying cause-of-death codes X40–X44 (unintentional), X60–X64 (suicide), X85 (homicide), or Y10–Y14 (undetermined intent).","\n","\n","Crude death rate indicates the number of deaths occurring during the year, per 1,000 population estimated at midyear.","\n","\n","Age-adjusted death rates is deaths per 100,000 U.S. standard population for 2000.","\n","\n","Populations are postcensal estimates based on the 2010 U.S. census." )
     })
    #data table
    output$dataTable <- DT::renderDataTable({
      DT::datatable(drugMtly,
                    caption=paste0("Drug Poisioning Mortality Data"),
                    options = list(scrollX = TRUE))
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
  
 
  #tab 2 middle plot
  output$TreeModelDescp<-renderText({
    paste0("Crude Death rate was reclassified into 4 levels:","0-10, 10-20, 20-30, >30" )
  })
  
  gettressData <- reactive({newData <-USsub %>% select(CDlevel,input$TreeVariable)})
  
  output$CrudeDeathTree <- renderVisNetwork({
    newData <- gettressData()
    res <- rpart(CDlevel~., data= newData)
    visTree(res, main = "Classification Tree Model for Crude Death Rate",edgesFontSize = 14, nodesFontSize = 16) 
  })
  
  
  #cluster data
  selectedData <- reactive({
    USsub[, c(input$xCluster, input$yCluster)]
  })
  #k-means cluster
  kclusters <- reactive({
    kmeans(selectedData(), input$numCluster)
  })
  output$KMeansCluster <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    par(mar = c(5.1, 4.1, 0, 1))
    fviz_cluster(kclusters(), geom = "point",data =selectedData(),main = "")
  })
  #download k-means cluster
  output$downPlotKCluster<-downloadHandler(
    filename <- function() {
      paste('KmeansCluster','png', sep = ".")
    },
    content <- function(file) {
      png(file)
      g<-fviz_cluster(kclusters(), geom = "point",data =selectedData())
      print(g)
      dev.off()
    },
    contentType = "image/png"
  )
  
  #hierarchical clustering
  ds<-reactive({dist(selectedData(), method = "euclidean")})
  hcclusters<- reactive({
    hclust(ds(), method = input$HirMethod )
  })
  hcgroup<-reactive({cutree(hcclusters(), k = input$numCluster)})
  
  output$HierarchicalCluster<- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    par(mar = c(5.1, 4.1, 0, 1))
    fviz_cluster(list(data = selectedData(), cluster = hcgroup()), geom = "point",main = "")
  })
 
  output$downPlotHirCluster<-downloadHandler(
    filename <- function() {
      paste('hierarchicalCluster','png', sep = ".")
    },
    content <- function(file) {
      png(file)
      g<-fviz_cluster(list(data = selectedData(), cluster = hcgroup()), geom = "point")
      print(g)
      dev.off()
    },
    contentType = "image/png"
  )
 
  
  #tab 2 middle left plot
  getData2 <- reactive({
    newData <- USdata %>% filter(`Age Group` == input$ageGroup)
  })
  
  
  output$PopvsDeathAge <- renderPlot({
    #get filtered data
    newData <- getData2()
    g <- ggplot(newData, aes(x = Population, y = Deaths))+ geom_point(size = input$size)
    #slm fit
    slmfit<-lm(Deaths~Population,data=newData)
    #quard fit
    quardfit<-lm(Deaths~Population+I(Population^2),data=newData)
    slmdata<-newData %>% add_pi(slmfit,names=c("lower","upper"))
    quarddata<-newData %>% add_pi(quardfit,names=c("lower","upper"))
    #create plot
    if(input$regline){
      if(input$regMethod=="Basic Simple Linear Regression"){
         
           if(input$pred){
             ggplot(slmdata, aes(x = Population, y = Deaths))+ 
                geom_point(size = input$size)+
                geom_smooth(method='lm',formula=y~x,fill="blue")+
                geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3,fill="red")+
                ggtitle("Scatter Plot With 95% PI")
                         }
            else{
             g+geom_smooth(method='lm',formula=y~x,fill=NA)}} #add a regression line
         
      else{ 
         
          if(input$pred){
           ggplot(quarddata, aes(x = Population, y = Deaths))+ 
           geom_point(size = input$size) +
           geom_smooth(method='lm',formula=y~x+I(x^2),fill="blue")+
           geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3,fill="red")+
           ggtitle("Scatter Plot With 95% PI")
                        }
          else{g+geom_smooth(method='lm',formula=y~x+I(x^2),fill=NA)}#add a regression line
           }
      }else{g}
  })
  
  
  
  
  output$downPlotAge<-downloadHandler(
    filename <- function() {
      paste('PopvsDeath','png', sep = ".")
    },
    content <- function(file) {
      png(file)
      newData <- getData2()
      g <- ggplot(newData, aes(x = Population, y = Deaths))+ geom_point(size = input$size)
      slmfit<-lm(Deaths~Population,data=newData)
      #quard fit
      quardfit<-lm(Deaths~Population+I(Population^2),data=newData)
      slmdata<-newData %>% add_pi(slmfit,names=c("lower","upper"))
      quarddata<-newData %>% add_pi(quardfit,names=c("lower","upper"))
      
      ifelse(input$regline,
             
             ifelse(input$regMethod=="Basic Simple Linear Regression",
                       ifelse(input$pred, 
                           print(ggplot(slmdata, aes(x = Population, y = Deaths))+ 
                             geom_point(size = input$size)+
                             geom_smooth(method='lm',formula=y~x,fill="blue")+
                             geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3,fill="red")+
                             ggtitle("Scatter Plot With 95% PI")),
                           print(g+geom_smooth(method='lm',formula=y~x,fill=NA))),
                    ifelse(input$pred,
                           print(ggplot(quarddata, aes(x = Population, y = Deaths))+ 
                             geom_point(size = input$size) +
                             geom_smooth(method='lm',formula=y~x+I(x^2),fill="blue")+
                             geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3,fill="red")+
                             ggtitle("Scatter Plot With 95% PI")),
                    print(g+geom_smooth(method='lm',formula=y~x+I(x^2),fill=NA)))),
            
              print(g))
      
      dev.off()
    },
    contentType = "image/png"
  )
  
  
  #tab 2 bottom right table
  output$lmEq <- renderUI({
    withMathJax(helpText('Simple Linear Regression Equation  $$Death=\\beta_0+(\\beta_1)Population$$'))
  })
  
 #model summary output
  output$modelSum<-renderPrint({
    newData <- getData2()
    #slm fit
    slmfit<-lm(Deaths~Population,data=newData)
    #quard fit
    quardfit<-lm(Deaths~Population+I(Population^2),data=newData)
    if(input$regMethod=="Basic Simple Linear Regression"){
      summary(slmfit)}else{summary(quardfit)}
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