library(ggplot2)

shinyUI(
  navbarPage(theme = shinytheme("united"),
            
             "Drug Poisioning Mortality",
                   
   ####tab 1
                   tabPanel("About this App",
                            tags$head(
                              tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                                              h1 {
                                              font-family: 'Lobster', cursive;
                                              font-weight: 500;
                                              line-height: 1.1;
                                              color: #ff6666;
                                              }"))),
                            headerPanel("Drug Poisioning Mortality Analysis in United States"),
                            br(),
                            navlistPanel(
                              tabPanel("About App",
                                       tags$style(type="text/css",
                                                  "#aboutapp{height: 200px;
                                                  background-color: #FAFAD2}"),
                                       verbatimTextOutput("aboutapp"),
                                       plotOutput("avgCDR"),
                                       downloadButton(outputId = "downMap", label = "Download the Map")
                                      ),
                              tabPanel("Data Source",
                                       tags$style(type="text/css",
                                                 "#datasource{height: 200px;
                                                 background-color: #FFE4E1}"),
                                       verbatimTextOutput("datasource"),
                                                 br(),
                                       uiOutput("datainfo"),br(),
                                       uiOutput("dataDownload")
                                       )
                             )
                            ),
      
  ####tab 2
                   tabPanel("National-level Analysis",
                            tags$head(
                              tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                                              h1 {
                                              font-family: 'Lobster', cursive;
                                              font-weight: 500;
                                              line-height: 1.1;
                                              color: #ff6666;
                                              }"))),
                            headerPanel("National-level Analysis"),
                            #row 1
                            fluidRow(
                              sidebarLayout(
                              sidebarPanel(
                                h3("Death caused by drug poisioning compared with factors"),
                                selectizeInput("year", "Year", selected = "1999", 
                                               choices = levels(as.factor(USdata$Year))),
                                br(),
                                checkboxInput("gray", h5("Black and White Figures")),
                                downloadButton(outputId = "downPlot.year", label = "Download the Plot")
                                ),
                              
                              # Show outputs
                              mainPanel(tabsetPanel
                                         (type = "tabs",
                                              tabPanel("Death by Sex",
                                                       plotOutput("sexDeath")),
                                              tabPanel("Death by Race", 
                                                       plotOutput("raceDeath"))
                                           ))
                                        )),
                            
                            #row 2
                            fluidRow(sidebarLayout(
                              sidebarPanel(
                                h3("Population vs Death by Age Group"),
                                selectizeInput("ageGroup", "Age Group", selected = "0-14", 
                                               choices = levels(as.factor(USdata$'Age Group'))),br(),
                                sliderInput("size", "Size of Points on Graph",
                                            min = 1, max = 10, value = 5, step = 1),
                                br(),
                                checkboxInput("regline", h5("Add a Simple Linear Regression Line")),
                                withMathJax(),
                                uiOutput('lmEq'),
                                downloadButton(outputId = "downPlot.age", label = "Download the Plot")
                              ),
                              
                              # Show outputs
                              mainPanel(plotOutput("PopvsDeathAge",click = "plot_click",
                                                                     brush = brushOpts(id = "plot_brush")),
                                       h3("Info of Brushed Points"),
                                       verbatimTextOutput("a"),
                                       h3("Info of selected Points"),
                                       verbatimTextOutput("b")
                                        )))
                   ),
  
  ####tab 3
                   tabPanel("State-level Analysis",
                            tags$head(
                              tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                                              h1 {
                                              font-family: 'Lobster', cursive;
                                              font-weight: 500;
                                              line-height: 1.1;
                                              color: #ff6666;
                                              }"))),
                            headerPanel("State-level Analysis"),
                          #row 1
                            fluidRow(sidebarLayout(
                              sidebarPanel(
                                h3("Deaths Trend by State"),
                                selectizeInput("stateName", "Select State", selected = "Alabama", 
                                               choices = levels(as.factor(StateData$State))),
                                br(),
                                checkboxInput("showPoint", "Show points on graph"),
                                #only show a slider if "showPoint" is checked
                                conditionalPanel(condition = "input.showPoint",
                                                 sliderInput("sizeDeath", "Size of Points on Graph",
                                                             min = 1, max = 10, value = 5, step = 1),
                                downloadButton(outputId = "deathTrend", label = "Download the Plot")
                                )),
                              # Show trend by year outputs
                              mainPanel(column(width = 8,plotOutput("deathTrend.State"),
                                               brush = brushOpts(id = "plot_brush")),
                                        column(width = 4, verbatimTextOutput("brush_info")
                                        )
                                        
                                        )
                            )),
                          br(),
                          hr(),
                     #row 2
                     fluidRow(
                       sidebarLayout(
                         sidebarPanel(
                           h3("Crude Death Rate by Drug Poisioning"),
                           selectizeInput("stateYear", "Select Year", selected = "1999", 
                                          choices = levels(as.factor(StateData$Year))),
                           br(),
                           downloadButton("stateDataDwld",label = "Click to Download Data"),
                           br(),
                           br(),
                           downloadButton(outputId = "deathUSmap", label = "Download the Map")
                                        ),
                         # Show map outputs
                         mainPanel(DT::dataTableOutput("stateTable"),
                                   plotOutput("USmap",click = "clickMap", 
                                              width = 500, height = 300)
                             )))),
  br(),
  hr(),
  fluidRow(column(6,textOutput("bottom1")),
           column(6,textOutput("bottom2")))
                              ))

