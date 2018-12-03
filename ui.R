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
                                                  "),
                                       textOutput("aboutapp"),
                                       plotOutput("avgCDR"),
                                       downloadButton(outputId = "downMap", label = "Download the Map")
                                      ),
                              tabPanel("Data Source",
                                       tags$style(type="text/css",
                                                 "#datasource{height: 200px;
                                                 background-color: #FFE4E1}"),
                                       verbatimTextOutput("datasource"),
                                                 br(),
                                       DT::dataTableOutput("dataTable"), br(),
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
                                checkboxInput("gray", h5("Black and White Figures"))
                                ),
                               # Show outputs
                               mainPanel(tabsetPanel
                                         (type = "tabs",
                                              tabPanel("Death by Sex",
                                                       plotOutput("sexDeath"),
                                                       downloadButton(outputId = "downPlotYear1", 
                                                                      label = "Download the Plot")),
                                              tabPanel("Death by Race", 
                                                       plotOutput("raceDeath"),
                                                       downloadButton(outputId = "downPlotYear2", 
                                                                      label = "Download the Plot"))))
                                        )),
                            br(),
                            br(),
                            hr(),
                            #row 2
                            fluidRow(sidebarLayout(
                              sidebarPanel(
                                h3("Population vs Death by Age Group"),
                                selectizeInput("ageGroup", "Age Group", selected = "0-14", 
                                               choices = levels(as.factor(USdata$'Age Group'))),br(),
                                sliderInput("size", "Size of Points on Graph",
                                            min = 1, max = 8, value = 3, step = 1),
                                br(),
                                checkboxInput("regline", h5("Add a Linear Regression Line")),
                                conditionalPanel(condition = "input.regline",
                                                 selectizeInput("regMethod", "Fitting the model with",
                                                                choices = c("Basic Simple Linear Regression",
                                                                            "Polynomial Regression"),
                                                                selected = "Basic Simple Linear Regression"),
                                                 checkboxInput("pred", h5("Add CI and PI"))),
                                withMathJax(),
                                uiOutput('lmEq'),
                                downloadButton(outputId = "downPlotAge", label = "Download the Plot")),
                              # Show outputs
                               mainPanel(plotOutput("PopvsDeathAge",click = "plot_click",
                                                   brush = brushOpts(id = "plot_brush")),
                                         conditionalPanel(condition = "input.regline",
                                                          verbatimTextOutput("modelSum")),
                                        h3("Info of Brushed Points"),
                                        verbatimTextOutput("a"),
                                        h3("Info of selected Points"),
                                        verbatimTextOutput("b")
                              ))),
                            br(),
                            br(),
                            hr(),
                            #row 3 add a tree model fit for factors
                            fluidRow(
                              sidebarLayout(
                                sidebarPanel(
                                  h3("CART-Classification Tree Model for Crude Death Rate"),
                                  textOutput("TreeModelDescp"),
                                  checkboxGroupInput("TreeVariable", label = h5("Variables included in the model"), 
                                                     choices = c("Sex" = "Sex", "Age Group" = "Age",
                                                                 "Race and Hispanic Origin" ="Race",
                                                                 "Population"="Population"),
                                                     selected =c ("Sex","Age","Race","Population"))),
                                # Show outputs
                                mainPanel(visNetworkOutput("CrudeDeathTree"))
                              )),
                            br(),
                            br(),
                            hr(),
                            #row 4 add clustering
                            fluidRow(column(12,h3("Clustering"),align="center",
                                        fluidRow(column(6,
                                                       h4("K-Means Cluster"),
                                                       plotOutput("KMeansCluster"),
                                                       downloadButton(outputId = "downPlotKCluster",
                                                                      label = "Download the Plot")),
                                                 column(6,
                                                       h4("Hierarchical Cluster"),
                                                       plotOutput("HierarchicalCluster"),
                                                       downloadButton(outputId = "downPlotHirCluster", 
                                                                      label = "Download the Plot"))),
                                        br(),
                                       fluidRow(column(4,align="left",
                                                selectizeInput("xCluster", "x Variables",selected = "Deaths", 
                                                                choices = c("Population" = "Population",
                                                                       "Deaths" = "Deaths",
                                                                       "Crude Death Rate" ="CDrate",
                                                                       "Year"="Year")),
                                                selectizeInput("yCluster", "y Variables",selected = "Population", 
                                                               choices = c("Population" = "Population",
                                                                           "Deaths" = "Deaths",
                                                                           "Crude Death Rate" ="CDrate",
                                                                           "Year"="Year"))),
                                                column(4,
                                                       sliderInput("numCluster", "Cluster Count",
                                                                   min = 1, max = 9, value = 3, step = 1)),
                                                column(4,align="left",
                                                       selectizeInput("HirMethod", "Hierarchical Agglomeration Method", 
                                                               selected = "ward.D", 
                                                               choices = c("ward.D", "ward.D2", "single", 
                                                                           "complete", "average", "median","centroid"))))
                                       ))
                                            
                            
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
                           downloadButton("stateDataDwld",label = "Click to Download Data")),
                         # Show map outputs
                         mainPanel(DT::dataTableOutput("stateTable")
                                   # ,plotOutput("USmap",click = "clickMap", 
                                   #            width = 500, height = 300)
                             )))),
  br(),
  hr(),
  fluidRow(column(6,textOutput("bottom1")),
           column(6,textOutput("bottom2")))
                              ))

