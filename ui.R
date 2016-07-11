
# Packages ----------------------------------------------------------------
#install.packages("plyr")
#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("shiny")
#install.packages("ggvis")
#install.packages("MASS")
#install.packages("googleVis")
#install.packages("grid")
#install.packages("shinydashboard")
#install.packages("magrittr")
#install.packages("plyr")
#install.packages("stringr")
#install.packages("lattice")
#install.packages("tools")
#install.packages("corrplot")
#install.packages("corrgram")
#install.packages("shinyjs")
#install.packages("e1071")
#install.packages("FNN")
#install.packages("randomForest")
#install.packages("shinythemes")
#install.packages("DT")

#Loading the required packages. (If trying out on local machine, install packages first)
library(plyr)
library(ggplot2)
library(reshape2)
library(shiny)
library(ggvis)
library(MASS)
library(googleVis)
library(grid)
library(shinydashboard)
library(magrittr)
library(plyr)
library(stringr)
library(lattice)
library(tools)
library(corrplot)
library(corrgram)
library(shinyjs)
library(e1071)
library(FNN)
library(randomForest)
library(shinythemes)
library(DT)
library(caret)
library(klaR)
library(e1071)

#The dashboard ui consists of three elements - the header, the sidebar and the body.
#The three elements are intergrated using the dashboardPage command at the bottom.

header <- dashboardHeader(title = "DViz Dashboard")

# the sidebar refers to the dropdown in the left side of the UI pane
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
      menuItem("Introduction", tabName = "introduction"),
      menuItem("Load Data", tabName = "loadData"),
      menuItem("View Data", tabName = "viewData"),
      menuItem("Data Summary", tabName = "dataSummary"),    
      menuItem("Explore Data", tabName = "exploreData"),    
      menuItem("Correlation Insights", tabName = "correlationInsights"),
      menuItem("Select Variables",tabName = "selectVariables"),
      menuItem("Population View", tabName = "populationView"),
      #menuItem("Cluster View", tabName = "clusterView"),
      menuItem("Patient View", tabName = "patientView"),
      menuItem("Intervention View", tabName = "interventionView")
      #menuItem("Prediction View", tabName = "predictionView")
  )
)

#Creating the body. The menuItems are created in the dashboardSidebar function. Their definitions are listed below.
body <- dashboardBody(
  tabItems(tabItem(tabName = "introduction",
            # fluidrow creates a fluid background n which other UI elements could be placed
            fluidRow(
              # you can embed HTML code using tags
              tags$style(HTML("background: url('background.jpg')")),
              # a page width completely is = 12, you can define an integer less than 12 to make it part of a smaller screen
              box(width = 12,solidHeader = TRUE,
                  h2("Introduction",align = "center")),
              box(width = 12, solidHeader = TRUE,
                  br(),br(),
                  style = "background-image: url('background.jpg'); ",
                    box(width = 12,solidHeader = TRUE,
                      p("DViz is a broadly applicable data visualization tool to help
                      assess disease risk and explore related interventions that may
                      be the most appropriate for a patient. It integrates statistical
                      dimensionality reduction methods with information visualization.
                      The tool is currently designed to help clinicians quickly, easily
                      and visually understand their patients.",align = "justify",style="font-size : 16pt")),
                  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                )
              )
            ),
    #This tab is used to allow the user to load a dataset. Once the data set location is loaded using the
    #fileInput widget, the data is automatically stored as a data frame.
    tabItem(tabName = "loadData",
      fluidRow(
        box(width = 12,solidHeader = TRUE,
        column(12,h2("Load the dataset"),align="center"),
        column(12,h4("(Please load a .csv file only)"),align="center")
        )
      ),
      fluidRow(
        box(solidHeader = TRUE,width = 12,
          # every input element has the first parameter as ID Eg. here ID is fileInput
          # this is the reference that will be used in the server code -> Eg. this code will be in input$filenput in server
          column(12,fileInput('fileInput', 'Upload File',accept = c('.csv')),align = "center")
        )
      )
    ),
    
    #The view tab is used to view the loaded data. Once the data has been loaded, it is displayed as a dataOutputTable.
    tabItem(tabName = "viewData",
      fluidRow(box(width = 12,solidHeader = TRUE,
                   h2("View the patient data",align = "center")),
      fluidRow(box(width = 12,solidHeader = TRUE,
          dataTableOutput('inputData'))
          )
        )
      ),
    # The data summary page gives a summaary statistics of each column of the dataset
    tabItem(tabName = "dataSummary",
            fluidRow(
              box(width = 12,solidHeader = TRUE,
                  h2("View the data summary",align = "center"))),
            fluidRow(
              box(width = 12,solidHeader = TRUE,
                  #column(width = 12,solidHeader = TRUE,
                  #DT::dataTableOutput('dataSummary')
                  tabsetPanel(
                    tabPanel("Numerical Attributes",DT::dataTableOutput("numericSummary")),
                    tabPanel("Categorical Attributes",DT::dataTableOutput("categoricSummary"))
                  ))
            )
    ),
    # the explore data tab gives the option to view the distributions of each column
    tabItem(tabName = "exploreData",
      fluidRow(box(width = 12,solidHeader = TRUE,
                   h2("Explore the data",align = "center")),
               box(width = 6,solidHeader = TRUE,
                   h4(textOutput("recordCount"),align = "center")),
               box(width = 6,solidHeader = TRUE,
                   h4(textOutput("attributeCount"),align = "center"))),
      fluidRow(
        box(width = 12,solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Individual Distribution",
                       htmlOutput('exploreColumnsSelection'),
                       column(width = 12, align = "center",htmlOutput('exploreVarPlot'))),
              tabPanel("Bivariate distribution",br(),
                       column(width = 6,htmlOutput('columnxSelection')),
                       column(width = 6,htmlOutput('columnySelection')),
                       column(width = 12, align = "center", htmlOutput('exploreBiPlot')))
            )))
      ),
    # this tab provides correlation insights among the different columns
    tabItem(tabName = "correlationInsights",
      fluidRow(
        box(solidHeader = TRUE,width = 12,
            h2("Correlation insights",align = "center")),
        box(width = 12,solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Population Correlation",
                       column(width=6,plotOutput("correlationPlot",height = 700)),
                       column(width=6,h4(textOutput("outputColCorList")),
                              dataTableOutput("correlationList"))),
              tabPanel("Individual Correlation",
                      column(width = 6, uiOutput("selectx")),
                      column(width = 6, uiOutput("selecty")),
                      column(width = 12, textOutput("correlationValue"),htmlOutput('scatterPlot')))))
      )
    ),
    # Tab that allows people to select different variables
    tabItem(tabName = "selectVariables",
      fluidRow(
        box(solidHeader = TRUE,width = 12,
            h2("Select the risk and monitored variables",align = "center")),
        box(solidHeader = TRUE,width = 3,
            h3("Select the output variable", align = "center"),
            h4("(List includes all categorical variables)",align = "center"),
            div(style = "overflow-y:scroll;height = 500", uiOutput("outputVariables"))),
        box(solidHeader = TRUE,width = 3,
            h3("Select the risk variables", align = "center"),
            h4("(All variables are selected by default)",align = "center"),
            div(style = "overflow-y:scroll;height = 500", uiOutput("selectRiskVariables"))),
        box(solidHeader = TRUE,width = 3,
            h3("Select the variables to be monitored", align = "center"),
            h4("(List includes selected risk variables)",align = "center"),
            div(style = "overflow-y:scroll;height = 500", uiOutput("selectMonitoredVariables"))),
        box(solidHeader = TRUE,width = 3,
            h3("Enter the ideal values of monitored variables", align = "center"),
            div(style = "overflow-y:scroll; height: 500",
                column(width = 6,solidHeader = TRUE, uiOutput('monitoredMinimum')),
                column(width = 6,solidHeader = TRUE, uiOutput('monitoredMaximum')),
                column(width = 12,solidHeader = TRUE,uiOutput('monitoredIdealCat'),br())))
      )
    ),
    #The population view allow the business users to view each patient against all other patients based on the risk levels.
    tabItem(tabName = "populationView",
            fluidRow(
              box(width = 12,solidHeader = TRUE,
                  h2("Population View",align = "center")),
              box(
                #Dsiplay the population view graph. The slider is provided to allow the user to specify the risk level.
                width = 10,solidHeader = TRUE, height = 700,
                #The output graph
                htmlOutput('populationGraph')
                ),
              box(width = 2,solidHeader = TRUE,height = 700,
                  h4("The overview graph uses an attraction metaphor :"), 
                    h4("Patients with high values for a given risk factor 
                      relative to other patients in the population are pulled closer to that
                      risk factor while patients with low values are pushed away. The size of each risk factor,
                      shown as a circle on the graph, represents its strength of attraction.",align = "justify"),
                  br(),br(),
                  #The slider input
                  sliderInput('sliderSelection', "Enter the Risk Threshold:", 1, 100, 1)
                )
              )
            ),
    # This tab provides various methods of clustering to understand the population
    tabItem(tabName = "clusterView",
            fluidRow(
              box(width = 12,solidHeader = TRUE,
                  h2("Cluster View",align = "center"))),
            fluidRow(
              box(width = 12, solidHeader = TRUE,
                  tabsetPanel(tabPanel("Risk based variable",uiOutput("riskVariableSelection"),
                                                             htmlOutput("riskClusterPlot"))))
              )
            ),
    #The population view allow the business users to view each patient against all other patients based on the risk levels.
    tabItem(tabName = "kMeansClusterView",
            fluidRow(
              box(width = 12,solidHeader = TRUE,
                  h2("k-means cluster view",align = "center")),
              box(width = 12, solidHeader = TRUE,
                  textInput("k", label = "Enter number of clusters", value = 3)),
              box(width = 12,solidHeader = TRUE,
                  htmlOutput("kMeansClusterPlot")
              )
            )
    ),
    #The patient view allows to get deeper insights of each patient
    # it highlights the position of patient in the entire population, 
    # it gives a comparison of actual and ideal values of monitored variables
    tabItem(tabName = "patientView",
            title = "Patient view",
            fluidRow(
              box(solidHeader = TRUE,width = 12,
                h2("Patient View",align = "center")),
              box(solidHeader = TRUE,width = 12,
                  uiOutput("inputPatientID"))
              ),
            sidebarLayout(
              sidebarPanel(width = 7,height=700,
                           tags$style(".well {background-color:#FFFFFF;}"),
                           htmlOutput('patientGraph')),
              mainPanel(style="width:40%",
                        box(width=12,solidHeader = TRUE,height = 700,
                            h3("Actual Vs Threshold",align="center"),
                            tabsetPanel(tabPanel("Graph",htmlOutput('actualIdealHist')),
                                        tabPanel("Table",dataTableOutput('actualIdealTable'))))
              )
            )
          ),
    # this tab allows users to enter the possible intervention values of different variables and compare the old and the new value
    tabItem(tabName = "interventionView",
            fluidRow(
              box(width = 12,solidHeader = TRUE,
                  h2("Intervention View",align = "center"))),
            fluidRow(
              box(width = 6,solidHeader = TRUE,
                h3("Modifiable variables",align = "center"),
                div(style = "overflow-y:scroll; max-height: 335px", uiOutput("modifiableVariableSelection"))),
              box(width = 6,solidHeader = TRUE,
                h3("Enter the intervention values",align = "center"),
                div(style = "overflow-y:scroll; max-height: 300px", 
                    uiOutput("interventionPatientID"),
                    uiOutput('interventionfields'),
                    br(),br()),
                    column(12,actionButton('interventionButton', 'Click to view change'),align = "center")),
              box(width = 12,solidHeader = TRUE,
                  htmlOutput('interventionPlot'),
                  br(),br())
          )
    ),
    # tab description for prediction view
    tabItem(tabName = "predictionView",
            fluidRow(
              box(width = 12,solidHeader = TRUE,
                  h2("Compare predictions of different models",align = "center"))),
            sidebarLayout(
              sidebarPanel(width = 6,
                           selectInput("tesrt","test",c("1"="1","2"="2"))),
              mainPanel(style="width:50%",
                        box(width=12,solidHeader = TRUE,height = 700,
                            h3("Model Metrics",align="center"),
                            tabsetPanel(
                              tabPanel("LDA"),
                              tabPanel("Logistic Regession",
                                       box(width=6, solidHeader = TRUE,dataTableOutput("logRegConfMat"))),
                              tabPanel("Naive Bayes"),
                              tabPanel("SVM"),
                              tabPanel("k NN"),
                              tabPanel("Random Forest")
                            )
                        )
                )
            )
      )
   )
)
#Combining the header, sidebar and body.
ui <- dashboardPage(header, sidebar, body,skin = "blue")