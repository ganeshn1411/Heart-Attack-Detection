
# Packages ----------------------------------------------------------------

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
library(foreach)


#The dashboard ui consists of three elements - the header, the sidebar and the body.
#The three elements are intergrated using the dashboardPage command at the bottom.

#Creating the header
header <- dashboardHeader(title = "DViz Dashboard")

#Creating the sidebar. The sidebar menuItems are listed which are referred to later
#in the body definition
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Introduction", tabName = "introduction"),
    menuItem("Load Data", tabName = "load"),
    menuItem("View Data", tabName = "view"),
    menuItem("Data Summary", tabName = "summ"),    
    menuItem("Explore Data", tabName = "explore"),    
    menuItem("Correlation Insights", tabName = "cor"),
    menuItem("Population View", tabName = "pop"),
    menuItem("Cluster View", tabName = "clu"),
    menuItem("Select Variables", tabName = "selcol"),
    menuItem("Patient View", tabName = "pat"),
    menuItem("Intervention View", tabName = "int"),
    menuItem("Prediction View", tabName = "pred")
  )
)

#Creating the body. The menuItems are created in the dashboardSidebar function. Their definitions
#are listed below.
body <- dashboardBody(
  tabItems(
    #The first tab is the introduction tab. This tab introduces the dashboard to a business user, with
    #a brief description of it's functionality.
    tabItem(tabName = "introduction",
            fluidRow(#Dashboard description
              box(width = 6,
                height = 400,
                solidHeader = TRUE,
                column(12,tags$div(class = "header", checked = NA, align = "center",tags$img(src = "test.jpg",height = 280,width = 300)))
              ),
              box(height = 400,
                solidHeader = TRUE,
                column(12,tags$div(class = "header", checked = NA,column(12,h1("Introduction"),align = "center"),
                    br(),
                    br(),
                    p("DViz is a broadly applicable data visualization tool to help
                      assess disease risk and explore related interventions that may
                      be the most appropriate for a patient. It integrates statistical
                      dimensionality reduction methods with information visualization.
                      The tool is currently designed to help clinicians quickly, easily
                      and visually understand their patients."
                    ,align = "justify",style="font-size : 16pt")
                  )
                )
              ),
              box(
                br(),
                width = 12,
                solidHeader = TRUE,
                column(12,actionButton('introload', 'Click to Proceed'),align = "center"),
                br(),
                br(),
                br()
              )
              )
            ),
    #The second tab is used to allow the user to load a dataset. Once the data set location is loaded using the
    #flieInput widget, the data is automatically stored as a data frame.
    tabItem(
      tabName = "load",
      fluidRow(
        box(width = 12,
        solidHeader = TRUE,
        column(12,h1("Load the dataset"),align="center"),
        column(12,h3("(Please load a .csv file only)"),align="center")
        )
      ),
      fluidRow(
        box(
          #The file input widget is used to load the location of the file. The definition for what
          #has to happen on the event of a button click is given below. Once the button is clicked,
          #the data is read from the location as a csv.
          #title = "Patient Data (Place the risk variable in the last column)",
          solidHeader = TRUE,
          width = 12,
          column(12,fileInput('file1', 'Upload File',accept = c('.csv')),align = "center")
        )
      ),
      fluidRow(
        box(width = 12,
          solidHeader = TRUE,
               tags$br(),
               #The loadview action button is used to move from the load page to the view data page. The definition
               #for what has to happen on the event of a button click is given in the server function below.
               column(12,actionButton('loadview', 'View the Data'),align = "center"),
          tags$br(),
          tags$br(),
          tags$br()
          )
        )
    ),
    
    #The view tab is used to view the loaded data. Once the data has been loaded, it is displayed as a dataOutputTable.
    #The dataOutputTable can be used to sort the data, searh for an individual record ot all records that match a
    #particular value.
    tabItem(
      tabName = "view",
      fluidRow(box(width = 12,
                   solidHeader = TRUE,
                   h1("View the patient data",align = "center")),
      fluidRow(box(dataTableOutput('contents2'),
          tags$br(),
          width = 12,solidHeader = TRUE,
          color = "black"
        )
      ),
      #After the dataset is viewed, the userr can click on the action button to proceed to the select column page.
      fluidRow(box(width = 12,solidHeader = TRUE,br(),
            column(12,actionButton('datasummaryview', 'View Data Summary'),align = "center"),
            br(),
            br(),
            br()
            )))),
    #The view tab is used to view the loaded data. Once the data has been loaded, it is displayed as a dataOutputTable.
    #The dataOutputTable can be used to sort the data, searh for an individual record ot all records that match a
    #particular value.
    tabItem(
      tabName = "summ",
      fluidRow(box(width = 12,solidHeader = TRUE,
               h1("View the Data Summary",align = "center")),
               fluidRow(box(
                   dataTableOutput('dataSummary'),
                   tags$br(),
                   width = 12,
                   solidHeader = TRUE,
                   color = "black"
                 )
               ),
               #After the dataset is viewed, the userr can click on the action button to proceed to the select column page.
               fluidRow(box(width = 12,solidHeader = TRUE,
                     br(),
                     column(12,actionButton('exploreview', 'Explore Data'),align = "center"),
                     br(),
                     br(),
                     br()
                 ))
      )),
    
    
    #The view tab is used to view the loaded data. Once the data has been loaded, it is displayed as a dataOutputTable.
    #The dataOutputTable can be used to sort the data, searh for an individual record ot all records that match a
    #particular value.
    tabItem(
      tabName = "explore",
      fluidRow(box(width = 12,
                   solidHeader = TRUE,
                   h1("Data Exploration",align = "center")),
               box(width = 6,
                   solidHeader = TRUE,
                   h4(textOutput("recordCount"),align = "center")),
               box(width = 6,
                   solidHeader = TRUE,
                   h4(textOutput("attributeCount"),align = "center")),
               box(width = 12,
                   solidHeader = TRUE,
                   h3("Select the column to explore the Distribution",align = "center"),
                   htmlOutput('exploreColumnsSelection')),
               box(width = 12,
                   solidHeader = TRUE,
                  htmlOutput('ExploreGraphs'),align = "center")),
               box(width = 12,
                   solidHeader = TRUE,
                   br(),
                   column(12,actionButton('viewselcol1', 'Click to Proceed'),align = "center"),
                   br(),
                   br())
    ),
    
    
    #The select column tab is used to select the columns required for analysis. There are two sets of required columns. The
    #first is the vital variable which is used to signify that the variable is important and is considered for analysis
    #at the patient page. The second set is the edtiable variable, which signifies that the variable can be modified at the
    #intervention view page.
    #Records can be common to both the sets of variables. The variables are taken uniquely into consideration.
    
    tabItem(
      tabName = "selcol",
      fluidRow(
        box(width = 6,
          solidHeader = TRUE,
          h3("Monitored Variables",align = "center"),
          div(style = "overflow-y:scroll; max-height: 300px", uiOutput("choose_columns1"))
        )
        #box(width = 6,
        #  solidHeader = TRUE,
        #  color = "black",
        #  h3("Modifiable Variables",align = "center"),
        #  div(style = "overflow-y:scroll; max-height: 300px", uiOutput("choose_columns2")
        #  )
        #)
        #box(
        #  width = 4,
        #  solidHeader = TRUE,
        #  height = 375,
        #  color = "black",
        #  h3("Output Variables",align = "center"),
        #  uiOutput("choose_columns3"),
        #  br(),
        #  h5(textOutput("outputColumn"))
        #)
        ,box(
          #As the vital and the editable variables are being checked, they appear in this tab. The user is asked to
          #provide inputs for ideal values for the editable or vital variables. The reading and setting values for
          #the input fuctions are done in the server function below.
          width = 6,
          solidHeader = TRUE,
          h3("Enter Ideal values for the Monitored variables",align = "center"),
          box(width = 6,
            solidHeader = TRUE,
            uiOutput('textbox')
            ),
          box(width = 6,
            solidHeader = TRUE,
            uiOutput('textbox1')
          ),
          box(width = 12,
            solidHeader = TRUE,
              uiOutput('textbox2')
          )
        ),
        box(
          width = 12,
          solidHeader = TRUE,
          column(12,h4("Select the action you want to perform"),align = "center"),
          tags$br(),tags$br(),
          column(12,h4("---------------"),align = "center"),
          
          column(12,actionButton('cor1', 'Correlation Insights'),align = "center"),
          tags$br(),tags$br(),
          column(12,h4("---------------"),align = "center"),
          
          column(12,actionButton('pop1', 'Population View'),align = "center"),
          tags$br(),tags$br(),
          column(12,h4("---------------"),align = "center"),
          
          #column(12,textInput("patient_text1", label = h5("Enter the Patient ID..."), value = "1"),align = "center"),
          column(12,actionButton('patient1', 'Patient View'),align = "center")
        )
      )
    ),
    tabItem(
      tabName = "cor",
      fluidRow(
        box(title = "Select the columns for the x axis",
            solidHeader = TRUE,
            width = 6,
            uiOutput("select1")),
        box(title = "Select the columns for the y axis",
            solidHeader = TRUE,
            width = 6,
            uiOutput("select2"))),
      fluidRow(
      box(
        title = "Scatter Plot",
        width = 12,
        solidHeader = TRUE,
        textOutput("cortext"),
        htmlOutput('scatter')
      ),
      box(width =12,
          solidHeader = TRUE,
          br(),
          column(12,actionButton('corpop1', 'Proceed to Population View'),align = "center"),
          br(),
          br()
          )
      )
    ),
    
    #The population view allow the business users to view each patient against all other patients based on the risk levels.
    tabItem(tabName = "pop",
            fluidRow(
              box(width = 12,
                  solidHeader = TRUE,
                  h2("Population View",align = "center")),
              box(
                #Dsiplay the population view graph. The slider is provided to allow the user to specify the risk level.
                width = 8,
                solidHeader = TRUE,
                height = 700,
                align = "center",
                #The output graph
                htmlOutput('popv')
                ),
              box(width = 4,
                  solidHeader = TRUE,
                  align = "center",
                  height = 500,
                  br(),
                  br(),
                  h4(
                    "The overview graph uses an attraction metaphor : patients with high values for a given
                    risk factor, relative to other patients in the population, are pulled closer to that
                    risk factor, while patients with low values are pushed away. The size of each risk factor,
                    shown as a circle on the graph, represents its strength of attraction."
                    ,align = "justify"),
                  br(),
                  br(),
                  #The slider input
                  sliderInput('size', "Enter the Risk Threshold:", 1, 99, 1)
              ),
              box(width = 4,
                  solidHeader = TRUE,
                  align = "center",
                  br(),
                  br(),
                  column(12,actionButton('patient2', 'Click to go to the Cluster View'),align = "center"),
                  br(),
                  br(),
                  br(),
                  br()
              )
              )
            ),
    
    #The population view allow the business users to view each patient against all other patients based on the risk levels.
    tabItem(tabName = "clu",
            fluidRow(
              box(width = 12,
                  solidHeader = TRUE,
                  h2("Cluster View",align = "center")),
              box(
                #Dsiplay the population view graph. The slider is provided to allow the user to specify the risk level.
                width = 12,
                solidHeader = TRUE,
                align = "center",
                #The output graph
                htmlOutput('Drivers'),
                htmlOutput('DriverGraphs')
              ),
              box(width = 12,
                  solidHeader = TRUE,
                  align = "center",
                  br(),
                  br(),
                  column(12,actionButton('patient5', 'Click to go to the Patient View'),align = "center"),
                  br(),
                  br(),
                  br(),
                  br()
              )
              )
            ),
    #The patient view shows the important parameter values of a patient and also plots a histogram comparing the
    #Patients values agains the ideal values
    tabItem(tabName = "pat",
            title = "Patient View",
            fluidRow(
              box(solidHeader = TRUE,
                  width = 12,
                  h3("Enter Patient ID",align = "center"),
                  textInput("patient_text5", label = h5(""), value = "1")
              ),
              box(width = 6,
                  solidHeader = TRUE,
                  align = "center",
                  height = 600,
                  h3("Population View",align = "center"),
                  htmlOutput('popv1')
              ),
              box(
                solidHeader = TRUE,
                width = 3,
                #A histogram of the patients vital values against the ideal values
                h3("Actual Values Vs. Minimum Threshold",align = "center"),
                height = 600,
                #uiOutput("vitalColSelect"),
                br(),
                htmlOutput('histold'),
                br()
              ),
              box(
                solidHeader = TRUE,
                width = 3,
                height = 600,
                div(style = "overflow-y:scroll; max-height: 600px",dataTableOutput('colValues'))
              ),
              box(width = 12,
                  solidHeader = TRUE,
                  br(),
                  column(12,actionButton('patient3', 'Click to go to the Intervention View'),align = "center"),
                  br(),
                  br(),
                  br())
              )
            ),
    #The user then proceeds to the intervention view screen. Here the user enters new values for parameters and checks
    #how much the risk reduces by.
    tabItem(tabName = "int",
            title = "Intervention View",
            fluidRow(
              box(width = 6,
                solidHeader = TRUE,
                color = "black",
                h3("Modifiable Variables",align = "center"),
                div(style = "overflow-y:scroll; max-height: 300px", uiOutput("choose_columns2")
                )
              ),
              box(
                solidHeader = TRUE,
                width = 6,
                h3("Enter the Intervention values",align = "center"),
                textInput("patient_text6", label = h5("Patient ID"), value = "1"),
                br(),
                htmlOutput('alltext'),
                column(12,actionButton('patint', 'Click to view change'),align = "center")
              ),
              box(
                solidHeader = TRUE,
                width = 12,
                align = "center",
                h3("Intervention View"),
                htmlOutput('contents3')),
              box(width=12,
                  solidHeader = TRUE,
                  align = "center",
                  h3("Click to go to the Prediction view"),
                  br(),
                  column(12,actionButton('patient4', 'Prediction View'),align = "center"),
                  br())
      )
    ),
    tabItem(tabName = "pred",
            title = "Prediction View",
            fluidRow( 
            box(width = 6,
                solidHeader = TRUE,
                height = 750,
                align = "center",
                h3("Model Accuracy Comparisons",align = "center"),
                htmlOutput('compareAcuuracyGraphs')
            ),
            box(width = 6,
                solidHeader = TRUE,
                h3("Select the Algorithm to use",align = "center"),
                selectInput("algoSelect","",choices = c("Linear Discriminant Analysis","Logistic Regression","Naive Bayes","k Nearest Neighbour","Support Vector Machines","Random Forest"))
            ),
            box(width = 6,
                solidHeader = TRUE,
                h3("Confusion Matrix",align = "center"),
                column(dataTableOutput("confusionMatrix"),width = 12)
            ),
            box(width = 6,
                solidHeader = TRUE,
                h3("Model Metrics",align = "center"),
                column(dataTableOutput("errorMetrics"),width = 12)
            )
         )
      )
   )
)

#Combining the header, sidebr and body.
ui <- dashboardPage(header, sidebar, body,skin = "green")

#shinyApp(ui,server)