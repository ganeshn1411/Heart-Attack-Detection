server <- function(input, output, session) {

  ##################################################################################################
  # functions called in the viewData Page
  ##################################################################################################
  
  # function to read the input file and store it (reactive will store data just like global variables)
  getinputData <- reactive({
    if(is.null(input$fileInput)){
      return(NULL)
    }
    inputFile <- input$fileInput
    inputData <- read.csv(inputFile$datapath,stringsAsFactors = FALSE)
    inputData
  })
  
  #Display the content to the view data page
  output$inputData <- renderDataTable({
    inputContents <- getinputData()
    output <- datatable(inputContents,class = 'cell-border stripe',options = list(scrollX = TRUE,lengthChange = FALSE,searching = FALSE,
                                                                                  initComplete = JS("function(settings, json) {",
                                                                                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}")))
  })
  
  ######################################################################################################
  # functions called in the Data Summary Page
  ######################################################################################################
  
  # Display a datatable showing different numeric summary metrics
  output$numericSummary <- renderDataTable({
    inputData <- getinputData()
    # remove the ID column
    inputData <- inputData[,-1]
    numericSummary <- getNumericSummary(inputData)
    output <- datatable(numericSummary,class = 'cell-border stripe',options=list(paging = FALSE,searching = FALSE,
                                                                                 initComplete = JS("function(settings, json) {",
                                                                                   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}")))
  })
  
  # Display a datatable showing different categorical summary metrics
  output$categoricSummary <- renderDataTable({
    inputData <- getinputData()
    categoricSummary <- getCategoricSummary(inputData)
    output <- datatable(categoricSummary,class = 'cell-border stripe',options=list(paging = FALSE,searching = FALSE,
                                                                                   initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}")))
  })
  
  ######################################################################################################
  # functions called in the exploreData Page
  ######################################################################################################
  
  # function to get the display the number of records in the input file
  output$recordCount <- renderText({
    inputData <- getinputData()
    numRecords <- nrow(inputData)
    printNumRecords <- paste("Number of patient records : ",numRecords)
    printNumRecords
  })
  
  # function to display the number of columns in the input file
  output$attributeCount <- renderText({
    inputData <- getinputData()
    # remove ID column
    numAttributes <- ncol(inputData) - 1
    printNumCols <- paste("Number of attributes : ",numAttributes)
    printNumCols
  })
  
  # function to display a dropdown with options as the list of columns
  output$exploreColumnsSelection <- renderUI({
    inputData <- getinputData()
    inputData <- inputData[,-1]
    listCols <- colnames(inputData)
    selectInput("exploreColSelection","Select the column to explore",listCols)
  })
  
  # fuction that will be called whenever a selection is made in input Colselection dropdown to store the value selected
  getExploreSelectedCol <- reactive({
    input$exploreColSelection
  })
  
  # function that will be called when a selection is made in the above dropdown to display
  # the graphs of distribution of different variables
  output$exploreVarPlot <- renderGvis({
    # get the column that is selected
    exploreSelectedColumn <- getExploreSelectedCol()
    # if data is null, do not show anything
    if(is.null(exploreSelectedColumn)){
      textOutput("")
    }
    # if data is present
    else{
      # Progress bar -> to show progress of steps being performed
      withProgress(message = 'Gettring required Data',value = 0.1,{
        Sys.sleep(0.1)
        inputData <- getinputData()
        inputCols <- colnames(inputData)
        inputColType <- getColType(inputData)
        # get the data of the column that is selected
        inputData <- inputData[,exploreSelectedColumn]
        #get the column type of selected column
        selectedColType <- inputColType[which(inputCols==exploreSelectedColumn)]
      })
      # if selected column is categorcal, display 
      if(selectedColType == "Categorical"){
        withProgress(message = 'Generating the plot',value = 0.1,{
          Sys.sleep(0.25)
          reqData <- as.data.frame(round(prop.table(table(inputData))*100,2))
          reqData$Count.style = c('#FF4000','#0073B7') 
          colnames(reqData) <- c(exploreSelectedColumn,"Percent","Count.style")
          gvisColumnChart(reqData,xvar = exploreSelectedColumn,yvar = c("Percent","Count.style"),
                          options= list(height = 400,width = 700,legend = 'none',title = paste("Distribution of ",exploreSelectedColumn),
                                        vAxis="{title:'Percentage'}",hAxis = paste("{title:'",exploreSelectedColumn,"'}")))
        })
      }
      else{
        withProgress(message = 'Generating the plot',value = 0.1,{
          Sys.sleep(0.25)
          colGvis <- gvisHistogram(data.frame(inputData),options = list(height = 400,legend = 'none',
                                                  title = paste("Distribution of ",exploreSelectedColumn),
                                                  vAxis="{title:'Count'}",hAxis = paste("{title:'",exploreSelectedColumn,"'}")))
        })
      }
    }
  })
  
  ##################################
  # functions in the bivariate tab
  ##################################
  
  # display dropdown of all columns
  output$columnxSelection <- renderUI({
    inputData <- getinputData()
    inputCols <- colnames(inputData)
    # remove ID column
    inputCols <- inputCols[-1]
    # display dropdown with first variable selected by default
    selectInput("columnx","Select the variable for x axis",inputCols,selected = inputCols[1])
  })
  
  # get the selected first column
  getColumnx <- reactive({
    input$columnx
  })
  
  # display dropdown of all columns
  output$columnySelection <- renderUI({
    inputData <- getinputData()
    inputCols <- colnames(inputData)
    # remove ID column
    inputCols <- inputCols[-1]
    # display dropdown with first variable selected by default
    selectInput("columny","Select the variable for y axis",inputCols,selected = inputCols[1])
  })
  
  # get the selected first column
  getColumny <- reactive({
    input$columny
  })
  
  output$exploreBiPlot <- renderGvis({
    columnx <- getColumnx()
    columny <- getColumny()
    if(is.null(columnx) || is.null(columny)){textOutput("")}
    else{
      inputData <- getinputData()
      inputColType <- getColType(inputData)
      inputCols <- colnames(inputData)
      # get the data and type of column 1
      columnxData <- inputData[,which(inputCols==columnx)]
      columnxType <- inputColType[which(inputCols==columnx)]
      # get the data ans type of column 2
      columnyData <- inputData[,which(inputCols==columny)]
      columnyType <- inputColType[which(inputCols==columny)]
      # handling the 4 cases of data types
      # if both are numerical, use scatter plot
      if(columnxType=="Numerical" && columnyType=="Numerical"){
        output <- gvisScatterChart(data.frame(columnxData,columnyData),options = list(height = 400,legend = 'none',
                                                                                      title = paste("Distribution of ",columny," against ",columnx),
                                                                                      vAxis=paste("{title:'",columny,"'}"),hAxis = paste("{title:'",columnx,"'}")))
      }
      # if both are categorical, use column chart
      else if(columnxType=="Categorical" && columnyType=="Categorical"){
        prop <- round(prop.table(table(data.frame(columnxData,columnyData)))*100,2)
        propData <- data.frame(row.names(prop),prop[,1],prop[,2])
        colnames(propData) <- c("Output","No","Yes")
        output <- gvisColumnChart(propData,options = list(height = 400,legend = 'none',width = 700,
                                                          title = paste("Distribution of ",columny," against ",columnx),
                                                          vAxis=paste("{title:'",columny,"(Percentage)'}"),hAxis = paste("{title:'",columnx,"'}")))
      }
      # if x is numerical, y is categorical, use bar chart of average
      else if(columnxType=="Numerical" && columnyType=="Categorical"){
        if(columny=="Gender"){
          colNo <- as.numeric(columnxData[which(columnyData=="Female")])
          colYes <- as.numeric(columnxData[which(columnyData=="Male")])
          variable <- c("Female","Male")
        }
        else{
          colNo <- as.numeric(columnxData[which(columnyData=="No")])
          colYes <- as.numeric(columnxData[which(columnyData=="Yes")])
          variable <- c("No","Yes")
        }  
        meanNo <- mean(colNo)
        meanYes <- mean(colYes)
        meanVals <- c(meanNo,meanYes)
        color <- c('color: red','color: green')
        outputData <- data.frame(variable,meanVals,color)
        colnames(outputData) <- c("Variable","Mean","color.style")
        output <- gvisBarChart(outputData,yvar = c("Mean","color.style"),xvar = "Variable",options = list(height = 400,legend = 'none',
                                                                                                          title = paste("Distribution of ",columny," against ",columnx),
                                                                                                          vAxis=paste("{title:'",columny,"'}"),hAxis = paste("{title:' Average",columnx,"'}")))
        
      }
      # if x is categorical, y is numerical, using candlestick chart
      else if(columnxType=="Categorical" && columnyType=="Numerical"){
        #get the metrics(low open close high) when categorical column is no
        if(columnx=="Gender"){
          colNo <- as.numeric(columnyData[which(columnxData=="Female")])
          colYes <- as.numeric(columnyData[which(columnxData=="Male")])
          variable <- c("Female","Male")
        }
        else{
          colNo <- as.numeric(columnyData[which(columnxData=="No")])
          colYes <- as.numeric(columnyData[which(columnxData=="Yes")])
          variable <- c("No","Yes")
        }
        lowNo <- min(colNo)
        highNo <- max(colNo)
        openNo <- mean(colNo) - sd(colNo)
        closeNo <- mean(colNo) + sd(colNo)
        # get the metrics when selected category is yes
        lowYes <- min(colYes)
        highYes <- max(colYes)
        openYes <- mean(colYes) - sd(colYes)
        closeYes <- mean(colYes) + sd(colYes)
        # combine the yes and no categories
        low <- c(lowNo,lowYes)
        open <- c(openNo,openYes)
        close <- c(closeNo,closeYes)
        high <- c(highNo,highYes)
        outputData <- data.frame(variable,low,open,close,high)
        output <- gvisCandlestickChart(outputData,options = list(height = 400,legend = 'none',width = 700,
                                                                 title = paste("Distribution of ",columny," against ",columnx),
                                                                 vAxis=paste("{title:'",columny,"'}"),hAxis = paste("{title:'",columnx,"'}")))
      }
    }
  })
  
  
  #################################################################################################################
  # functions within the correlation Insights page
  #################################################################################################################
  
  # plot the corrplot to show correlation among all the columns
  output$correlationPlot <- renderPlot({
    inputData <- getinputData()
    inputData <- inputData[,-1]
    inputData <- getNumericalInputData(inputData)
    # get the set of correlations of each column with every other column
    correlations <- cor(inputData)
    corrPlot <- corrplot(correlations,type="lower",method = "color")
    corrPlot
  })
  
  output$outputColCorList <- renderText({
    inputData <- getinputData()
    lastCol <- colnames(inputData)[ncol(inputData)]
    return(paste("Correlation with ",lastCol))
  })
  
  
  output$correlationList <- renderDataTable({
    inputData <- getinputData()
    inputData <- getNumericalInputData(inputData)
    outputCol <- inputData[ncol(inputData)]
    inputData <- inputData[,-c(1,ncol(inputData))]
    inputColumns <- colnames(inputData)
    correlations <- sapply(inputData,function(x){return(cor(x,outputCol))})
    corrList <- head(data.frame(correlations[order(correlations,decreasing = TRUE)]),15)
    colnames(corrList) <- c("Correlation")
    output <- datatable(corrList,class = 'cell-border stripe',options = list(lengthChange = FALSE,searching = FALSE,paging=FALSE,
                                                                             columnDefs = list(list(className = 'dt-center', targets = 0:1)),
                                                                             initComplete = JS("function(settings, json) {",
                                                                                                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}")))%>% 
      formatStyle('Correlation',textAlign = 'center')
    
  })
  
  # function to display a dropdown of all numerical columns to select x axis
  output$selectx <- renderUI({
    inputData <- getinputData()
    # remove ID column
    inputData <- inputData[,-1]
    numericalData <- getNumericalData(inputData)
    numericalColumns <- colnames(numericalData)
    selectInput("selectx", label = "Select the x-axis column",choices = numericalColumns)
  })
  
  # function that will get the selected X value
  getX <- reactive({
    input$selectx
  })
  
  # function to display a dropdown of all numerical columns to select y axis
  output$selecty <- renderUI({
    inputData <- getinputData()
    # remove ID column
    inputData <- inputData[,-1]
    numericalData <- getNumericalData(inputData)
    numericalColumns <- colnames(numericalData)
    selectInput("selecty", label = "Select the y-axis column",choices = numericalColumns)
  })
  
  # function that will get the selected Y value
  getY <- reactive({
    input$selecty
  })
  
  # function to calculate correlation between the selected axis
  output$correlationValue <- renderText({
    inputData <- getinputData()
    x <- getX()
    y <- getY()
    # if data is null, do not show anything
    if(is.null(x)){paste("")}
    # get the subset of only x and y
    else{
      selectedSubset <- subset(inputData,select = c(x,y))
      # function cor will calculate the correlation between 2 columns
      output <- paste("The correlation between ",x,"and",y,"is",round(cor(as.numeric(selectedSubset[,1]),as.numeric(selectedSubset[,2])),2))
    }
  })
  
  # scatter plot to show distribution and correlation and distribution of 2 columns
  output$scatterPlot <- renderGvis({
    inputData <- getinputData()
    patientIDCol <- inputData[,1]
    x <- getX()
    y <- getY()
    # if data is null, do not show anything
    if(is.null(x)){textOutput("")}
    else{
      withProgress(message = "Getting the data ready",value = 0.1,{
        Sys.sleep(0.1)
        # get the selected column data
        selectedSubset <- subset(inputData,select = c(x,y))
        # get the patient IDs
        colSubset <- cbind(selectedSubset,patientIDCol)
        # change format of Patient ID to display
        colSubset[,3] <- paste(" Patient ID : ",colSubset[,3])
        colnames(colSubset) <- c("Xaxis","Yaxis","pop.html.tooltip")
    })
      withProgress(message = "Generating the plot",value = 0.1,{
        Sys.sleep(0.25)
        output <- gvisScatterChart(colSubset,options = list(tooltip="{isHtml:'True'}",trendlines="0",legend = "none", height = 600,
                                   title = paste("Scatter plot of ",x," and ",y),
                                   hAxis = paste("{title : '",x,"'}"),vAxis = paste("{title : '",y,"'}")))
        })
      }
    })
  
  #################################################################################################################
  # functions of the Select Variable
  #################################################################################################################
  
  output$outputVariables <- renderUI({
    inputData <- getinputData()
    inputColType <- getColType(inputData)
    outputVariables <- colnames(inputData)[which(inputColType=="Categorical")]
    radioButtons("outputVariable", "",choices=outputVariables,selected = outputVariables[length(outputVariables)])
  })
  
  # function that gets the selected risk variables
  getOutputVariable <- reactive({
    input$outputVariable
  })
  
  # function to display columns to select risk variables
  output$selectRiskVariables <- renderUI({
    inputData <- getinputData()
    riskVariables <- colnames(inputData)[-1]
    riskVariables <- riskVariables[-which(riskVariables==getOutputVariable())]
    checkboxGroupInput("riskVariables","", choices  = riskVariables, selected = riskVariables)
  })
  
  # function that gets the selected risk variables
  getRiskVariables <- reactive({
    input$riskVariables
  })
  
  # function that displays risk variable columns to select for monitoring
  output$selectMonitoredVariables <- renderUI({
    riskVariables <- getRiskVariables()
    if(is.null(riskVariables)){
      textOutput("")
    }
    else{
      checkboxGroupInput("monitoredVariables","", choices  = riskVariables)
    }
  })
  
  # function that gets the selected monitored variables
  getMonitoredVariables <- reactive({
    input$monitoredVariables
  })
  
  output$monitoredMinimum <- renderUI({
    # get the variables that are selected for monitoring
    monitoredVariables <- getMonitoredVariables()
    # if no variable is selected, show nothing
    if (is.null(monitoredVariables)) {
      textOutput("")
    } 
    # if variable is selected
    else{
      inputData <- getinputData()
      # get the subset of selected columns
      selectedSubset <- subset(inputData,select = monitoredVariables)
      # get the type of columns selected
      inputColType <- getColType(selectedSubset)
      # if columns selected are numerical, display inputbox for minimumvalue
      output <- lapply(1:length(inputColType), function(i) {
        if (inputColType[i] == "Numerical") {
          numericInput(paste("minimum",i,sep = "_"), label = paste("Minimum ",monitoredVariables[i]), value = 0,min = 0,max = 1000)
        }
      })
      output
    }
  })
  
  output$monitoredMaximum <- renderUI({
    # get the variables that are selected for monitoring
    monitoredVariables <- getMonitoredVariables()
    # if no variable is selected, show nothing
    if (is.null(monitoredVariables)) {
      textOutput("")
    } 
    # if variable is selected
    else{
      inputData <- getinputData()
      # get the subset of selected columns
      selectedSubset <- subset(inputData,select = monitoredVariables)
      # get the type of columns selected
      inputColType <- getColType(selectedSubset)
      # if columns selected are numerical, display inputbox for minimumvalue
      output <- lapply(1:length(inputColType), function(i) {
        if (inputColType[i] == "Numerical") {
          numericInput(paste("maximum",i,sep = "_"), label = paste("Maximum ",monitoredVariables[i]), value = 0,min = 0,max = 1000)
        }
      })
      output
    }
  })
  
  output$monitoredIdealCat <- renderUI({
    # get the variables that are selected for monitoring
    monitoredVariables <- getMonitoredVariables()
    # if no variable is selected, show nothing
    if (is.null(monitoredVariables)) {
      textOutput("")
    } 
    # if variable is selected
    else{
      inputData <- getinputData()
      # get the subset of selected columns
      selectedSubset <- subset(inputData,select = monitoredVariables)
      # get the type of columns selected
      inputColType <- getColType(selectedSubset)
      # if columns selected are numerical, display inputbox for minimumvalue
      output <- lapply(1:length(inputColType), function(i) {
        if (inputColType[i] == "Categorical") {
          selectInput(paste("idealCat",i,sep = "_"), label = monitoredVariables[i], choices = list("Yes"="Yes", "No"="No"),selected = "Yes")
        }
      })
      output
    }
  })
  
  #################################################################################################################
  # functions of the population view
  #################################################################################################################
  
  # function that will get the value of slider selected
  sliderSelection <- reactive({
    input$sliderSelection
  })
  
  # function that will display population plot
  output$populationGraph <- renderGvis({
    # get the selected slider value
    sliderSelected <- sliderSelection()
    # get the risk variables selected
    riskVariables <- getRiskVariables()
    if(is.null(riskVariables)){textOutput("")}
    monitoredVariables <- getMonitoredVariables()
    inputData <- getinputData()
    # get the selected output column
    outputVariable <- getOutputVariable()
    outputCol <- inputData[,which(colnames(inputData)==outputVariable)]
    inputData <- getNumericalInputData(inputData)
    IDCol <- inputData[,1]
    withProgress(message = 'Getting required Data',value=0.1,{
      Sys.sleep(0.25)
      # get the subset of inputData that are selected risk variables
      riskData <- subset(inputData,select = riskVariables)
      # get set of anchors that are scaled by variance
      anchorCoordinates <- getVarSclaedAnchorCoordinates(riskData,outputCol,monitoredVariables)
      # get the coordinates of each patient
      patientCoordinates <- getSelectedPatientCoordinates(riskData,IDCol,outputCol,sliderSelected)
      # combine patient coordinates and anchor coordinates
      combinedCoordinates <- rbind(patientCoordinates,anchorCoordinates)
      # sort the data by type
      combinedCoordinates <- combinedCoordinates[order(as.character(combinedCoordinates[,4])),]
      colnames(combinedCoordinates) <- c("ID","PCA","LDA","Type","Influence")
    })
    withProgress(message = 'Generating Plot',value=0.1,{
      Sys.sleep(0.25)
      # if data includes both low risk and high risk and monitored variable
      if(("Monitored Feature" %in% unique(combinedCoordinates$Type)) && ("Low Risk" %in% unique(combinedCoordinates$Type))){
        # display bubble chart of patient and anchor coordinates
        popGraph <- gvisBubbleChart(combinedCoordinates,colorvar = "Type",sizevar="Influence",
                                   options = list(height = 800,
                                   title = "Population View",
                                   vAxis = "{title:'<---   Low Risk Region.................High Risk Region   --->'}",
                                   hAxis = "{title: 'Population Variation'}",
                                   colors = "['#819FF7','#FE642E','#58FA82','#FF69B4']"))
      }
      else if(("Monitored Feature" %in% unique(combinedCoordinates$Type))){
        popGraph <- gvisBubbleChart(combinedCoordinates,colorvar = "Type",sizevar="Influence",
                                    options = list(height = 800,
                                                   title = "Population View",
                                                   vAxis = "{title:'<---   Low Risk Region.................High Risk Region   --->'}",
                                                   hAxis = "{title: 'Population Variation'}",
                                                   colors = "['#819FF7','#FE642E','#FF69B4']"))
      }
      else if(("Low Risk" %in% unique(combinedCoordinates$Type))){
        popGraph <- gvisBubbleChart(combinedCoordinates,colorvar = "Type",sizevar="Influence",
                                    options = list(height = 800,
                                                   title = "Population View",
                                                   vAxis = "{title:'<---   Low Risk Region.................High Risk Region   --->'}",
                                                   hAxis = "{title: 'Population Variation'}",
                                                   colors = "['#819FF7','#FE642E','#58FA82']"))
      }
      # if data only includes high risk and no monitored variable
      else{
      popGraph <- gvisBubbleChart(combinedCoordinates,colorvar = "Type",sizevar="Influence",
                                  options = list(height = 800,
                                                 title = "Population Plot",
                                                 vAxis = "{title:'<- Low Risk Region.................High Risk Region ->'}",
                                                 hAxis = "{title: 'Population Variation'}",
                                                 colors = "['#819FF7','#FE642E']"))
      
      }
    })
    withProgress(message = 'Displaying Plot',value = 0.1,{
      popGraph
    })
})          

  #################################################################################################################
  # functions of the risk based cluster view
  #################################################################################################################
  
  # Function that will get the maximum risk variable 
  output$riskVariableSelection <- renderUI({
    inputData <- getinputData()
    # remove the ID column
    IDCol <- inputData[,1]
    # get the output variable selected
    outputCol <- getOutputVariable()
    # get the selected risk variable
    riskVariables <- getRiskVariables()
    # get the subset of data that are the selected risk variables
    riskData <- subset(inputData,select = riskVariables)
    # get the column type of each risk Data
    inputColType <- getColType(riskData)
    # column names of risk data
    inputCols <- colnames(riskData)
    # get the numerical converted risk data
    riskData <- getNumericalInputData(riskData)
    # scale the different columns of risk data
    riskData <- scale(riskData)
    # get the mean of each column of risk data
    meanVals <- getNumericalMetric(riskData,"mean")
    # convert categorical variables as 0
    meanVals <- as.numeric(ifelse(meanVals==" ","0",meanVals))
    # create a blank vector for columns of maximum risk Data
    maxValsList <- character()
    # for each row of risk Data
    for(i in 1:nrow(riskData)){
      # get the column that has maximum difference from average
      maxCol <- getMaxCol(riskData,i,inputColType,inputCols,meanVals)
      # get the list of columns that are maximum from average
      maxValsList <- c(maxValsList,maxCol)
    }
    # remove the catgerocial columns
    if("0" %in% maxValsList){
      maxValsList <- maxValsList[-which(maxValsList=="0")]
    }
    # get the unique of the maximum difference columns
    colsToSelect <- unique(maxValsList)
    selectInput("riskDriver",'Select the driving factor',colsToSelect)
  })
  
  # get the selected risk variable
  getRiskDriver <- reactive({
    input$riskDriver
  })
  
  # function 
  output$riskClusterPlot <- renderGvis({
    riskDriver <- getRiskDriver()
    if(is.null(riskDriver)){textOutput("")}
    else{
        inputData <- getinputData()
        IDCol <- inputData[,1]
        outputCol <- inputData[,ncol(inputData)]
        withProgress(message = 'Getting required Data',value=0.1,{
        Sys.sleep(0.25)
        riskVariables <- getRiskVariables()
        riskData <- subset(inputData,select = riskVariables)
        inputColType <- getColType(riskData)
        inputCols <- colnames(riskData)
        riskData <- getNumericalInputData(riskData)
        riskData <- scale(riskData)
        patientCoordinates <- getPatientCoordinates(riskData,IDCol,outputCol)
        meanVals <- getNumericalMetric(riskData,"mean")
        meanVals <- ifelse(meanVals==" ","0",meanVals)
        meanVals <- as.numeric(meanVals)
        maxValsList <- character()
        for(i in 1:nrow(riskData)){
          maxCol <- getMaxCol(riskData,i,inputColType,inputCols,meanVals)
          maxValsList <- c(maxValsList,maxCol)
        }
        patientCoordinates[,4] <- maxValsList
        patientCoordinates <- patientCoordinates[which(patientCoordinates[,4]==riskDriver),]
        patientCoordinates <- patientCoordinates[order(as.character(patientCoordinates[,4])),]
        colnames(patientCoordinates) <- c("ID","PCA","LDA","Type","Influence")
        for(i in 1:nrow(patientCoordinates)){
          patientCoordinates[i,4] <- ifelse(patientCoordinates[i,3]>0,"High Risk","Low Risk")
        }
        colorSet <- getClusterColors(patientCoordinates)
        patientCoordinates <- patientCoordinates[,-5]
        patientCoordinates <- patientCoordinates[order(patientCoordinates[,4]),]
      })
      withProgress(message = 'Generating Plot',value=0.1,{
      Sys.sleep(1.25)
        popGraph <- gvisBubbleChart(patientCoordinates,colorvar = "Type",
                                    options = list(height = 800, sizeAxis ='{minValue:0,  maxSize:5}',
                                                   vAxis = "{title:'<- Low Risk Region.................High Risk Region ->',minValue:-2,  maxValue:2}",
                                                   hAxis = "{title: 'Population Variation', minValue:-2,  maxValue:2}",
                                                   title = "Population Plot", colors = colorSet,
                                                   legend='none'))
      })
      popGraph
    }
  })
  
  
  #################################################################################################################
  # functions of the k means cluster view
  #################################################################################################################
  
  getk <- reactive({
    input$k
  })
  
  output$kMeansClusterPlot <- renderGvis({
    k <- getk()
    if(is.null(k)){
      textOutput("")
    }
    else{
      withProgress(message = 'Getting required Data',value=0.1,{
        Sys.sleep(0.25)
        inputData <- patientData()
        IDCol <- inputData[,1]
        outputCol <- inputData[,ncol(inputData)]
        selectedRiskVariables <- riskVariables()
        riskData <- subset(inputData, select = selectedRiskVariables)
        patientCoordinates <- getPatientCoordinates(riskData,IDCol,outputCol)
        riskData <- getScaledInput(riskData)
        fit <- kmeans(riskData, k)
        patientCoordinates[,4] <- as.character(fit$cluster)
        patientCoordinates <- patientCoordinates[order(as.character(patientCoordinates[,4])),]
        colnames(patientCoordinates) <- c("ID","PCA","LDA","Cluster","Influence")
        patientCoordinates <- patientCoordinates[,-5]
      })
      withProgress(message = 'Generating Plot',value=0.1,{
        Sys.sleep(1.25)
        popGraph <- gvisBubbleChart(patientCoordinates,colorvar = "Cluster",
                                    options = list(width = 1300, height = 800, vAxis='{minValue:-2,  maxValue:2}',
                                                   hAxis='{minValue:-2,  maxValue:2}',
                                                   title = "Population Plot",
                                                   vAxis = "{title:'<- Low Risk Region.................High Risk Region ->'}",
                                                   hAxis = "{title: 'Population Variation'}",
                                                   legend='none'))
      })
      popGraph
    }
  })
  
  #################################################################################################################
  # functions of Patient View
  #################################################################################################################
  
  # Display Text Input for users to enter Patient ID
  output$inputPatientID <- renderUI({
    inputData <- getinputData()
    # display the patient ID of the first patient by default
    firstPatientID <- inputData[1,1]
    textInput("patientIDInput", label = "Enter Patient ID", value = firstPatientID)
  })
  
  # function that gets the updated patient ID that is entered
  getPatientID <- reactive({
    input$patientIDInput
  })
  
  output$patientGraph <- renderGvis({
    patientID <- getPatientID()
    riskVariables <- getRiskVariables()
    if(is.null(patientID) || is.null(riskVariables)){
      textOutput("")
    }
    else{
      withProgress(message = 'Getting required Data',value=0.1,{
        monitoredVariables <- getMonitoredVariables()
        inputData <- getinputData()
        riskData <- subset(inputData,select = riskVariables)
        inputColType <- getColType(riskData)
        riskData <- getNumericalInputData(riskData)
        IDCol <- inputData[,1]
        outputVariable <- getOutputVariable()
        outputCol <- inputData[,which(colnames(inputData)==outputVariable)]
        patientCoordinates <- getPatientCoordinates(riskData,IDCol,outputCol)
        anchorCoordinates <- getPatAnchorCoordinates(riskData,IDCol,outputCol,patientID,inputColType,monitoredVariables)
        combinedCoordinates <- rbind(patientCoordinates,anchorCoordinates)
        combinedCoordinates[,4] <- as.character(combinedCoordinates[,4])
        combinedCoordinates[combinedCoordinates[,1]==patientID,4] <- "Selected Patient"
        combinedCoordinates <- combinedCoordinates[order(as.character(combinedCoordinates[,4])),]
        colnames(combinedCoordinates) <- c("ID","PCA","LDA","Type","Influence")
      })
      if("Monitored Feature" %in% unique(combinedCoordinates$Type)){
        withProgress(message = 'Getting required Data',value=0.1,{
          output <- gvisBubbleChart(combinedCoordinates,sizevar = "Influence",colorvar = "Type",
                                  options = list(height = 700,
                                                 title = "Patient View",
                                                 vAxis = "{title:'<- Low Risk Region.................High Risk Region ->'}",
                                                 hAxis = "{title: 'Population Variation'}",
                                                 colors = "['#819FF7','#FE642E','#58FA82','#FF69B4','#000000']"))
        })
      }
      else{
        withProgress(message = 'Getting required Data',value=0.1,{
          output <- gvisBubbleChart(combinedCoordinates,sizevar = "Influence",colorvar = "Type",
                                options = list(height = 700,
                                               title = "Patient View",
                                               vAxis = "{title:'<- Low Risk Region.................High Risk Region ->'}",
                                               hAxis = "{title: 'Population Variation'}",
                                               colors = "['#819FF7','#FE642E','#58FA82','#000000']"))
          })
        }
      }
    })
  
  # Display a histogram showing comparison of actual and Ideal values for numerical columns
  output$actualIdealHist <- renderGvis({
    # get the patient ID of the selected patient
    patientIDSelected <- getPatientID()
    monitoredVariables <- getMonitoredVariables()
    # patient ID or selected Variables are null, display blank
    if(is.null(patientIDSelected) || is.null(monitoredVariables)){textOutput("")}
    # the variables are not null
    else{
      inputData <- getinputData()
      inputCols <- colnames(inputData)
      # get the ideal values entered by calling the helper function
      idealValues <- getIdealValues(inputData)
      # select only numerical values -> ones where ideal cat is ""
      idealValues <- idealValues[idealValues[,3]=="",c(1,2)]
      # get the set of selected variables
      monitoredVariables <- getMonitoredVariables()
      # add Patient ID to the selected variables
      monitoredVariables <- c(monitoredVariables,inputCols[1])
      # select only the columns selected for monitoring
      selectedSubset <- subset(inputData,select = monitoredVariables)
      # get column type of each column
      subsetColType <- getColType(selectedSubset)
      # select the numerical columns
      selectedSubset <- subset(selectedSubset,select = which(subsetColType=="Numerical"))
      # get the Patient's record of the selected columns
      actualVals <- unlist(selectedSubset[selectedSubset[,ncol(selectedSubset)]==patientIDSelected,])
      actualVals <- actualVals[-length(actualVals)]
      variables <- row.names(idealValues)
      minimumVals <- idealValues[,1]
      maximumVals <- idealValues[,2]
      # dataframe combining all columns
      actualVsThreshold <- data.frame(variables,minimumVals,actualVals,maximumVals)
      colnames(actualVsThreshold) <- c("Variable","Minimum","Actual","Maximum")
      gvisColumnChart(actualVsThreshold,
                        options = list(height = 500,series = "[{color:'#3c8dbc'},{color:'#dc3912'},{color:'#228b22'}]"))
    }
  })
  
  # dispaly a datatable that shows a comparison of actual and Ideal values
  output$actualIdealTable <- renderDataTable({
    # get the ID of patient selected
    patientIDSelected <- getPatientID()
    # get the set of variables selected to monitor
    monitoredVariables <- getMonitoredVariables()
    # if Patient ID or Variables selected are none, display blank
    if(is.null(monitoredVariables) || is.null(patientIDSelected)){output <- data.frame()}
    # if selected variables are not blank
    else{
      inputData <- getinputData()
      inputCols <- colnames(inputData)
      inputColType <- getColType(inputData)
      # get the data of the selected patient
      inputData <- inputData[inputData[,1] == patientIDSelected,]
      actualVals <- unlist(inputData)
      for(i in 1:length(inputColType)){
        if(inputColType[i]=="Numerical"){
          actualVals[i] <- round(as.numeric(actualVals[i]),2)
        }
      }
      # get the ideal values using the helper function
      idealValues <- getIdealValues(inputData)
      # create a dataframe of actual values and dummy column for ideal values
      outputData <- data.frame(actualVals,rep("",length(inputCols)),stringsAsFactors=FALSE)
      colnames(outputData) <- c("Actual", "Ideal")
      for(i in 1:nrow(outputData)){
        # if column is Categorical
        if(inputColType[i] == "Categorical"){
          # if value is 0, display No
          if(outputData[i,2]==0){outputData[i,1] <- 'No'}
          # if value is 1, display Yes
          else{outputData[i,1] <- 'Yes'}
        }
      }
      for(i in 1:nrow(outputData)){
        # get the ith columnname
        entry <- row.names(outputData)[i]
        # if the vale is part of column that is selected to be monitored
        if(entry %in% row.names(idealValues)){
          # If it is a categorical Column -> when ideal Cat is ""
          if(idealValues[entry,]$idealCat!=""){
            # display the value in the idealCat column
            outputData[i,2] <- as.character(idealValues[entry,]$idealCat)}
          # if it is numerical Column
          else{
            # get the mnimum and maximum values
            minimum <- as.character(idealValues[entry,]$minimumVals)
            maximum <- as.character(idealValues[entry,]$maximumVals)
            # if minimum is 0, display <maximum
            if(minimum==0){outputData[i,2] <- paste("<=",maximum)}
            # if maximum is 0, display > minimum
            else if(maximum==0){outputData[i,2] <- paste(">=",minimum)}
            # if both exist, display minimum - maximum
            else{outputData[i,2] <- paste(minimum,"-",maximum)}
          }
        }
      }
      # remove the ID column
      outputData <- outputData[-1,]
      # reorder the dataframe to show the monitored column on top
      outputData <- outputData[order(outputData[,2],decreasing = TRUE),]
      output <- DT::datatable(data = outputData,class = 'cell-border stripe',options = list(searching = FALSE,lengthChange = FALSE,
            # call Javascript code to give color codes based on actual and ideal values
            rowCallback = DT::JS('function(row,data){
                          //If selected variable is Categorical and Ideal is equal to actual => highlight in green
                          if(data[2]!= "" && data[1] == data[2] && isNaN(parseFloat(data[1]))){$("td", row).css("background", "#83F52C");}
                          //If selected variable is Categorical and Ideal is not equal to actual => highlight in green
                           else if(data[2]!= "" && data[1] != data[2] && isNaN(parseFloat(data[1]))){$("td", row).css("background", "#ff7f7f");}
                           // If data is numeric
                           else if(data[2]!=""){
                              // If it contains < Symbol
                              if(data[2].indexOf("&lt;")>=0){
                                var maximum = parseFloat(data[2].split("=")[1])
                                if(parseFloat(data[1])<=maximum){$("td", row).css("background", "#83F52C");}
                                else{$("td", row).css("background", "#ff7f7f");}
                              }
                              // If it contains > Symbol
                              else if(data[2].indexOf("&gt;")>=0){
                                var minimum = parseFloat(data[2].split("=")[1])
                                if(parseFloat(data[1])>=minimum){$("td", row).css("background", "#83F52C");}
                                else{$("td", row).css("background", "#ff7f7f");}
                              }
                              // If it contains - Symbol
                              else if(data[2].indexOf("-")>=0){
                                var minimum = parseFloat(data[2].split("-")[0])
                                var maximum = parseFloat(data[2].split("-")[1])
                                var actual = parseFloat(data[1])
                                if(actual>=minimum && actual <= maximum){$("td", row).css("background", "#83F52C");}
                                else{$("td", row).css("background", "#ff7f7f");}
                              }
                            }
                           }')))
      }
  })
  
  
  #################################################################################################################
  # functions in the Intervention View
  #################################################################################################################
  
  output$modifiableVariableSelection <- renderUI({
    riskVariables <- getRiskVariables()
    checkboxGroupInput("modifiableVariables", "Choose columns", choices  = riskVariables)
  })
  
  getModifiableVariables <- reactive({
    input$modifiableVariables
  })
  
  # Display Text Input for users to enter Patient ID
  output$interventionPatientID <- renderUI({
    patientID <- getPatientID()
    if(is.null(patientID)){
      inputData <- getinputData()
      patientID <- inputData[1,1]
    }
    textInput("interventionPatientIDInput", label = "Enter Patient ID", value = patientID)
  })
  
  getInterventionPatientID <- reactive({
    input$interventionPatientIDInput
  })
  
  output$interventionfields <- renderUI({
    modifableVariables <- getModifiableVariables()
    patientID <- getInterventionPatientID()
    if(is.null(modifableVariables) || is.null(patientID)){
      textOutput("")
    }
    else{
      inputData <- getinputData()
      inputData <- inputData[inputData[,1]==patientID,]
      inputColType <- getColType(inputData)
      output <- lapply(1:length(modifableVariables), function(i) {
          colType <- inputColType[which(colnames(inputData)==modifableVariables[i])]
          actualval <- inputData[,which(colnames(inputData)==modifableVariables[i])]
          if(colType=="Numerical"){
            numericInput(paste("modifiable",i,sep = "_"), label = paste(modifableVariables[i]," | Actual : ",round(as.numeric(actualval),2)), value = round(as.numeric(actualval),2),min = 0,max = 1000)
          }
          else{
            selectInput(paste("modifiable",i,sep = "_"),label = paste(modifableVariables[i]," | Actual : ",actualval), c("Yes" = 1,"No" = 0),selected = actualval)
          }
        })
      output
    }
  })
  
  getInterventionValues <- function(modifableVariables){
    interventionValues <- numeric()
    for(i in 1:length(modifableVariables)){
      interventionValues <- c(interventionValues,eval(parse(text = paste("input$modifiable_",i,sep = ""))))
    }
    interventionValues
  }
  
  getPatIntCoords <- eventReactive(input$interventionButton,{
    patientID <- getInterventionPatientID()
    modifableVariables <- getModifiableVariables()
    if(is.null(patientID) || is.null(modifableVariables)){
      return(NULL)
    }
    else{
      interventionValues <- getInterventionValues(modifableVariables)
      inputData <- getinputData()
      inputData <- getNumericalInputData(inputData)
      IDcol <- inputData[,1]
      patientRow <- inputData[which(IDcol==patientID),]
      newPatientRow <- patientRow
      for(i in 1:length(modifableVariables)){
        newPatientRow[,which(colnames(newPatientRow)==modifableVariables[i])] <- interventionValues[i]
      }
      inputData <- rbind(inputData,newPatientRow)
      outputCol <- inputData[,ncol(inputData)]
      inputData <- data.frame(data.matrix(inputData))
      inputData <- as.data.frame(scale(inputData))
      riskVariables <- getRiskVariables()
      riskData <- subset(inputData,select = riskVariables)
      PCALDA <- getPCALDA(riskData,outputCol)
      patientCoords <- as.matrix(riskData) %*% as.matrix(PCALDA)
      # scale patient Coordinates between -1 and 1
      patientCoords[,1] <- scaleColumn(patientCoords[,1],-1,1)
      patientCoords[,2] <- scaleColumn(patientCoords[,2],-1,1)
      oldPatientVals <- patientCoords[which(IDcol==patientID),]
      newPatientVals <- patientCoords[nrow(patientCoords),]
      oldValCoords <- matrix(c("Old",oldPatientVals[1],oldPatientVals[2],"Old",10),nrow = 1)
      newValCoords <- matrix(c("New",newPatientVals[1],newPatientVals[2],"New",10),nrow = 1)
      userCoords <- as.data.frame(rbind(oldValCoords,newValCoords),stringsAsFactors = FALSE)
      for( i in 1:nrow(userCoords)){
        userCoords[i,4] <- ifelse(as.numeric(userCoords[i,3])>0,"High Risk","Low Risk")
      }
      colnames(userCoords) <- c("ID","X","Y","Color","Size")
      return(userCoords)
    }
  })
  
  output$interventionPlot <- renderGvis({
    patIntCoords <- getPatIntCoords()
    if(is.null(patIntCoords)){
      textOutput("")
    }
    else{
      monitoredVariables <- getMonitoredVariables()
      inputData <- getinputData()
      inputData <- getNumericalInputData(inputData)
      outputCol <- inputData[,ncol(inputData)]
      riskVariables <- getRiskVariables()
      riskData <- subset(inputData,select = riskVariables)
      anchorCoordinates <- getVarSclaedAnchorCoordinates(riskData,outputCol,monitoredVariables)
      combinedCoordinates <- rbind(anchorCoordinates,patIntCoords)
      colnames(combinedCoordinates) <- c("ID","PCA","LDA","Type","Influence")
      intColors <- getIntColors(combinedCoordinates)
      combinedCoordinates <- combinedCoordinates[order(as.character(combinedCoordinates[,4])),]
      output <- gvisBubbleChart(combinedCoordinates,sizevar = "Influence",colorvar = "Type",
                                options = list(height = 1000,title = "Intervention View",colors = intColors,
                                               vAxis = "{title:'<---   Low Risk Region.................High Risk Region   --->'}",
                                               hAxis = "{title: 'Population Variation'}"))
    }
  })
  
  
  #################################################################################################################
  # functions in the prediction view
  #################################################################################################################
  
  output$logRegConfMat <- renderDataTable({
    inputData <- patientData()
    split=0.80
    nf <- ncol(inputData)
    trainIndex <- createDataPartition(as.factor(unlist(inputData[nf])), p=split, list=FALSE)
    data_train <- inputData[trainIndex,]
    data_test <- inputData[-trainIndex,]
    train.y<-colnames(data_train)[nf]
    formula<-paste(train.y," ~. -", train.y)
    model <- NaiveBayes(as.formula(formula), data=data_train)
    x_test <- data_test[,-nf]
    predictions <- predict(model, x_test)
    y_test <- data_test[,nf]
    a <- confusionMatrix(predictions$class, y_test)
    a
  })
  
  
  #################################################################################################################
  # helper functions
  #################################################################################################################
  
  # function to get column type of each column, return a vector of column types
  getColType <- function(inputData){
    #check if each column is numerical or not
    isNumeric <- as.character(sapply(inputData,is.numeric))
    colType <- mapvalues(isNumeric,c("TRUE","FALSE"),c("Numerical","Categorical"))
    return(colType)
  }
  
  # function to convert categorical column categorical columns from Yes,No to 1,0
  getNumericalInputData <- function(inputData){
    inputColType <- getColType(inputData)
    for(i in 1:length(inputColType)){
      # if a column is categorical
      if(inputColType[i]=="Categorical"){
        # convert Yes -> 1, No -> 0
        if("Male" %in% inputData[,i]){inputData[,i] <- ifelse(inputData[,i]=="Male",1,0)}
        else{inputData[,i] <- ifelse(inputData[,i]=="Yes",1,0)}
      }
    }
    return(inputData)
  }
  
  getNumericSummary <- function(input.csv){
    col.type <- as.character(sapply(input.csv,is.numeric))
    numeric.data <- subset(input.csv,select = which(col.type=="TRUE"))
    col.missing <- as.numeric(sapply(numeric.data, function(x){sum(is.na(x))}))
    col.mean <- round(as.numeric(sapply(numeric.data,mean)),2)
    col.median <- round(as.numeric(sapply(numeric.data, median)),2)
    col.mode <- round(as.numeric(sapply(numeric.data, function(x){getMode(x)})),2)
    col.sd <- round(as.numeric(sapply(numeric.data, sd)),2)
    col.min <- round(as.numeric(sapply(numeric.data,min)),2)
    col.max <- round(as.numeric(sapply(numeric.data, max)),2)
    numeric.summary <- data.frame(colnames(numeric.data),col.missing,col.mean,col.median,col.mode,col.sd,col.min,col.max)
    colnames(numeric.summary) <- c("Attribute","Missing","Mean","Median","Mode","SDev","Min","Max")
    return(numeric.summary)
  }
  
  # get the mode of a list
  getMode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  getCategoricSummary <- function(input.csv){
    col.type <- as.character(sapply(input.csv,is.numeric))
    categoric.data <- subset(input.csv,select = which(col.type=="FALSE"))
    col.missing <- as.numeric(sapply(categoric.data, function(x){sum(is.na(x))}))
    col.arity <- as.numeric(sapply(categoric.data, function(x){length(unique(x))}))
    mcv.count <- as.character(sapply(categoric.data, function(x){
      cat.combined <- character()
      cat.count <- data.frame(table(x))
      cat.prop <- data.frame(prop.table(table(x))*100)
      cat.prop[,2] <- round(cat.prop[,2],2)
      cat.count <- head(cat.count[order(cat.count[,2],decreasing = TRUE),],3)
      cat.prop <- head(cat.prop[order(cat.prop[,2],decreasing = TRUE),],3)
      cat.combined <- c(cat.combined,paste(cat.count[,1],"(",cat.count[,2],"[",cat.prop[,2],"%])",sep=""))
      return(toString(cat.combined))
    }))
    categoric.summary <- data.frame(colnames(categoric.data),col.missing,col.arity,mcv.count)
    colnames(categoric.summary) <- c("Attribute","Missing","Arity","Top Levels")
    return(categoric.summary)
  }
  
  # function used to get subset of all numerical columns
  getNumericalData <- function(inputData){
    inputColType <- getColType(inputData)
    # select the subset of columns which are numerical
    numericalData <- subset(inputData,select = which(inputColType=="Numerical"))
    return(numericalData)
  }
  
  # function used to get subset of all categorical columns
  getCategoricalData <- function(inputData){
    inputColType <- getColType(inputData)
    # select the subset of columns which are categorical
    categoricalData <- subset(inputData,select = which(inputColType=="Categorical"))
    return(categoricalData)
  }
  
  getPCALDA <- function(inputData,outputCol){
    nf <- ncol(inputData)
    # PCA output
    pcaOutput <- prcomp(cbind(inputData,outputCol)[,-(nf+1)], center = FALSE, scale. = FALSE, retx = TRUE)
    # run LDA on data
    ldaOutput <- lda(inputData,outputCol)
    # get the combination of first principal component and 1st LDA => 
    # dimension of output will be (no. of input Cols) x (2) 
    PCALDA <- cbind((-1 * pcaOutput$rotation[,1]), ldaOutput$scaling)
    return(PCALDA)
  }
  
  # function that will scale a vector between low and high
  # input -> inputColumn (vector), low(numeric), high (numeric)
  # output -> scaled vector
  scaleColumn <- function(inputColumn,low,high){
    # calculate min and max of the vector
    maxVal <- max(inputColumn)
    minVal <- min(inputColumn)
    if(maxVal == minVal){
      avgVal <- mean(inputColumn) 
      inputColumn <- rep(avgVal,length(inputColumn))
    }
    # scale values between low and high
    else{
      inputColumn <- low + (((high - low)*(inputColumn - minVal))/(maxVal - minVal))
    }
    return(inputColumn)
  }
  
  getPatientCoordinates <- function(inputData,IDCol,outputCol){
    # scale the values
    inputData <- as.data.frame(scale(inputData))
    PCALDA <- getPCALDA(inputData,outputCol)
    patientCoordinates <- as.data.frame(as.matrix(inputData) %*% PCALDA)
    # scale all patient coordinates within the range of -1 to 1
    patientCoordinates[,1] <- scaleColumn(patientCoordinates[,1],-1,1)
    patientCoordinates[,2] <- scaleColumn(patientCoordinates[,2],-1,1)   
    # set the color of everyone above 0 in y axis as high risk and others as low risk
    color <- ifelse(patientCoordinates[,2]>0,"High Risk","Low Risk")
    # combine ID, coordinates, color and size columns, size is 1 for all patients
    patientCoordinates <- cbind(IDCol,patientCoordinates,color,rep(1,nrow(patientCoordinates)))
    colnames(patientCoordinates) <- c("ID","X","Y","Color","Size")
    return(patientCoordinates)
  }
  
  
  getVarSclaedAnchorCoordinates <- function(riskData,outputCol,monitoredVariables){
    # scale the values
    riskData <- as.data.frame(scale(riskData))
    PCALDA <- getPCALDA(riskData,outputCol)
    x <- PCALDA[,1]
    y <- PCALDA[,2]
    # calculate angle of each data point = > tanInverse(y/x)
    theta <- atan(y / x)
    # the actual projects on the circle will be => 
    # x axis => cos(theta) and y axis is sign of (theta)
    # sign will determine the direction and 1.7 is the radius
    anchorCoordinates <- data.frame(sign(x) * 1.7 * abs(cos(theta)),sign(y) * 1.7 * abs(sin(theta)))
    # sizes of anchors are calculated as <- sqrt(LDA^2 + PCA^2) 
    anchorSize <- sqrt(x^2 + y^2)
    # scale anchor size to range 1 - 30
    anchorSize <- round(scaleColumn(anchorSize,1,30),0)
    # add the ID column, color and size
    anchorType <- rep("Feature",nrow(anchorCoordinates))
    anchorType[which(colnames(riskData) %in% monitoredVariables)] <- "Monitored Feature"
    anchorCoordinates <- cbind(row.names(PCALDA),anchorCoordinates,anchorType,anchorSize)
    colnames(anchorCoordinates) <- c("ID","X","Y","Color","Size")
    return(anchorCoordinates)
  }
  
  getSelectedPatientCoordinates <- function(riskData,IDCol,outputCol,sliderSelected){
    # order input Data by Y axis
    patientCoordinates <- getPatientCoordinates(riskData,IDCol,outputCol)
    selectedPatientCoordinates <- patientCoordinates[order(patientCoordinates[,3],decreasing = TRUE),]
    lastRow <- round(nrow(riskData)*(1-(sliderSelected/100)),0)
    selectedPatientCoordinates <- selectedPatientCoordinates[1:lastRow,]
    return(selectedPatientCoordinates)
  }
  
  # function that captures the ideal values that are entered
  getIdealValues <- function(inputData){
    inputCols <- colnames(inputData)
    # get the column type of each column
    inputColType <- getColType(inputData)
    # get the set of selected variables
    monitoredVariables <- getMonitoredVariables()
    lengthSelected <- length(monitoredVariables)
    # create a dummy array of 0s for minimum and maximum values
    minimumVals <- rep(0,lengthSelected)
    maximumVals <- rep(0,lengthSelected)
    # create a dummy array of "" for ideal category of selected variables
    idealCat <- rep("",lengthSelected)
    for(i in 1:length(monitoredVariables)){
      # get the column type of selected variable
      colType <- inputColType[which(inputCols==monitoredVariables[i])]
      # if column type is numerical
      if(colType=="Numerical"){
        # extarct the informarion in the inut tabs and add to minimum and maximum arrays
        eval(parse(text = paste("minimumVals[",i,"] <- input$minimum_",i,sep = "")))
        eval(parse(text = paste("maximumVals[",i,"] <- input$maximum_",i,sep = "")))
      }
      # if column is categorical
      else{
        # extract the selected category and add to idealCat vector
        eval(parse(text = paste("idealCat[",i,"] <- input$idealCat_",i,sep = "")))
      }
    }
    # make a dataframe combining minimum, maximum and ideal Category return that
    idealValsData <- data.frame(minimumVals,maximumVals,idealCat)
    row.names(idealValsData) <- monitoredVariables
    return(idealValsData)
  }
  
  getPatAnchorCoordinates <- function(inputData,IDCol,outputCol,patientID,inputColType,monitoredVariables){
    # scale the values
    inputData <- as.data.frame(scale(inputData))
    inputCols <- colnames(inputData)
    nf <- ncol(inputData)
    PCALDA <- getPCALDA(inputData,outputCol)
    x <- PCALDA[,1]
    y <- PCALDA[,2]
    # calculate angle of each data point = > tanInverse(y/x)
    theta <- atan(y / x)
    # the actual projects on the circle will be => 
    # x axis => cos(theta) and y axis is sign of (theta)
    # sign will determine the direction and 1.7 is the radius
    anchorCoordinates <- data.frame(sign(x) * 1.7 * abs(cos(theta)),sign(y) * 1.7 * abs(sin(theta)))
    # sizes of anchors are calculated as <- sqrt(diagoinal(covariance matrix)) 
    # add the ID column, color and size
    anchorSize <- numeric()
    patientRow <- inputData[which(IDCol==patientID),]
    for(i in 1:nf){
      colType <- inputColType[i]
      if(colType=="Numerical"){
        avgVal <- mean(inputData[,i])
        actualVal <- as.numeric(patientRow[i])
        if(actualVal>avgVal){
          diff <- actualVal - avgVal
          # adding 1 to make evrything above 1
          diff <- round(diff,0)+1
          anchorSize <- c(anchorSize,diff)
        }
        else{anchorSize <- c(anchorSize,1)}
      }
      else{anchorSize <- c(anchorSize,1)}
    }
    # scale anchorsize from 1 - 30
    scaledColumns <- scaleColumn(anchorSize[which(anchorSize>1)],2,30) 
    j <- 1
    for(i in 1:length(anchorSize)){
      if(anchorSize[i]>1){
        anchorSize[i] <- scaledColumns[j]
        j <- j+1
      }
    }
    anchorType <- rep("Feature",nrow(anchorCoordinates))
    anchorType <- character()
    for(i in 1:length(inputCols)){
      ifelse(inputCols[i] %in% monitoredVariables,anchorType <- c(anchorType,"Monitored Feature"),anchorType <- c(anchorType,"Feature"))
    }
    anchorCoordinates <- cbind(row.names(PCALDA),anchorCoordinates,anchorType,anchorSize)
    colnames(anchorCoordinates) <- c("ID","X","Y","Color","Size")
    return(anchorCoordinates)
  }
  
  # get the column that is the highest risk for the patient
  getMaxCol <- function(inputData,i,colType,colName,meanVals){
    # get the patient row data
    patientRow <- inputData[i,]
    # get a blank vector for the patient values compared to average
    patientVals <- numeric()
    # for each column in risk Data
    for(j in 1:length(colName)){
      # if column type is categorical, 
      if(colType[j] == 'Categorical'){patientVals <- c(patientVals,0)}
      # if column type is numerical
      else{
        # get the patient value of the column
        colVal <- as.numeric(patientRow[j])
        # get the difference of column and mean of column
        diff <- meanVals[j] - colVal
        # add the difference to patientvals
        patientVals <- c(patientVals,diff)
      }
    }
    # if max column is 0, return as 0
    if(max(patientVals)==0){ maxCol <- "0" }
    # if not return the column with mamum value
    else{ maxCol <- colName[which.max(patientVals)] }
    return(maxCol)
  }
  
  # function to get combnation of PCA and LDA
  getPCALDAoutput <- function(inputData,outputCol){
    # scale the values
    inputData <- as.data.frame(scale(inputData))
    nf <- ncol(inputData)
    # PCA output
    pcaOutput = prcomp(cbind(inputData,outputCol)[,-(nf+1)], center = FALSE, scale. = FALSE, retx = TRUE)
    # run LDA on data
    ldaOutput <- lda(inputData,outputCol)
    # at this point if you can project lda as y axis and pca as x axis
    # but we want all variables to form an outer circle around the data points
    # in order to do that, we get the angle of each data point and project them on a common radius circle
    y <- ldaOutput$scaling
    x <- -1 * pcaOutput$rotation[,1]
    output <- data.frame(x,y)
    return(output)
  }
  
  # function that will get the different colors that will be selected based on input for the intervention view
  getIntColors <- function(combinedCoordinates){
    colorTypes <- combinedCoordinates[,4]
    uniqueColors <- unique(colorTypes)
    if(("Low Risk" %in% uniqueColors) && ("High Risk" %in% uniqueColors) && ("Feature" %in% uniqueColors) && ("Monitored Feature" %in% uniqueColors)){
      output <-  "['#819FF7','#FE642E','#58FA82','#FF69B4']"
    }
    else if(("Low Risk" %in% uniqueColors) && ("High Risk" %in% uniqueColors) && ("Feature" %in% uniqueColors)){
      output <-  "['#819FF7','#FE642E','#58FA82']"
    }
    else if(("Low Risk" %in% uniqueColors) && ("Monitored Feature" %in% uniqueColors) && ("Feature" %in% uniqueColors)){
      output <-  "['#819FF7','#58FA82','#FF69B4']"
    }
    else if(("Monitored Feature" %in% uniqueColors) && ("High Risk" %in% uniqueColors) && ("Feature" %in% uniqueColors)){
      output <-  "['#819FF7','#FE642E','#FF69B4']"
    }
    else if(("Low Risk" %in% uniqueColors) && ("Feature" %in% uniqueColors)){
      output <-  "['#819FF7','#58FA82']"
    }
    else if(("High Risk" %in% uniqueColors) && ("Feature" %in% uniqueColors)){
      output <-  "['#819FF7','#FE642E']"
    }
  }
  
  getClusterColors <- function(patientCoordinates){
    colorTypes <- patientCoordinates[,4]
    uniqueColors <- unique(colorTypes)
    if(("Low Risk" %in% uniqueColors) && ("High Risk" %in% uniqueColors)){
      output <-  "['#FE642E','#58FA82']"
    }
    else if(("Low Risk" %in% uniqueColors)){
      output <-  "['#58FA82']"
    }
    else{ 
      output <-  "['#FE642E']"
    }
  }
  
}