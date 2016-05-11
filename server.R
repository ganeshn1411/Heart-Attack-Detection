

  #The server function is used to define the fuctions for the UI defined above. It contains a input and out parameter to
  #define what should be sent in and out of the UI and the session variable to keep track of what tabs are clicked.
  server <- function(input, output, session) {
  ischeck <- reactive({
    b <- input$bar()
  })
  #Used to read the patient data
  patdata <- reactive({
    if (is.null(input$file1))
      return(NULL)
    infile1 <- input$file1
    #Read dataset
    a <- read.csv(infile1$datapath,stringsAsFactors = FALSE)
    n1 = nrow(a);
    st2 = as.data.frame(c(1:n1));
    a <- cbind(st2,a)
    colnames(a)[1] <- "Patient_ID"
    a[nrow(a) + 1,] <- 0
    #Check if arity is less than 2. If yes, use a value of 1 else 9
    for (i in 1:ncol(a)) {
      if (length(unique(a[,i])) < 3) {
        a[nrow(a),i] <- 1
      }
      else{
        a[nrow(a),i] <- 0
      }
    }
    a
  })
  
  #Used to calculate the coordinates and the sizes of anchors
  anch1 <- reactive({
    values <- patdata()
    values <- values[,-1]
    values <- values[-nrow(values),]
    maincolnames <-  colnames(values)
    #Select the columns we re interested in
    #subs=c(7,9:12,15:16,22:27,29,32,34:37,39:41)
    #Create a vector containing numbers from 1 to the number of patients
    n1 = nrow(values);
    st2 = as.data.frame(c(1:n1));
    s <- st2[,1]
    #data = t2_all = data fraame version of original data set
    t2_all = values
    data = t2_all
    #Load the scale cat file
    
    #Obtain a subset of the data
    #data=cbind(as.data.frame(s),t2_all)
    data <- data[,1:(ncol(data) - 1)]
    #Seperate the risk variable only
    risk = t2_all[,ncol(t2_all)]
    #Create a data frame version of data called data_all and write it to a file
    data_all = as.data.frame(data)
    #Scale the file
    data_all = scale(data_all)
    
    p = data_all
    for (x in c(1:length(colnames(p)))) {
      colnames(p)[x] = paste('a',colnames(p)[x], sep = '')
    }
    for (x in 1:length(p)) {
      p[x] = sqrt(abs(p[x]))
    }
    for (x in 1:length(p[,1])) {
      max = max(p[x,]); for (y in 1:length(p[1,])) {
        p[x,y] = p[x,y] / max;
      }
    }
    
    #Perform pca and lda
    prc3 = prcomp(data_all, center = FALSE, scale. = FALSE, retx = TRUE)
    classif = lda(data_all, as.matrix(risk))
    lda_pc_rot = cbind((prc3$rotation[,1]), classif$scaling)

    #Get the anchor data ready
    ancAng = as.data.frame(atan(classif$scaling / prc3$rotation[,1]))
    ancAng[,2] <- rownames(ancAng)
    colnames(ancAng) <- c("ancAng","Names")
    rownames(ancAng) <- NULL
    ancCoords = cbind(7 * sign(prc3$rotation[,1]) * abs(cos(ancAng[,1])),7 * sign(classif$scaling) * abs(sin(ancAng[,1])))
    test1 <- as.data.frame(8.5 * sign(prc3$rotation[,1]) * abs(cos(ancAng[,1])))
    rownames(test1) <- NULL
    colnames(test1) <- 'coord1'
    test2 <-
      as.data.frame(18 * sign(classif$scaling) * abs(sin(ancAng[,1])))
    rownames(test2) <- NULL
    colnames(test2) <- 'coord2'
    test3 <- as.data.frame(cbind(test1,test2))
    ancCoords <- cbind(test1,test2)
    
    anchSize = as.data.frame(sqrt(diag(lda_pc_rot %*% t(lda_pc_rot))))
    anchSize <- anchSize / 200
    rownames(anchSize) <- NULL
    colnames(anchSize) <- 'anchSize'
    
    anch <- as.data.frame(cbind(ancAng,ancCoords,anchSize))
    anch <- anch[-1,]
    anch
    
  })
  
  #To calculate the   coordiinates of indicidual patients based on the risk factor.
  flex1 <- reactive({
    values <- patdata()
    values <- values[,-1]
    values <- values[-nrow(values),]
    maincolnames = colnames(values)
    maincolnames <- maincolnames[-1]
    #Create a vector containing numbers from 1 to the number of patients
    n1 = nrow(values);
    st2 = as.data.frame(c(1:n1));
    s <- st2[,1]
    #data = t2_all = data fraame version of original data set
    t2_all = values
    data = t2_all
    #Load the scale cat file
    #Obtain a subset of the data
    data <- data[,1:(ncol(data) - 1)]
    #Seperate the risk variable only
    risk = t2_all[,ncol(t2_all)]
    
    #Create a data frame version of data called data_all and write it to a file
    data_all = as.data.frame(data)
    
    #Scale the file
    data_all = scale(data_all)
    p = data_all
    for (x in c(1:length(colnames(p)))) {
      colnames(p)[x] = paste('a',colnames(p)[x], sep = '')
    }
    for (x in 1:length(p)) {
      p[x] = sqrt(abs(p[x]))
    }
    for (x in 1:length(p[,1])) {
      max = max(p[x,]); for (y in 1:length(p[1,])) {
        p[x,y] = p[x,y] / max;
      }
    }
    
    #Perform pca and lda
    prc3 = prcomp(data_all, center = FALSE, scale. = FALSE, retx = TRUE)
    classif = lda(data_all, as.matrix(risk))
    lda_pc_rot = cbind((prc3$rotation[,1]), classif$scaling)
    
    rot_x = as.matrix(data_all) %*% lda_pc_rot
    colnames_data = data.frame(colnames(data_all))
    ID = s
    
    rot_x <- as.data.frame(rot_x)
    patdat <- patdata()
    actual <- patdat[,ncol(patdat)]
    actual <- unlist(actual)
    actual <- actual[-length(actual)]
    maxthresh <- 0
    thresh <- -0.5
    thresholds <- c(-0.5,-0.25,0,0.25,0.5,0.75,1)
    for(i in 1:length(thresholds)){
      cutoff <- thresholds[i]
      count <- 0
      for(j in 1:length(actual)){
        if(rot_x[j,2]>cutoff){
          if(actual[j]==1){
            count <- count + 1;
          }
        }
        else{
          if(actual[j]==0){
            count <- count + 1;
          }
        }
      }
      if(count > maxthresh){
        maxthresh <- count
        thresh <- thresholds[i]
      }
    }
    
    rot_x[,1] <- (rot_x[,1] * 1.2) + 1
    rot_x[,2] <- (rot_x[,2] * 2.5) - (thresh*2.5)
    
    test_ID <- ID
    test_data <- data
    test_p <- p
    test_rot <- rot_x
    flex <- as.data.frame(cbind(ID,data,p,rot_x))
    flex
  })
  
  #Read the input from the vital variable checkbox
  vital <- reactive({
    Monitored_variables <- input$columns1
    Monitored_variables <-as.data.frame(Monitored_variables, stringsAsFactors = FALSE)
  })
  
  #Read the input from the editable variable checkbox
  edit <- reactive({
    Modifiable_variables <- input$columns2
    Modifiable_variables <- as.data.frame(Modifiable_variables, stringsAsFactors = FALSE)
  })
  
  #Select only those users who have a risk above the value specified by the slider.
  slide <- reactive({
    sflex <- flex1()
    #Get the input size
    siz1 <- input$size
    maxy <- nrow(sflex)
    #Sort the rows
    sflex <- sflex[order(sflex[,ncol(sflex)]),]
    sper <- (siz1 * maxy) / 100
    #Take a subset
    flexsub <- sflex[-(1:sper),]
    flexsub
  })
  
  #Select only those users who have a risk above the value specified by the slider.
  slideStatic <- function(){
    sflex <- flex1()
    #Get the input size
    #siz1 <- input$size
    maxy <- nrow(sflex)
    #Sort the rows
    sflex <- sflex[order(sflex[,ncol(sflex)]),]
    sper <- maxy / 100
    #Take a subset
    flexsub <- sflex[-(1:sper),]
    return(flexsub)
  }
  
  slide2 <- reactive({
    siz2 <- input$size
    siz2
  })
  
  #Get the input for the final values in the intervention page
  df <- eventReactive(input$patint, {
    edi2 <- edit()
    allv <- c(edi2[,1])
    a12 <- unique(allv)

    temp1 <- patdata()
    temp1 <- temp1[,-1]
    temp2 <- subset(temp1,select = a12)

    final3 <- data.frame(Ideal = character(),stringsAsFactors = FALSE)
    
    for (i in 1:ncol(temp2)) {
      if (temp2[nrow(temp2),i] == 0) {
        eval(parse(text = paste("final3[",i,",1] <- input$finedit1_",i,sep = "")))
      }
      if (temp2[nrow(temp2),i] == 1) {
        eval(parse(text = paste("if(input$finedit2_",i," == 1){final3[",i,",1] <- '1'} else{final3[",i,",1] <- '0'}",sep = "")))
      }
    }
    final9 <- final3
  })
  
  #Output the graph to show the change in risk value graph
  output$contents3 <- renderGvis({
    tv <- flex1()
    tvt <- patdata()
    tvt <- tvt[,-1]
    tvt <- tvt[-nrow(tvt),]
    
    edi2 <- edit()
    if(length(edi2)==0){
      textOutput("")
    }
    else{
    allv <- c(edi2[,1])
    a12 <- unique(allv)
    
    edf <- a12
    cc <- df()
    pno3 <- patientIDInspect()
    
    temp6 <- patdata()
    temp7 <- subset(temp6,select = a12)
    for (i in 1:length(edf)) {
      cc[i,2] <- edf[i]
    }
    colnames(cc)[2] <- 'variable'
    cc <- cc[c(2,1)]
    
    cc[,2] <- as.numeric(cc[,2])
    
    tv2 <- tv[,c(1:23)]
    tv3 <- subset(tv2,tv2$ID == pno3)
    tvt2 <- subset(
      tvt,(
        tvt$Gender == tv3$Gender &
          tvt$SBP == tv3$SBP &
          tvt$DBP == tv3$DBP &
          tvt$LDL == tv3$LDL &
          tvt$HDL == tv3$HDL &
          tvt$LastA1C == tv3$LastA1C &
          tvt$Family_CVD == tv3$Family_CVD &
          tvt$Smoker == tv3$Smoker &
          tvt$BMI == tv3$BMI &
          tvt$Age == tv3$Age &
          tvt$Family_History_Diabetes == tv3$Family_History_Diabetes &
          tvt$Albuminuria == tv3$Albuminuria &
          tvt$Retinopathy == tv3$Retinopathy &
          tvt$CHF == tv3$CHF &
          tvt$Bypass == tv3$Bypass &
          tvt$Aspirin == tv3$Aspirin &
          tvt$Antihyper == tv3$Antihyper &
          tvt$Glycemic == tv3$Glycemic &
          tvt$Antilipemic == tv3$Antilipemic &
          tvt$Angina == tv3$Angina &
          tvt$Angioplasty == tv3$Angioplasty
      )
    )
    tvt3 <- rbind(tvt,tvt2)
    
    for (i in 1:ncol(tvt3)) {
      if (colnames(tvt3)[i] %in% cc[,1]) {
        n <- which(cc[,1] == colnames(tvt3)[i])
        tvt3[nrow(tvt3),i] <- cc[n,2]
      }
    }
    values <- tvt3
    maincolnames = colnames(values);
    #Create a vector containing numbers from 1 to the number of patients
    n1 = nrow(values);
    st2 = as.data.frame(c(1:n1));
    s <- st2[,1]
    #data = t2_all = data fraame version of original data set
    t2_all = values
    data = t2_all
    #Load the scale cat file
    
    #Obtain a subset of the data
    #data=cbind(as.data.frame(s),t2_all)
    data <- data[,1:(ncol(data) - 1)]
    #Seperate the risk variable only
    risk = t2_all[,ncol(t2_all)]
    
    #Create a data frame version of data called data_all and write it to a file
    data_all = as.data.frame(data)
    
    #Scale the file
    data_all = scale(data_all)
    p = data_all
    for (x in c(1:length(colnames(p)))) {
      colnames(p)[x] = paste('a',colnames(p)[x], sep = '')
    }
    for (x in 1:length(p)) {
      p[x] = sqrt(abs(p[x]))
    }
    for (x in 1:length(p[,1])) {
      max = max(p[x,]); for (y in 1:length(p[1,])) {
        p[x,y] = p[x,y] / max;
      }
    }
    #Perform pca and lda
    prc3 = prcomp(data_all, center = FALSE, scale. = FALSE, retx = TRUE)
    classif = lda(data_all, as.matrix(risk))
    lda_pc_rot = cbind((prc3$rotation[,1]), classif$scaling)
    
    rot_x = as.matrix(data_all) %*% lda_pc_rot
    colnames_data = data.frame(colnames(data_all))
    ID = s
    
    flex234 <- as.data.frame(cbind(ID,data,p,rot_x))
    tv7 <- subset(tv,tv$ID == pno3)
    tv5 <- tv7[,c(ncol(tv7) - 1,ncol(tv7))]
    colnames(tv5)[1] <- "x"
    colnames(tv5)[2] <- "y"
    tv6 <- flex234[nrow(flex234),c(ncol(flex234) - 1,ncol(flex234))]
    colnames(tv6)[1] <- "x"
    colnames(tv6)[2] <- "y"
    
    ttt <- rbind(tv5,tv6)
    ttt[1,3] <- "Old"
    ttt[1,4] <- "Old Value"
    ttt[2,3] <- "New"
    ttt[2,4] <- "New Value"
    
    colnames(ttt)[3] <- "ID"
    colnames(ttt)[4] <- "color"
    ttt$size <- 0.001
    ttt <- ttt[c(3,1,2,5,4)]
    tv9 <- subset(tv3,select = a12)
    tv9 <- as.data.frame(t(tv9))
    tv8 <- tv9
    tv8$atype <- c(rep("Modifiable Anchors",nrow(tv8)))
    tv8$var <- rownames(tv8)
    anch2 <- anch1()
    
    temp22 <- anch2
    temp22 <- temp22[,-1]
    colnames(temp22)[1] <- "ID"
    colnames(temp22)[2] <- "x"
    colnames(temp22)[3] <- "y"
    colnames(temp22)[4] <- "size"
    temp22[,5] <- 'Anchors'
    colnames(temp22)[5] <- "color"
    
    temp33 <- temp22
    tv9 <- tv8
    
    for (i in 1:nrow(temp33)) {
      if (temp33$ID[i] %in% tv9[,3]) {
        n <- which(tv9[,3] == temp33$ID[i])
        temp33$color[i] <- tv9[n,2]
      }
    }
    
    temp44 <- temp33
    ttt2 <<- rbind(ttt,temp44)
    for(i in 1:nrow(ttt2)){
      ttt2[i,2] <- -1 * ttt2[i,2]
    }
    if(ttt2[ttt2$ID=='Old',3]<ttt2[ttt2$ID=='New',3]){
      ttt2[ttt2$ID=='New',3] <- ttt2[ttt2$ID=='New',3] - 2*(ttt2[ttt2$ID=='New',3] - ttt2[ttt2$ID=='Old',3])
    }
    ttt2 <- ttt2[order(ttt2$color),]
          bub <- gvisBubbleChart(ttt2, idvar="ID",
                              xvar="x", yvar="y",sizevar = "size", colorvar = "color",
                              options=list(
                                width=800, height=500,
                                colors="['#819FF7','#FE642E','#006600','#CC0000']"))
bub  }})
  
  #The output text in the intervention view page
  output$pid <- renderText({
    pno <- patientIDInspect()
    paste("Patient No. : ",pno,sep = " ")
  })
  
  
  #Display the content to the view data page
  output$contents2 <- renderDataTable({
    data2 <- patdata()
    data2 <- data2[-nrow(data2),-1]
    data2
  }, options = list(scrollX = TRUE,pageLength = 10))
  
  #Input checkboxes to choose vital variables
  output$choose_columns1 <- renderUI({
    data <- patdata()
    data <- data[,-1]
    data <- data[-nrow(data),]
    colnames <- names(data[,-ncol(data)])
    colnames <- colnames[-1]
    checkboxGroupInput("columns1", "Choose columns", choices  = colnames)
  })
  
  #Input checkboxes to choose editable variables
  output$choose_columns2 <- renderUI({
    data <- patdata()
    data <- data[,-1]
    data <- data[-nrow(data),]
    colnames <- names(data[,-ncol(data)])
    colnames <- colnames[-1]
    checkboxGroupInput("columns2", "Choose columns", choices  = colnames)
  })
  
  #Input checkboxes to choose editable variables
  output$choose_columns3 <- renderUI({
    data <- patdata()
    data <- data[,-1]
    data <- data[-nrow(data),]
    colnames <- names(data[,])
    colnames <- colnames[-1]
    colnames <- rev(colnames)
    selectInput("columns3", "Choose Output Column", colnames)
  })
  
  output$outputColumn <- renderText({
    colSelected <- outputColSelection()
    patdat <- patdata()
    patdat1 <- patdat
    patdat <- patdat[!names(patdat) %in% colSelected]
    patdat1 <- patdat1[names(patdat1) %in% colSelected]
    patdat <- cbind(patdat,patdat1)
    colnames(patdat)[ncol(patdat)] <- colSelected
    colOutput <- paste("Current Output Column : ",colSelected)
  })
  
  
  #Table to display the selected vital variables
  output$vital_v <- renderDataTable({
    data1 <- vital()
  }, options = list(
    pageLength = 10,searching = FALSE,paging = FALSE
  ))
  
  #Table to display the selected editable variables
  output$edit_v <- renderDataTable({
    data2 <- edit()
  }, options = list(
    pageLength = 25,searching = FALSE,paging = FALSE
  ))
  
  #The text boxes in select column page to input the ideal values
  output$textbox <- renderUI({
    # only monitored get thhreshold
    vit2 <- vital()
    if (nrow(vit2) == 0) {
      bc <- textOutput("Please select vital/editable columns")
    } 
    else{
      allv <- vit2[,1]
      allv <- unique(allv)
      temp1 <- patdata()
      temp1 <- temp1[,-1]
      temp2 <- subset(temp1,select = allv)
      temp3 <- as.data.frame(temp2[nrow(temp2),])
      if (ncol(temp3) == 1) {
        colnames(temp3)[1] <- colnames(temp2)[1]
      }
      temp2 <- temp3
      
      bc <- lapply(1:ncol(temp2), function(i) {
        if (temp2[1,i] == 0) {
          numericInput(paste("ideal1",i,sep = "_"), label = paste("Minumum ",colnames(temp2)[i]), value = temp2[1,i],min = 0,max = 100000000)
        }
      })
    } 
    bc
  })
  
  #The text boxes in select column page to input the ideal values
  output$textbox2 <- renderUI({
    # only monitored get threshold
    vit3 <- vital()
    if (nrow(vit3) == 0) {
      bc <- textOutput(" ")
    } 
    else{
      allv <- vit3[,1]
      allv <- unique(allv)
      temp1 <- patdata()
      temp1 <- temp1[,-1]
      temp2 <- subset(temp1,select = allv)
      temp3 <- as.data.frame(temp2[nrow(temp2),])
      if (ncol(temp3) == 1) {
        colnames(temp3)[1] <- colnames(temp2)[1]
      }
      temp2 <- temp3
      
      bc <- lapply(1:ncol(temp2), function(i) {
        if(temp2[1,i] != 0){
        selectInput(paste("ideal2",i,sep = "_"), label = colnames(temp2)[i], choices = list("Yes" = 1, "No" = 2),selected = 1)
        }
      })
    } 
    bc
  })
  
  
  #The text boxes in select column page to input the ideal values
  output$textbox1 <- renderUI({
    # only monitored get thhreshold
    vit <- vital()
    if (nrow(vit) == 0) {
      bc <- textOutput("")
    } 
    else{
    allv <- unique(vit[,1])
      temp1 <- patdata()
      temp1 <- temp1[,-1]
      temp2 <- subset(temp1,select = allv)
      temp3 <- as.data.frame(temp2[nrow(temp2),])
      if (ncol(temp3) == 1) {
        colnames(temp3)[1] <- colnames(temp2)[1]
      }
      temp2 <- temp3
      
      bc <- lapply(1:ncol(temp2), function(i) {
        if (temp2[1,i] == 0) {
          numericInput(paste("ideal3",i,sep = "_"), label = paste("Maximum ",colnames(temp2)[i]), value = temp2[1,i],min = 0,max = 100000000)
        }
      })
    }
    bc
  })
  
  
  #Action button event
  observeEvent(input$introload, {
    newtab1 <- switch(input$tabs,
                      "introduction" = "load",
                      "load" = "introduction")
    updateTabItems(session,"tabs", newtab1)
  })
  
  observeEvent(input$corpop1, {
    newtab1 <- switch(input$tabs,
                      "cor" = "pop",
                      "pop" = "cor")
    updateTabItems(session,"tabs", newtab1)
  })
  
  
  observeEvent(input$corpop2, {
    newtab1 <- switch(input$tabs,
                      "cor" = "pop",
                      "pop" = "cor")
    updateTabItems(session,"tabs", newtab1)
  })
  
  
  #Action button event
  observeEvent(input$loadview, {
    newtab2 <- switch(input$tabs,
                      "load" = "view",
                      "view" = "load")
    updateTabItems(session,"tabs", newtab2)
  })
  
  
  #Action button event
  observeEvent(input$datasummaryview, {
    newtab5 <- switch(input$tabs,
                      "summ" = "view",
                      "view" = "summ")
    updateTabItems(session,"tabs", newtab5)
  })
  
  #Action button event
  observeEvent(input$exploreview, {
    newtab5 <- switch(input$tabs,
                      "explore" = "summ",
                      "summ" = "explore")
    updateTabItems(session,"tabs", newtab5)
  })
  
  #Action button event
  observeEvent(input$viewselcol, {
    newtab3 <- switch(input$tabs,
                      "selcol" = "view",
                      "view" = "selcol")
    updateTabItems(session,"tabs", newtab3)
  })
  
  observeEvent(input$viewselcol1, {
    newtab3 <- switch(input$tabs,
                      "selcol" = "explore",
                      "explore" = "selcol")
    updateTabItems(session,"tabs", newtab3)
  })
  
  #Action button event
  observeEvent(input$proc, {
    newtab1 <- switch(input$tabs,
                      "selcol" = "selact",
                      "selact" = "selcol")
    updateTabItems(session, "tabs", newtab1)
  })
  
  #Action button event
  observeEvent(input$pop1, {
    newtab <- switch(input$tabs,
                     "selcol" = "pop",
                     "pop" = "selcol")
    
    updateTabItems(session, "tabs", newtab)
  })
  
  #Action button event
  observeEvent(input$cor1, {
    newtab <- switch(input$tabs,
                     "selcol" = "cor",
                     "cor" = "selcol")
    
    updateTabItems(session, "tabs", newtab)
  })
  
  #Action button event
  observeEvent(input$patient1, {
    newtab <- switch(input$tabs,
                     "selcol" = "pat",
                     "pat" = "selcol")
    t1 <<- 1
    t2 <<- 0
    updateTabItems(session, "tabs", newtab)
  })
  
  #Action button event
  observeEvent(input$patient2, {
    newtab <- switch(input$tabs,
                     "pop" = "clu",
                     "clu" = "pop")
    t1 <<- 1
    t2 <<- 0
    updateTabItems(session, "tabs", newtab)
  })
  
  
  #Action button event
  observeEvent(input$patient5, {
    newtab <- switch(input$tabs,
                     "clu" = "pat",
                     "pat" = "clu")
    updateTabItems(session, "tabs", newtab)
  })
  
  
  #Action button event
  observeEvent(input$patient4, {
    newtab <- switch(input$tabs,
                     "int" = "pred",
                     "pred" = "int")
    updateTabItems(session, "tabs", newtab)
  })
  
  
  #Action button event
  observeEvent(input$patient3, {
    newtab <- switch(input$tabs,
                     "int" = "pat",
                     "pat" = "int")
    updateTabItems(session, "tabs", newtab)
  })
  
  
  #Infobox to show all of patients values
  output$allpat <- renderUI({
    if (t1 == 1 & t2 == 0) {
      pno <- input$patient_text1
    }
    if (t1 == 0  & t2 == 1) {
      pno <- input$patient_text2
    }
    if (t1 == 1  & t2 == 1) {
      pno <- input$patient_text4
    }
    
    vit2 <- vital()
    edi2 <- edit()
    
    if (nrow(vit2) == 0 & nrow(edi2) == 0) {
      bc <- textOutput("Please select vital/editable columns")
      
    } else if (nrow(edi2) == 0 & nrow(vit2) != 0) {
      temp1 <- flex1()
      temp2 <- subset(temp1,temp1[1] == pno)
      a
      allv <- c("ID",vit2[,1])
      allv <- unique(allv)
      
      temp2 <- subset(temp2,select = allv)
      lapply(1:ncol(temp2), function(i) {
        infoBox(
          colnames(temp2)[i], temp2[1,i],width = 3, color = "blue",fill = TRUE
        )
      })
      
    } else if (nrow(vit2) == 0 & nrow(edi2) != 0) {
      temp1 <- flex1()
      temp2 <- subset(temp1,temp1[1] == pno)
      
      allv <- c("ID",edi2[,1])
      allv <- unique(allv)
      
      temp2 <- subset(temp2,select = allv)
      lapply(1:ncol(temp2), function(i) {
        infoBox(
          colnames(temp2)[i], temp2[1,i],width = 3, color = "blue",fill = TRUE
        )
      })
      
    } else{
      temp1 <- flex1()
      temp2 <- subset(temp1,temp1[1] == pno)
      
      allv <- c("ID",vit2[,1],edi2[,1])
      allv <- unique(allv)
      
      temp2 <- subset(temp2,select = allv)
      lapply(1:ncol(temp2), function(i) {
        infoBox(
          colnames(temp2)[i], temp2[1,i],width = 3, color = "blue",fill = TRUE
        )
      })
      
    }
  })
  
  
  patientlist <- function(){
    temp1 <- slideStatic()
    siz <- 1
    anch2 <- anch1()
    temp11 <- temp1[,c(1,ncol(temp1) - 1,ncol(temp1))]
    temp11[,4] <- 0.1
    colnames(temp11)[1] <- "ID"
    colnames(temp11)[2] <- "x"
    colnames(temp11)[3] <- "y"
    colnames(temp11)[4] <- "size"
    temp11$size <- temp11$size / 100000000

    temp22 <- anch2
    temp22 <- temp22[,-1]
    colnames(temp22)[1] <- "ID"
    colnames(temp22)[2] <- "x"
    colnames(temp22)[3] <- "y"
    colnames(temp22)[4] <- "size"

    temp11$x <- as.numeric(as.character(temp11$x))
    temp11$y <- as.numeric(as.character(temp11$y))
    
    temp22$x <- as.numeric(as.character(temp22$x))
    temp22$y <- as.numeric(as.character(temp22$y))

    Bx <- as.numeric(temp22[2,2])
    By <- as.numeric(temp22[3,2])
    funlist <- apply(temp11,1,function(x)custDist(x[2],x[3],Bx,By))
    funlistmain <- data.frame(funlist)
    
    for(i in 2:nrow(temp22)){
      Bx <- temp22[i,2]
      By <- temp22[i,3]
      funlistnew <- apply(temp11,1,function(x)custDist(x[2],x[3],Bx,By))
      funlistmain <- cbind(funlistmain,funlistnew)
    }
    colnames(funlistmain) <- temp22$ID
    minCol <- apply(funlistmain,1,which.min)
    minCol <- mapvalues(minCol,1:length(temp22$ID),temp22$ID)
    temp11 <- cbind(temp11,minCol)
    return(temp11)
  }
  
  custDist <- function(x,y,Bx,By){
    cal <- sqrt((x-Bx)*(x-Bx) + (y-By)*(y-By))
    if(!is.numeric(cal)){
      return(100)
    }
    else{
      return(cal)
    }
  }
  
  
  
  patientID <- reactive({
    patientIDSelected <- input$patient_text5
  })
  
  patientIDInspect <- reactive({
    patientIDInspectSelected <- input$patient_text6
  })
  
  csel1 <- reactive({
    sel1_variables <- input$sel1
  })
  
  csel2 <- reactive({
    sel2_variables <- input$sel2
  })
  
  csel3 <- reactive({
    sel3_variables <- input$sel3
  })
  
  algoSelection <- reactive({
    algoforSelection <- input$algoSelect
  })
  
  
  outputColSelection <- reactive({
    col3Selected <- input$columns3
  })
  
  
  
  
  #Function for logistic regression
  get_pred_logreg <- function(train,test) {
    nf <- ncol(test)#Finds out the number of columns in the test set
    strfunc <-
      paste(names(train)[nf],"~.",sep = "") # we assume the label to be in the lastcolumn
    func <- as.formula(strfunc)
    mylogit <-
      glm(func, data = train, family = binomial)#Creates the model
    ntrain <- test[,-nf]#Creates the test set(newdata)
    pred1 <- NULL
    pred <-
      predict(mylogit,newdata = ntrain,type = 'response')#Creates the predicton values
    final <-
      cbind(test[,nf],as.data.frame(pred))#Binds it with the existing y values
    return(final)
  }
  
  
  #Function for SVM
  get_pred_svm <- function(train,test) {
    nf <- ncol(test)#Finds out the number of columns in the test set
    train[,nf] <- as.factor(train[,nf])
    strfunc <-
      paste(names(train)[nf],"~.",sep = "") # we assume the label to be in the last
    func <- as.formula(strfunc)
    model <-
      svm(func, data = train, probability = TRUE)#building the model
    nwine <- test[,-nf]
    pred <-
      predict(model, nwine,prob = TRUE)#Applying the model to the test set
    pred1 <-
      attr(pred, "probabilities")#extracting the probabilities for the prediction
    pred2 <- as.data.frame(pred1)
    pred2 <- pred2[,order(colnames(pred2))]
    final <-
      cbind(as.data.frame(pred2[,2]),test[,nf])#Constructing the final data frame
    return(final)
  }
  
  
  #Function for randomForest
  get_pred_rf <- function(train,test) {
    
    nf <- ncol(test)#Finds out the number of columns in the test set
    train[,ncol(train)] <- as.factor(train[,ncol(train)])
    test[,ncol(test)] <- as.factor(test[,ncol(test)])
    strfunc <-
      paste(names(train)[nf],"~.",sep = "") # we assume the label to be in the firstcolumn
    func <- as.formula(strfunc)
    classifier <- randomForest(func, data = train) #building the model
    nf <- ncol(test)
    nwine <- test[,-nf]
    pred <-
      predict(classifier, nwine,type = "prob")#applying the model to thetst set
    pred2 <- as.data.frame(pred)
    pred2 <- pred2[,order(colnames(pred2))]
    final <-
      cbind(as.data.frame(pred2[,2]),test[,nf])#binding the actual output and the prediction
    return(final)
  }
  
  
  # naive bayes function
  get_pred_nb <- function(train,test){
    colnames(test)<-colnames(train)
    nf <- ncol(train)
    train.y<-colnames(train)[nf]
    formula<-paste(train.y," ~. -", train.y)
    # applying naive Bayes function on the training data
    model <- naiveBayes(as.formula(formula), data = train,na.action=na.omit)
    # predict to get the probabilities
    pred <- predict(model, test,type = "raw")
    # reorder the probability values by column names
    pred <- pred[,order(colnames(pred))]
    pred <- rbind(pred)
    # extract the column with probabilities of 1
    pred <- pred[,2]
    outputdf <- data.frame(pred,test[nf])
    colnames(outputdf) <- c("pred","true output")
    return(outputdf)
  }
  
  
  # k-nearest neighbour function
  get_pred_knn <- function(train,test,k){
    colnames(test)<-colnames(train)
    nf <- ncol(test)
    # knn function to predict output
    pred <- knn(train[,-nf],test[,-nf],cl=train[,nf],k,prob=TRUE)
    pred.value <- as.vector(pred)
    #extracting probabilities from output
    pred <- attr(pred,"prob")
    outputdf <- data.frame(pred,pred.value,test[nf])
    # changing probabilities fro predicted values of 0
    outputdf[,1]<- ifelse(outputdf[,2]==0, 1-outputdf[,1],outputdf[,1])
    colnames(outputdf) <- c("pred","value","true_output")
    outputdf <- subset(outputdf,select=c(pred,true_output))
    return(outputdf)
  }
  
  
  

  do_cv_class <-function(df,num_folds,model_name){
    # sample function to randomize data
    dfr <- df[sample(nrow(df)),]
    icount <- nrow(df)%/%num_folds
    j <- 1
    result <- data.frame()
    # check if patten matches with format of knn
    if(length(grep("[0-9]nn",model_name))!=0){
      #extract k from the input
      k <- as.numeric(substr(model_name,1,1))
      # call the knn function numfolds-1 times
      for(i in 1:(num_folds-1)){
        train <- data.frame(dfr[-(j:(j+icount-1)),])
        test <- data.frame(dfr[j:(j+icount-1),])
        j <- j + icount
        gen.data <- get_pred_knn(train,test,k)
        result <- rbind(result,gen.data)
      }
      # repeat the same thing for the remaining last set of elements (kth iteration)
      train <- data.frame(dfr[-(j:nrow(df)),])
      test <- data.frame(dfr[j:nrow(df),])
      gen.data <- get_pred_knn(train,test,k)
      result <- rbind(result,gen.data)
    }
    else{
      #if input does not match knn, append get_pred_ to input name
      model_name <- paste("get_pred_",model_name,sep="")
      # loop numfolds-1 times
      for(i in 1:(num_folds-1)){
        train <- data.frame(dfr[-(j:(j+icount-1)),])
        test <- data.frame(dfr[j:(j+icount-1),])
        j <- j + icount
        #call  the method to perform prediction 
        gen.data <- do.call(model_name,list(train,test))
        result <- rbind(result,gen.data)
      }
      # repeat the same thing for the remaining last set of elements (kth iteration)
      train <- data.frame(dfr[-(j:nrow(df)),])
      test <- data.frame(dfr[j:nrow(df),])
      gen.data <- do.call(model_name,list(train,test))
      result <- rbind(result,gen.data)
    }
    return(result)
  }
  
  
  get_metrics <- function(df,cutoff = 0.5){
    #create a column of 1s and 0s based on cutoff values
    colnames(df) <- c("pred","test")
    df$pred<- ifelse(df$pred>= cutoff, 1,0)
    # get the confusion matrix values
    tpcount <- sum(df[,1]==0 & df[,2]==1)
    fncount <- sum(df[,1]==1 & df[,2]==1)
    fpcount <- sum(df[,1]==0 & df[,2]==0)
    tncount <- sum(df[,1]==1 & df[,2]==0)
    
    pos <- tpcount + fncount
    neg <- fpcount + tncount
    
    tpr <-  tpcount / pos
    fpr <- fpcount / neg

    accuracy <- (tpcount+tncount)/(pos+neg)
    
    precision <- tpcount/(tpcount + fpcount)
    
    recall <- tpr
    
    outputdf <- data.frame(tpr,fpr,accuracy,precision,recall)
    acuuracy <- 1-unlist(outputdf[1,])
    returndf <- data.frame(c("True Positive Rate","False Postitive Rate","Accuracy","Precision","Recall"),
                           acuuracy)
    colnames(returndf) <- c("Measure","Value")
    return(returndf)
  }
  
  
  get_confusion_matrix <- function(df,cutoff = 0.5){
    #create a column of 1s and 0s based on cutoff values
    colnames(df) <- c("pred","test")
    df$pred<- ifelse(df$pred>= cutoff, 1,0)
    # get the confusion matrix values
    tpcount <- sum(df[,1]==0 & df[,2]==1)
    fncount <- sum(df[,1]==1 & df[,2]==1)
    fpcount <- sum(df[,1]==0 & df[,2]==0)
    tncount <- sum(df[,1]==1 & df[,2]==0)
    
    confMatrix <- data.frame(c("Predicted True","Predicted False"),
                             c(tpcount,fncount),
                            c(fpcount,tncount))
    colnames(confMatrix) <- c("","Actually True","Actually False")
  return(confMatrix)
  }
  
  
  
  get_accuracy_comparisons <- function(){
    output.dataframe <- data.frame(c("Linear Discriminant Analysis","Logistic Regression","Naive Bayes","k Nearest Neighbour","Support Vector Machine","Random Forest"),
                                   c(0.82988685,0.907300566,0.8443534,0.892619466,0.9176906573,0.9066906573))
    return(output.dataframe)
  }
  
  
  output$compareAcuuracyGraphs <- renderGvis({
    outputTable <- get_accuracy_comparisons()
    outputTable$Accuracy.style <- c("#FF4000","#0073B7","#FE642E","#FFFF00","#808080","#00FF00") 
    colnames(outputTable) <- c("Model","Accuracy","Accuracy.style")
    colGvis <- gvisColumnChart(outputTable,xvar = "Model",yvar = c("Accuracy","Accuracy.style"),
                               options= list(height = 600,
                                             width = 700,
                                             legend = 'none',
                                             title = "Comparison of Model Accuracy",
                                             titleTextStyle = "{fontName:'Arial',fontSize:16}",
                                             vAxes="[{title:'Accuracy', 
                                             viewWindow:{min:0, max:1},
                                             titleTextStyle : {fontName:'Arial'},
                                             titleTextStyle : {fontSize:14}}]"))
    colGvis
  })
  
  
  output$errorMetrics <- renderDataTable({
    patdat <- patdata()
    algoSelected <- algoSelection()
    
    if(algoSelected == 'Logistic Regression'){
      pred.glm <- do_cv_class(patdat,10,"logreg")
      pred.glm <- pred.glm[,c(2,1)]
      glm.acc <- get_metrics(pred.glm,0.5)
      ErrorMet <- glm.acc
    }
    
    if(algoSelected == 'Support Vector Machines'){
      pred.svm <- do_cv_class(patdat,10,"svm")
      svm.acc <- get_metrics(pred.svm,0.5)
      ErrorMet <- svm.acc
    }

    if(algoSelected == 'Naive Bayes'){
      pred.nb <- do_cv_class(patdat,10,"nb")
      nb.acc <- get_metrics(pred.nb,0.5)
      ErrorMet <- nb.acc
      }
      
    if(algoSelected == 'k Nearest Neighbour'){
      pred.knn <- do_cv_class(patdat,10,"5nn")
      knn.acc <- get_metrics(pred.knn,0.5)
      ErrorMet <- knn.acc
    }
    
    if(algoSelected == 'Random Forest'){
      pred.rf <- do_cv_class(patdat,10,"rf")
      rf.acc <- get_metrics(pred.rf,0.5)
      ErrorMet <- rf.acc
    }
    
    
    if(algoSelected == 'Linear Discriminant Analysis'){
      a <- flex1()
      
      b <- patdata()
      b <- b[-1,]
      a1 <- a[,ncol(a)]
      for(i in 1:length(a1)){
        if(a1[i] > 0.8){
          a1[i] =1
        }
        if(a1[i] <= 0.8){
          a1[i] =0
        }
      }
      
      b1 <- b[,ncol(b)]
      cc <- cbind(as.data.frame(a1),as.data.frame(b1))
      ErrorMet <- get_metrics(cc,0.5)
    }
    
    
    ErrorMet
  },options = list(searching = FALSE,paging = FALSE))
  
  
  
  output$confusionMatrix <- renderDataTable({
    patdat <- patdata()
    algoSelected <- algoSelection()
    
    if(algoSelected == 'Logistic Regression'){
      pred.glm <- do_cv_class(patdat,10,"logreg")
      pred.glm <- pred.glm[,c(2,1)]
      glm.acc <- get_confusion_matrix(pred.glm,0.5)
      ErrorMet <- glm.acc
    }
    
    if(algoSelected == 'Support Vector Machines'){
      pred.svm <- do_cv_class(patdat,10,"svm")
      svm.acc <- get_confusion_matrix(pred.svm,0.5)
      ErrorMet <- svm.acc
    }
    
    if(algoSelected == 'Naive Bayes'){
      pred.nb <- do_cv_class(patdat,10,"nb")
      nb.acc <- get_confusion_matrix(pred.nb,0.5)
      ErrorMet <- nb.acc
    }
    
    if(algoSelected == 'k Nearest Neighbour'){
      pred.knn <- do_cv_class(patdat,10,"5nn")
      knn.acc <- get_confusion_matrix(pred.knn,0.5)
      ErrorMet <- knn.acc
    }
    
    if(algoSelected == 'Random Forest'){
      pred.rf <- do_cv_class(patdat,10,"rf")
      rf.acc <- get_confusion_matrix(pred.rf,0.5)
      ErrorMet <- rf.acc
    }
    
    if(algoSelected == 'Linear Discriminant Analysis'){
      a <- flex1()
      
      b <- patdata()
      b <- b[-1,]
      a1 <- a[,ncol(a)]
      for(i in 1:length(a1)){
        if(a1[i] > 0.8){
          a1[i] =1
        }
        if(a1[i] <= 0.8){
          a1[i] =0
        }
      }
      
      b1 <- b[,ncol(b)]
      cc <- cbind(as.data.frame(a1),as.data.frame(b1))
      ErrorMet <- get_confusion_matrix(cc)
    }
    
    ErrorMet
  },options = list(searching = FALSE,paging = FALSE))
  
  
  
  
  output$select1 <- renderUI({
     patdat <- patdata()
     cols <- colnames(patdat)
     colType <- getcolType(patdat,cols)
     vital1 <- list()
     for(i in 1:length(colType)){
       if(colType[i]=="Numeric" && cols[i] != "ID" && cols[i] != "Patient_ID"){
         vital1 <- append(vital1,cols[i])
       }
     }
     selectInput("sel1", label = "",choices = vital1)
  })
  
  output$select2 <- renderUI({
    patdat <- patdata()
    cols <- colnames(patdat)
    colType <- getcolType(patdat,cols)
    vital1 <- list()
    for(i in 1:length(colType)){
      if(colType[i]=="Numeric" && cols[i] != "ID" && cols[i] != "Patient_ID"){
        vital1 <- append(vital1,cols[i])
      }
    }
    selectInput("sel2", label = "",choices = vital1)
  })
  
  
  output$scatter <- renderGvis({
    data <- patdata()
    data <- data[,-1]
    x <- csel1()
    y <- csel2()
    if(is.null(x)){
      x< - a
      out <- textOutput("")
      out
    }
    else{
    data12 <- subset(data,select = c(x,y))
    data14 <<- as.data.frame(data[,1],stringsAsFactors = FALSE)
    data12 <- cbind(data14,data12)
    colnames(data12)[1] <- "ID"
    colnames(data12)[2] <- "xaxis"
    colnames(data12)[3] <- "yaxis"
    data12new <- data.frame(data12[2],data12[3])
    for(i in 1:nrow(data12)){
      data12[i,1] <- paste(" Patient ID : ",data12[i,1]," ")
    }
    data12new <- cbind(data12new,data12[1])
    colnames(data12new) <- c("Xaxis","Yaxis","pop.html.tooltip")
    bub <- gvisScatterChart(data12new, 
      options = list(tooltip="{isHtml:'True'}",
        trendlines="0",legend = "none"
      )
    )
    bub
  }})
  
  output$cortext <- renderText({
    data <- patdata()
    data <- data[,-1]
    x <- csel1()
    y <- csel2()
    data12 <- subset(data,select = c(x,y))
    fintext <-
      paste("The correlation between ",x,"and",y,"is",cor(as.numeric(data12[,1]) , as.numeric(data12[,2])))
    fintext
  })
  
  
  
  output$recordCount <- renderText({
    patdat <- patdata()
    numRecords <- (nrow(patdat)-1)
    printNumRecords <- paste("Number of patient records : ",numRecords)
    printNumRecords
  })
  
  output$attributeCount <- renderText({
    patdat <- patdata()
    numAttributes <- (ncol(patdat) - 2)
    printNumCols <- paste("Number of factors considered : ",numAttributes)
    printNumCols
  })
  
  colSelect <- reactive({
    colSelect_var <- input$ColSelection
  })
  
  
  impVarSelect <- reactive({
    imp_var_sel <- input$impVariableSelection
  })
  
  DriverSelect <- reactive({
    imp_Driver <- input$DriverSelection
  })

  
    output$exploreColumnsSelection <- renderUI({
    patdat <- patdata()
    patdat <- patdat[,-1]
    listCols <- colnames(patdat)
    listCols <- listCols[-1]
    selectInput("ColSelection",'',listCols)
  })
    
    output$Drivers <- renderUI({
      patlist <- patientlist()
      listCols <- levels(patlist$minCol)
      selectInput("DriverSelection",'',listCols)
    })
    
    output$dataSummary <- renderDataTable({
      patdat <- patdata()
      cols <- colnames(patdat)
      # remove ID column form columns
      colType <- getcolType(patdat,cols)
      minVals <- getNumericalMetric(patdat,colType,cols,"min")
      maxVals <- getNumericalMetric(patdat,colType,cols,"max")
      meanVals <- getNumericalMetric(patdat,colType,cols,"mean")
      sdVals <- getNumericalMetric(patdat,colType,cols,"sd")
      YesCount <- getCategoricalMetric(patdat,colType,cols,1)
      NoCount <- getCategoricalMetric(patdat,colType,cols,0)
      dataSumm <- data.frame(cols,colType,minVals,maxVals,meanVals,sdVals,YesCount,NoCount)
      dataSumm <- dataSumm[-c(1,2),]
      colnames(dataSumm) <- c("Variable Name","Variable Type","Minimum", "Maximum", "Mean", "Standard Deviation","Yes","No")
      dataSumm
    },options=list(paging = FALSE,searching = FALSE))
    
    getcolType <- function(df,cols){
      typeVector <- character(length(cols))
      for(i in 1:length(cols)){
        df[,i] <- as.numeric(df[,i])
        if(cols[i] == 'Gender'){
          typeVector[i] = 'Categorical'
        }
        else if(max(df[,i]) == 1){
          typeVector[i] = 'Categorical'
        }
        else{
          typeVector[i] = 'Numeric'
        }
      }
      return(typeVector)
    }
    
    getCategoricalMetric <- function(df,colType,cols,value){
      metricValVector <- character(length(cols))
      for(i in 1:length(cols)){
        if(colType[i] == 'Categorical'){
          oneCount <- sum(as.numeric(df[,i]))
          if(value == 1){
            percent <- round(oneCount/nrow(df)*100,2)
            metricValVector[i] <- paste(oneCount," ,%(",percent,")")
          }
          else{
            percent <- round((nrow(df)-oneCount)/nrow(df)*100,2)
            metricValVector[i] <- paste(nrow(df)-oneCount," ,%(",percent,")")
          }
        }
        else{
          metricValVector[i] <- " "
        }
      }
      return(metricValVector)
    }
    
    getNumericalMetric <- function(df,colType,cols,fn){
      metricValVector <- character(length(cols))
      for(i in 1:length(cols)){
        if(colType[i] == 'Categorical'){
          metricValVector[i] <- " "
        }
        else{
          valList <- as.numeric(as.character(df[-nrow(df),i]))
          metricValVector[i] <- do.call(fn,list(valList))
          metricValVector[i] <- round(as.numeric(metricValVector[i]),2)
        }
      }
      return(metricValVector)
    }
    
    
  output$DriverGraphs <- renderGvis({
    patlist <- patientlist()
    patlist$size <- as.numeric(as.character(patlist$size))
    patlist$size <- patlist$size
    patlist$minCol <- as.character(patlist$minCol)
    driverSelected <- DriverSelect()
    if(is.null(driverSelected)){
      print("here")
      a <- textOutput("")
      a
    }
    else{
    rowSele <- which(patlist$minCol == driverSelected)
    subpatlist <- patlist[rowSele,]
    subpatlist <- subset(subpatlist,select = -minCol)
    subpatlist$color <- rep("Patient",nrow(subpatlist))
    hasHighRisk = 0
    hasLowRisk = 0
   for(i in 1:nrow(subpatlist)){
      if(subpatlist[i,3] > 0){
        subpatlist[i,5] <- "High Risk Patients"
        hasHighRisk = 1
      }
      else{
        subpatlist[i,5] <- "Low Risk Patients"
        hasLowRisk = 1
      }
   }
    subpatlistnew <- data.frame(subpatlist[2],subpatlist[3])
    subpatlistnew <- cbind(subpatlistnew,subpatlist[1])
    colnames(subpatlistnew) <- c("Xaxis","Yaxis","pop.html.tooltip")
    if(hasHighRisk == 1 && hasLowRisk == 1){
      bub <- gvisScatterChart(subpatlistnew,options = list(tooltip="{isHtml:'True'}",legend = 'none'))      
    }
    else if (hasHighRisk == 0){
      bub <- gvisScatterChart(subpatlistnew,options = list(tooltip="{isHtml:'True'}",legend = 'none'))      
    }
    else{
      bub <- gvisScatterChart(subpatlistnew,options = list(tooltip="{isHtml:'True'}",legend = 'none'))      
    }
  bub
  }
  }
  )
  

  
  
  output$ExploreGraphs <- renderGvis({
    colSelected <<- colSelect()
    if(is.null(colSelected)){
      textOutput("")
    }
    else{
    colOrig <- colSelected
    patdat <- patdata()
    patdat <- patdat[,colSelected]
    colSelected <- tolower(colSelected)
    patdat <- as.data.frame(patdat)
    colnames(patdat) <- c(colSelected)
    if(!is.null(colSelected)){
      if(ncol(patdat)!=0){
        patdat[,1] <- as.numeric(patdat[,1])
        if(colSelected == 'gender'){
            patdat[,1] <- mapvalues(patdat[,1],c(0,1),c("Female","Male"))
            patdat[,1] <- as.factor(patdat[,1])
            reqData <- as.data.frame(round(prop.table(table(patdat[,1]))*100,2))
            reqData$Count.style = c('#FF4000','#0073B7') 
            colnames(reqData) <- c("Gender","Percent","Count.style")
            colGvis <- gvisColumnChart(reqData,xvar = "Gender",yvar = c("Percent","Count.style"),
                                       options= list(height = 400,
                                                     width = 700,
                                                     legend = 'none',
                                         title = "Distribution of Gender",
                                                     titleTextStyle = "{fontName:'Arial',fontSize:16}",
                                       vAxes="[{title:'Percentage',
                                       titleTextStyle : {fontName:'Arial'},
                                       titleTextStyle : {fontSize:14}}]"))
        }
        else if(max(patdat[,1]) == 1){
          patdat[,1] <- mapvalues(patdat[,1],c(0,1),c("No","Yes"))
          patdat[,1] <- as.factor(patdat[,1])
          reqData <- as.data.frame(round(prop.table(table(patdat[,1]))*100,2))
          reqData$Count.style = c('#FF4000','#0073B7') 
          colnames(reqData) <- c(colOrig,"Percent","Count.style")
          colGvis <- gvisColumnChart(reqData,
                                     xvar = colOrig,
                                     yvar = c("Percent","Count.style"),
                                     options= list(height = 400,
                                                   width = 700,
                                                   legend = 'none',
                                       title = paste("Distribution of ",colOrig),
                                                   titleTextStyle = "{fontName:'Arial',fontSize:16}",
                                                   vAxes="[{title:'Percentage',
                                                   titleTextStyle : {fontName:'Arial'},
                                                   titleTextStyle : {fontSize:14}}]"))
        }
        else{
          colGvis <- gvisHistogram(patdat,
                                   options = list(
                                     height = 400,
                                     legend = 'none',
                                     title = paste("Distribution of ",colOrig),
                                     titleTextStyle = "{fontName:'Arial',fontSize:16}",
                                     vAxes="[{title:'Count',
                                     titleTextStyle : {fontName:'Arial'},
                                     titleTextStyle : {fontSize:14}}]"
                                   ))
        }
      }
    }
    colGvis
  }})
  
  
  output$vitalColSelect <- renderUI({
    vitalVariables <- vital()
    editableVariables <- edit()
    impVariables <- c(vitalVariables,editableVariables)
    impVariables <- unique(impVariables)
    impVariables <- unlist(impVariables)
    selectInput("impVariableSelection",'',impVariables)
  })
  
  
  # helper function to determine if a column is numeric or categorical
  colType <- function(){
    patdat <- patdata()
    # removing ID column
    patdat <- patdat[-1]
    maxlist <- apply(patdat,2,max)
    numericList <- numeric(length(maxlist))
    for(i in 1:length(maxlist)){
      if(maxlist[i] > 1){
        numericList[i] <- 1
      }
      else{
        numericList[i] <- 0
      }
    }
    # returns a list where 1 is a numeric column and 0 indicates categorical column
    return(numericList)
  }
  
  
  #helper function to get 
  idealValues <- function(){
    impVariables <- vital()
    impVariables <- unique(impVariables)
    impVariables <- unlist(impVariables)
    temp1 <- patdata()
    temp1 <- temp1[,-1]
    temp2 <- subset(temp1,select = impVariables)
    final1 <- data.frame(Ideal = character(),stringsAsFactors = FALSE)
    max <- data.frame(Ideal = character(),stringsAsFactors = FALSE)
    for (i in 1:ncol(temp2)) {
      if (temp2[nrow(temp2),i] == 0) {
        eval(parse(text = paste("final1[",i,",1] <- input$ideal1_",i,sep = "")))
        eval(parse(text = paste("max[",i,",1] <- input$ideal3_",i,sep = "")))
      }
      if (temp2[nrow(temp2),i] == 1) {
        eval(parse(text = paste("if(input$ideal2_",i," == 1){final1[",i,",1] <- '1'} else{final1[",i,",1] <- '0'}",sep = "")))
        #eval(parse(text = paste("max[",i,",1] <- 'NA'",i,sep = "")))
      }
    }
    colnames(max) <- c("Max")
    final1$Ideal <- as.character(final1$Ideal)
    final2 <- cbind(impVariables,final1)
    final3 <- cbind(final2,max)
    return(final3)
  }
  
  
  
  output$colValues <- renderDataTable({
    patdat <- patdata()
    columnTypes <- colType()
    selectedPatient <- patientID()
    patdat <- patdat[patdat$Patient_ID == selectedPatient,]
    namesList <- colnames(patdat)
    valueList <- unlist(patdat)
    namesList <- namesList[-1]
    valueList <- valueList[-1]
    vitalVariables <- vital()
    editableVariables <- edit()
    impVariables <- c(vitalVariables,editableVariables)
    impVariables <- unique(impVariables)
    impVariables <- unlist(impVariables)
    completeData <- data.frame(namesList,valueList,rep(0,length(namesList)))
    for(i in 1:nrow(completeData)){
      if(completeData[i,1]=='Gender'){
        if(completeData[i,2]==0){
          completeData[i,2] <- 'Female'
        }
        else{
          completeData[i,2] <- 'Male'
        }
      }
      else if(columnTypes[i] == 0){
        if(completeData[i,2]==0){
          completeData[i,2] <- 'No'
        }
        else{
          completeData[i,2] <- 'Yes'
        }
      }
    }
    for(i in 1:nrow(completeData)){
      if(completeData[i,1] %in% impVariables){
        completeData[i,3] <- 1  
      }
    }
    idealVals <- idealValues()
    completeData$Ideal <- rep("NA",nrow(completeData))
    colnames(completeData) <- c("Column","Actual","isImp","Ideal")
    
    for(i in 1:nrow(completeData)){
      if(as.character(completeData[i,1]) %in% as.character(idealVals$impVariables)){
        if(!is.na(idealVals[idealVals$impVariables==as.character(completeData[i,1]),3])){
          completeData[i,4] <- paste(idealVals[idealVals$impVariables==as.character(completeData[i,1]),2]," - ",idealVals[idealVals$impVariables==as.character(completeData[i,1]),3])
        }
        else{
          completeData[i,4] <- idealVals[idealVals$impVariables==as.character(completeData[i,1]),2]
        }
      }
    }
    
    for(i in 1:nrow(completeData)){
      if(completeData[i,1]=='Gender'){
        if(completeData[i,4]==0){
          completeData[i,4] <- 'Female'
        }
        else if(completeData[i,4]==1){
          completeData[i,4] <- 'Male'
        }
      }
      else if(columnTypes[i] == 0){
        if(completeData[i,4]==0){
          completeData[i,4] <- 'No'
        }
        else if(completeData[i,4]==1){
          completeData[i,4] <- 'Yes'
        }
      }
    }
    # drop row containing ID
    completeData <- completeData[-1,]
    completeData <- completeData[-3]
    completeData$Ideal <- mapvalues(completeData$Ideal,"NA","")
    completeData <- completeData[order(completeData[,3],decreasing = T),]
    colnames(completeData) <- c("Feature","Actual Values","Ideal Values")
    # adding callback function to highlight those rows where value is greater than Ideal
    completeData
  },options = list(rowCallback = I(
    'function(row, data) {
      if(data[2] != "" && data[1] != data[2] && (isNaN(parseFloat(data[1]))))
         $("td", row).css("background", "red");
      else if(data[2] != "" && (parseFloat(data[1]) > parseFloat(data[2].split("-")[0])) && (parseFloat(data[1]) > parseFloat(data[2].split("-")[1])))
          $("td", row).css("background", "red");
      else if (data[2] != "")
          $("td", row).css("background", "green");
}'
  ),paging = FALSE,searching = FALSE)
  )
  
  
  
  #Histogram to show patients values vs ideal values
  output$hist <- renderGvis({
    impVarSelected <<- impVarSelect()
    pno <- patientID()
    
    vit2 <- vital()
    edi2 <- edit()
    
    if (nrow(vit2) == 0 & nrow(edi2) == 0) {
      bc <- textOutput("Please select vital/editable columns")
      
    } else if (nrow(edi2) == 0 & nrow(vit2) != 0) {
      allv <- c(vit2[,1])
      a12 <- unique(allv)

      temp1 <- patdata()
      temp1 <- temp1[,-1]
      temp2 <- subset(temp1,select = a12)

      final1 <- data.frame(Ideal = character(),stringsAsFactors = FALSE)
      for (i in 1:ncol(temp2)) {
        if (temp2[nrow(temp2),i] == 0) {
          eval(parse(text = paste("final1[",i,",1] <- input$ideal1_",i,sep = "")))
          #eval(parse(text = paste("final1[",i,",1] <- input$ideal3_",i,sep = "")))
        }
        if (temp2[nrow(temp2),i] == 1) {
          eval(parse(text = paste("if(input$ideal2_",i," == 1){final1[",i,",1] <- '1'} else{final1[",i,",1] <- '0'}",sep = "")))
        }
      }
      fl <- flex1()
      temp1 <- patdata()
      temp1 <- temp1[,-1]
      fl <- c(fl[,1],1000)
      
      temp1 <- cbind(fl,temp1)
      colnames(temp1)[1] <- "ID"
      
      temp4 <- subset(temp1,temp1$ID == pno)
      act2 <- subset(temp4,select = a12)
      final2 <- cbind(t(act2),final1)
      final2[,3] <- rownames(final2)
      final3 <- final2[c(3,1,2)]
      final3[,3] <- as.numeric(final3[,3])
      colnames(final3)[1] <- "variable"
      colnames(final3)[2] <- "actual"
      colnames(final3)[3] <- "ideal"
      for (i in 1:nrow(final3)) {
        if (final3[i,2] == 1) {
          final3[i,2] = 100
        }
        if (final3[i,3] == 1) {
          final3[i,3] = 100
        }
      }
      final3  <- final3[final3[1]==impVarSelected,]
      mycolnames <- c("Actual","Ideal")
      mysecondcols <- c(final3[1,2],final3[1,3])
      final4 <- data.frame(mycolnames,mysecondcols)
      final4$Count.style = c('#FF4000','#0073B7') 
      colnames(final4) <- c("Gender","Value","Count.style")
      gvisColumnChart(final4, xvar = "Gender", yvar = c("Value","Count.style"), options = list(legend = 'none', height = 350))
      
    } else if (nrow(vit2) == 0 & nrow(edi2) != 0) {
      allv <- c(edi2[,1])
      a12 <- unique(allv)

      temp1 <- patdata()
      temp1 <- temp1[,-1]
      temp2 <- subset(temp1,select = a12)
      final1 <- data.frame(Ideal = character(),stringsAsFactors = FALSE)
      
      for (i in 1:ncol(temp2)) {
        if (temp2[nrow(temp2),i] == 0) {
          eval(parse(text = paste("final1[",i,",1] <- input$ideal1_",i,sep = "")))
        }
        if (temp2[nrow(temp2),i] == 1) {
          eval(parse(text = paste("if(input$ideal2_",i," == 1){final1[",i,",1] <- '1'} else{final1[",i,",1] <- '0'}",sep = "")))
        }
      }
      fl <- flex1()
      temp1 <- patdata()
      temp1 <- temp1[,-1]
      fl <- c(fl[,1],1000)
      
      temp1 <- cbind(fl,temp1)
      colnames(temp1)[1] <- "ID"
      
      temp4 <- subset(temp1,temp1$ID == pno)
      act2 <- subset(temp4,select = a12)
      final2 <- cbind(t(act2),final1)
      final2[,3] <- rownames(final2)
      final3 <- final2[c(3,1,2)]
      final3[,3] <- as.numeric(final3[,3])
      colnames(final3)[1] <- "variable"
      colnames(final3)[2] <- "actual"
      colnames(final3)[3] <- "ideal"
      
      for (i in 1:nrow(final3)) {
        if (final3[i,2] == 1) {
          final3[i,2] = 100
        }
        if (final3[i,3] == 1) {
          final3[i,3] = 100
        }
      }
      final3  <- final3[final3[1]==impVarSelected,]
      mycolnames <- c("Actual","Ideal")
      mysecondcols <- c(final3[1,2],final3[1,3])
      final4 <- data.frame(mycolnames,mysecondcols)
      final4$Count.style = c('#FF4000','#0073B7') 
      colnames(final4) <- c("Gender","Value","Count.style")
      gvisColumnChart(final4,xvar = "Gender",yvar = c("Value","Count.style"),options = list(legend = 'none',height = 350))
    } else{
      allv <- c(edi2[,1],vit2[,1])
      a12 <- unique(allv)
      
      temp1 <- patdata()
      temp1 <- temp1[,-1]
      temp2 <- subset(temp1,select = a12)

      final1 <- data.frame(Ideal = character(),stringsAsFactors = FALSE)
      
      for (i in 1:ncol(temp2)) {
        if (temp2[nrow(temp2),i] == 0) {
          eval(parse(text = paste("final1[",i,",1] <- input$ideal1_",i,sep = "")))
        }
        if (temp2[nrow(temp2),i] == 1) {
          eval(parse(
            text = paste("if(input$ideal2_",i," == 1){final1[",i,",1] <- '1'} else{final1[",i,",1] <- '0'}",sep = "")))
        }
      }
      
      fl <- flex1()
      temp1 <- patdata()
      temp1 <- temp1[,-1]
      fl <- c(fl[,1],1000)
      
      temp1 <- cbind(fl,temp1)
      colnames(temp1)[1] <- "ID"
      
      temp4 <- subset(temp1,temp1$ID == pno)
      act2 <- subset(temp4,select = a12)
      final2 <- cbind(t(act2),final1)
      final2[,3] <- rownames(final2)
      final3 <- final2[c(3,1,2)]
      final3[,3] <- as.numeric(final3[,3])
      colnames(final3)[1] <- "variable"
      colnames(final3)[2] <- "actual"
      colnames(final3)[3] <- "ideal"
      
      for (i in 1:nrow(final3)) {
        if (final3[i,2] == 1) {
          final3[i,2] = 100
        }
        if (final3[i,3] == 1) {
          final3[i,3] = 100
        }
      }
      final3  <- final3[final3[1]==impVarSelected,]
      mycolnames <- c("Actual","Ideal")
      mysecondcols <- c(final3[1,2],final3[1,3])
      final4 <- data.frame(mycolnames,mysecondcols)
      final4$Count.style = c('#FF4000','#0073B7') 
      colnames(final4) <- c("Gender","Value","Count.style")
      gvisColumnChart(final4,xvar = "Gender",yvar = c("Value","Count.style"),options = list(legend = 'none',height = 350))
    }
  })
  
  #Histogram to show patients values vs ideal values
  output$histold <- renderGvis({
    pno <- patientID()
    
    vit2 <- vital()

    if (nrow(vit2) == 0) {
      bc <- textOutput("Please select vital/editable columns")
      
    }  else{
      allv <- c(vit2[,1])
      a12 <- unique(allv)
      
      temp1 <- patdata()
      temp1 <- temp1[,-1]
      temp2 <- subset(temp1,select = a12)

      final1 <- data.frame(Ideal = character(),
                           Maximum = character(),
                           stringsAsFactors = FALSE)
      for (i in 1:ncol(temp2)) {
        if (temp2[nrow(temp2),i] == 0) {
          eval(parse(text = paste("final1[",i,",1] <- input$ideal1_",i,sep = "")))
          eval(parse(text = paste("final1[",i,",2] <- input$ideal3_",i,sep = "")))
        }
        if (temp2[nrow(temp2),i] == 1) {
          eval(parse(text = paste("if(input$ideal2_",i," == 1){final1[",i,",1] <- '1'} else{final1[",i,",1] <- '0'}",sep = "")))
          final1[i,2] <- "0"
        }
      }
      fl <- flex1()
      temp1 <- patdata()
      temp1 <- temp1[,-1]
      fl <- c(fl[,1],1000)
      
      temp1 <- cbind(fl,temp1)
      colnames(temp1)[1] <- "ID"
      temp4 <- subset(temp1,temp1$ID == pno)
      act2 <- subset(temp4,select = a12)
      final2 <- cbind(t(act2),final1)
      final2[,4] <- rownames(final2)
      final3 <- final2[c(4,2,1,3)]
      final3[,3] <- as.numeric(final3[,3])
      final3[,2] <- as.numeric(final3[,2])
      final3[,4] <- as.numeric(final3[,4])
      colnames(final3)[1] <- "Variable"
      colnames(final3)[2] <- "Minumum"
      colnames(final3)[3] <- "Actual"
      colnames(final3)[4] <- "Maximum"
      for (i in 1:nrow(final3)) {
        if (final3[i,2] == 1) {
          final3[i,2] = 100
        }
        if (final3[i,3] == 1) {
          final3[i,3] = 100
        }
      }
      gvisColumnChart(final3,options = list(height = 500,legend = "none",series = "[{color:'#FF4000'},{color:'#0073B7'},{color:'#FF0000'}]"))
      
    }
  })
  
  
  
  #Plot to show where one patient is with respect to other patients in terms of risk
  output$popv <- renderGvis({
    temp1 <- slide()
    siz <- slide2()
    anch2 <- anch1()
    temp11 <- temp1[,c(1,ncol(temp1) - 1,ncol(temp1))]
    temp11[,4] <- 0.1
    colnames(temp11)[1] <- "ID"
    colnames(temp11)[2] <- "x"
    colnames(temp11)[3] <- "y"
    colnames(temp11)[4] <- "size"
    
    temp11$size <- temp11$size / 100000000
    
    for (i in 1:nrow(temp11)) {
      if (temp11[i,3] > 0) {
        temp11[i,5] <- 'High Risk Patients'
      }
      else{
        temp11[i,5] <- 'Low Risk Patients'
      }
    }
    
    colnames(temp11)[5] <- 'color'
    
    temp22 <- anch2
    temp22 <- temp22[,-1]
    colnames(temp22)[1] <- "ID"
    colnames(temp22)[2] <- "x"
    colnames(temp22)[3] <- "y"
    colnames(temp22)[4] <- "size"
    temp22[,5] <- 'Anchors'
    colnames(temp22)[5] <- "color"
    
    if(siz == 100){
      temp33 <- temp22 
    } else{
      temp33 <- rbind(temp11,temp22)
    }
    for(i in 1:nrow(temp33)){
      temp33[i,2] <- -1*as.numeric(temp33[i,2])
    }
    
    if(siz >= 98 & siz < 100){
      bub <- gvisBubbleChart(
        temp33, idvar = "ID",
        xvar = "x", yvar = "y",
        colorvar = "color",sizevar = "size",
        options = list(
          width = 800, height = 600,bubble.textStyle = '{fontSize : 1}',
          vAxis = "{title:'Low Risk Region...................High Risk Region'}",
          colors = "['#FE642E', '#819FF7']"
        )
      )
    }
    if(siz <98){
      bub <- gvisBubbleChart(
        temp33, idvar = "ID",
        xvar = "x", yvar = "y",
        colorvar = "color",sizevar = "size",
        options = list(
          width = 800, height = 600,bubble.textStyle = '{fontSize : 1}',
          vAxis = "{title:'Low Risk Region...................High Risk Region'}",
          colors = "['#58FA82','#FE642E', '#819FF7']"
        )
      )
      
    }
    if(siz == 100){
      bub <- gvisBubbleChart(
        temp33, idvar = "ID",
        xvar = "x", yvar = "y",
        colorvar = "color",sizevar = "size",
        options = list(
          width = 800, height = 600,bubble.textStyle = '{fontSize : 1}',
          vAxis = "{title:'Low Risk Region...................High Risk Region'}",
          colors = "['#819FF7']"
        )
      )
    }
    bub
  })
  
  #Plot to show where one patient is with respect to other patients in terms of risk
  output$popv1 <- renderGvis({
    pno <- patientID()
    temp1 <- slideStatic()
    siz <- 1
    anch2 <- anch1()
    temp11 <- temp1[,c(1,ncol(temp1) - 1,ncol(temp1))]
    temp12 <- temp1[,c(1,ncol(temp1) - 1,ncol(temp1))]
    
    temp11[,4] <- 0.1
    colnames(temp11)[1] <- "ID"
    colnames(temp11)[2] <- "x"
    colnames(temp11)[3] <- "y"
    colnames(temp11)[4] <- "size"
    
    temp12[,4] <- 0.1
    temp12[,5] <- "Selected Patient"
    colnames(temp12)[1] <- "ID"
    colnames(temp12)[2] <- "x"
    colnames(temp12)[3] <- "y"
    colnames(temp12)[4] <- "size"
    colnames(temp12)[5] <- 'color'
    
    temp11$size <- temp11$size / 100000000
    temp12$size <- temp12$size / 100000000
    temp12 <- temp12[temp12[1] == pno]
    for (i in 1:nrow(temp11)) {
      if (temp11[i,3] > 0) {
        temp11[i,5] <- 'High Risk Patients'
      }
      else{
        temp11[i,5] <- 'Low Risk Patients'
      }
    }
    temp11 <- temp11[!temp11[1]==pno,]
    colnames(temp11)[5] <- 'color'
    
    vit <- vital()
    #edit <- edit()
    
    temp22 <- anch2
    temp22 <- temp22[,-1]
    colnames(temp22)[1] <- "ID"
    colnames(temp22)[2] <- "x"
    colnames(temp22)[3] <- "y"
    colnames(temp22)[4] <- "size"
    temp22[,5] <- 'Anchors'
    colnames(temp22)[5] <- "color"
    
    #temp22$size <- temp22$size*2
    if(siz == 100){
      temp33 <- temp22 
    } else{
      temp33 <- rbind(temp12,temp11)
      temp33 <- rbind(temp33,temp22)
    }
    colnames(vit) <- c("ID")
    #colnames(edit) <- c("ID")
    temp44 <- temp33[temp33$ID %in% vit$ID,]
    #temp55 <- temp33[temp33$ID %in% edit$ID,]
    #temp44 <- rbind(temp44,temp55)
    temp44 <- unique(temp44)
    temp44[,5] <- "Vital Variables"
    temp33 <- subset(temp33, ! (ID %in% vit$ID))
    #temp33 <- subset(temp33, ! (ID %in% edit$ID))
    temp33 <- rbind(temp33,temp44)
    for(i in 1:nrow(temp33)){
      temp33[i,2] <- -1*as.numeric(temp33[i,2])
    }
    if(siz >= 50 & siz < 100){
      bub <- gvisBubbleChart(temp33, options = list(
          width = 700, height = 500,bubble.textStyle = '{fontSize : 1}',
          vAxis = "{title:'Low Risk Region...................High Risk Region'}",
          colors = "['#000000','#FE642E','#819FF7','#FF0000']"
        )
      )
    }
    if(siz <50){
      bub <- gvisBubbleChart(
        temp33, idvar = "ID",
        xvar = "x", yvar = "y",
        colorvar = "color",sizevar = "size",
        options = list(
          width = 700, height = 500,bubble.textStyle = '{fontSize : 1}',
          vAxis = "{title:'Low Risk Region...................High Risk Region'}",
          colors = "[ '#000000','#58FA82','#FE642E','#819FF7','#FF0000']"
        )
      )
      
    }
    if(siz == 100){
      bub <- gvisBubbleChart(
        temp33, idvar = "ID",
        xvar = "x", yvar = "y",
        colorvar = "color",sizevar = "size",
        options = list(
          width = 700, height = 500,bubble.textStyle = '{fontSize : 1}',
          vAxis = "{title:'Low Risk Region...................High Risk Region'}",
          colors = "['#819FF7','#FF0000']"
        )
      )
    }
    bub
  })
  
  
  #Final set of input widgets to select the values to represent change.
  output$alltext <- renderUI({
    edi2 <- edit()
    if(length(edi2)==0){
      textOutput('Please Select the Modifiable Variables')
    }
    else{
    allv <- c(edi2[,1])
    allv <- unique(allv)
    
    fl <- flex1()
    temp1 <- patdata()
    temp1 <- temp1[,-1]
    fl <- c(fl[,1],1000)
    
    temp1 <- cbind(fl,temp1)
    colnames(temp1)[1] <- "ID"
    pno <- patientIDInspect()
    
    temp2 <- subset(temp1,temp1$ID == pno,select = allv)
    temp3 <- subset(temp1,temp1$ID == 1000,select = allv) 
    temp2 <- rbind(temp2,temp3)
    print(temp2)
    temp6 <- patdata()
    temp6 <- temp6[,-1]
    temp7 <- subset(temp6, select = allv)

    temps2 <- temp2
    temp10 <<- temps2
    print(temp10)
    lapply(1:ncol(temp10), function(i) {
      if(nrow(temp6) == 589){
        if (temp10[2,i] == 0) {
          textInput(paste("finedit1",i,sep = "_"), label = paste(colnames(temp10)[i]," | Actual :",temp10[1,i],sep =""), value = temp10[1,i])
      }
      else{
        if (temp2[1,i] == 1) {
          sel = 1
          selval = "Yes"
        }
        else{
          sel = 2
          selval = "No"
        }
        selectInput(paste("finedit2",i,sep = "_"), label = paste(colnames(temp10)[i]," | Actual : ",selval,sep = ""), choices = list("Yes" = 1, "No" = 2),selected = sel)
      }
      }
      else{
        if (temp10[3,i] == 0) {
          textInput(paste("finedit1",i,sep = "_"), label = paste(colnames(temp10)[i]," | Actual :",temp10[1,i],sep =""), value = temp10[1,i])
        }
        else{
          if (temp2[1,i] == 1) {
            sel = 1
            selval = "Yes"
          }
          else{
            sel = 2
            selval = "No"
          }
          selectInput(paste("finedit2",i,sep = "_"), label = paste(colnames(temp10)[i]," | Actual : ",selval,sep = ""), choices = list("Yes" = 1, "No" = 2),selected = sel)
        }
      }
    })
  }})
}