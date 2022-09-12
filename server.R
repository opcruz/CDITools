library(shiny)
library(shinydashboard)
library(shinyjs)
library(Cairo)
library(igraph)
library(multinet)
library(MUNA)
library(visNetwork)
library(threejs)
library(rCharts)
library(RColorBrewer)
library(DT)
library(XML)

source("global.R")

shinyServer(function(input, output, session) {
  
  # Global variables
  
  #object netInfo
  netInfo <- reactiveVal()
  #object graph monoplex -> igraph object
  graphMoxReactive <- reactiveVal()
  #List of calculated centrality
  centralityMox <- reactiveVal()
  #list of calculated community detection
  communityList <- reactiveVal(list())
  #named vector of community list
  communityIdent <- reactiveVal(c())
  #object graph multiplex -> MultxData object
  graphMultxReactive <- reactiveVal()
  
  
  noLoadGraphMox <- function(){
    
    showModal(modalDialog(title = HTML("<font color='#3c8dbc'>Info</font>"), 
                          HTML("First load a monoplex graph"), 
                          easyClose = TRUE,
                          size = "s"))
  }
  
  noLoadGraphMoxAndCommDetection <- function(){
    
    showModal(modalDialog(title = HTML("<font color='#3c8dbc'>Info</font>"), 
                          HTML("First load a monoplex graph with community detection"), 
                          easyClose = TRUE,
                          size = "s"))
  }
  
  noLoadGraphMultx <- function(){
    
    showModal(modalDialog(title = HTML("<font color='#3c8dbc'>Info</font>"), 
                          HTML("First load a multiplex graph"), 
                          easyClose = TRUE,
                          size = "s"))
  }
  
  
  #TAB LOAD DATA
  
  #path of .zip file network
  datapath_network <- reactive({
    files <- input$loadFile
    if (is.null(files)) {
      return()
    }
    
    return (files$datapath)
    
  })
  
  # datapath_network trigger
  observeEvent(datapath_network() , {
    
    datapath <- datapath_network()
    if (is.null(datapath)) {
      return()
    }
    
    zipFiles <- unzip(datapath, list = TRUE)
    if(!is.element("config.xml" , zipFiles$Name)){
      showModal(modalDialog(title = HTML("<font color='red'>Error</font>"), 
                            HTML("<font color='red'>Missing file:</font> \'config.xml\'"), 
                            easyClose = TRUE))
    }else{
      
      #list files inside .zip file
      output$filesList <- renderTable({
        
        datapath <- datapath_network()
        if (is.null(datapath)) {
          return()
        }
        
        zipFiles <- zipFiles[,1:2]
        zipFiles[[2]] <- paste(round(zipFiles[[2]] / 1024 , digits = 2) , "KB")
        names(zipFiles) <- c("Names" , "Size")
        
        return (zipFiles)
        
      })

      result <- try({
      
        # object netInfo
        net <- parseConfig(datapath)
        
        # update network options values
        updateSelectInput(session,
                          inputId = "directedNet",
                          selected = if (net$graphInfo$directed)
                            "Directed"
                          else
                            "Undirected")
        
        valueW <- net$graphInfo$weighted
        updateCheckboxInput(session,
                            inputId = "weightyNet",
                            value = valueW != -1)
        
        labelLayer <- colnames(net$layersData[[1]])
        if (length(labelLayer) > 2) {
          optionsW <- as.list(tail(1:length(labelLayer) , n = -2))
          names(optionsW) <- tail(labelLayer , n = -2)
          
          if (valueW != -1) {
            updateSelectInput(
              session,
              inputId = "weightyColumn",
              choices = optionsW,
              selected = valueW
            )
            
          } else{
            updateSelectInput(session,
                              inputId = "weightyColumn",
                              choices = optionsW)
            
          }
        }else{
          updateSelectInput(session,
                            inputId = "weightyColumn",
                            choices = c("(None)"))
          
        }
        
        # Label Node
        valueLabel <- net$graphInfo$labelNode
        updateCheckboxInput(session,
                            inputId = "labelNodeNet",
                            value = valueLabel != -1)
        
        labelNodes <- colnames(net$nodesData)
        if(length(labelNodes) > 1) {
          
          optionsLN <- as.list(tail(1:length(labelNodes) , n = -1))
          names(optionsLN) <- tail(labelNodes , n = -1)
          
          if (valueW != -1) {
            updateSelectInput(
              session,
              inputId = "labelNodeColumn",
              choices = optionsLN,
              selected = valueLabel
            )
            
          } else{
            updateSelectInput(session,
                              inputId = "labelNodeColumn",
                              choices = optionsLN)
          }
        }else{
          updateSelectInput(session,
                            inputId = "labelNodeColumn",
                            choices = c("(None)"))
        }
        
        # assign global variable
        netInfo(net)
        # enable button generate graph
        enable("applyNet")
      
      }, silent = FALSE)
      
      if (class(result) == "try-error") {
        msg <- geterrmessage()
        showModal(modalDialog(title = HTML("<font color='red'>Error</font>"), 
                              HTML(msg), 
                              easyClose = TRUE,
                              size = "m"))
      }
    }
  })
  
  #list nodes in a table
  output$nodesNetwork <- DT::renderDataTable(

    DT::datatable({

      net <- netInfo()
      if (is.null(net)) {
        return()
      }
      
      net[["nodesData"]]

    },
    options = list(lengthMenu = list(c(5, 15, 30), c('5', '15', '30')),
                   pageLength = 15),
    style = "bootstrap"
    ))
  
  #list first layer in a table
  output$firstlayerNetwork <- DT::renderDataTable(
    
    DT::datatable({
      
      net <- netInfo()
      if (is.null(net)) {
        return()
      }
      
      net$layersData[[1]]
      
    },
    options = list(lengthMenu = list(c(5, 15, 30), c('5', '15', '30')),
                   pageLength = 15),
    style = "bootstrap"
    ))
  
  #list network properties
  output$netSummary <- renderTable({

    net <- netInfo()
    if (is.null(net)) {
      return()
    }
    
    g <- data.frame(
      stringsAsFactors = FALSE,
      Indicators = c(
        "Type graph",
        "Directed",
        "Weigthed",
        "Nodes"
      ),
      Values = c(
        net$graphInfo$type,
        net$graphInfo$directed,
        net$graphInfo$weighted != -1,
        if (!is.null(net[["nodesData"]])) nrow(net[["nodesData"]]) else NA
      )
    )
    
    listLayer <- net$layersData
    i <- 1
    for(l in listLayer){
      
      name <- paste0("Edges Layer",i)
      value <- nrow(l)
      g <- rbind(g, list(name , value))
      
      i <- i + 1
    }

    return(g)

  })
  
  #update values network and build graph
  observeEvent(input$applyNet,{
    
    net<- netInfo()
    if (is.null(net)) {
      return()
    }
    
    progress <- shiny::Progress$new(session)
    on.exit(progress$close())
    progress$set(message = 'Importing options...', value = 0.2)
    
    net$graphInfo$directed <- if(input$directedNet == "Directed") TRUE else FALSE
   
    if(input$weightyNet && input$weightyColumn != "(None)"){
      net$graphInfo$weighted <- as.numeric(input$weightyColumn)
    }else{
      net$graphInfo$weighted <- -1
    }
    
    if(input$labelNodeNet && input$labelNodeColumn != "(None)") {
      net$graphInfo$labelNode <- as.numeric(input$labelNodeColumn)
    } else{
      net$graphInfo$labelNode <- -1
    }
    
    netInfo(net)
    
    progress$set(detail = 'Building the network...', value = 0.4)
    Sys.sleep(0.5)
    
    if(net$graphInfo$type == "monoplex"){
      
      g <- graph_from_data_frame(d=net$layersData[[1]], 
                       vertices= net[["nodesData"]], 
                       directed=net$graphInfo$directed)
      
      if(net$graphInfo$weighted != -1){
        
        nameAttr <- colnames(net$layersData[[1]])[net$graphInfo$weighted]
        edge_attr(g, "weight") <- edge_attr(g , name = nameAttr)
        
      }
      
      if(net$graphInfo$labelNode != -1){
        
        labelAttr <- colnames(net$nodesData)[net$graphInfo$labelNode]
        vertex_attr(g, "label_ID") <- vertex_attr(g , name = labelAttr)
        
      }
      
      #reset community value
      communityList(list())
      communityIdent(c())
      
      #set graph object
      graphMoxReactive(g)
      
      showModal(modalDialog(title = HTML("<font color='#3c8dbc'>Info</font>"), 
                            HTML("Monoplex graph load successfully"), 
                            easyClose = TRUE,
                            size = "s"))
    
    }else{
      
      mult <- importMultiLayer(net)
      
      #set multiplex graph object
      graphMultxReactive(mult)
      
      showModal(modalDialog(title = HTML("<font color='#3c8dbc'>Info</font>"), 
                            HTML("Multiplex graph load successfully"), 
                            easyClose = TRUE,
                            size = "s"))
      
    }
    
    progress$set(detail = 'Import Completed!', value = 1)
    Sys.sleep(0.5)
    
  })
  
  
  #TAB ANALYSIS MONOPLEX
  
  # apply graph filters
  observeEvent(input$apply_filters,{
    
    g <- graphMoxReactive()
    if (is.null(g)) {
      noLoadGraphMox()
      return()
    }
    
    if (input$remove_loops){
      g <- simplify(g, remove.multiple = FALSE, remove.loops = TRUE)
    }
    if (input$remove_mult_edges){
      g <- simplify(g, remove.multiple = TRUE, remove.loops = FALSE)
    }
    
    if(input$remove_weak_connection){
      
      if(input$threshold_weak_connection == "mean_min_weak_connection"){
        
        list <- incident_edges(g , v = V(g))
        minValues <- c()
        for(inc_edge in list){
          if(length(inc_edge) != 0){
            minValues <- append(minValues , min(edge_attr(g, "weight", index = inc_edge)))
          }
        }
        
        threshold <- mean(minValues)
        
      }else{
        
        threshold <- input$custom_value_weak_connection
        
      }
      
      v <- E(g)$weight < threshold
      g <- delete_edges(g , as_ids(E(g))[v])
    }
    
    if (input$zero_degree_net) {
      g <- delete_vertices(g, degree(g) == 0)
    }
    
    #reset community value
    communityList(list())
    communityIdent(c())
    
    #set graph object
    graphMoxReactive(g)
    
    showModal(modalDialog(title = HTML("<font color='#3c8dbc'>Info</font>"), 
                          HTML("Filters applied correctly"), 
                          easyClose = TRUE,
                          size = "s"))
    
  })
  
  
  output$down_mon_graph <- downloadHandler(
    filename = function(){
      
      paste0('Network',format(Sys.time(),"%Y%m%d_%H%M%S"),'.zip')
      
    },
    content = function(file){
      
      g <- graphMoxReactive()
      if (is.null(g)) {
        noLoadGraphMox()
        return()
      }
      
      save_monoplex_graph(g , file)
      
    },
    contentType = "application/zip"
    
  )
  
  
  # table properties
  output$netProperties <- renderTable({
    g <- graphMoxReactive()
    if (is.null(g)) {
      return()
    }
    
    values <- data.frame(
      stringsAsFactors = FALSE,
      Indicators = c(
        "Nodes",
        "Edges",
        "Directed",
        "Weighted",
        "Density",
        "Radius",
        "Diameter",
        "Average path length",
        "Grade average",
        "Cluster coefficient",
        "Assortativity degree"
      ),
      Values = c(
        vcount(g),
        ecount(g),
        as.character(is_directed(g)),
        as.character(is_weighted(g)),
        round(edge_density(g), digits = 3) ,
        radius(g),
        diameter(g),
        round(mean_distance(g), digits = 3),
        round(mean(degree(g)), digits = 3),
        round(transitivity(g), digits = 3),
        round(assortativity_degree(g), digits = 3)
      )
    )
    
    return(values)
    
  },
  striped = TRUE,
  # hover = TRUE,
  bordered = TRUE,
  spacing = 'l',
  width = "100%"
  )
  
  # click calculate centrality
  observeEvent(input$cal_centrality,{
    
    g <- graphMoxReactive()
    if (is.null(g)) {
      noLoadGraphMox()
      return()
    }
    
    # Calculate Centrality
    progress <- shiny::Progress$new(session)
    on.exit(progress$close())
    progress$set(message = 'Calculation in progress')
    
    
    l <- list()
    v <- vector()
    if(input$degreeNet){
      
      progress$set(detail = "Calculating Degree Centrality", value = 0.2)
      Sys.sleep(0.2)
      
      temp <- list(degree(g))
      names(temp) <- c("degree")
      l <- append(l , temp )
      v <- append(v , c("Degree"="degree"))
    }
    
    if(input$strengthNet){
      
      progress$set(detail = "Calculating Strength Centrality", value = 0.3)
      Sys.sleep(0.2)
      
      temp <- list(strength(g))
      names(temp) <- c("strength")
      l <- append(l , temp )
      v <- append(v , c("Strength"="strength"))
    }
    
    if(input$closenessNet){
      
      progress$set(detail = "Calculating Closeness Centrality", value = 0.4)
      Sys.sleep(0.2)
      
      temp <- list(closeness(g))
      names(temp) <- c("closeness")
      l <- append(l , temp )
      v <- append(v , c("Closeness"="closeness"))
    }
    
    if(input$betweennessNet){
      
      progress$set(detail = "Calculating Betweenness Centrality", value = 0.5)
      Sys.sleep(0.2)
      
      temp <- list(betweenness(g))
      names(temp) <- c("betweenness")
      l <- append(l , temp )
      v <- append(v , c("Betweenness"="betweenness"))
    }
    
    if(input$pagerankNet){
      
      progress$set(detail = "Calculating PageRank Centrality", value = 0.6)
      Sys.sleep(0.2)
      
      temp <- list(page_rank(g)$vector)
      names(temp) <- c("page_rank")
      l <- append(l , temp )
      v <- append(v , c("PageRank"="page_rank"))
    }
    
    if(input$kcoreNet){
      
      progress$set(detail = "Calculating K-core Centrality", value = 0.7)
      Sys.sleep(0.2)
      
      temp <- list(coreness(g))
      names(temp) <- c("k_core")
      l <- append(l , temp )
      v <- append(v , c("K-core"="k_core"))
    }
   
    if(input$eigenvectorNet){
      
      progress$set(detail = "Calculating Eigenvector Centrality", value = 0.8)
      Sys.sleep(0.2)
      
      temp <- list(eigen_centrality(g)$vector)
      names(temp) <- c("eigen_centrality")
      l <- append(l , temp )
      v <- append(v , c("Eigenvector"="eigen_centrality"))
    }
    
    if(input$hubNet){
      
      progress$set(detail = "Calculating Hub Centrality", value = 0.9)
      Sys.sleep(0.2)
      
      temp <- list(hub_score(g)$vector)
      names(temp) <- c("hub_score")
      l <- append(l , temp )
      v <- append(v , c("Hub"="hub_score"))
    }
    
    if(input$authorityNet){
      
      progress$set(detail = "Calculating Authority Centrality", value = 1)
      Sys.sleep(0.2)
      
      temp <- list(authority_score(g)$vector)
      names(temp) <- c("authority_score")
      l <- append(l , temp )
      v <- append(v , c("Authority"="authority_score"))
    }
    
    #update global value
    centralityMox(l)
    
    progress$set(detail = 'Calculation Completed!', value = 1)
    Sys.sleep(0.45)
    
    output$boxCentrality <- renderUI({
      
        box(
          title = "Plot",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
       
          selectInput(
            inputId = "pickCentrality",
            label = "Select Centrality:",
            choices = v
          ),
          
          selectInput(
            inputId = "tpyeChartCentrality",
            label = "Type:",
            choices = c("line","column")
          ),
          
          sliderInput("breaksChartCentrality", "Breaks Number:", value = 20, 
                      min = 2 , max = 100),
         
          tags$hr(style = "border-color: #3c8dbc;"),
        
        HTML("<center>"),
        actionButton("plotButton_centrality",
                     "Plot Centrality"),
        HTML("</center>")
        
      )
    })
    
  })
  
  # click plot centrality
  observeEvent(input$plotButton_centrality,{
    
    list <- centralityMox()
    if (is.null(list)) {
      return()
    }
    
    centrality <- input$pickCentrality
    typeChart <- input$tpyeChartCentrality
    breaksChart <- input$breaksChartCentrality
    
    hh <- hist(list[[centrality]], breaks = breaksChart , plot = FALSE)
    
    output$centralityChart <- renderChart2({
        
        value <- (hh$mids[2] - hh$mids[1])/2
       
        s <- hPlot(x= "mids", y = "counts", data = hh , type = typeChart)
        s$tooltip(formatter = paste0("#! function() { return '<strong>Intervale:</strong> ' + (this.x - ", 
                                     value ,") + ' - ' + (this.x + ", value ,
                                     ") + '<br><strong>Count:</strong> ' + this.y; } !#"))
        
        
        s$xAxis(title = list(text = centrality))
        s$yAxis(title = list(text = "Count"))
        
        return(s)
        
      })
  })
  
  #TAB COMMUNITY DETECTION MONOPLEX
  
  # disable algorithm no available for directed network and more
  observeEvent(graphMoxReactive(), {
    
    g <- graphMoxReactive()
    if (is.null(g)) {
      return()
    } 
    
    if(is_directed(g)){
      
      #no available for directed networks
      updateCheckboxInput(session, "community_fastgreedy", value = FALSE)
      disable("community_fastgreedy")
      updateCheckboxInput(session, "community_multLouv", value = FALSE)
      disable("community_multLouv")
      
    }else{
      
      enable("community_fastgreedy")
      enable("community_labelprop")
      enable("community_multLouv")
      enable("community_walktrap")
      enable("community_eigenvector")
      enable("community_infomap")
      enable("community_betweenness")
      enable("community_spinglass")
      
    }
    
    if(is_weighted(g)){
      
      enable("remove_weak_connection")
      
    }else{
      
      updateCheckboxInput(session, "remove_weak_connection", value = FALSE)
      disable("remove_weak_connection")
    }
    
    # vertex label
    if(is.element("label_ID" , vertex_attr_names(g))) {
      updateSelectInput(session,
                        "label_simplePlot",
                        choices = c("(None)" , "ID" , "Label"))
      
    } else{
      updateSelectInput(session, "label_simplePlot",
                        choices = c("(None)" , "ID"))
    }
    
  })
  
  # button run community detection
  observeEvent(input$cal_comm_detec,{
    
    g <- graphMoxReactive()
    if (is.null(g)) {
      noLoadGraphMox()
      return()
    }
    
    showModal(
      modalDialog(
        "Compute Community Detection" ,
        title = "Community Detection",
        size = 's',
        # easyClose = TRUE,
        footer = NULL,
        fade = FALSE
      )
    )
      
    result <- try({
      
      progress <- shiny::Progress$new(session)
      on.exit(progress$close())
      progress$set(message = 'Calculation in progress')
      
      l <- list()
      v <- vector()
      
      cl <- communityList()
      ci <- communityIdent()
      
      if(input$community_fastgreedy && !is.element("fastgreedy", ci)){
        
        progress$set(detail = "Compute Fast Greedy algorithm", value = 0)
        
        t <- system.time(com <- cluster_fast_greedy(g))
        com$time <- t[["elapsed"]]
        temp <- list(com)
        names(temp) <- c("fastgreedy")
        l <- append(l , temp )
        v <- append(v , c("Fast Greedy"="fastgreedy"))
      }
      
      if(input$community_labelprop && !is.element("labelprop", ci)){
        
        progress$set(detail = "Compute Label Propagation algorithm", value = 0.125)
        
        t <- system.time(com <- cluster_label_prop(g))
        com$time <- t[["elapsed"]]
        temp <- list(com)
        names(temp) <- c("labelprop")
        l <- append(l , temp )
        v <- append(v , c("Label Propagation"="labelprop"))
      }
      
      if(input$community_multLouv && !is.element("multLouv", ci)){
        
        progress$set(detail = "Compute MultiLevel Louvain algorithm", value = 0.25)
        
        t <- system.time(com <- cluster_louvain(g))
        com$time <- t[["elapsed"]]
        temp <- list(com)
        names(temp) <- c("multLouv")
        l <- append(l , temp )
        v <- append(v , c("MultiLevel Louvain"="multLouv"))
      }
      
      if(input$community_walktrap && !is.element("walktrap", ci)){
        
        progress$set(detail = "Compute WalkTrap algorithm", value = 0.375)
        
        t <- system.time(com <- cluster_walktrap(g))
        com$time <- t[["elapsed"]]
        temp <- list(com)
        names(temp) <- c("walktrap")
        l <- append(l , temp )
        v <- append(v , c("WalkTrap"="walktrap"))
      }
      
      if(input$community_eigenvector && !is.element("eigenvector", ci)){
        
        progress$set(detail = "Compute Leading Eigenvector algorithm", value = 0.5)
        
        t <- system.time(com <- cluster_leading_eigen(g))
        com$time <- t[["elapsed"]]
        temp <- list(com)
        names(temp) <- c("eigenvector")
        l <- append(l , temp )
        v <- append(v , c("Leading Eigenvector"="eigenvector"))
      }
      
      if(input$community_infomap && !is.element("infomap", ci)){
        
        progress$set(detail = "Compute InfoMap algorithm", value = 0.625)
        
        t <- system.time(com <- cluster_infomap(g))
        com$time <- t[["elapsed"]]
        temp <- list(com)
        names(temp) <- c("infomap")
        l <- append(l , temp )
        v <- append(v , c("InfoMap"="infomap"))
      }
      
      if(input$community_betweenness && !is.element("betweenness", ci)){
        
        progress$set(detail = "Compute Edge Betweenness algorithm", value = 0.75)
        
        t <- system.time(com <- cluster_edge_betweenness(g))
        com$time <- t[["elapsed"]]
        temp <- list(com)
        names(temp) <- c("betweenness")
        l <- append(l , temp )
        v <- append(v , c("Edge Betweenness"="betweenness"))
      }
      
      if(input$community_spinglass && !is.element("spinglass", ci)){
        
        progress$set(detail = "Compute Spinglass algorithm", value = 0.875)
        
        t <- system.time(com <- cluster_spinglass(g))
        com$time <- t[["elapsed"]]
        temp <- list(com)
        names(temp) <- c("spinglass")
        l <- append(l , temp )
        v <- append(v , c("Spinglass"="spinglass"))
      }
      
      #update global value
      communityList(append(cl, l))
      communityIdent(append(ci, v))
      
    }, silent = FALSE)
    
    
    
    if (class(result) == "try-error") {
      
      removeModal(session = session)
      msg <- geterrmessage()
      showModal(modalDialog(title = HTML("<font color='red'>Error</font>"),
                            HTML(msg),
                            easyClose = TRUE,
                            size = "m"))
      
    }else{
      
      progress$set(detail = 'Calculation Completed!', value = 1)
      Sys.sleep(0.45)
      
      removeModal(session = session)
      
    }
    
  })
  
  # set community detection 
  observeEvent(communityList(),{  
    
    l <- communityList()
    v <- communityIdent()
    
    default <- c("(None)" = "(None)")
    
    if (length(l) == 0 || length(v) == 0) {
      
      updateSelectInput(session, "selectCommunity", choices = default)
      updateSelectInput(session, "community_simplePlot", choices = default)
      updateSelectInput(session, "community_3dPlot", choices = default)
      updateSelectInput(session, "community_plotVis", choices = default)
      updateSelectInput(session, "community_compare_first", choices = default)
      updateSelectInput(session, "community_compare_second", choices = default)
      
    }else{
      
      updateSelectInput(session, "selectCommunity", choices = v)
      updateSelectInput(session, "community_plotVis", choices = v)
      updateSelectInput(session, "community_compare_first", choices = v)
      
      listComm <- append(default , v)
      updateSelectInput(session, "community_simplePlot", choices = listComm)
      updateSelectInput(session, "community_3dPlot", choices = listComm)
      
    }
    
  })
  
  # set values second community compare
  observe({  
    
    label1 <- input$community_compare_first
    
    if (label1 == "(None)") {
      return()
    }
    
    ci <- communityIdent()
    
    if(length(ci)>1) {
      
      updateSelectInput(session, 
                        "community_compare_second", 
                        choices = ci[ci!=label1])
      
    }else{
      
      updateSelectInput(session, 
                        "community_compare_second", 
                        choices = c("(None)" = "(None)"))
      
    }
  })
  
  # share graph (similarity)
  observeEvent(input$shareGraph,{  
    
    g <- graphMoxReactive()
    l <- communityList()
    
    if(!is.element(input$selectCommunity , names(l)) || is.null(g)){
      noLoadGraphMoxAndCommDetection()
      return()
    }
    
    com <- l[[input$selectCommunity]]
    
    # graphToModVis(g)
    # 
    # comm <- membership(com)
    # 
    # commtoshare<-matrix(0,length(V(g)),2)
    # 
    # commtoshare[,1]<-V(g)
    # commtoshare[,2]<-comm
    # 
    # 
    # communityToModVis(commtoshare)
    # 
    # isOverlap(FALSE)
    
    
  })
  
  # Add file community
  
  community_file <- reactive({
    
    file <- input$community_file
    if (is.null(file)) {
      return()
    }
    
    read.table(
      file = file$datapath ,
      sep = input$community_sep,
      header = input$community_header,
      stringsAsFactors = FALSE
    )
  })
  
  
  output$table_community_file <- DT::renderDataTable(
    
    DT::datatable({
      
      data <- community_file()
      if (!is.null(data)) {
        
        return(data)
        
      }
      
    },
    options = list(lengthMenu = list( c(7,15,-1), c('5','15','All')),
                   pageLength = 15),
    style = "bootstrap"
    ))
  
  
  observeEvent(community_file(),{  
    
    data <- community_file()
    if (is.null(data)) {
      return()
    }
    
    values <- 1:ncol(data)
    names(values) <- colnames(data)
    
    updateSelectInput(
      session,
      inputId = "community_column",
      choices = values
      
    )
    
  })
  
  observeEvent(input$load_community,{  
    
    data <- community_file()
    g <- graphMoxReactive()
    
    if (is.null(data) || is.null(g)) {
      noLoadGraphMox()
      return()
    }
    
    v <- data[[as.numeric(input$community_column)]]
    
    if(length(v) == gorder(g)){
      
      name <- input$community_file$name
      minVal <- min(v)
      
      if(minVal != 1){
        v <- v + (1 - minVal)
      }
      
      com <- make_clusters(
        g,
        membership = v,
        algorithm = name,
        modularity = TRUE
      )
      
      com$time <- 0
      
      cl <- communityList()
      ci <- communityIdent()
      
      temp <- list(com)
      value <- c(name)
      names(temp) <- value
      names(value) <- value
      
      cl <- append(cl , temp)
      ci <- append(ci , value)
      
      communityList(cl)
      communityIdent(ci)
      
      showModal(modalDialog(
        title = HTML("Add Community Detection"),
        HTML("Community detection added successfully"),
        easyClose = TRUE,
        size = "s"
      ))
      
    }else{
      
      showModal(modalDialog(
        title = HTML("<font color='red'>Error</font>"),
        HTML("Different vertex number"),
        easyClose = TRUE,
        size = "s"
      ))
      
    }
    
  })
  
  
  
  output$down_all_communities <- downloadHandler(
    filename = function(){
      
      paste0('Communities',format(Sys.time(),"%Y%m%d_%H%M%S"),'.zip')
      
    },
    content = function(file){
      
      cl <- communityList()
      if (length(cl) == 0) {
        return()
      }
      
      tmpdir <- tempdir()
      path <- getwd()
      setwd(tempdir())
      
      fn <- c()
      
      for(com in cl){
        
        name <- paste0(com$algorithm, ".txt")
        
        write.table(
          com$membership,
          file = name ,
          row.names = FALSE,
          col.names = FALSE
        )
        
        fn <- append(fn, name)
      }
      
      zip(zipfile = file, files = fn)
      
      setwd(path)
      
    },
    contentType = "application/zip"
    
  )
  
  ##### Evaluation #####
  
  df_community_properties <- reactive({
    
    l <- communityList()
    if (is.null(l) || length(l) == 0) {
      return()
    }
    
    nam <- c()
    mod <- c()
    nCom <- c()
    timeElap <- c()
    
    for(i in 1:length(l)){
      
      com <- l[[i]]
      nam <- append(nam, com$algorithm)
      mod <- append(mod, modularity(com))
      nCom <- append(nCom, max(membership(com)))
      timeElap <- append(timeElap, round(com$time, digits = 3))
      
    }
    
    df <- data.frame(
      stringsAsFactors = FALSE,
      as.character(nam),
      mod,
      as.integer(nCom),
      timeElap
    )
    colnames(df) <-
      c("Algorithm" ,
        "Modularity" ,
        "Community Number" ,
        "Elapsed Time(sec)")
    
    return(df)
  })
  
  # table community properties
  output$communityComparison <- renderTable({
    
    return(df_community_properties())
    
  },
  striped = TRUE,
  # hover = TRUE,
  bordered = TRUE,
  spacing = 'l',
  width = "100%"
  
  )
  
  output$down_community_properties <- downloadHandler(
    filename = function(){
      
      paste0('CommunityProperties',format(Sys.time(),"%Y%m%d_%H%M%S"),'.txt')
      
    },
    content = function(file){
      
      write.table(
        df_community_properties(),
        file = file,
        row.names = FALSE,
        col.names = TRUE,
        quote = TRUE,
        sep = '\t'
      )
    }
  )
  
  
  df_community_compare <- reactive({
    
    if(input$action_community_compare == 0){
      return()
    }
    
    cl <- isolate(communityList())
    labelcom1 <- isolate(input$community_compare_first)
    labelcom2 <- isolate(input$community_compare_second)
    
    if (length(cl) < 2 || labelcom1 == ("(None)") || labelcom2 == ("(None)")) {
      return()
    }
    
    com1 <- cl[[labelcom1]]
    com2 <- cl[[labelcom2]]
    
    values <- data.frame(
      stringsAsFactors = FALSE,
      Indicators = c(
        "vi",
        "nmi",
        "split.join",
        "rand",
        "adjusted.rand"
      ),
      Values = c(
        compare(com1, com2, method = c("vi")),
        compare(com1, com2, method = c("nmi")),
        compare(com1, com2, method = c("split.join")),
        compare(com1, com2, method = c("rand")),
        compare(com1, com2, method = c("adjusted.rand"))
      )
    )
    
    return (list(
      "df" = values ,
      "community1" = labelcom1 ,
      "community2" = labelcom2
    ))
  
  })
  
  
  output$table_community_compare <- renderTable({
    
    return(df_community_compare()$df)
    
  },
  striped = TRUE,
  # hover = TRUE,
  bordered = TRUE,
  spacing = 'l',
  width = "100%"
  
  )
  
  output$down_community_compare <- downloadHandler(
    filename = function(){
      
      paste0(
        'Compare',
        '_' ,
        df_community_compare()$community1,
        '_' ,
        df_community_compare()$community2,
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        '.txt'
      )
      
    },
    content = function(file){
      
      write.table(
        df_community_compare()$df,
        file = file,
        row.names = FALSE,
        col.names = TRUE,
        quote = TRUE,
        sep = '\t'
      )
    }
  )
  
  ###############Simple Plot#############
  
  layout_simpleGraph <- reactive({
    
    g <-graphMoxReactive()
    switch (input$layout_simplePlot,
            
            "layout_nicely" = layout_nicely(g, dim = 2),
            "layout_fr" = layout_with_fr(g, dim = 2),
            "layout_graphopt" = layout_with_graphopt(g),
            "layout_kk" = layout_with_kk(g, dim = 2),
            "layout_lgl" = layout_with_lgl(g)
    )
    
  })
  
  #function to plot simple graph
  plotSimpleGraph <- reactive({
    
    if (is.null(graphMoxReactive())) {
      return()
    }
    
    g <-graphMoxReactive()
    
    if(input$vertex_size_simplePlot == "fixed"){
      
      V(g)$size <- input$fixed_vsize_simplePlot
      
    }else{
      
      centrality <- switch (input$centrality_pick_simplePlot,
                            
                              degree = degree(g),
                              strength = strength(g)
                            )
      
      rangeSize <- input$centrality_vsize_simplePlot
      V(g)$size <- normalize_size(centrality , rangeSize[[1]] , rangeSize[[2]])
      
    }
    
    g$layout <- layout_simpleGraph()
    
    palette <- switch (input$palette_simplePlot,
                            
                       "color_rainbow" = rainbow(15),
                       "color_brewer_paired" = brewer.pal(12, "Paired"),
                       "color_brewer_set1" = brewer.pal(9, "Set1"),
                       "color_brewer_set2" = brewer.pal(8, "Set2"),
                       "color_brewer_set3" = brewer.pal(12, "Set3")
    )
    
    
    
    if(input$community_simplePlot == "(None)"){
    
      V(g)$color <- palette[3]
    
    }else{
      
      l <- communityList()
      if(is.element(input$community_simplePlot , names(l))){

        comm <- l[[input$community_simplePlot]]
        colorsVertex <- membership(comm) %% length(palette)
        V(g)$color <- palette[colorsVertex + 1]
        
      }
    }
    
    V(g)$label <- switch (input$label_simplePlot,
                          
                          "(None)" = NA,
                          "ID" = as_ids(V(g)),
                          "Label" = vertex_attr(g, name = "label_ID")
                          
    )
    
    return(g)
    
  })

  
  output$simple_plot_output <- renderPlot({
    
    if(input$plot_simpleGraph > 0){
      
      g <- isolate(plotSimpleGraph())
      if(!is.null(g)){
        plot(
          g,
          edge.arrow.size=0.5
        )
      }else{
        noLoadGraphMox()
      }
    }
    
  })
  
  
  output$down_simple_plot <- downloadHandler(
    filename = function(){
      
      paste0('Network',format(Sys.time(),"%Y%m%d_%H%M%S"),'.pdf')
      
    },
    content = function(file){
      
      pdf(file)
      plot(
        plotSimpleGraph(),
        edge.arrow.size=0.5
      )
      dev.off()
      
    }
    
  )
  
  # 3d graph
  layout_3dGraph <- reactive({
    
    g <-graphMoxReactive()
    switch (input$layout_3dPlot,
            
            "layout_nicely" = layout_nicely(g, dim = 3),
            "layout_fr" = layout_with_fr(g, dim = 3 , niter = 100),
            "layout_sphere" = layout_on_sphere(g),
            "layout_kk" = layout_with_kk(g, dim = 3),
            "layout_drl" = layout_with_drl(g, dim = 3)
    )
   
  })
  
  
  plot3dGraph <- reactive({
    
    if (is.null(graphMoxReactive())) {
      return()
    }
    
    g <-graphMoxReactive()
    
    if(input$vertex_size_3dPlot == "fixed"){
      
      V(g)$size <- input$fixed_vsize_3dPlot
      
    }else{
      
      centrality <- switch (input$centrality_pick_3dPlot,
                            
                            degree = degree(g),
                            strength = strength(g)
      )
      
      rangeSize <- input$centrality_vsize_3dPlot
      V(g)$size <- normalize_size(centrality , rangeSize[[1]] , rangeSize[[2]])
      
    }
    
    palette <- switch (input$palette_3dPlot,
                       
                       "color_rainbow" = rainbow(15),
                       "color_brewer_paired" = brewer.pal(12, "Paired"),
                       "color_brewer_set1" = brewer.pal(9, "Set1"),
                       "color_brewer_set2" = brewer.pal(8, "Set2"),
                       "color_brewer_set3" = brewer.pal(12, "Set3")
    )
    
    initialLayout <- layout_3dGraph()
    click <- NULL
    
    labels <- paste0("<b>Node:</b> ",as_ids(V(g)))
    
    if(is.element("label_ID" , vertex_attr_names(g))) {
      
      labels <- paste0(labels, " ",
                       "<b>Label: </b>", vertex_attr(g, name = "label_ID"))
    }
    
    if(input$community_3dPlot == "(None)"){
      
      V(g)$color <- palette[3]
      V(g)$label <- paste0("<p>", labels ,"</p>")
      
    }else{
      
      l <- communityList()
      if(is.element(input$community_3dPlot , names(l))){
        
        comm <- l[[input$community_3dPlot]]
        
        V(g)$label <- paste0("<p>" , labels , " ",
                             "<b>Community: </b>", membership(comm) , "</p>")
        
        if(input$collapsed_3dPlot){
          
          # Vertex degree values (a measure of network centrality for each vertex)
          pr <- degree(g)
          # order degree values
          i <- order(pr, decreasing=TRUE)
          # Vertex cluster membership
          cl <- membership(comm)
          # Find the index of the highest centrality vertex in each cluster
          idx <- aggregate(i, by=list(cl[i]), FUN=head, 1)$x
          # Create a default force-directed layout for the whole network
          l1 <- initialLayout
          # Collapse the layout to just the idx vertices
          l0 <- Reduce(rbind, Map(function(i) l1[idx[i],], cl))
          
          #overwrite layout
          initialLayout <- l0
          
          # Create grouped vertex colors, setting all but idx vertices transparent
          col <- rainbow(length(idx), alpha=0)[cl]
          col[idx] <- rainbow(length(idx), alpha=1)
          
          V(g)$color <- col
          
          # animation layouts, one for each of the idx vertices, and
          # animation color schemes, one scheme for each idx vertex
          click <- Map(function(i)
          {
            x <- l0
            x[cl == i, ] <- l1[cl == i, ]
            c <- col
            c[cl == i] <- rainbow(length(idx), alpha=1)[i]
            list(layout=x, vertex.color=c)
          }, seq(idx))
          names(click) <- paste(idx)
          
        }else{
          
          colorsVertex <- membership(comm) %% length(palette)
          V(g)$color <- palette[colorsVertex + 1]
          
        }
      }
    }
    
    gjs <- graphjs(
      g,
      vertex.shape = "@",
      fps = 60,
      click = click,
      bg = if(input$background_3dPlot) "#333333" else "white" ,
      fpl=300,
      layout= if(input$animated_3dPlot && !input$collapsed_3dPlot){
        list(
          layout_randomly(g, dim=3),
          layout_on_sphere(g),
          initialLayout)
      }else{
        initialLayout
      }
    )
    
    return(gjs)
    
  })
  
  
  output$graph_3d <- renderScatterplotThree({
    
    if(input$plot_3dGraph > 0){
    
      g <- isolate(plot3dGraph())
      if(!is.null(g)){
        return(g)
      }else{
        noLoadGraphMox()
      }
    }
    
  })
  
  
  output$down_3d_plot <- downloadHandler(
    filename = function(){
      
      paste0('Network3D',format(Sys.time(),"%Y%m%d_%H%M%S"),'.html')
      
    },
    content = function(file){
      
      saveWidget(plot3dGraph(), file=file, selfcontained = TRUE)
      
    },
    contentType = 'html'
    
  )
  
  
  #simplify graph with community detection (visNetwork)
  
  plotCommunityGraph <- reactive({
    
    l <- communityList()
    
    if (is.null(graphMoxReactive()) || !is.element(input$community_plotVis , names(l))) {
      return()
    }
    g <- graphMoxReactive()
    
    progress <- shiny::Progress$new(session)
    on.exit(progress$close())
    progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.2)
    
    
    k <- simplifyCommunity(g, l[[input$community_plotVis]])
    
    dataVis <- toVisNetworkData(k)
    nodesVis <- dataVis [['nodes']]
    edgesVis <- dataVis [['edges']]
    
    if (!is_directed(k)) {
      nodesVis$title <-
        paste0(
          "<p><b>Community: </b>",
          nodesVis$id,
          "<br>",
          "<b>Degree: </b>",
          degree(k, mode = "all"),
          "<br>",
          "<b>Number Vertex: </b>",
          nodesVis$nVertex,
          "<br>",
          "<b>Number Edges: </b>",
          nodesVis$nEdges,
          "<br>",
          "<b>Density: </b>",
          round((nodesVis$nEdges * 2)/((nodesVis$nVertex) * (nodesVis$nVertex - 1)) , digits = 3),
          "</p>"
        ) # Text on click
    } else{
      
      nodesVis$title <-
        paste0(
          "<p><b>Community: </b>",
          nodesVis$id,
          "<br>",
          "<b>Degree (in): </b>",
          degree(k, mode = "in"),
          "<br>",
          "<b>Degree (out): </b>",
          degree(k, mode = "out"),
          "<br>",
          "<b>Number Vertex: </b>",
          nodesVis$nVertex,
          "<br>",
          "<b>Number Edges: </b>",
          nodesVis$nEdges,
          "<br>",
          "<b>Density: </b>",
          round((nodesVis$nEdges)/((nodesVis$nVertex) * (nodesVis$nVertex - 1)) , digits = 3),
          "</p>"
        ) # Text on click
      
    }
    
    # Node size -> proportional ->  number vertex
    nodesVis$size <- normalize_size(nodesVis$nVertex , min_size_node , max_size_node)
    # Node label
    nodesVis$label <-  nodesVis$id
    
    nodesVis$color.highlight.border <- "darkblue"
    
    if (gsize(k) > 0) {
      edgesVis$width <- normalize_size(edgesVis$weight , min_size_edge , max_size_edge)
      edgesVis$title <-
        paste0("<p><b>Number Edges: </b>", edgesVis$weight, "</p>")
      
      if (is_directed(k)) {
        edgesVis$arrows <- "to" # arrows: 'from', 'to', or 'middle'
      }
    }
    
    progress$set(message = 'Calculation Completed!', detail = "", value = 1)
    Sys.sleep(0.5)
    
    visGraph <- visNetwork(
      nodesVis ,
      edgesVis,
      main = "Interactive Network"
    ) %>%
      visNodes(
        # physics = FALSE,
        shadow = TRUE,
        mass = 2.1,
        font = list(size = "13")
      ) %>%
      visEdges(
        # smooth = FALSE,
        shadow = FALSE
        
      ) %>%
      visOptions(
        highlightNearest = TRUE,
        selectedBy = list(
          variable = "group" ,
          main = "Communities :",
          style = 'width: 120px; '
        )
      )%>%
      visInteraction(navigationButtons = TRUE)
    
    if(!input$fluid_plotVis){
      
      visGraph <- visIgraphLayout(
        graph = visGraph,
        layout = "layout_with_fr",
        physics = FALSE,
        smooth = FALSE
      )
      
    }
    
    return(visGraph)
    
  })
  
  
  output$community_plot <- renderVisNetwork({
    
    if(input$plot_communityVis > 0){
      
      g <- isolate(plotCommunityGraph())
      if(!is.null(g)){
        return(g)
      }else{
        noLoadGraphMoxAndCommDetection()
      }
    }
    
  })
  
  output$down_community_plot <- downloadHandler(
    filename = function(){
      
      paste0('CommunityNetwork',format(Sys.time(),"%Y%m%d_%H%M%S"),'.html')
      
    },
    content = function(file){
      
      visSave(plotCommunityGraph(), file= file, selfcontained = TRUE)
      
    },
    contentType = 'html'
    
  )
  
  
  ###########################################
  #           Module Multiplex              #
  ###########################################
  
  # disable algorithm no available for directed network (multiplex)
  observeEvent(graphMultxReactive(), {
    
    MX <- graphMultxReactive()
    if (is.null(MX)) {
      return()
    } 
    
    if(is_directed(MX$igraphLayers[[1]])){
      
      #no available for directed networks
      updateSelectInput(session, "community_mulxPlot",
                        choices = c("(None)" = "(None)",
                                    "Label Propagation" = "community_labelprop",
                                    "WalkTrap" = "community_walktrap",
                                    "Leading Eigenvector" = "community_eigenvector",
                                    "InfoMap" = "community_infomap",
                                    "Edge Betweenness" = "community_betweenness"
                        ))
      
    }else{
      
      updateSelectInput(session, "community_mulxPlot",
                        choices = c("(None)" = "(None)",
                                    "Fast Greedy" = "community_fastgreedy",
                                    "Label Propagation" = "community_labelprop",
                                    "MultiLevel Louvain" = "community_multLouv",
                                    "WalkTrap" = "community_walktrap",
                                    "Leading Eigenvector" = "community_eigenvector",
                                    "InfoMap" = "community_infomap",
                                    "Edge Betweenness" = "community_betweenness"
                        ))
    }
    
    updateSliderInput(
      session,
      "pick_layer_number" ,
      min = 1,
      max = length(MX$munaGraph@layers),
      value = 1,
      step = 1
    )
    
    updateSliderInput(
      session,
      "pick_ensemble_layer_number" ,
      min = 1,
      max = length(MX$munaGraph@layers),
      value = 1,
      step = 1
    )
    
    
  })
  
  #TAB ANALYSIS MULTIPLEX
  
  output$mulxNetProperties <- renderTable({
    
    if (is.null(graphMultxReactive())) {
      return()
    }
    MX <- graphMultxReactive()
    firstLayer <- MX$igraphLayers[[1]]
    
    values <- data.frame(
      stringsAsFactors = FALSE,
      Indicators = c(
        "Nodes",
        "Number of layers",
        "Directed",
        "Weighted"
        
      ),
      Values = c(
        vcount(firstLayer),
        length(MX$igraphLayers),
        as.character(is_directed(firstLayer)),
        as.character(is_weighted(firstLayer))
      )
    )
    return(values)
  },
  striped = TRUE,
  # hover = TRUE,
  bordered = TRUE,
  spacing = 'l',
  width = "100%"
  )
  
  # Analysis by layer
  output$mulxLayerProperties <- renderTable({
    
    if (is.null(graphMultxReactive())) {
      return()
    }
    MX <- graphMultxReactive()
    layers <- MX$igraphLayers
    
    indicators <- c(
      "Nodes (All)",
      "Nodes (Connected)",
      "Edges",
      "Density",
      "Radius",
      "Diameter",
      "Average Path Length",
      "Grade Average",
      "Cluster Coefficient",
      "Assortativity Degree"
    )
    
    df <- data.frame(stringsAsFactors = FALSE , indicators)
    names <- c("Indicators")
    
    for(gx in layers){
      
      g <- delete.vertices(gx, degree(gx) == 0)
      
      l <- c(
        vcount(gx),
        vcount(g),
        ecount(g),
        round(edge_density(g), digits = 3) ,
        radius(g),
        diameter(g),
        round(mean_distance(g), digits = 3),
        round(mean(degree(g)), digits = 3),
        round(transitivity(g), digits = 3),
        round(assortativity_degree(g), digits = 3)
      )
     
      names <- append(names , graph_attr(g, "name"))
      df <- cbind(df, as.character(l))
      
    }
    
    colnames(df) <- names
    return(df)
    
  },
  striped = TRUE,
  # hover = TRUE,
  bordered = TRUE,
  spacing = 'l',
  width = "100%"
  )
  
  # community detection multiplex (Layer aggregation)
  
  observeEvent(input$apply_aggregation_mulxGraph , {
    
    MX <- graphMultxReactive()
    if (is.null(MX)) {
      noLoadGraphMultx()
      return()
    }
    
    result <- try({
      
      munGraph <-MX$munaGraph
      
      g <- switch (input$choice_type_aggregation,
        
        "binary" = flatten_binary(munGraph),
        "redundancy" = flatten_redundancy(munGraph),
        "minlayer" = flatten_MinLayers(munGraph, MinLayers = input$pick_layer_number)
      )
      
      #reset community value
      communityList(list())
      communityIdent(c())
      
      #set object graph
      graphMoxReactive(g)
    
    }, silent = FALSE)
    
    if (class(result) == "try-error") {
      
      msg <- geterrmessage()
      showModal(modalDialog(title = HTML("<font color='red'>Error</font>"), 
                            HTML(msg), 
                            easyClose = TRUE,
                            size = "m"))
      
    }else{
      
      showModal(modalDialog(title = HTML("<font color='#3c8dbc'>Info</font>"), 
                            HTML("Monoplex graph generated successfully"), 
                            easyClose = TRUE,
                            size = "s"))
      
    }
    
  })
  
  
  # community detection multiplex (Ensemble clustering)
  observeEvent(input$apply_ensemble_mulxGraph , {
    
    MX <- graphMultxReactive()
    if (is.null(MX)) {
      noLoadGraphMultx()
      return()
    }
    
    result <- try({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = 'Calculation in progress', value = 0.4)
      
      munGraph <-MX$munaGraph
      umbral <- input$pick_ensemble_layer_number / length(munGraph@layers)
      
      t <- system.time( 
        
        result <- switch (input$community_ensemble_mulxPlot,
          
          "community_fastgreedy" = ensemble_partition_CSPA_v2(munGraph,
                                                              community_algorithm = cluster_fast_greedy,
                                                              alpha = umbral),
          "community_labelprop" = ensemble_partition_CSPA_v2(munGraph,
                                                             community_algorithm = cluster_label_prop,
                                                             alpha = umbral),
          "community_multLouv" = ensemble_partition_CSPA_v2(munGraph,
                                                            community_algorithm = cluster_louvain,
                                                            alpha = umbral),
          "community_walktrap" = ensemble_partition_CSPA_v2(munGraph,
                                                            community_algorithm = cluster_walktrap,
                                                            alpha = umbral),
          "community_eigenvector" = ensemble_partition_CSPA_v2(munGraph,
                                                               community_algorithm = cluster_leading_eigen,
                                                               alpha = umbral),
          "community_infomap" = ensemble_partition_CSPA_v2(munGraph,
                                                           community_algorithm = cluster_infomap,
                                                           alpha = umbral),
          "community_betweenness" = ensemble_partition_CSPA_v2(munGraph,
                                                               community_algorithm = cluster_edge_betweenness,
                                                               alpha = umbral)
        )
      )
      
      com <- result$community
      com$time <- t[["elapsed"]]
      
      #set object graph
      graphMoxReactive(result$graph)
      
      #set community
      name <- com$algorithm
      temp <- list(com)
      value <- c(name)
      names(temp) <- value
      names(value) <- value
      
      communityList(temp)
      communityIdent(value)
      
      #set multiplex modularity
      output$out_mulModularity <- renderText({
  
        return (modularity_multiplex_v2(munGraph , membership(com)))
  
      })
    
    }, silent = FALSE)
    
    if (class(result) == "try-error") {
      
      msg <- geterrmessage()
      showModal(modalDialog(title = HTML("<font color='red'>Error</font>"), 
                            HTML(msg), 
                            easyClose = TRUE,
                            size = "m"))
      
    }else{
    
      showModal(modalDialog(title = HTML("<font color='#3c8dbc'>Info</font>"), 
                            HTML("Monoplex graph and community detection generated successfully"), 
                            easyClose = TRUE,
                            size = "s"))
      
      progress$set(message = 'Calculation Completed!', value = 1)
      Sys.sleep(0.45)
    
    }
    
  })
  
  
  # community detection multiplex (Generalization)
  
  observeEvent(input$apply_generalization_mulxGraph , {
    
    MX <- graphMultxReactive()
    if (is.null(MX)) {
      noLoadGraphMultx()
      return()
    }
    
    result <- try({
    
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = 'Calculation in progress', value = 0.4)
      
      flatten_graph <- flatten_redundancy(MX$munaGraph)
      t <- system.time(memb <- community.muxLicod(MX$munaGraph))
      com <- make_clusters(flatten_graph , membership = memb , algorithm = "muxLicod")
      com$time <- t[["elapsed"]]
      
      #set object graph
      graphMoxReactive(flatten_graph)
      
      #set community
      name <- "muxLicod"
      temp <- list(com)
      value <- c(name)
      names(temp) <- value
      names(value) <- "MuxLicod"
      
      communityList(temp)
      communityIdent(value)
      
      #set multiplex modularity
      output$out_mulModularity <- renderText({
        
        return (modularity_multiplex_v2(MX$munaGraph , memb))
        
      })
    
    }, silent = FALSE)
    
    if (class(result) == "try-error") {
      
      msg <- geterrmessage()
      showModal(modalDialog(title = HTML("<font color='red'>Error</font>"), 
                            HTML(msg), 
                            easyClose = TRUE,
                            size = "m"))
      
    }else{
      
      showModal(modalDialog(title = HTML("<font color='#3c8dbc'>Info</font>"), 
                            HTML("Monoplex graph and community detection generated successfully"), 
                            easyClose = TRUE,
                            size = "s"))
      
      progress$set(message = 'Calculation Completed!', value = 1)
      Sys.sleep(0.45)
      
    } 
    
  })
  
  
  # visualze multiplex network
  plotmulxGraph <- reactive({
    
    if (is.null(graphMultxReactive())) {
      return()
    }
    MX <- graphMultxReactive()
    layers <- MX$igraphLayers
    
    if(!input$all_nodes_mulxPlot){
      
      i <- 1
      for(g in layers){
        
        layers[[i]] <- delete.vertices(g, degree(g) == 0)
        
        i <- i + 1
      }
    }
    
    i <- 1
    for(g in layers){
      
      # save original name
      V(g)$id <- as_ids(V(g))
      # name vertex
      V(g)$name <- paste0(as_ids(V(g)),"-L", i)
      
      layers[[i]] <- g
      
      i <- i + 1
    }
    
    # fusion layers
    gz <- graph.union(layers , byname = TRUE)
    
    palette <- switch (input$palette_mulxPlot,
                       
                       "color_rainbow" = rainbow(12),
                       "color_brewer_paired" = brewer.pal(12, "Paired"),
                       "color_brewer_set1" = brewer.pal(9, "Set1"),
                       "color_brewer_set2" = brewer.pal(8, "Set2"),
                       "color_brewer_set3" = brewer.pal(12, "Set3")
    )
    
    acum <- 0
    i <- 1
    for(g in layers){
      
      l <- layout_nicely(
        g,
        dim = 3
      )
      
      l <- norm_coords(l, xmin = 0, xmax = 6, ymin = 0, ymax = 6,
                       zmin = acum, zmax = acum + 1.5)
      
      
      vertex_attr(gz, "axisX", index = as_ids(V(g))) <- l[,1]
      vertex_attr(gz, "axisY", index = as_ids(V(g))) <- l[,2]
      vertex_attr(gz, "axisZ", index = as_ids(V(g))) <- l[,3]
      
      acum <- acum - 10
      
      labels <- paste0("<p><b>Node: </b>", V(g)$id )
      
      if(is.element("label_ID" , vertex_attr_names(g))) {
        
        labels <- paste0(labels, " ",
                         "<b>Label: </b>", vertex_attr(g, name = "label_ID"))
      }
      
      
      #color and community detection
      if(input$community_mulxPlot == "(None)"){
        
        vertex_attr(gz, "color", index = as_ids(V(g))) <- palette[(i %% length(palette)) + 1]
        
        #label
        vertex_attr(gz, "label", index = as_ids(V(g))) <- paste0("<p>" , labels , " ",
                                                                 "<b>Layer: </b>", graph_attr(g, "name") , "</p>")
        
      }else{
        
        comm <- switch (input$community_mulxPlot,
          
          "community_fastgreedy" = cluster_fast_greedy(g),
          "community_labelprop" = cluster_label_prop(g),
          "community_multLouv" = cluster_louvain(g),
          "community_walktrap" = cluster_walktrap(g),
          "community_eigenvector" = cluster_leading_eigen(g),
          "community_infomap" = cluster_infomap(g),
          "community_betweenness" = cluster_edge_betweenness(g)
        )
        
        
        colorsVertex <- membership(comm) %% length(palette)
        vertex_attr(gz, "color", index = as_ids(V(g))) <- palette[colorsVertex + 1]
        
        #label
        vertex_attr(gz, "label", index = as_ids(V(g))   ) <- paste0("<p>" , labels , " ",
                                                                    "<b>Layer: </b>", graph_attr(g, "name") ," ",
                                                                    "<b>Community: </b>", membership(comm),"</p>"
        )
      }
      
      i <- i + 1
    }
    
    finalLayout <- matrix(nrow = gorder(gz) , ncol = 3)
    finalLayout[,1] <- vertex_attr(gz, "axisX")
    finalLayout[,2] <- vertex_attr(gz, "axisY")
    finalLayout[,3] <- vertex_attr(gz, "axisZ")
    
    if(input$vertex_size_mulxPlot == "fixed"){
      
      V(gz)$size <- input$fixed_vsize_mulxPlot
      
    }else{
      
      centrality <- switch (input$centrality_pick_mulxPlot,
                            
                            degree = degree(gz),
                            strength = strength(gz)
      )
      
      rangeSize <- input$centrality_vsize_mulxPlot
      V(gz)$size <- normalize_size(centrality , rangeSize[[1]] , rangeSize[[2]])
      
    }
    
    # show inter layers
    if(length(layers) >= 2 && input$link_layer_mulxPlot){

      links <- c()
      for(i in 1:(length(layers)-1)){

        source <- V(layers[[i]])$id
        target <- V(layers[[i + 1]])$id

        inters <- intersect(source, target)
        
        for(j in inters) {
          
          links <- append(links , c(paste0(j, "-L", i), paste0(j, "-L", i + 1)))
          
        }
      }
      gz <- add.edges(gz , links)
    }
    
    gjs <- graphjs(gz,
                   vertex.shape = '@',
                   fps = 100,
                   bg = if(input$background_mulxPlot) "#333333" else "white",
                   layout = if(input$animated_mulxPlot){
                     list(
                       layout_randomly(gz, dim=3),
                       layout_on_sphere(gz),
                       finalLayout)
                   }else{
                     finalLayout
                   }
                   )
    return(gjs)
    
  })
  
  output$mulxPlot <- renderScatterplotThree({
    
    if(input$plot_mulxGraph > 0){
      
      gjs <- isolate(plotmulxGraph())
      if(!is.null(gjs)){
        return(gjs)
      }else{
        noLoadGraphMultx()
      }
    }
    
  })
  
  output$down_mulxPlot <- downloadHandler(
    filename = function(){
      
      paste0('MultiplexNetwork',format(Sys.time(),"%Y%m%d_%H%M%S"),'.html')
      
    },
    content = function(file){
      
      saveWidget(plotmulxGraph(), file=file, selfcontained = TRUE)
      
    },
    contentType = 'html'
    
  )
  
  
  
  #####################################################################################################################
  
 
  
})
