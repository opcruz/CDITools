library(shiny)
library(shinydashboard)
library(shinyjs)
library(visNetwork)
library(threejs)
library(shinycssloaders)
library(rCharts)
library(shinydashboardPlus)
library(DT)

dashboardPage(
  skin = "blue",
  
  title = "CDITools",
  
  dashboardHeader(
    title = tags$img(src = "app_logo.png",
                     height = '45',
                     width = '45',
                     "\t\tCDITools")
    
  ),
  
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Load Data",
      tabName = "upload",
      icon = icon("upload")
      
    ),
    menuItem(
      "Monoplex Network",
      tabName = "monoplex",
      icon = icon("ellipsis-h"),
      startExpanded = TRUE,
      
      menuSubItem(
        "Analysis",
        tabName = "mon_analysis" ,
        icon = icon("angle-right")
      ),
      menuSubItem(
        "Community Detection",
        tabName = "mon_commun_detect" ,
        icon = icon("angle-right")
      ),
      menuSubItem(
        "Visualization",
        tabName = "mon_visualization" ,
        icon = icon("angle-right")
      )
      # ,
      # menuSubItem("Similarity between community",
      #             tabName = "proc_data" ,
      #             icon = icon("angle-right")),
      # menuSubItem(
      #   "Plot community graph",
      #   tabName = "plot" ,
      #   icon = icon("angle-right")
      # )
    ),
    
    menuItem(
      "Multiplex Network",
      tabName = "multiplex",
      icon = icon("bars"),
      # startExpanded = TRUE,
      
      menuSubItem(
        "Analysis",
        tabName = "mul_analysis" ,
        icon = icon("angle-right")
      ),
      menuSubItem(
        "Community Detection",
        tabName = "mul_commun_detect" ,
        icon = icon("angle-right")
      ),
      menuSubItem(
        "Visualization",
        tabName = "mul_visualization" ,
        icon = icon("angle-right")
      )
    )
  )),
  
  dashboardBody(
    
    tags$head(tags$link(rel = "icon", type = "image/png", href = "app_logo_icon.png")),
    
    # Shiny Dashboardpage lock dashboardHeader on top
    tags$script(HTML("$('body').addClass('fixed');")),
    tags$head(tags$style(HTML(pin_header))),
    tags$head(tags$style(HTML(custom_nav))),
   
    # shinyjs for javascript
    useShinyjs(),
    tabItems(
      
      # LOAD DATA
      tabItem(tabName = "upload",
              
              fluidRow(
                column(
                  3,
                  box(
                    title = "Load Network",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    #footer = "UCLV",
                    width = NULL,
                    
                    fileInput(
                      inputId = "loadFile",
                      label = h4("Select file"),
                      #buttonLabel = "Buscar...",
                      #placeholder = "Ningún archivo seleccionado"
                      accept = ".zip"
                    ),
                    tags$hr(),
                    helpText("Default max. file size is 50 MB")
                    
                  ),
                  
                  box(
                    title = "Features",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    
                    selectInput(
                      inputId = "directedNet",
                      label = "Edge Format:",
                      choices = c("Undirected", "Directed")
                    ),
                    
                    checkboxInput(
                      inputId = "weightyNet",
                      label = "Weighted",
                      value = FALSE
                    ),
                    conditionalPanel(
                      condition = "input.weightyNet",
                      selectInput(
                        inputId = "weightyColumn",
                        label = "Weighty Column:",
                        choices = "(None)"
                      )
                    ),
                    
                    checkboxInput(
                      inputId = "labelNodeNet",
                      label = "Label Node",
                      value = FALSE
                    ),
                    conditionalPanel(
                      condition = "input.labelNodeNet",
                      selectInput(
                        inputId = "labelNodeColumn",
                        label = "Label Node Column:",
                        choices = "(None)"
                      )
                    )
                    
                  ), 
                  
                  HTML("<center>"),
                  disabled(actionButton(
                    inputId = "applyNet",
                    label = "Apply and Generate Graph",
                    icon("superpowers"),
                    style = "align: center; color: #fff; background-color: #3c8dbc; border-color: #3c8dbc; margin-bottom: 15px"
                  )),
                  HTML("</center>")
                  
                ),
                
                column(
                  
                  9,
                  tabBox(
                    # Title can include an icon
                    title = tagList(icon("tasks"), "Information"),
                    width = NULL,
                    
                    tabPanel("Files",
                        tableOutput("filesList")),
                    tabPanel("Nodes",
                             DT::dataTableOutput("nodesNetwork")),
                    tabPanel("First Layer",
                             DT::dataTableOutput("firstlayerNetwork")),
                    tabPanel("Properties",
                             tableOutput("netSummary"))
                    
                  )
                  
                )
                
                )
              
              ),
      
      # ANALYSIS DATA
      tabItem(tabName = "mon_analysis",
              #h2("Widgets tab content")
              
              fluidRow(
                
                tabBox(
                  width = 12,
                  title = tagList(icon("gear"), "Network Analysis"),
                  
                  tabPanel(
                    "Preprocess",
                    
                    fluidRow(
                      
                      column(
                        3,
                        
                        box(
                          
                          title = "Filter",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          width = NULL,
                          
                          HTML("<strong>Network filters:</strong>"),
                          checkboxInput(inputId = "remove_loops", label = "Remove loops", value = FALSE),
                          checkboxInput(inputId = "remove_mult_edges", label = "Remove multiple edges", value = FALSE),
                          checkboxInput(inputId = "zero_degree_net", label = "Remove nodes with zero degree", value = FALSE),
                          checkboxInput(inputId = "remove_weak_connection", label = "Remove weak connections", value = FALSE),
                          
                          conditionalPanel(
                            condition = "input.remove_weak_connection",
                            
                            radioButtons("threshold_weak_connection", "Threshold:", 
                                         choices = c("Mean of min values" = "mean_min_weak_connection",
                                                     "Custom value" = "custom_weak_connection")),
                            
                            
                            conditionalPanel(
                              condition = "input.threshold_weak_connection == 'custom_weak_connection'",
                              
                              numericInput("custom_value_weak_connection", "Value", value = 0.5,
                                           min = 0.1, step = 0.1)
                              
                              
                            )
                            
                          )
                          
                          
                          ),
                        
                        HTML("<center>"),
                        actionButton(
                          inputId = "apply_filters",
                          label = "Apply filters",
                          icon("superpowers"),
                          style = "align: center; color: #fff; background-color: #3c8dbc; border-color: #3c8dbc; margin-bottom: 15px"
                        ),
                        HTML("</center>")
                        
                      ),
                      
                      column(
                        width = 3,
                        
                        downloadButton(outputId = "down_mon_graph",
                                       label = "Download Network",
                                       style = "align: center; margin-bottom: 15px")
                        
                      ),
                      
                      column(
                        width = 5,
                        
                        tableOutput("netProperties") %>% withSpinner(type = 6)
                        
                        )
                      )
                  ),
                  
                  tabPanel("Centrality",
                           # style = "background-color: #c0c3c714;",
                           
                     fluidRow(
                       column(
                         3,
                         
                         box(
                           title = "Descriptors",
                           status = "primary",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = NULL,
                           
                           HTML("<strong>Centralities:</strong>"),
                           checkboxInput(inputId = "degreeNet", label = "Degree",value = TRUE),
                           checkboxInput(inputId = "strengthNet", label = "Strength", value = TRUE),
                           checkboxInput(inputId = "closenessNet", label = "Closeness", value = TRUE),
                           checkboxInput(inputId = "betweennessNet", label = "Betweenness", value = TRUE),
                           checkboxInput(inputId = "pagerankNet", label = "PageRank", value = FALSE),
                           checkboxInput(inputId = "kcoreNet", label = "K-core", value = FALSE),
                           checkboxInput(inputId = "eigenvectorNet", label = "Eigenvector", value = ),
                           checkboxInput(inputId = "hubNet", label = "Hub", value = FALSE),
                           checkboxInput(inputId = "authorityNet", label = "Authority", value = FALSE)
                         )
                       ),
                       column(
                         3,
                        
                         actionButton("cal_centrality", "Calculate Centrality")
                         
                       )
                       
                   ),
                   
                   h3("Descriptors" , style = "color: #3c8dbc; border-color: #3c8dbc;"),
                   tags$hr(style = "border-color: #3c8dbc;"),
                   
                   fluidRow(
                     column(
                       3,
                       
                        uiOutput("boxCentrality")
                       
                       ),
                     
                     column(
                       9,
                       
                       showOutput("centralityChart", "highcharts")
                       
                     )
                   )
                   
                   )
                )
              )),
      
      # Comunity Detection Monoplex
      tabItem(tabName = "mon_commun_detect",
              
              fluidRow(column(
                width = 12,
                
                tabBox(
                  # Title can include an icon
                  title = tagList(icon("tasks"), "Community Detection"),
                  width = NULL,
                  
                  tabPanel(
                    "Community Detection",
                    
                    fluidRow(column(
                      width = 3,
                      
                      box(
                        title = "Community Detection",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = NULL,
                        
                        HTML("<strong>Community Detection:</strong>"),
                        checkboxInput("community_fastgreedy", label = "Fast Greedy", value = TRUE),
                        checkboxInput("community_labelprop", label = "Label Propagation", value = TRUE ),
                        checkboxInput("community_multLouv", label = "MultiLevel Louvain", value = TRUE ),
                        checkboxInput("community_walktrap", label = "WalkTrap", value = TRUE ),
                        checkboxInput("community_eigenvector", label = "Leading Eigenvector", value = TRUE ),
                        checkboxInput("community_infomap", label = "InfoMap", value = TRUE ),
                        checkboxInput("community_betweenness", 
                                      label = HTML("Edge Betweenness (<font color='red'>slow</font>)"), value = FALSE ),
                        checkboxInput("community_spinglass",
                                      label = HTML("Spinglass (<font color='red'>slow</font>)"), value = FALSE ),
                        
                        tags$hr(style = "border-color: #3c8dbc;"),
                        HTML("<center>"),
                        actionButton("cal_comm_detec",
                                     "Run algorithms"),
                        HTML("</center>")
                      )
                    ),
                    
                    column(
                      width = 3,
                      
                      box(
                        title = "Community Selection",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = NULL,
                        
                        selectInput(
                          inputId = "selectCommunity",
                          label = "Select Community:",
                          choices = "(None)"
                        ),
                        helpText("Select community detection algorithm"),
                        tags$hr(style = "border-color: #3c8dbc;"),
                        HTML("<center>"),
                        actionButton(
                          inputId = "shareGraph",
                          label = "Share Graph",
                          icon("share")
                        ),
                        HTML("</center>")
                        
                      )
                    ),
                    
                    column(
                      width = 3,
                      
                      downloadButton(outputId = "down_all_communities", label = "Download all Communities")
                      
                    )
                    ),
                    
                    h3("Add Community Detection" , style = "color: #3c8dbc; border-color: #3c8dbc;"),
                    tags$hr(style = "border-color: #3c8dbc;"),
                    
                    fluidRow(column(
                      width = 3,
                      
                      box(
                        title = "Load file",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = NULL,
                        
                        fileInput(
                          inputId = "community_file",
                          label = h4("Select file"),
                          accept = "text/plain"
                        ),
                        # helpText("Select community detection file"),
                        tags$hr(),
                        selectInput(
                          inputId = "community_column",
                          label = "Community Column:",
                          choices = "(None)"
                        ),
                        
                        checkboxInput( inputId = "community_header", label = "Header", value = FALSE),
                        radioButtons(inputId = "community_sep" , label = "Separator",
                                     choices = c(Space = ' ', Comma = ',',Semicolon = ';', Tabulation = '\t' ))
                      ),
                      
                      HTML("<center>"),
                      actionButton(
                        inputId = "load_community",
                        label = "Add Community",
                        icon("superpowers"),
                        style = "align: center; color: #fff; background-color: #3c8dbc; border-color: #3c8dbc"
                      ),
                      HTML("</center>")
                      
                    ),
                    
                    column(
                      width = 9,
                      
                      # box(
                      #   title = "Table",
                      #   status = "primary",
                      #   solidHeader = TRUE,
                      #   collapsible = TRUE,
                      #   width = 12,
                        
                        DT::dataTableOutput("table_community_file")
                      # )
                      
                    )
                    
                    )
                    
                  ),
                  
                  tabPanel(
                    "Evaluation",
                    
                    fluidRow(
                      column(width = 6,
                             tableOutput("communityComparison") %>% withSpinner(type = 6)),
                      
                      column(width = 3,
                             downloadButton(outputId = "down_community_properties",
                                            label = "Download",
                                            style = "align: center; margin-bottom: 15px")
                             )
                    ),
                    
                    h3("Compare" , style = "color: #3c8dbc; border-color: #3c8dbc;"),
                    tags$hr(style = "border-color: #3c8dbc;"),
                    
                    fluidRow(column(
                      width = 3,
                      
                      box(
                        title = "Community Compare",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = NULL,
                        
                        selectInput(
                          inputId = "community_compare_first",
                          label = "First Community:",
                          choices = c("(None)" = "(None)")
                        ),
                        
                        selectInput(
                          inputId = "community_compare_second",
                          label = "Second Community:",
                          choices = c("(None)" = "(None)")
                        ),
                        
                        tags$hr(style = "border-color: #3c8dbc;"),
                        HTML("<center>"),
                        actionButton("action_community_compare",
                                     "Compare"),
                        HTML("</center>")
                        
                      )
                    ),
                    
                    column(width = 4,
                           # offset = 1,
                           
                           tableOutput("table_community_compare") %>% withSpinner(type = 6)
                           
                           ),
                    column(width = 3,
                           
                           downloadButton(outputId = "down_community_compare",
                                          label = "Download",
                                          style = "align: center; margin-bottom: 15px")
                    )
                    
                    )
                  )
                )
                
              ))
      ), 
      
      # Visualization Monoplex 
      tabItem(tabName = "mon_visualization",
              
              fluidRow(column(
                width = 12,
                
                tabBox(
                  # Title can include an icon
                  title = tagList(icon("tasks"), "Community Detection"),
                  width = NULL,

                  tabPanel(
                    "Simple Plot",
                    
                    fluidRow(
                      
                      column(
                        width = 3,
                        
                        box(
                          title = "Options",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          width = NULL,
                          
                          radioButtons("vertex_size_simplePlot", "Vertex Size:", 
                                       choices = c("Fixed" = "fixed",
                                                   "Centrality" = "centrality")),
                          
                          conditionalPanel(
                            condition = "input.vertex_size_simplePlot == 'fixed'",
                            
                            sliderInput("fixed_vsize_simplePlot", "Size:", 
                                        min = 1, max = 25, value = 5)
                            
                          ),
                          
                          conditionalPanel(
                            condition = "input.vertex_size_simplePlot == 'centrality'",
                            
                            selectInput(
                              inputId = "centrality_pick_simplePlot",
                              label = "Centrality:",
                              choices = c("Degree" = "degree",
                                          "Strength" = "strength")
                            ),
                            
                            sliderInput("centrality_vsize_simplePlot", "Size Range:", 
                                        min = 1, max = 25, value = c(2,10) )
                            
                          )
                        )
                      ),
                      
                      column(
                        width = 3,
                        
                        box(
                          title = "Community Detection",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          width = NULL,
                          
                          selectInput(
                            inputId = "community_simplePlot",
                            label = "Community Detection:",
                            choices = c("(None)" = "(None)")
                          ),
                          
                          selectInput(
                            inputId = "palette_simplePlot",
                            label = "Palette of Colors:",
                            choices = c("Rainbow" = "color_rainbow",
                                        "Brewer Paired" = "color_brewer_paired",
                                        "Brewer Set1" = "color_brewer_set1",
                                        "Brewer Set2" = "color_brewer_set2",
                                        "Brewer Set3" = "color_brewer_set3"
                            )
                          )
                        )
                      ),
                      
                      column(
                        width = 3,
                        
                        box(
                          title = "More Options",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          width = NULL,
                          
                          selectInput(
                            inputId = "layout_simplePlot",
                            label = "Select Layout:",
                            choices = c("Nicely" = "layout_nicely",
                                        "Fruchterman-Reingold" = "layout_fr",
                                        "Graphopt" = "layout_graphopt",
                                        "Kamada-Kawai" = "layout_kk",
                                        "Large Graph" = "layout_lgl")
                          ),
                          
                          selectInput(
                            inputId = "label_simplePlot",
                            label = "Node Label:",
                            choices = c("(None)" , "ID")
                          )
                        )
                      ),
                      
                      column(
                        width = 3,
                        actionButton("plot_simpleGraph", "Plot Graph")
                      )
                    ),
                    
                    h3("Plot" , style = "color: #3c8dbc; border-color: #3c8dbc;"),
                    tags$hr(style = "border-color: #3c8dbc;"),
                    
                    fluidRow(
                      column(
                        width = 12,
                        plotOutput("simple_plot_output" , height = 650) %>% withSpinner(type = 6 ),
                        downloadButton(outputId = "down_simple_plot",
                                       label = "Download Plot (PDF)",
                                       style = "align: center; margin-top: 10px")
                      )
                    )
                    
                  ),
                  tabPanel(
                    title = HTML("3D Plot (<font color='red'>Undirected Graph</font>)"),
                    
                    fluidRow(
                      
                      column(
                        width = 3,
                        
                        box(
                          title = "Options",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          width = NULL,
                          
                          radioButtons("vertex_size_3dPlot", "Vertex Size:", 
                                       choices = c("Fixed" = "fixed",
                                                   "Centrality" = "centrality")),
                          
                          conditionalPanel(
                            condition = "input.vertex_size_3dPlot == 'fixed'",
                            
                            sliderInput("fixed_vsize_3dPlot", "Size:", 
                                        min = 0.1, max = 10, value = 2.5)
                            
                          ),
                          
                          conditionalPanel(
                            condition = "input.vertex_size_3dPlot == 'centrality'",
                            
                            selectInput(
                              inputId = "centrality_pick_3dPlot",
                              label = "Centrality:",
                              choices = c("Degree" = "degree",
                                          "Strength" = "strength")
                            ),
                            
                            sliderInput("centrality_vsize_3dPlot", "Size Range:", 
                                        min = 0.1, max = 10, value = c(2,6) )
                          )
                          
                        )
                      ),
                      
                      column(
                        width = 3,
                        
                        box(
                          title = "Community Detection",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          width = NULL,
                          
                          selectInput(
                            inputId = "community_3dPlot",
                            label = "Community Detection:",
                            choices = c("(None)" = "(None)")
                          ),
                          
                          selectInput(
                            inputId = "palette_3dPlot",
                            label = "Palette of Colors:",
                            choices = c("Rainbow" = "color_rainbow",
                                        "Brewer Paired" = "color_brewer_paired",
                                        "Brewer Set1" = "color_brewer_set1",
                                        "Brewer Set2" = "color_brewer_set2",
                                        "Brewer Set3" = "color_brewer_set3"
                            )
                          )
                        )
                      ),
                      
                      column(
                        width = 3,
                        
                        box(
                          title = "More Options",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          width = NULL,
                          
                          selectInput(
                            inputId = "layout_3dPlot",
                            label = "Select Layout:",
                            choices = c("Nicely" = "layout_nicely",
                                        "Fruchterman-Reingold" = "layout_fr",
                                        "Kamada-Kawai" = "layout_kk",
                                        "DrL generator" = "layout_drl",
                                        "Sphere" = "layout_sphere"
                            )
                          ),
                          
                          checkboxInput("collapsed_3dPlot", label = "Collapsed communities", value = FALSE),
                          checkboxInput("background_3dPlot", label = "Dark background", value = FALSE),
                          checkboxInput("animated_3dPlot", label = "Animated", value = FALSE)
                          
                        )
                      ),
                      
                      column(
                        width = 3,
                        
                        actionButton("plot_3dGraph", "Plot Graph")
                        
                      )
                      
                    ),
                    
                    h3("3D Plot" , style = "color: #3c8dbc; border-color: #3c8dbc;"),
                    tags$hr(style = "border-color: #3c8dbc;"),
                    
                    fluidRow(
                      column(
                        width = 12,
                        scatterplotThreeOutput("graph_3d",height = "710px") %>% withSpinner(type = 6),
                        downloadButton(outputId = "down_3d_plot",
                                       label = "Download Plot (HTML)",
                                       style = "align: center; margin-top: 10px")
                      )
                    )
                    
                  ),
                  tabPanel(
                    "Community Plot",
                    
                    fluidRow(
                      
                      column(
                        width = 3,
                        
                        box(
                          title = "Community Detection",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          width = NULL,
                          
                          selectInput(
                            inputId = "community_plotVis",
                            label = "Community Detection:",
                            choices = c("(None)" = "(None)")
                          ),
                          checkboxInput("fluid_plotVis", 
                                        label = HTML("Fluid Graph (<font color='red'>lower performance</font>)"), 
                                        value = FALSE)
                        )
                      ),
                      column(
                        width = 3,
                        
                        actionButton("plot_communityVis", "Plot Graph")
                        
                      )
                      
                    ),
                    
                    h3("Community Plot" , style = "color: #3c8dbc; border-color: #3c8dbc;"),
                    tags$hr(style = "border-color: #3c8dbc;"),
                    
                    fluidRow(
                      column(
                        width = 12,
                        visNetworkOutput("community_plot", height = 650) %>% withSpinner(type = 6 ),
                        downloadButton(outputId = "down_community_plot",
                                       label = "Download Plot (HTML)",
                                       style = "align: center; margin-top: 10px")
                      )
                    )
                  )
                )
                
              ))),
      
      ###################################
      #           Multiplex
      ###################################
      
      # ANALYSIS DATA
      tabItem(tabName = "mul_analysis",
              
          fluidRow(tabBox(
            width = 12,
            title = tagList(icon("gear"), "Multiplex Network Analysis"),
            
            tabPanel("Information",
                     
                     fluidRow(
                       column(
                         width = 3,
                         
                         tableOutput("mulxNetProperties") %>% withSpinner(type = 6)
                         
                       ),
                       
                       column(
                         width = 9,
                         
                         tableOutput("mulxLayerProperties") %>% withSpinner(type = 6)
                         
                       )
                       
                       
                     ))
              ))),
      
      tabItem(tabName = "mul_commun_detect",	
              
              fluidRow(column(
                width = 12,	
                
                tabBox(
                  # Title can include an icon
                  title = tagList(icon("cubes"), "Multiplex Network Community Detection"),
                  width = NULL,
                  
                  tabPanel(
                    title = HTML("Community Detection"),
                    
                    fluidRow(
                      
                      column(
                        width = 12,
                        
                        box(
                          title = "Community Detection Types",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          width = NULL,
                        
                          tags$img(src = "multiplex_types.png" , width="100%" , alt="community detection types")
                          
                        )
                      )
                    ),
                    
                    fluidRow(
                      
                      column(
                        width = 3,
                        
                        box(
                          title = "Community Detection",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          width = NULL,
                          
                          radioButtons("choice_community_mulxNet", "Community Detection:", 
                                       choices = c(
                                         "Layer aggregation" = "aggregation",
                                         "CSPA ensemble clustering" = "ensemble",
                                         "Generalization of algorithms" = "generalization"
                                       )) 
                          
                        )
                      ),
                      
                      conditionalPanel(
                        condition = "input.choice_community_mulxNet == 'aggregation'",
                        
                        div(
                          column(
                            width = 3,
                            
                            box(
                              title = "Layer Aggregation",
                              status = "primary",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              width = NULL,
                              
                              radioButtons("choice_type_aggregation", "Type:", 
                                           choices = c(
                                             "Flatten binary" = "binary",
                                             "Flatten binary (weigthed)" = "redundancy",
                                             "Min layer number" = "minlayer"
                                           ))
                            )
                          ),
                          
                          conditionalPanel(
                            condition = "input.choice_type_aggregation == 'minlayer'",
                            
                            column(
                              width = 3,
                              
                              box(
                                title = "Options",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = NULL,
                                
                                sliderInput("pick_layer_number", "Layer number:", 
                                            min = 1, max = 2, value = 1 , step = 1)
                                
                              )
                            )
                            
                          ),
                          
                          column(
                            width = 3,
                            
                            actionButton("apply_aggregation_mulxGraph", "Apply")
                            
                          )
                        
                        )
                      ),
                      
                      conditionalPanel(
                        condition = "input.choice_community_mulxNet == 'ensemble'",
                        
                        div(
                          column(
                            width = 3,
                            
                            box(
                              title = "Ensemble clustering",
                              status = "primary",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              width = NULL,
                              
                              selectInput(
                                inputId = "community_ensemble_mulxPlot",
                                label = "Community Detection (by layer):",
                                choices = c(
                                  "Fast Greedy" = "community_fastgreedy",
                                  "Label Propagation" = "community_labelprop",
                                  "MultiLevel Louvain" = "community_multLouv",
                                  "WalkTrap" = "community_walktrap",
                                  "Leading Eigenvector" = "community_eigenvector",
                                  "InfoMap" = "community_infomap",
                                  "Edge Betweenness" = "community_betweenness"
                                )
                              ),
                              
                              sliderInput("pick_ensemble_layer_number", "Umbral (Layer number):", 
                                          min = 1, max = 2, value = 1 , step = 1)
                              
                            )
                          ),
                          
                          column(
                            width = 3,
                            
                            actionButton("apply_ensemble_mulxGraph", "Apply")
                            
                          )
                          
                        )
                      ),
                      
                      conditionalPanel(
                        condition = "input.choice_community_mulxNet == 'generalization'",
                        
                        div(
                          column(
                            width = 3,
                            
                            box(
                              title = "Generalization",
                              status = "primary",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              width = NULL,
                              
                              selectInput(
                                inputId = "community_generalization_mulxPlot",
                                label = "Algorithms:",
                                choices = c(
                                  "MuxLicod" = "muxlicod"
                                )
                              )
                              
                            )
                          ),
                          
                          column(
                            width = 3,
                            
                            actionButton("apply_generalization_mulxGraph", "Apply")
                            
                          )
                          
                        )
                      )
                      
                    ),
                    
                    h3("More" , style = "color: #3c8dbc; border-color: #3c8dbc;"),
                    tags$hr(style = "border-color: #3c8dbc;"),
                    
                    fluidRow(
                      
                      column(width = 3,
                             
                             tags$p(HTML(paste0 ("<strong>", "Multiplex Modularity:"  ,"</strong>"))),
                             verbatimTextOutput("out_mulModularity", placeholder = TRUE)
                             ),
                      
                      column(
                        width = 3,
                        
                        box(
                          title = "Community Selection",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          width = NULL,
                          
                          selectInput(
                            inputId = "community_mulx_share",
                            label = "Community Detection (by layer):",
                            choices = c("Fast Greedy" = "community_fastgreedy",
                                        "Label Propagation" = "community_labelprop",
                                        "MultiLevel Louvain" = "community_multLouv",
                                        "WalkTrap" = "community_walktrap",
                                        "Leading Eigenvector" = "community_eigenvector",
                                        "InfoMap" = "community_infomap"
                                      )
                          ),
                          helpText("Select community detection algorithm"),
                          tags$hr(style = "border-color: #3c8dbc;"),
                          HTML("<center>"),
                          actionButton(
                            inputId = "shareMulCommunity",
                            label = "Share Layer Community",
                            icon("share")
                          ),
                          HTML("</center>")
                          
                        )
                      )
                      
                    )
                    
                  )
                )
              )
              )
      ),
      
      
      tabItem(tabName = "mul_visualization",	
              
          fluidRow(column(
            width = 12,	
            
            tabBox(
              # Title can include an icon
              title = tagList(icon("cubes"), "Multiplex Network Visualization"),
              width = NULL,
              
              tabPanel(
                title = HTML("Visualization (<font color='red'>Undirected Graph</font>)"),
                
                fluidRow(
                  
                  column(
                    width = 3,
                    
                    box(
                      title = "Options",
                      status = "primary",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      width = NULL,
                      
                      radioButtons("vertex_size_mulxPlot", "Vertex Size:", 
                                   choices = c("Fixed" = "fixed",
                                               "Centrality" = "centrality")),
                      
                      conditionalPanel(
                        condition = "input.vertex_size_mulxPlot == 'fixed'",
                        
                        sliderInput("fixed_vsize_mulxPlot", "Size:", 
                                    min = 0.1, max = 10, value = 2.5)
                        
                      ),
                      
                      conditionalPanel(
                        condition = "input.vertex_size_mulxPlot == 'centrality'",
                        
                        selectInput(
                          inputId = "centrality_pick_mulxPlot",
                          label = "Centrality:",
                          choices = c("Degree" = "degree",
                                      "Strength" = "strength")
                        ),
                        
                        sliderInput("centrality_vsize_mulxPlot", "Size Range:", 
                                    min = 0.1, max = 10, value = c(2,6) )
                        
                      )
                      
                    )
                  ),
                  
                  column(
                    width = 3,
                    
                    box(
                      title = "Community Detection",
                      status = "primary",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      width = NULL,
                      
                      selectInput(
                        inputId = "community_mulxPlot",
                        label = "Community Detection (by layer):",
                        choices = c("(None)" = "(None)")
                      ),
                      
                      selectInput(
                        inputId = "palette_mulxPlot",
                        label = "Palette of Colors:",
                        choices = c("Rainbow" = "color_rainbow",
                                    "Brewer Paired" = "color_brewer_paired",
                                    "Brewer Set1" = "color_brewer_set1",
                                    "Brewer Set2" = "color_brewer_set2",
                                    "Brewer Set3" = "color_brewer_set3"
                        )
                      )
                      
                    )
                    
                  ),
                  
                  column(
                    width = 3,
                    
                    box(
                      title = "More Options",
                      status = "primary",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      width = NULL,
                      
                      checkboxInput("all_nodes_mulxPlot", label = "Show all Nodes", value = FALSE),
                      checkboxInput("link_layer_mulxPlot", label = "Show links Layers", value = FALSE),
                      checkboxInput("background_mulxPlot", label = "Dark background", value = FALSE),
                      checkboxInput("animated_mulxPlot", label = "Animated", value = FALSE)
                      
                    )
                    
                  ),
                  
                  column(
                    width = 3,
                    
                    actionButton("plot_mulxGraph", "Plot Graph")
                    
                  )
                  
                ),
                
                h3("3D Plot" , style = "color: #3c8dbc; border-color: #3c8dbc;"),
                tags$hr(style = "border-color: #3c8dbc;"),
                
                fluidRow(
                  column(
                    width = 12,
                    scatterplotThreeOutput("mulxPlot",height = "710px") %>% withSpinner(type = 6),
                    downloadButton(outputId = "down_mulxPlot",
                                   label = "Download Plot (HTML)",
                                   style = "align: center; margin-top: 10px")
                  )
                )
              )
            )
          )
        )
      )
      
      
##############################################################################################################
      
      
      
    ))
  
)

