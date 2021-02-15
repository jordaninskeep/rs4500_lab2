pckgs <- c("shiny","DT","readxl","vctrs",
           "dplyr","tidyverse","visNetwork",
           "igraph","hues","scales",
           "shinydashboard","shinyjs","gt")
lapply(pckgs, 
       function(x) if(!require(x,character.only = TRUE)) install.packages(x, dependencies = TRUE))

ui <- dashboardPage(
      dashboardHeader(title = "Social Network"),
      dashboardSidebar(
          sidebarMenu(
              menuItem("Network",tabName = "visNetwork",icon = icon("project-diagram")),
              menuItem("Tables",tabName = "tables",icon = icon("table")),
              selectInput("layout","Select Layout:",
                          choices = c("layout_nicely","layout_on_grid",
                                      "layout_on_sphere","layout_randomly",
                                      "layout_with_dh","layout_with_fr",
                                      "layout_with_gem","layout_with_graphopt",
                                      "layout_with_kk","layout_with_lgl",
                                      "layout_with_mds","layout_with_sugiyama")
                          ),
            selectInput("nodeValue","Node Size Based on:",
                        choices = c(
                          "Choose Algorithm:",
                          "betweenness",
                          "degree",
                          "closeness",
                          "pagerank"
                          )),
            selectInput("groupBy","Group Based on:",selected = "Louvain",
                        choices = c(
                          "Louvain",
                          "Edge betweenness",
                          "Leading Eigen",
                          "Spinglass",
                          "Walktrap"
                        )),
            checkboxInput("nodePhysics",label = "Node Physics",value = FALSE),
            checkboxInput("edgePhysics",label = "Edge Physics",value = FALSE)
            )),
        dashboardBody(
            tabItems(
                tabItem(tabName = "visNetwork",
                        box(title = NULL,width = 12,visNetworkOutput("visNetwork",height = 580),height = 640),
                        box(title = "Nodes",width = "6",gt_output("nodeLegendGT")),
                        box(title = "Edges",gt_output("edgeLegendGT"))
                ),
                tabItem(tabName = "tables",
                        tabBox(title = "Data Tables", width = 12,
                               id = "tableTabs1",
                               tabPanel("Nodes",dataTableOutput("nodeTable")),
                               tabPanel("Edges",dataTableOutput("edgeTable")),
                               tabPanel("Centrality",dataTableOutput("centrality"))
                )))))

server <- function(input, output, session) {
  
    rNodeData <- reactiveFileReader(1000,NULL,"fhact50data.xlsx", read_excel,sheet=1)
    rEdgeData <- reactiveFileReader(1000,NULL,"fhact50data.xlsx", read_excel,sheet=2)
  
    graphData <- reactiveValues(edgeData = NULL,
                                nodeData = NULL)
    graphData$rigraph <- reactive({
    graph_from_data_frame(d = data.frame(rEdgeData()),
                            directed = FALSE,
                            vertices = data.frame(rNodeData()))
    })
    graphData$edgeData <- reactive({
      edgeData <- data.frame(
        from = rEdgeData()$from,
        to = rEdgeData()$to,
        type = rEdgeData()$type,
        value = rEdgeData()$value,
        label = rEdgeData()$label,
        arrows = rEdgeData()$arrows,
        title = rEdgeData()$title
        )
      edgeData <- left_join(edgeData,graphData$edgeLegend(),by = c("type" = "type"))
    })
    graphData$edgeLegend <- reactive({
      edgeLegend <- data.frame(
        type = unique(rEdgeData()$type)
        ) %>%
        rowid_to_column(var = "typeNumber")
      edgeLegend <-  data.frame(
        type = edgeLegend$type,
        color = iwanthue(max(unique(edgeLegend$typeNumber)),1,190,20,89,40,80))
    })
    graphData$edgeLegendGT <- reactive({
      cEdgeLegend <- graphData$edgeLegend()
        data_color(gt(cEdgeLegend),
          columns = vars(color),
          colors = scales::col_factor(palette = c(cEdgeLegend$color),
                                      domain = NULL,
                                      ordered = TRUE)
        )
    })
    
    graphData$centrality <- reactive({
      data.frame(
        betweenness = betweenness(graphData$rigraph()),
        degree = degree(graphData$rigraph()),
        closeness = closeness(graphData$rigraph()),
        pagerank = page_rank(graphData$rigraph())$vector
      ) %>%
        rownames_to_column(var = "name")
    })
    observe({
      graphData$value <- if(input$nodeValue=="degree"){
        graphData$centrality()$degree
      } else if (input$nodeValue=="betweenness"){
        graphData$centrality()$betweenness
      } else if(input$nodeValue=="closeness"){
        graphData$centrality()$closeness
      } else if(input$nodeValue=="pagerank"){
        graphData$centrality()$pagerank
      } else {
        "0"
      }
    })
    observe({
      graphData$groups <- if (input$groupBy=="Louvain"){
        cluster_louvain(graphData$rigraph())$membership
      } else if (input$groupBy=="Edge betweenness"){
        cluster_edge_betweenness(graphData$rigraph())$membership
      } else if (input$groupBy=="Leading Eigen"){
        cluster_leading_eigen(graphData$rigraph())$membership
      } else if (input$groupBy=="Spinglass"){
        cluster_spinglass(graphData$rigraph())$membership
      } else if (input$groupBy=="Walktrap"){
        cluster_walktrap(graphData$rigraph())$membership
      } else {
        cluster_optimal(graphData$rigraph())
      }
      data.frame(graphData$groups)
    })
    graphData$nodeData <- reactive({
      nodeData <- data.frame(
        name = rNodeData()$name,
        type = rNodeData()$type,
        title = rNodeData()$title,
        group = graphData$groups,
        value = graphData$value
      ) 
      nodeData <- left_join(nodeData,graphData$nodeLegend(), by = c("group" = "group"))
    })
    graphData$nodeLegend <- reactive({
      data.frame(color = iwanthue(max(graphData$groups),1,350,27,100,5,55)) %>%
        rowid_to_column(var = "group")
    })
    graphData$nodeLegendGT <- reactive({
      cNodeLegend <- graphData$nodeLegend()
        data_color(gt(cNodeLegend),
          columns = vars(color),
          colors = scales::col_factor(palette = c(cNodeLegend$color),
                                      domain = NULL,
                                      ordered = TRUE)
        )
    })
    
    graphData$igraph <- reactive({
      graph_from_data_frame(d = data.frame(graphData$edgeData()),
                            directed = FALSE,
                            vertices = data.frame(graphData$nodeData()))
    })

    output$nodeTable <- renderDataTable({graphData$nodeData()},
                                 server = FALSE,
                                 options = list(c
                                     (scrollX = TRUE,
                                     scrollY = TRUE))
    )
    output$edgeTable <- renderDataTable({graphData$edgeData()})
    
    output$nodeLegendGT <- render_gt({graphData$nodeLegendGT()})
    
    output$edgeLegendGT <- render_gt({graphData$edgeLegendGT()})
    
    output$centrality <- renderDataTable({graphData$centrality()})
    
    output$visNetwork <- renderVisNetwork({
        network <- toVisNetworkData(graphData$igraph())
        
        visNetwork(nodes = network$nodes,
                   edges = network$edges
            ) %>%
            visIgraphLayout(randomSeed = 11,
                            layout = input$layout
            ) %>%
            visNodes(
                shape = "dot",
                physics = input$nodePhysics,
                scaling = list(min = 5,max = 30)
            ) %>%
            visEdges(
                shadow = FALSE,
                smooth = c(TRUE,TRUE),
                physics = input$edgePhysics,
                font = 0,
                arrows = list(
                  to = list(scaleFactor = 0.6),
                  from = list(scaleFactor = 0.6)
                  )
            ) %>%
            visPhysics(
                solver = "barnesHut",
                enabled = TRUE,
                stabilization = list(enabled = FALSE)
            ) %>%
            visInteraction(
                hover = TRUE,
                keyboard = T
            ) %>%
          visOptions(
            highlightNearest = list(
              enabled = T,
              degree = 1,
              hover = T),
            selectedBy = "group",
            nodesIdSelection = TRUE
            )
      })
}


shinyApp(ui = ui, server = server)
