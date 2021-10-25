#' @title get_data
#' @description starts shiny app with GUI for data selection and visualization
#' @param source either "LRE" (default) to access inbuilt data sets (alternatively "lrec" for older data set)
#' @param start startdate as character string (e.g., "2015-01-01", defaults to "2015-09-01"). Has to be in the boundaries of the year 2015.
#' @param end enddate as character string (e.g., "2015-12-31", defaults to "2015-09-30"). Has to be in the boundaries of the year 2015.
#' @return (optionally) returns selected data set as .csv or `dataframe`
#' @examples
#' \dontrun{
#' get_data(source = "LRE")
#' }
#' @export
#' @import igraph
#' @import shiny
#' @import shinydashboard
#' @importFrom magrittr %>%


get_data <- function(source = "LRE", start = "2015-09-01", end = "2015-09-30"){
          if(start < "2015-01-01" | start > "2015-12-31"){
                    start <- "2015-09-01"
          }
          if(end < "2015-01-01" | end > "2015-12-31"){
                    end <- "2015-09-30"
          }
          if(source == "LRE"){
                    example <- "LRE"
                    data <- LRE
                    data$cdate <- lubridate::ymd(data$cdate)
                    data$parties[data$parties == "AFD"] <- "AfD"
                    data$parties <- gsub("Gr.ne", "Green", data$parties)
                    data$parties[data$parties == "Linke"] <- "Left"
          } else if(source == "lrec"){
                    example <- "lrec"
                    data <- LREC
                    data$cdate <- lubridate::ymd(data$cdate)
                    data$parties[sapply(1:nrow(data), function(x) length(data$parties[[x]]) != 1)] <- NA
                    data$parties <- unlist(data$parties)
                    data$parties <- gsub("Gr.ne", "Green", data$parties)
                    data$parties[data$parties == "Linke"] <- "Left"
          }
          data <- data %>% dplyr::mutate(rowid = paste0("C", 1:nrow(.)))
          ui <- dashboardPage(
                    dashboardHeader(title = "Project Mardy",
                                    tags$li(class = "dropdown", actionLink("assign", "Export selection to .GlobalEnv"))), # 10.03.2021; adapted from: https://stackoverflow.com/questions/48923091/adding-actionbutton-in-shiny-dashboardheader
                    dashboardSidebar(
                              shinyjs::useShinyjs(),
                              selectInput("var",
                                          label = "add. variables:",
                                          c("all", colnames(data)[5:length(data)]),
                                          selected = c("quote", "parties"),
                                          multiple = T),
                              radioButtons("unnest",
                                           label = "Unnest actors and claims?",
                                           choices = c("nested", "unnest"),
                                           selected = "unnest"),
                              radioButtons("legacy",
                                           label = "use all articles?",
                                           choices = c("all", "lrec only"),
                                           selected = "all"),
                              dateRangeInput("date",
                                             label = "time: yyyy-mm-dd",
                                             start = as.character(start),
                                             end = as.character(end),
                                             min = "2015-01-01",
                                             max = "2015-12-31",
                                             width = "1000px",
                                             startview = "year"),
                              selectInput("twoslice", "slice",
                                          choices = c(0, 1, 2, 3, 4, 5)),
                              # shinyBS::bsTooltip("twoslice", title = "extract n-slice of network (omits same-day-duplicates & adds multiple edges of the same weight)",
                              #                    placement = "right", trigger = "hover"),
                              sliderInput("degree", "Degree", value = 0, min = 0, max = 100),
                              #shinyBS::bsTooltip("degree", title = "set minimum vertex degree", placement = "right", trigger = "hover"),
                              sidebarMenu(id = "tabs",
                                          menuItem("Data", tabName = "data", icon = icon("dashboard")),
                                          menuItem("Network", tabName = "network", icon = icon("connectdevelop")),
                                          menuItem("Keywords", tabName = "keywords", icon = icon("comments")))),

                    dashboardBody(

                              tabItems(
                                        tabItem(tabName = "data",
                                                fluidRow(valueBoxOutput("rows"),
                                                         valueBoxOutput("actors"),
                                                         valueBoxOutput("claims"),
                                                         box(DT::dataTableOutput("table"), width = 12, solidHeader = F))),
                                        tabItem(tabName = "network",
                                                fluidRow(valueBoxOutput("meandegree"),
                                                         valueBoxOutput("nodes"),
                                                         valueBoxOutput("edges"),
                                                         box(visNetwork::visNetworkOutput("network_plot", height = "500px", width = "auto"), width = 12, solidHeader = F, status = "info"),
                                                         box(selectInput("weight", "Weight", choices = c("all" = "all", "neg" = -1, "pos" = 1, "conf" = 2)), width = 3),
                                                         box(selectInput("projection", "Projection", choices = c("affiliation", "actor", "concept", "claim_cooc")), width = 3),
                                                         # shinyBS::bsTooltip("weight", title = "filter edges according to weight", placement = "left", trigger = "hover"),
                                                         # shinyBS::bsTooltip("projection", title = "switch between bipartite and unipartite projections as well as co-occurence networks (warning: projections are experimental)",
                                                         #                    placement = "left", trigger = "hover"),
                                                         box(selectInput("layout", "Layout", choices = c("layout_nicely", "layout_as_bipartite", "layout_as_star", "layout_as_tree",
                                                                                                         "layout_with_dh", "layout_with_drl", "layout_with_fr", "layout_with_gem",
                                                                                                         "layout_with_graphopt", "layout_with_kk", "layout_with_lgl",
                                                                                                         "layout_with_mds", "layout_with_sugiyama"), selected  = "layout_nicely"), width = 3),
                                                         #shinyBS::bsTooltip("layout", title = "Choose layout (experimental), use with caution", placement = "left", trigger = "hover"),
                                                         box(selectInput("multi", "show multiplex",choices = c(FALSE,TRUE)), width = 3)
                                                )),
                                        tabItem(tabName = "keywords",
                                                fluidRow(valueBoxOutput("tokens"),
                                                         valueBoxOutput("types"),
                                                         valueBoxOutput("sentences"),
                                                         box(DT::dataTableOutput("keywordtable"), width = 12, solidHeader = F)))
                              )
                    )
          )
          server <- function(input, output, session){
                    tt <- reactiveValues(t = NULL)
                    gg <- reactiveValues(g = NULL)
                    observe({
                              if(input$tabs == "data"){
                                        shinyjs::enable("date")
                                        shinyjs::enable("var")
                                        shinyjs::enable("unnest")
                                        shinyjs::enable("twoslice")
                                        shinyjs::reset("projection")
                                        shinyjs::reset("degree")
                                        shinyjs::reset("weight")
                                        shinyjs::disable("degree")
                                        shinyjs::enable("legacy")

                              }
                              data <- data %>%
                                        {if (input$legacy != "all") dplyr::filter(., legacy == "old") else(.)} %>%
                                        dplyr::filter(cdate >= min(input$date) & cdate <= max(input$date))
                              if(input$unnest == "unnest" & example == "lrec"){
                                        data <- data %>%
                                                  tidyr::unnest_legacy(claimvalues, .drop = F) %>%
                                                  dplyr::filter(!grepl("[1-9]00|999", claimvalues))
                              }
                              if(input$unnest == "unnest" & example == "LRE"){
                                        data <- data %>%
                                                  tidyr::separate_rows(claimvalues, sep = ",") %>%
                                                  dplyr::mutate(claimvalues = gsub("\\D", "", claimvalues)) %>%
                                                  dplyr::filter(!grepl("[1-9]00|999", claimvalues)) %>%
                                                  dplyr::mutate(label = suppressWarnings(lookup_codes(claimvalues))) %>%
                                                  as.data.frame()
                              }

                              if(all(input$var != "all")){
                                        data <- data %>% dplyr::select(name, claimvalues, cpos, cdate, input$var)
                              }
                              if(input$twoslice != 0 & input$unnest == "unnest"){
                                        data <- data %>%
                                                  dplyr::distinct(name, claimvalues, cpos, cdate, .keep_all = T) %>%
                                                  dplyr::group_by(name, claimvalues, cpos) %>%
                                                  dplyr::mutate(cpos = dplyr::n()*as.numeric(cpos)) %>%
                                                  dplyr::filter(abs(cpos) >= as.numeric(input$twoslice)) %>%
                                                  dplyr::ungroup() %>%
                                                  dplyr::distinct(name, claimvalues, cpos, .keep_all = T) %>%
                                                  as.data.frame()
                              }
                              tt$t <- data
                    })
                    output$rows <- renderValueBox({
                              valueBox(paste0(nrow(tt$t[input[["table_rows_all"]],])), "observations", icon = icon("list"),
                                       color = "purple")
                    })
                    output$actors <- renderValueBox({
                              valueBox(paste0(length(unique(tt$t[input[["table_rows_all"]], "name"]))), "actors", icon = icon("list"),
                                       color = "yellow")
                    })
                    output$claims <- renderValueBox({
                              valueBox(paste0(length(unique(tt$t[input[["table_rows_all"]], "claimvalues"]))), "claims", icon = icon("list"),
                                       color = "green")
                    })
                    output$table <- DT::renderDataTable(DT::datatable(tt$t,
                                                                      filter = "bot",
                                                                      extensions = "Buttons",
                                                                      options = list(dom = "Blrtip",
                                                                                     buttons = c("csv", "pdf"),
                                                                                     pagelength = 10,
                                                                                     lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "all")),
                                                                                     searching = T,
                                                                                     stateSave = F,
                                                                                     search = list(regex = T, caseInsensitive = TRUE))),
                                                        server = T)
                    observeEvent(input$assign, {
                              if(input$tabs == "data"){
                                        position <- 1                                                                                  # 10.03.2021; adapted from: https://github.com/floybix/latticist/issues/13
                                        showModal(modalDialog(
                                                  title = "Task successful",
                                                  "Data set exported to .globalEnv", fade = T))
                                        assign("mardy_data_set", tt$t[input[["table_rows_all"]], ], envir = as.environment(position))
                              }
                              if(input$tabs == "network"){
                                        position <- 1
                                        showModal(modalDialog(
                                                  title = "Task successful",
                                                  "Network exported to .globalEnv", fade = T))
                                        assign("mardy_network", gg$g, envir = as.environment(position))
                              }
                    })
                    observe({
                              if(input$tabs == "network"){
                                        shinyjs::disable("date")
                                        shinyjs::disable("var")
                                        shinyjs::disable("unnest")
                                        shinyjs::disable("twoslice")
                                        shinyjs::enable("degree")
                                        shinyjs::disable("legacy")
                                        req(tt$t)
                                        data <- tt$t[input[["table_rows_all"]], ]
                                        if(input$projection == "affiliation"){
                                                  g <- graph_from_data_frame(data[,c("name", "claimvalues", "cpos")], directed = F)
                                                  if(input$degree != 0){
                                                            g <- induced_subgraph(g, degree(g) >= input$degree)
                                                  }
                                                  if(input$twoslice != 0){
                                                            E(g)$label <- as.numeric(E(g)$cpos)
                                                            E(g)$width <- abs(as.numeric(E(g)$cpos))
                                                            E(g)$cpos <- sign(as.numeric(E(g)$cpos))
                                                  }
                                                  if(input$weight == "all"){
                                                            g <- subgraph.edges(g, which(E(g)$cpos != 0), delete.vertices = F)
                                                  } else {
                                                            g <- subgraph.edges(g, which(E(g)$cpos == input$weight))
                                                  }
                                                  V(g)$type <- V(g)$name %in% data[,1]
                                                  if("parties" %in% colnames(data) & vcount(g) > 0){
                                                            V(g)$color <- coloring(g, data = data)
                                                  } else {
                                                            V(g)$color <- ifelse(V(g)$type, "skyblue", "salmon")
                                                  }
                                                  V(g)$shape <- ifelse(V(g)$type, "dot", "square")
                                                  E(g)$color <- ifelse(E(g)$cpos < 0, "red", "blue")
                                                  gg$g <- g
                                        } else if(input$projection == "actor" & input$unnest == "unnest" & length(unique(data$name)) > 1) {
                                                  g <- two2one(data[,c("name", "claimvalues", "cpos")])
                                                  if(input$degree != 0){
                                                            g <- induced_subgraph(g, degree(g) >= input$degree)
                                                  }
                                                  if(input$weight == "all"){
                                                            g <- subgraph.edges(g, which(E(g)$cpos != 0), delete.vertices = F)
                                                  } else {
                                                            g <- subgraph.edges(g, which(E(g)$cpos == input$weight))
                                                  }
                                                  if("parties" %in% colnames(data) & vcount(g) > 0){
                                                            V(g)$color <- coloring(g, data = data)
                                                  }
                                                  gg$g <- g

                                        } else if(input$projection == "concept" & input$unnest == "unnest"  & length(unique(data$claimvalues)) > 1){
                                                  g <- two2one(data[,c("claimvalues", "name", "cpos")])
                                                  if(input$degree != 0){
                                                            g <- induced_subgraph(g, degree(g) >= input$degree)
                                                  }
                                                  if(input$weight == "all"){
                                                            g <- subgraph.edges(g, which(E(g)$cpos != 0), delete.vertices = F)
                                                  } else {
                                                            g <- subgraph.edges(g, which(E(g)$cpos == input$weight))
                                                  }
                                                  gg$g <- g
                                        } else if (input$projection == "claim_cooc" & input$unnest == "unnest" & "rowid" %in% colnames(data)){
                                                  g <- graph_from_data_frame(data[, c("rowid",
                                                                                      "claimvalues", "cpos")], directed = F)
                                                  V(g)$type <- bipartite.mapping(g)$type
                                                  g <- igraph::bipartite.projection(g)[[2]]
                                                  if (input$degree != 0) {
                                                            g <- induced_subgraph(g, degree(g) >= input$degree)
                                                  }
                                                  if(vcount(g) != 0){
                                                            V(g)$color <- membership(fastgreedy.community(g))
                                                  }
                                                  gg$g <- g
                                        } else {
                                                  showModal(modalDialog(
                                                            title = "Warning",
                                                            "Nested projection and projection of singular nodes not implemented.
                                                             Please select multiple actors and reset to 'unnest'.", fade = T))
                                        }
                              }
                    })
                    output$nodes <- renderValueBox({
                              req(gg$g)
                              valueBox(paste0(vcount(gg$g)), "nodes", icon = icon("project-diagram"),
                                       color = "purple")
                    })
                    output$edges <- renderValueBox({
                              req(gg$g)
                              valueBox(paste0(ecount(gg$g)), "edges", icon = icon("connectdevelop"),
                                       color = "yellow")
                    })
                    output$meandegree <- renderValueBox({
                              req(gg$g)
                              valueBox(paste0(round(mean(degree(gg$g)),2)), "mean degree", icon = icon("signal"),
                                       color = "green")
                    })
                    output$network_plot <- visNetwork::renderVisNetwork({
                              req(gg$g)
                              visNetwork::visIgraph(gg$g) %>%
                                        visNetwork::visIgraphLayout(input$layout, smooth = list(enabled = as.logical(input$multi), type = 'dynamic'))

                    })


                    observeEvent(c(input$tabs),{

                    })

          }
          shinyApp(ui, server)
}
