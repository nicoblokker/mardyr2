#' @title get_data
#' @description starts shiny app with GUI for data selection and visualization
#' @param source either "online" (default) to download data or "example" to access inbuild data set
#' @return (optionally) returns selected data set as .csv or `dataframe`
#' @examples
#' \dontrun{
#' get_data(source = "example")
#' }

#' @export
#' @import igraph
#' @import shiny
#' @import shinydashboard
#' @importFrom magrittr %>%
#' @importFrom utils write.csv

get_data <- function(source = "online"){
          if(source == "online"){
                    data <- jsonlite::fromJSON("https://clarin09.ims.uni-stuttgart.de/debatenet/DebateNet-migr15.json")$data
                    data <- data %>% tidyr::unnest_legacy(actorclusters, .drop = F) %>% dplyr::select(name, claimvalues, cpos, cdate, quote, parties)
                    data$cdate <- lubridate::ymd(data$cdate)
                    data$parties[sapply(1:nrow(data), function(x) length(data$parties[[x]]) != 1)] <- NA
                    data$parties <- unlist(data$parties)
                    data$parties <- gsub("Gr.ne", "Green", data$parties)
                    data$parties[data$parties == "Linke"] <- "Left"
          } else if(source == "example"){
                    data <- mardydemo
                    data$cdate <- lubridate::ymd(data$cdate)
                    data$parties[sapply(1:nrow(data), function(x) length(data$parties[[x]]) != 1)] <- NA
                    data$parties <- unlist(data$parties)
                    data$parties <- gsub("Gr.ne", "Green", data$parties)
                    data$parties[data$parties == "Linke"] <- "Left"
          }
          ui <- dashboardPage(
                    dashboardHeader(title = "Project Mardy",
                                    tags$li(class = "dropdown", actionLink("assign", "Export selection to .GlobalEnv", class = "my_class"))),
                    dashboardSidebar(
                              shinyjs::useShinyjs(),
                              selectInput("var",
                                          label = "add. variables:",
                                          c("all", colnames(data)[4:6]),
                                          selected = c("quote"),
                                          multiple = T),
                              radioButtons("unnest",
                                           label = "Unnest actors and claims?",
                                           choices = c("nested", "unnest"),
                                           selected = "unnest"),
                              dateRangeInput("date",
                                             label = "time: yyyy-mm-dd",
                                             start = "2015-09-01",
                                             end = "2015-09-30",
                                             min = "2015-01-01",
                                             max = "2015-12-31",
                                             width = "1000px",
                                             startview = "year"),
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
                                                         box(sliderInput("degree", "Degree", value = 0, min = 0, max = 100), width = 3),
                                                         box(selectInput("weight", "Weight", choices = c("all" = "all", "neg" = -1, "pos" = 1, "conf" = 2)), width = 3),
                                                         box(selectInput("projection", "Projection", choices = c("affiliation", "actor", "concept")), width = 3),
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
                                        shinyjs::reset("projection")
                                        shinyjs::reset("degree")
                                        shinyjs::reset("weight")
                              }
                              data <- data %>%
                                        dplyr::filter(cdate >= min(input$date) & cdate <= max(input$date)) #%>%
                              if(input$unnest == "unnest"){
                                        data <- data %>%
                                                  tidyr::unnest_legacy(claimvalues, .drop = F) %>%
                                                  dplyr::filter(!grepl("[1-9]00|999", claimvalues))
                              }
                              if(all(input$var != "all")){
                                        data <- data %>% dplyr::select(name, claimvalues, cpos, input$var)
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
                                                                      #editable = list(target = "cell", disable = list(columns = c(1:6))),
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
                                        position <- 1
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
                                        req(tt$t)
                                        data <- tt$t[input[["table_rows_all"]], ]
                                        if(input$projection == "affiliation"){
                                                  g <- graph_from_data_frame(data[,c("name", "claimvalues", "cpos")], directed = F)
                                                  if(input$degree != 0){
                                                            g <- induced_subgraph(g, degree(g) >= input$degree)
                                                  }
                                                  if(input$weight == "all"){
                                                            g <- subgraph.edges(g, which(E(g)$cpos != 0))
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
                                                  g <- project(data[,c("name", "claimvalues", "cpos")])
                                                  if(input$degree != 0){
                                                            g <- induced_subgraph(g, degree(g) >= input$degree)
                                                  }
                                                  if(input$weight == "all"){
                                                            g <- subgraph.edges(g, which(E(g)$cpos != 0))
                                                  } else {
                                                            g <- subgraph.edges(g, which(E(g)$cpos == input$weight))
                                                  }
                                                  if("parties" %in% colnames(data) & vcount(g) > 0){
                                                            V(g)$color <- coloring(g, data = data)
                                                  }
                                                  gg$g <- g

                                        } else if(input$projection == "concept" & input$unnest == "unnest"  & length(unique(data$claimvalues)) > 1){
                                                  g <- project(data[,c("claimvalues", "name", "cpos")])
                                                  if(input$degree != 0){
                                                            g <- induced_subgraph(g, degree(g) >= input$degree)
                                                  }
                                                  if(input$weight == "all"){
                                                            g <- subgraph.edges(g, which(E(g)$cpos != 0))
                                                  } else {
                                                            g <- subgraph.edges(g, which(E(g)$cpos == input$weight))
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
                              visNetwork::visIgraph(gg$g,
                                                    smooth = list(enabled = as.logical(input$multi), type = 'dynamic'))

                    })


                    observeEvent(c(input$tabs),{

                    })

          }
          shinyApp(ui, server)
}
