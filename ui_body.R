body <- dashboardBody(
    tabItems(
        tabItem(tabName = "load",
                h2("Load"),
                tags$hr(),
                tabsetPanel(type = "tabs",
                            tabPanel(
                                "Use Config", 
                                h3("Available Configurations"),
                                withSpinner(DT::dataTableOutput(outputId = "DT_configSelect")),
                                box(title = "Config Details", collapsible = FALSE, height = "300px",
                                    withSpinner(uiOutput(outputId = "HTML_configDetail", height = "200px"), proxy.height = "200px")
                                )
                            ),
                            tabPanel(
                                "Create Config", 
                                ui_create_config()
                            ),
                            tabPanel(
                                "Add Files", 
                                h3("Add Files To System"),
                                tags$hr(),
                                shinyFilesButton(id = "FilesLoadSet", 
                                                 label = "Find Files on Server", 
                                                 title = "Find Peaks to Annotate", 
                                                 multiple = F),
                                fileInput(inputId = "UploadLoadSet", 
                                          label = "Browse Local Files")
                            )
                )
        ),
        tabItem(tabName = "intersect",
                h2("Intersect"),
                actionButton("redrawPlot", label = "Redraw"),
                tags$br(),
                tags$br(),
                fluidRow(
                    # tags$div(
                    shiny_ssvPlotBox("Bars", "intBars", collapsed = FALSE),
                    shiny_ssvPlotBox("Euler", "intEuler"),
                    shiny_ssvPlotBox("Membership Map", "intMemb"),
                    shiny_ssvPlotBox("Venn", "intVenn")
                    
                )
        ),
        tabItem(tabName = "inspect",
                h2("Inspect"),
                fluidRow(
                    shiny_ssvPlotBox("Line - aggregated", "intLineAgg", collapsed = FALSE)       
                )
        )
    )
)