body <- dashboardBody(
    tabItems(
        tabItem(tabName = "load",
                h2("Load"),
                tags$hr(),
                DT::dataTableOutput(outputId = "DT_cache"),
                h3("Pick a bed file (ideally after annotating with peak_annotatR)"),
                tags$hr(),
                shinyFilesButton(id = "FilesLoadSet", label = "Find Files on Server", title = "Find Peaks to Annotate", multiple = F),
                fileInput(inputId = "UploadLoadSet", label = "Browse Local Files"),
                tags$hr()
        ),
        tabItem(tabName = "intersect", 
                h2("Intersect"),
                actionButton("redrawPlot", label = "Redraw"),
                tags$br(),
                tags$br(),
                fluidRow(
                    # tags$div(
                    shiny_ssvPlotBox("Bars", 1, "plotTest1", collapsed = FALSE),
                    shiny_ssvPlotBox("Euler", 2, "plotTest2"),
                    shiny_ssvPlotBox("Membership Map", 3, "plotTest3"),
                    shiny_ssvPlotBox("Venn", 4, "plotTest4"),
                    shiny_ssvPlotBox("Line - aggregated", 5, "plotTest5")
                    
                )
        ),
        tabItem(tabName = "inspect",
                h2("Inspect")       
        )
    )
)