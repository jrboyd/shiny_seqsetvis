#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(shinydashboard)
    library(magrittr)
    library(shinyFiles)
    library(shinycssloaders)
})


appCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

bw_cache_path = "~/ShinyApps/shiny_peak_data/cached_profiles"
bed_path = "~/ShinyApps/shiny_peak_data/beds/"
names(bed_path) = "intersectR"

user_roots = dir("/slipstream/home/", full.names = T) %>% paste0(. , "/ShinyData")
user_roots = subset(user_roots, dir.exists(user_roots))
names(user_roots) = dirname(user_roots) %>% basename()
qcframework_load <<- dir("/slipstream/galaxy/uploads/working/qc_framework", pattern = "^output", full.names = T)

names(qcframework_load) <- basename(qcframework_load)
roots_load_set = c(bed_path, user_roots, qcframework_load)
roots_load_set = roots_load_set[dir.exists(roots_load_set)]
# names(roots_load_set) <- basename(roots_load_set)
roots_load_bw <<- c("/slipstream/galaxy/production/galaxy-dist/static/UCSCtracks/", 
                    qcframework_load)
names(roots_load_bw) <- basename(roots_load_bw)
roots_load_bw = c(user_roots, roots_load_bw)
roots_load_bw = roots_load_bw[dir.exists(roots_load_bw)]

roots_save_set = c(bed_path, user_roots)
roots_save_set = roots_save_set[dir.exists(roots_save_set)]

shinyFiles2load = function(shinyF, roots){
    root_path = roots[shinyF$root]
    rel_path = paste0(unlist(shinyF$files), collapse = "/")
    file_path = paste0(root_path, "/", rel_path)
    return(file_path)
}

shinyFiles2save = function(shinyF, roots){
    root_path = roots[shinyF$root]
    rel_path = paste0(unlist(shinyF$name), collapse = "/")
    file_path = paste0(root_path, "/", rel_path)
    return(file_path)
}

bw_cache_path = "~/ShinyApps/shiny_peak_data/cached_profiles"
bed_path = "~/ShinyApps/shiny_peak_data/beds/"
names(bed_path) = "intersectR"


shiny_ssvPlotBox = function(box_title = "Main Plot", id = 1, plot_id = "plotTest", collapsed = TRUE){
    id = as.character(id)
    mybox = box(title = box_title, collapsible = TRUE, collapsed = collapsed, solidHeader = TRUE, status = "primary", 
        fluidRow(
            column(width = 6,
                   withSpinner(plotOutput(plot_id, width = "280px", height = "280px"))),
            column(width = 6,
                   style = "overflow-y:scroll; max-height: 280px",
                   h1("Settings:"),
                   
                   textInput(paste0("textTest-", id), "Textin", placeholder = "text here"),
                   sliderInput(paste0("sliderTest-", id) , "Slip'n slide", min = 0, max = 3, value = 1),
                   radioButtons(paste0("radioTest-", id), "ABC", LETTERS[1:3]),
                   h1("asdfasd"),
                   h1("asdfasd"),
                   h1("asdfasd"),
                   h1("asdfasd"),
                   h1("asdfasd"),
                   h1("asdfasd"),
                   h1("asdfasd"),
                   h1("asdfasd"),
                   h1("asdfasd"),
                   p("asdfasd")
            )
        )
    )
    # mybox = as.character(mybox)
    # to_ins =  'data-widget="collapse"'
    # mybox = sub(to_ins, "", mybox)
    # mybox = sub('class="box-header"', paste('class="box-header"', to_ins), mybox)
    # HTML(mybox)
    mybox
}

header = dashboardHeader(
    
    dropdownMenu(
        type = "notifications", 
        icon = icon("question-circle"),
        badgeStatus = NULL,
        headerText = "See also:",
        
        notificationItem("shiny", icon = icon("file"),
                         href = "http://shiny.rstudio.com/"),
        notificationItem("shinydashboard", icon = icon("file"),
                         href = "https://rstudio.github.io/shinydashboard/")
    ),
    title = "seqsetvis"
)
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "load",
                h2("Load"),
                tags$hr(),
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

sidebar = dashboardSidebar(
    sidebarMenuOutput("menu")
)

shinyApp(
    ui = fluidPage(
        useShinyjs(),
        
        inlineCSS(appCSS),
        div(
            id = "loading-content",
            h2("Loading...")
        ),
        hidden(
            div(
                id = "app-content",
                dashboardPage(
                    header,
                    sidebar,
                    body
                ))),
        tags$script(src="tony.js", charset="utf-8")
        ),
    server = function(input, output, session) {
        suppressPackageStartupMessages({
            library(seqsetvis)
            library(cowplot)
        })
        # Stop app when browser tab closed
        session$onSessionEnded(stopApp)
        # Hide the loading message when the rest of the server function has executed
        hide(id = "loading-content", anim = TRUE, animType = "fade")    
        show("app-content")
        
        # The currently selected tab from the first box
        shinyFileChoose(input, 'FilesLoadSet', 
                        roots= roots_load_set, 
                        filetypes=c("bed", "txt", "Peak"))
        
        shinyFileChoose(input, 'FilesLoadBigwig', 
                        roots= roots_load_bw, 
                        filetypes=c("bigwig", "bw"))
        
        shinyFileSave(input, 'FilesSaveSet', 
                      roots= roots_save_set, 
                      filetypes=c("bed"))
        
        pcols = safeBrew(3, pal = "Dark2")
        
        output$plotTest1 = renderPlot(width = 280, height = 280, {
            input$redrawPlot
            ssvFeatureBars(CTCF_in_10a_overlaps_gr, bar_colors = pcols) + 
                guides(color = "none")
        })
        
        output$plotTest2 = renderPlot(width = 280, height = 280, {
            input$redrawPlot
            ssvFeatureEuler(CTCF_in_10a_overlaps_gr, circle_colors = pcols) + 
                guides(color = "none", fill = "none")
        })
        
        output$plotTest3 = renderPlot(width = 280, height = 280, {
            input$redrawPlot
            ssvFeatureBinaryHeatmap(CTCF_in_10a_overlaps_gr) + 
                guides(color = "none")
        })
        
        output$plotTest4 = renderPlot(width = 280, height = 280, {
            input$redrawPlot
            ssvFeatureVenn(CTCF_in_10a_overlaps_gr) + 
                scale_color_manual(values = safeBrew(3, pal = "Dark2")) +
                guides(color = "none", fill = "none")
        })
        
        output$plotTest5 = renderPlot(width = 280, height = 280, {
            input$redrawPlot
            ssvSignalLineplotAgg(CTCF_in_10a_profiles_dt, spline_n = 5) + 
                scale_color_manual(values = safeBrew(3, pal = "Dark2")) +
                theme_cowplot() + guides(color = "none")
        })
        
        output$tabset1Selected <- renderText({
            input$tabset1
        })
        
        output$menu <- renderMenu({
            sidebarMenu(
                menuItem("Load", 
                         tabName = "load", 
                         icon = icon("file-o", lib = "font-awesome"), 
                         badgeLabel = "!", badgeColor = "red"),
                menuItem("Intersect", tabName = "intersect", icon = icon("bar-chart", lib = "font-awesome"), badgeLabel = "!", badgeColor = "red", selected = TRUE),
                menuItem("Inspect", tabName = "inspect", icon = icon("area-chart", lib = "font-awesome"), badgeLabel = "!", badgeColor = "red")
            )
        })
        
        
        # updateTabItems(session, "tabs", "intersect")
    }
)