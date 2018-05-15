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
    library(BiocFileCache)
})
source("functions_app.R")
source("setup.R")

source("ui_header.R")
source("ui_body.R")

sidebar = dashboardSidebar(
    sidebarMenuOutput("menu")
)

shinyApp(
    ui = fluidPage(
        useShinyjs(),
        inlineCSS(appCSS),
        inlineCSS(btnCSS),
        div(
            id = "loading-content",
            # h2("Loading...", class="centerPseudo")
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
        # Stop app when browser tab closed
        session$onSessionEnded(stopApp)
        
        load_libs_withProgress(
            libs = c(
                "data.table",
                "rtracklayer",
                "GenomicRanges",
                "ggplot2",
                "cowplot",
                "seqsetvis"
            ), 
            session = session
        )
        
        source("start_cache.R")
        
        # Hide the loading message when the rest of the server function has executed
        shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")    
        shinyjs::show("app-content")
        
        shinyFileChoose(input, 'FilesLoadSet', 
                        roots= roots_load_set, 
                        filetypes=c("bed", "txt", "Peak"))
        
        shinyFileChoose(input, 'FilesLoadBigwig', 
                        roots= roots_load_bw, 
                        filetypes=c("bigwig", "bw"))
        
        shinyFileSave(input, 'FilesSaveSet', 
                      roots= roots_save_set, 
                      filetypes=c("bed"))
        
        pcols = safeBrew(n = 3)
        rvColors = reactiveVal(pcols)
        
        
        rvCache.old = reactiveVal(bfc_dt_disp)
        rvCache = reactiveVal(bfc_dt_disp)
        
        output$DT_cache = DT::renderDataTable({
            DT::datatable(rvCache(), filter = "top", options = list(pageLength = 10, scrollX = T))
        })
        
        rvActive.old = reactiveVal(bfc_dt_disp[numeric()])
        rvActive = reactiveVal(bfc_dt_disp[numeric()])
        
        output$DT_active = DT::renderDataTable({
            DT::datatable(rvActive(), filter = "top", options = list(pageLength = 10, scrollX = T))
        })
        
        output[["plotTest1"]] = renderPlot(width = 280, height = 280, {
            input$redrawPlot
            ssvFeatureBars(CTCF_in_10a_overlaps_gr, bar_colors = rvColors()) + 
                guides(color = "none")
        })
        
        output$plotTest2 = renderPlot(width = 280, height = 280, {
            input$redrawPlot
            ssvFeatureEuler(CTCF_in_10a_overlaps_gr, circle_colors = rvColors()) + 
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
                         badgeLabel = "!", 
                         badgeColor = "red", 
                         selected = TRUE),
                menuItem("Intersect", tabName = "intersect", icon = icon("bar-chart", lib = "font-awesome"), badgeLabel = "!", badgeColor = "red"),
                menuItem("Inspect", tabName = "inspect", icon = icon("area-chart", lib = "font-awesome"), badgeLabel = "!", badgeColor = "red")
            )
        })
        
        
        # updateTabItems(session, "tabs", "intersect")
    }
)