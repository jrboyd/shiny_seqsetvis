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
    library(colourpicker)
    library(magrittr)
    library(shinyFiles)
    library(shinycssloaders)
    library(BiocFileCache)
    library(DT)
})
source("functions_app.R")
source("class_ssv_config.R")
source("setup.R")
source("module_create_config.R")
source("module_plots.R")
source("module_debug.R")

source("ui_header.R")
source("ui_body.R")

sidebar = dashboardSidebar(
    sidebarMenuOutput("menu")
)

DEBUG = FALSE

cb1 = DT::JS(
    "
table.on('click.dt', 'tr', function() {
});")

cb1a = DT::JS(
    "
table.on('click.dt', 'tr', function() {
table.$('tr.odd').className = table.$('tr.odd').className.concat(' selected');
});")

cb2 = DT::JS(
    "    
table.on('user-select', function (e, dt, type, cell, originalEvent) {
if ($(cell.node()).parent().hasClass('selected')) {
e.preventDefault();
}
});
"
)

shinyApp(
    ui = fluidPage(
        #this appears to be necessary for any colorpicker to work
        shinyjs::hidden(colourpicker::colourInput(inputId = "colTest", label = "colTest")),
        useShinyjs(),
        inlineCSS(appCSS),
        inlineCSS(btnCSS),
        inlineCSS(tblCSS),
        inlineCSS(boxCSS),
        shinyjs::hidden(
            ui_debug()
        ),
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
        tags$script(src="jrb_customize.js", charset="utf-8")
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
        #shiny-tab-intersect
        shinyjs::addClass(class = "my_plotbox", selector = "#shiny-tab-intersect .box")
        
        shinyFileChoose(input, 'FilesLoadSet', 
                        roots= roots_load_set, 
                        filetypes=c("bed", "txt", "Peak"))
        
        shinyFileChoose(input, 'FilesLoadBigwig', 
                        roots= roots_load_bw, 
                        filetypes=c("bigwig", "bw"))
        
        shinyFileSave(input, 'FilesSaveSet', 
                      roots= roots_save_set, 
                      filetypes=c("bed"))
        
        bfc_dt_disp = make_bfc_table(bfc_data, displayOnly = TRUE)
        
        
        
        ### config create
        rvFilesTable = reactiveVal(bfc_dt_disp)
        rvCfgTable = reactiveVal(make_cfg_table())
        rvValidCfg = reactiveVal(NULL)
        
        
        server_create_config(rv_valid_cfg = rvValidCfg,
                             rv_files_table = rvFilesTable, 
                             rv_cfg_table = rvCfgTable,
                             input, output, session)
        
        rvLastSel = reactiveVal(NULL)
        
        observeEvent(rvValidCfg(), 
                     {req(rvValidCfg()); showNotification("Valid CFG!")})
        
        observeEvent(
            eventExpr = {
                input$DT_configSelect_rows_selected
            }, 
            handlerExpr = {
                req(input$DT_configSelect_rows_selected)
                new_sel = input$DT_configSelect_rows_selected
                rvLastSel(new_sel)
            })
        
        output$DT_configSelect = DT::renderDataTable({
            DT::datatable(rvCfgTable(), rownames = FALSE,
                          filter = "top", 
                          selection = list(mode = 'single', selected = 1, target = 'row'),
                          options = list(pageLength = 10, scrollX = T, scrollY = TRUE))
        })
        
        rSelectedCfgDesc = reactive({
            req(rvLastSel())
            req(rvCfgTable())
            req(cfgs_list)
            rvCfgTable()[rvLastSel(), "Description"]
        })
        
        
        ### config select
        output$HTML_configDetail = renderUI({
            req(rSelectedCfgDesc())
            cfg = rSelectedCfgDesc()
            # save(cfg, file = "mycfg.save")
            tags$div(
                make_cfg_detail(rSelectedCfgDesc())
            )
        })
        
        ### plots
        rvFeatures = reactiveVal(CTCF_in_10a_overlaps_gr)
        rvSignals = reactiveVal(CTCF_in_10a_profiles_dt)
        rvColors = reactiveVal(safeBrew(3))
        
        observeEvent(
            eventExpr = {
                rSelectedCfgDesc()
            }, 
            handlerExpr = {
                cfg_name = rSelectedCfgDesc()
                cfg = cfgs_list[[cfg_name]]
                np_fpaths = extract_cfg_files(cfg = cfg, name_by = cfg@color_var, qtype = "narrowPeak")
                bw_fpaths = extract_cfg_files(cfg = cfg, name_by = cfg@color_var, qtype = "bigWig")
                peaks_gr = easyLoad_narrowPeak(np_fpaths)
                olaps = ssvOverlapIntervalSets(peaks_gr)
                # bw_dt = ssvFetchBigwig(file_paths = bw_fpaths, qgr = olaps, return_data.table = TRUE)
                bw_dt = CTCF_in_10a_profiles_dt
                bw_dt$sample = sub("_.+", "", bw_dt$sample)
                bw_dt = bw_dt[sample %in% names(cfg@colors)]
                #triggers plot updates
                rvFeatures(olaps)
                rvSignals(bw_dt)
                rvColors(cfg@colors)
            }
        )
        
        server_plots(rvColors, rvFeatures, rvSignals,
                     input, output, session)
        
        ### menus
        output$tabset1Selected <- renderText({
            input$tabset1
        })
        
        output$menu <- renderMenu({
            sidebarMenu(
                menuItem("Load", 
                         tabName = "load", 
                         icon = icon("file-o", lib = "font-awesome"), 
                         # badgeLabel = "!", 
                         # badgeColor = "red", 
                         selected = TRUE
                ),
                menuItem("Intersect", 
                         tabName = "intersect", 
                         icon = icon("bar-chart", lib = "font-awesome") 
                ),
                menuItem("Inspect", 
                         tabName = "inspect", 
                         icon = icon("area-chart", lib = "font-awesome")
                )
            )
        })
        
        if(DEBUG){
            server_debug(input, output, session)
        }
        # changes starting tab
        # updateTabItems(session, "tabs", "intersect")
    }
)
