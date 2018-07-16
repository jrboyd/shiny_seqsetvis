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
source("class_ssv_config.R")
source("setup.R")

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
        useShinyjs(),
        inlineCSS(appCSS),
        inlineCSS(btnCSS),
        inlineCSS(tblCSS),
        inlineCSS(boxCSS),
        shinyjs::hidden(
            div(id = "debug",
                p("debug out", id = "debugOut"),
                p("debug details", id = "debugDetails")
            )
        ),
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
        
        rvCache.old = reactiveVal(bfc_dt_disp)
        rvCache = reactiveVal(bfc_dt_disp)
        
        rvCacheForCfg = reactiveVal(integer())
        
        get_cache_avail_rows = function(){
            cached_files = rvCache()
            avail = setdiff(seq_len(nrow(cached_files)), rvCacheForCfg())
            avail
        }
        
        sel_row_to_cache_row = function(sel_row){
            avail = get_cache_avail_rows()
            avail[sel_row]
        }
        
        rm_sel_row_from_cache_row = function(sel_row){
            all = seq_len(nrow(rvCache()))
            avail = get_cache_avail_rows()
            added = setdiff(all, avail)
            added[sel_row]
            # c(3, 6)
        }
        
        output$DT_cache = DT::renderDataTable({
            # cached_files = rvCache()
            # avail = setdiff(seq_len(nrow(cached_files)), rvCacheForCfg())
            avail = get_cache_avail_rows()
            DT::datatable(rvCache()[avail,], filter = "top", options = list(pageLength = 10, scrollX = T))
        })
        
        output$DT_newCfg = DT::renderDataTable({
            cached_files = rvCache()
            DT::datatable(cached_files[rvCacheForCfg(),], options = list(pageLength = 10, scrollX = T))
        })
        
        observeEvent(
            eventExpr = {
                input$btnAddFileCfg
            }, 
            handlerExpr = {
                toadd = input$DT_cache_rows_selected
                old_rv = rvCacheForCfg()
                rvCacheForCfg(sort(c(old_rv, sel_row_to_cache_row(toadd))))
            }
        )
        
        observeEvent(
            eventExpr = {
                input$btnRemoveFileCfg
            }, 
            handlerExpr = {
                rm = rm_sel_row_from_cache_row(input$DT_newCfg_rows_selected)
                print(rm)
                old_rv = rvCacheForCfg()
                message("old_rv: ", old_rv)
                message("rm: ", rm)
                rvCacheForCfg(sort(setdiff(old_rv, rm)))
            }
        )
        
        rvCfgTable = reactiveVal(make_cfg_table())
        rvLastSel = reactiveVal(NULL)
        
        
        observeEvent(
            eventExpr = {
                input$DT_configSelect_rows_selected
            }, 
            handlerExpr = {
                req(input$DT_configSelect_rows_selected)
                new_sel = input$DT_configSelect_rows_selected
                rvLastSel(new_sel)
                rvLastSel(new_sel)
            })
        
        output$DT_configSelect = DT::renderDataTable({
            DT::datatable(rvCfgTable(), rownames = FALSE,
                          # callback = cb1a,
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
        
        ### Config Create
        output$uiCfgColorBy = renderUI({
            tags$div(id = "uiCfgColorByDiv",
                     selectInput("selectColorBy", label = "Color By", choices = c("CELL", "MARK")),
                     gen_color_picker_ui(grps = LETTERS[1:5], brew_name = "Dark2", is_free_color = FALSE)
                     
            )   
        })
        
        
        
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
        
        output[["intBars"]] = renderPlot(width = 280, height = 280, {
            req(rSelectedCfgDesc())
            req(rvFeatures())
            input$redrawPlot
            ssvFeatureBars(rvFeatures(), bar_colors = rvColors()) + 
                guides(color = "none")
        })
        
        output$intEuler = renderPlot(width = 280, height = 280, {
            req(rSelectedCfgDesc())
            req(rvFeatures())
            input$redrawPlot
            ssvFeatureEuler(rvFeatures(), circle_colors = rvColors()) + 
                guides(color = "none", fill = "none")
        })
        
        output$intMemb = renderPlot(width = 280, height = 280, {
            req(rSelectedCfgDesc())
            req(rvFeatures())
            input$redrawPlot
            ssvFeatureBinaryHeatmap(rvFeatures(), raster_approximation = TRUE) + 
                guides(color = "none")
        })
        
        output$intVenn = renderPlot(width = 280, height = 280, {
            req(rSelectedCfgDesc())
            req(rvFeatures())
            input$redrawPlot
            ssvFeatureVenn(rvFeatures(), circle_colors = rvColors()) + 
                # scale_color_manual(values = safeBrew(3, pal = "Dark2")) +
                guides(color = "none", fill = "none")
        })
        
        output$intLineAgg = renderPlot(width = 280, height = 280, {
            req(rSelectedCfgDesc())
            req(rvFeatures())
            input$redrawPlot
            ssvSignalLineplotAgg(rvSignals(), spline_n = 5) + 
                scale_color_manual(values = rvColors()) +
                theme_cowplot() + guides(color = "none")
        })
        
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
            shinyjs::show(id = "debug")
            
            observeEvent(input$DT_configSelect_rows_selected,
                         {
                             showNotification(paste("configSelect selected:", paste(input$DT_configSelect_rows_selected, collapse = " ")))
                         })
            
            observeEvent(input$DT_configSelect_rows_all,
                         {
                             showNotification(paste("configSelect all:", paste(input$DT_configSelect_rows_all , collapse = " ")))
                         })
            
            observeEvent(input$DT_active_rows_selected,
                         {
                             showNotification(paste("active selected:", paste(input$DT_active_rows_selected, collapse = " ")))
                         })
            
            observeEvent(input$DT_active_rows_all,
                         {
                             showNotification(paste("active all:", paste(input$DT_active_rows_all , collapse = " ")))
                         })
            
            observeEvent(input$DT_cache_rows_selected,
                         {
                             showNotification(paste("cache selected:", paste(input$DT_cache_rows_selected, collapse = " ")))
                         })
            
            observeEvent(input$DT_cache_rows_all,
                         {
                             showNotification(paste("cache all:", paste(input$DT_cache_rows_all , collapse = " ")))
                         })
        }
        # changes starting tab
        # updateTabItems(session, "tabs", "intersect")
    }
)
