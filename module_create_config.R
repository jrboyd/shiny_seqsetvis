server_create_config = function(dt_table, input, output, session){
    
    rvalsDataFiles = reactiveValues(
        table = dt_table,
        idx_in_cfg = integer(),
        idx_out_cfg = seq_len(nrow(dt_table))
    )
    
    output$DT_filesAvailable = DT::renderDataTable({
        DT::datatable(rvalsDataFiles$table[rvalsDataFiles$idx_out_cfg,], options = list(pageLength = 10, scrollX = T))
    })
    
    output$DT_filesNewCfg = DT::renderDataTable({
        DT::datatable(rvalsDataFiles$table[rvalsDataFiles$idx_in_cfg,], options = list(pageLength = 10, scrollX = T))
    })
    
    observeEvent(
        eventExpr = {
            input$btnAddFileCfg
        }, 
        handlerExpr = {
            #find table row ids of selected items in available, move from available to cfg
            idx_added = rvalsDataFiles$idx_out_cfg[input$DT_filesAvailable_rows_selected]
            rvalsDataFiles$idx_out_cfg = sort(setdiff(rvalsDataFiles$idx_out_cfg, idx_added))
            rvalsDataFiles$idx_in_cfg = sort(c(rvalsDataFiles$idx_in_cfg, idx_added))
        }
    )
    
    observeEvent(
        eventExpr = {
            input$btnRemoveFileCfg
        }, 
        handlerExpr = {
            #find table row ids of selected items in new cfg, move from cfg to available
            idx_removed = rvalsDataFiles$idx_in_cfg[input$DT_filesNewCfg_rows_selected]
            rvalsDataFiles$idx_out_cfg = sort(c(rvalsDataFiles$idx_out_cfg, idx_removed))
            rvalsDataFiles$idx_in_cfg = sort(setdiff(rvalsDataFiles$idx_in_cfg, idx_removed))
        }
    )
    
    output$uiCfgColorBy = renderUI({
        selectInput(inputId = "selectColorBy", label = "Color By", choices = c("CELL", "MARK"))
    })
    
    ### use table to move group items up and down
    rvColorsOrder = reactiveVal(data.frame(num = 1:10, let = letters[1:10]))
    rvColorsOrderSel = reactiveVal(1)
    
    output$DT_colorsOrder = DT::renderDataTable(
        {
            cached_files = rvalsDataFiles$table
            if(length(rvalsDataFiles$idx_in_cfg) > 0){
                grps = unique(cached_files[rvalsDataFiles$idx_in_cfg, ][[input$selectColorBy]])
            }
            
            isolate(DT::datatable(rvColorsOrder(), 
                                  options = list(processing = FALSE),
                                  selection = list(mode = "single", selected = 1, target = "row"),
                                  rownames = FALSE))
        }
    )
    
    proxy_colorsOrder = dataTableProxy('DT_colorsOrder')
    
    observeEvent(
        eventExpr = {
            input$btnMoveGrpUp
        }, 
        handlerExpr = {
            rs = input$DT_colorsOrder_rows_selected
            co = rvColorsOrder()
            o = seq_len(nrow(co))
            o[rs] = o[rs] - 1.5
            co = co[order(o), ]
            rvColorsOrder(co)
            DT::replaceData(proxy = proxy_colorsOrder, data = co, rownames = FALSE)
            DT::selectRows(proxy = proxy_colorsOrder, selected = max(input$DT_colorsOrder_rows_selected - 1, 1))
            # rvColorsOrder(co[order(o), ])
            # rvColorsOrderSel(max(input$DT_colorsOrder_rows_selected - 1, 1))
        }
    )
    
    observeEvent(
        eventExpr = {
            input$btnMoveGrpDown
        }, 
        handlerExpr = {
            rs = input$DT_colorsOrder_rows_selected
            co = rvColorsOrder()
            o = seq_len(nrow(co))
            o[rs] = o[rs] + 1.5
            
            co = co[order(o), ]
            rvColorsOrder(co)
            DT::replaceData(proxy = proxy_colorsOrder, data = co, rownames = FALSE)
            DT::selectRows(proxy = proxy_colorsOrder, selected = min(input$DT_colorsOrder_rows_selected + 1, nrow(co)))
        }
    )
    ###
    
    ### generate color pickers for every group item
    output$uiCfgColorPickers = renderUI({
        cached_files = rvalsDataFiles$table
        if(length(rvalsDataFiles$idx_in_cfg) < 1){
            return(tags$p("waiting for selection"))
        }
        grps = unique(cached_files[rvalsDataFiles$idx_in_cfg, ][[input$selectColorBy]])
        if(length(grps) < 1){
            return(tags$p("waiting for selection"))
        }
        # grps <<- grps
        # cached_files <<- cached_files
        # save.image()
        gen_color_picker_ui(grps = grps, brew_name = "Dark2", is_free_color = FALSE)
    })
    ###
}

ui_create_config = function(){
    tags$div(
        h3("Step 1 : Select Files"),
        (DT::dataTableOutput(outputId = "DT_filesAvailable")),
        fluidRow(
            actionButton("btnAddFileCfg", label = "+"),
            actionButton("btnRemoveFileCfg", label = "-")
        ),
        (DT::dataTableOutput(outputId = "DT_filesNewCfg")),
        h3("Step 2 : Configuration Settings"),
        tags$div(id = "uiCfgColorByDiv",
                 uiOutput("uiCfgColorBy"),
                 DT::dataTableOutput(outputId = "DT_colorsOrder"),
                 # uiOutput("uiCfgColorOrder"),
                 uiOutput("uiCfgColorPickers"),
                 fluidRow(
                     actionButton("btnMoveGrpDown", label = "+"),
                     actionButton("btnMoveGrpUp", label = "-")
                 )
        )
    )
}