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
        cached_files = rvalsDataFiles$table
        grps = cached_files[rvalsDataFiles$idx_in_cfg, "CELL"]
        tags$div(id = "uiCfgColorByDiv",
                 selectInput("selectColorBy", label = "Color By", choices = c("CELL", "MARK")),
                 gen_color_picker_ui(grps = grps, brew_name = "Dark2", is_free_color = FALSE)
                 
        )   
    })
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
        uiOutput("uiCfgColorBy")
    )
}