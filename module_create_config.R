ui_create_config = function(){
    tags$div(
        h3("Step 1 : Select Files"),
        tags$hr(),
        (DT::dataTableOutput(outputId = "DT_filesAvailable")),
        fluidRow(
            actionButton("btnAddFileCfg", label = "", 
                         icon = icon("plus", lib = "font-awesome", class = "myBtnSwitchFile")),
            actionButton("btnRemoveFileCfg", label = "", 
                         icon = icon("minus", lib = "font-awesome", class = "myBtnSwitchFile"))
        ),
        tags$p(),
        (DT::dataTableOutput(outputId = "DT_filesNewCfg")),
        h3("Step 2 : Set Colors"),
        tags$hr(),
        uiOutput("uiCfgColorBy"),
        uiOutput("uiCfgColorPickers"),
        h3("Step 3 : Order Groups"),
        tags$hr(),
        tags$div(id = "uiCfgColorByDiv",
                 DT::dataTableOutput(outputId = "DT_colorsOrder", width = "400px"),
                 # uiOutput("uiCfgColorOrder"),
                 
                 fluidRow(
                     actionButton("btnMoveGrpDown", label = "", 
                                  icon = icon("arrow-circle-down", lib = "font-awesome", class = "myBtnMoveArrow")),
                     actionButton("btnMoveGrpUp", label = "", 
                                  icon = icon("arrow-circle-up", lib = "font-awesome", class = "myBtnMoveArrow"))
                 )
        ),
        h3("Step 4 : Name and Finish"),
        tags$hr(),
        textInput("txtCfgDescription", label = "Description", width = "600px"),
        actionButton("btnCfgAdd", label = "Add New Config"),
        htmlOutput("htmlCfgValidity")
    )
}

server_create_config = function(rv_valid_cfg, rv_files_table, rv_cfg_table, input, output, session){
    
    rvalsDataFiles = reactiveValues(
        table = NULL,
        idx_in_cfg = NULL,
        idx_out_cfg = NULL
    )
    
    observe({
        rvalsDataFiles$table = rv_files_table()
        rvalsDataFiles$idx_in_cfg = integer()
        rvalsDataFiles$idx_out_cfg = seq_len(nrow(rv_files_table()))
    })
    
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
    
    
    ### maintain active groups based on selectColorBy and files
    rvActiveGroups = reactiveVal()
    
    observe({
        cached_files = rvalsDataFiles$table
        if(length(rvalsDataFiles$idx_in_cfg) < 1){
            return()
        }
        grps = unique(cached_files[rvalsDataFiles$idx_in_cfg, ][[input$selectColorBy]])
        rvActiveGroups(grps)
    })
    ###
    ### generate color pickers for every group item
    output$uiCfgColorPickers = renderUI({
        grps = rvActiveGroups()
        if(length(grps) < 1){
            return(tags$p("waiting for selection", class = "myInvalidCfg"))
        }
        gen_color_picker_ui(grps = grps, brew_name = "Dark2", is_free_color = FALSE)
    })
    
    rvPickedColors = reactiveVal()
    
    observe({
        grps = rvActiveGroups()
        ids = paste0("color_", grps)
        cols = sapply(ids, function(id){
            input[[id]]
        })
        names(cols) = grps
        sapply(cols, req)
        rvPickedColors(cols)
        
    })
    ###
    ### use table to move group items up and down
    # rvColorsOrder = reactiveVal(data.frame(num = 1:10, let = letters[1:10]))
    rvColorsOrder = reactiveVal(NULL)
    rvColorsOrderSel = reactiveVal(1)
    rvColorsRdy = reactiveVal(FALSE)
    
    output$DT_colorsOrder = DT::renderDataTable({
        req(rvalsDataFiles$table)
        req(rvalsDataFiles$idx_in_cfg)
        req(rvPickedColors())
        
        cached_files = rvalsDataFiles$table
        if(length(rvalsDataFiles$idx_in_cfg) > 0){
            grps = unique(cached_files[rvalsDataFiles$idx_in_cfg, ][[input$selectColorBy]])
        }
        cols = rvPickedColors()
        df = data.frame(id = seq_along(cols), name = names(cols), color = cols)
        rvColorsOrder(df)
        isolate(showNotification(as.character(rvColorsRdy())))
        isolate({
            if(!rvColorsRdy()){
                rvColorsRdy(TRUE)
                DT::datatable(rvColorsOrder(),
                              options = list(processing = FALSE),
                              selection = list(mode = "single", selected = 1, target = "row"),
                              rownames = FALSE) %>%
                    formatStyle(., 
                                columns = 2, 
                                target = "row", 
                                color = styleEqual(names(cols), cols), 
                                fontWeight = "bold")
                
            }
        })
    })
    
    proxy_colorsOrder = dataTableProxy('DT_colorsOrder')
    
    observeEvent(#updates DT_colorsOrder with move up
        eventExpr = {
            input$btnMoveGrpUp
        }, 
        handlerExpr = {
            if(rvColorsRdy()){
                rs = input$DT_colorsOrder_rows_selected
                co = rvColorsOrder()
                o = seq_len(nrow(co))
                o[rs] = o[rs] - 1.5
                co = co[order(o), ]
                rvColorsOrder(co)
                DT::replaceData(proxy = proxy_colorsOrder, data = co, rownames = FALSE)
                DT::selectRows(proxy = proxy_colorsOrder, selected = max(input$DT_colorsOrder_rows_selected - 1, 1))
            }
        }
    )
    
    observeEvent(#updates DT_colorsOrder with move down
        eventExpr = {
            input$btnMoveGrpDown
        }, 
        handlerExpr = {
            if(rvColorsRdy()){
                rs = input$DT_colorsOrder_rows_selected
                co = rvColorsOrder()
                o = seq_len(nrow(co))
                o[rs] = o[rs] + 1.5
                
                co = co[order(o), ]
                rvColorsOrder(co)
                DT::replaceData(proxy = proxy_colorsOrder, data = co, rownames = FALSE)
                DT::selectRows(proxy = proxy_colorsOrder, selected = min(input$DT_colorsOrder_rows_selected + 1, nrow(co)))
            }
        }
    )
    
    observe({#updates DT_colorsOrder with color choices
        req(rvActiveGroups())
        req(rvPickedColors())
        if(rvColorsRdy()){
            cols = rvPickedColors()
            # df = data.frame(ids = names(cols), colors = cols)
            df = data.frame(id = seq_along(cols), name = names(cols), color = cols)
            rvColorsOrder(df)
            DT::replaceData(proxy = proxy_colorsOrder, data = df, rownames = FALSE)
            DT::selectRows(proxy = proxy_colorsOrder, selected = 1)
        }
        # rvColorsOrder(data.frame(ids = names(cols), colors = cols))
    })
    ###
    ### Config Finalize
    # textInput("txtCfgDescription", label = "Description", width = "600px"),
    # actionButton("btnCfgAdd", label = "Add New Config"),
    # htmlOutput("htmlCfgValidity")
    
    rvPotentialCfg = reactiveVal()
    
    observeEvent(#monitor config for validity
        #validity criteria 
        #1) txtCfgDescription is >= 6 character and unique
        eventExpr = {
            input$txtCfgDescription
        }, 
        handlerExpr = {
            output$htmlCfgValidity = renderUI({
                req(rvalsDataFiles$table)
                req(input$DT_configSelect_rows_current)
                desc_len = nchar(input$txtCfgDescription)
                existing_desc = rv_cfg_table()[,"Description"]
                desc_notUnique = tolower(input$txtCfgDescription) %in% tolower(existing_desc)
                if(length(rvalsDataFiles$idx_in_cfg) < 1){#check for any files
                    shinyjs::disable(id = "btnCfgAdd")
                    tags$p("No files have been selected.", class = "myInvalidCfg")
                }else{
                    req(rvPickedColors())
                    rid = bfcinfo(bfc_data)$rid[rvalsDataFiles$idx_in_cfg]
                    new_cfg = ssv_config(bfc = bfc_data, 
                                         bfc_id = rid, 
                                         color_var = input$selectColorBy, 
                                         colors = rvPickedColors())
                    # browser()
                    new_bw = extract_cfg_files(cfg = new_cfg, name_by = input$selectColorBy, qtype = "bigWig")
                    new_np = extract_cfg_files(cfg = new_cfg, name_by = input$selectColorBy, qtype = "narrowPeak")
                    if(length(new_np) == 0){
                        shinyjs::disable(id = "btnCfgAdd")
                        tags$p("At least 1 Feature type file (.narrowPeak) is required.", class = "myInvalidCfg")
                    }else if(any(duplicated(names(new_bw)))){#each file of type (feature or signal) should be unique per color group
                        shinyjs::disable(id = "btnCfgAdd")
                        tags$p("Some bigWig group names are duplicated.", class = "myInvalidCfg")
                    }else if(any(duplicated(names(new_np)))){#each file of type (feature or signal) should be unique per color group
                        shinyjs::disable(id = "btnCfgAdd")
                        tags$p("Some narrowPeak group names are duplicated.", class = "myInvalidCfg")
                    }else if(desc_len < 6){#check desc length
                        shinyjs::disable(id = "btnCfgAdd")
                        tags$p("Description is too short, minimum of 6 characters required.", class = "myInvalidCfg")
                    }else if(desc_notUnique){#check desc unique
                        shinyjs::disable(id = "btnCfgAdd")
                        tags$p("Description name conflicts with exisiting, must be unique.", class = "myInvalidCfg")
                    }else{#finally, cfg looks valid
                        shinyjs::enable(id = "btnCfgAdd")
                        rvPotentialCfg(new_cfg)
                        tags$p("Configuration is valid.", class = "myValidCfg")   
                    }
                    
                }
                
            })
        })
    
    observeEvent(
        eventExpr = {
            input$btnCfgAdd
        }, 
        handlerExpr = {
            showNotification("click")
            req(rvPotentialCfg())
            rv_valid_cfg(rvPotentialCfg())
        })
    ###
}

