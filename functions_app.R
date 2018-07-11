load_libs_withProgress = function(libs, session){
    tmp.sink = suppressPackageStartupMessages({
        # libs = c(
        #     "data.table",
        #     "rtracklayer",
        #     "GenomicRanges",
        #     "ggplot2",
        #     "cowplot",
        #     "seqsetvis"
        # )
        withProgress(session = session, 
                     message = 'Loading libraries', 
                     value = 0, 
                     max = length(libs),  
                     expr = {
                         for(i in seq_along(libs)){
                             incProgress(session = session,
                                         amount = 1, 
                                         message = libs[i],
                                         detail = paste0("(", i, "/", length(libs), ")"))
                             library(libs[i], character.only = TRUE)
                         }
                         
                         
                     })
    })
}

shiny_ssvPlotBox = function(box_title = "Main Plot", id = "mainPlot", collapsed = TRUE){
    id = as.character(id)
    mybox = box(title = box_title,
                collapsible = TRUE, collapsed = collapsed, solidHeader = TRUE, status = "primary", 
                fluidRow(
                    column(width = 6,
                           withSpinner(plotOutput(id, width = "280px", height = "280px"), custom.css = F)),
                    column(width = 5, 
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
    mybox
}

htmltools::save_html(fluidRow(
    # tags$div(
    shiny_ssvPlotBox("Bars", "intBars", collapsed = FALSE),
    shiny_ssvPlotBox("Euler", "intEuler")
    
), file = "tmp_box_noconv.html")

make_bfc_table = function(bfc, displayOnly = FALSE){
    rnamel = strsplit(bfcinfo(bfc)$rname, ",")
    names(rnamel) = bfcinfo(bfc)$rid
    rnamel = lapply(rnamel, function(x)data.table(attrib = x))
    bfc_dt = rbindlist(rnamel, use.names = TRUE, idcol = "id")
    bfc_dt[, c("parameter", "value") := tstrsplit(attrib, "=")]
    bfc_dt$attrib = NULL
    bfc_dt = dcast(bfc_dt, id ~ parameter)
    o = c(REQ_CN, setdiff(colnames(bfc_dt), REQ_CN))
    bfc_dt = bfc_dt[, o, with = FALSE]
    bfc_dt$fpath = sapply(bfc_dt$id, function(id){
        bfcpath(bfc, id)[1]
    })
    if(displayOnly){
        bfc_dt = bfc_dt[, 
                        toupper(colnames(bfc_dt)) == colnames(bfc_dt), 
                        with = FALSE]
    }
    for(i in seq_len(ncol(bfc_dt))){
        cn = colnames(bfc_dt)[i]
        bfc_dt[[cn]] = factor(bfc_dt[[cn]])
    }
    if(!displayOnly){
        setkey(bfc_dt, id)
    }
    bfc_dt
}

make_cfg_detail = function(rname){
    # path = bfcrpath(bfc_cfg, paste0("^", rname, "$"))
    # cfg = readRDS(path)
    cfg = cfgs_list[[rname]]
    cfg_by_type(cfg, rname)
}

cfg_by_type = function(cfg, desc){
    cfg_tab = make_bfc_table(bfc_data)[.(cfg@bfc_id)]
    tokeep = colnames(cfg_tab)
    tokeep = tokeep[toupper(tokeep) == tokeep]
    tokeep = setdiff(tokeep, c("GENOME", "TYPE"))
    types = unique(cfg_tab$TYPE)
    
    typ_l = lapply(types, function(typ){
        cfg_tab[TYPE == typ, tokeep, with = FALSE]
    })
    names(typ_l) = types
    
    # lapply(seq_along(typ_l), function(i){
    #     cat(
    #     as.character(tags$h3(paste0(names(typ_l)[i], ":")))
    #     )
    # })
    cols = cfg@colors
    cols_html = paste(sapply(seq_along(cols), function(i){
        as.character(tags$span(names(cols)[i], style = paste0("color:", cols[i])))
    }), collapse = ", ")
    cols_html = HTML(cols_html)
    out = tags$div(
        tags$h3(paste("Description:", desc)),
        tags$h3(paste("Genome:", unique(as.character(cfg_tab$GENOME)))),
        tags$h3("Colors:", cols_html)
    )
    out
    ###TODO BUILD UP HTML OUTPUT
    
    # out = append(out, typ_l)
    # 
    # col_l = as.list(cfg@colors)
}

#gathers some info about cfg contents
make_cfg_table = function(){
    cfg_lines = t(sapply(bfcinfo(bfc_cfg)$rid, function(my_rid){
        rname = subset(bfcinfo(bfc_cfg), rid == my_rid)$rname
        cfg = readRDS(bfcpath(bfc_cfg, my_rid))
        
        tbl_data = make_bfc_table(bfc_data)[.(cfg@bfc_id)]
        todo = colnames(tbl_data)[colnames(tbl_data) == toupper(colnames(tbl_data))]
        var_counts = sapply(todo, function(cn){
            tab = table(tbl_data[[cn]])
            paste(paste(names(tab), tab, sep = "="), collapse = ", ")
        })
        cbind(rname, rbind(var_counts))
    }))
    
    my_rid = bfcinfo(bfc_cfg)$rid[1]
    cfg = readRDS(bfcpath(bfc_cfg, my_rid))
    tbl_data = make_bfc_table(bfc_data)[.(cfg@bfc_id)]
    todo = colnames(tbl_data)[colnames(tbl_data) == toupper(colnames(tbl_data))]
    
    colnames(cfg_lines) = c("Description", todo)
    cfg_lines
}

#' Extract encoded bigwigs from config
#'
#' @param cfg valid object of class ssv_config
#' @param name_by refers to rname encode attribute - CELL, MARK, etc
#' @param qtype file type to extract
#'
#' @return named character vector with paths to bigwigs
#' @export
#'
#' @examples
extract_cfg_files = function(cfg, name_by = c("CELL", "MARK"), qtype = "bigWig"){
    tab = make_bfc_table(bfc = cfg@bfc)
    if(!qtype %in% tab$TYPE){
        stop(qtype, " - not in availble TYPE")
    }
    if(!all(name_by %in% colnames(tab))){
        stop(paste(name_by, collapse = ", "), " - not in availble TYPE")
    }
    tab = tab[id %in% cfg@bfc_id & TYPE %in% qtype]
    paths = as.character(tab$fpath)
    nam = apply(
        sapply(name_by, function(x)tab[[x]])
        , 1, function(y)paste(y, collapse = " "))
    names(paths) = nam
    paths
    
}
