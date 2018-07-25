library(BiocFileCache)
setClass("ssv_config", list(
    bfc = "BiocFileCache", 
    bfc_id = "character", 
    color_var = "character", 
    colors = "character"
))
ssv_config = function(bfc, bfc_id, color_var, colors){
    obj = new(Class = "ssv_config", 
        bfc = bfc, 
        bfc_id = bfc_id, 
        color_var = color_var, 
        colors = colors)
    cfg_set_color_mapping(obj, colors)
}

setMethod("initialize", "ssv_config", 
          function(.Object, ...) {
              .Object <- callNextMethod()
              .Object
          }
)

bfcfilter = function(x, filters){
    filters = paste0("(?=.*", filters, ")")
    filters = paste0(filters, collapse = "")
    bfcinfo(x)[grepl(filters, bfcinfo(x)$rname, perl = TRUE),]
}

cfg_get_path = function(cfg, ftype = "."){
    if(!grepl("TYPE=", ftype)){
        ftype = paste0("TYPE=", ftype)
    }
    if(ftype != "."){
        sbfc = bfcfilter(cfg@bfc, c(ftype))        
    }else{
        sbfc = cfg@bfc
    }
    sbfc$rpath
}

cfg_get_color_groups = function(cfg){
    as.character(sort(unique(make_bfc_table(cfg@bfc)[id %in% cfg@bfc_id][[cfg@color_var]])))
}

color_mapping  = function(cfg, colMap = character()){
    grps = cfg_get_color_groups(cfg)
    if(length(grps) < length(colMap)){
        colMap = colMap[seq_along(grps)]
    }
    if(length(grps) > length(colMap)){
        nmiss = length(grps) - length(colMap)
        colMap = c(colMap, safeBrew(nmiss))
    }
    colMap = seqsetvis::col2hex(colMap)
    if(is.null(names(colMap))){
        names(colMap) = grps
    }
    colMap
}

cfg_set_color_mapping = function(cfg, colMap = character()){
    cfg@colors = color_mapping(cfg, colMap)
    cfg
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
        warning(qtype, " - not in availble TYPE")
        return(character())
    }
    if(!all(name_by %in% colnames(tab))){
        stop(paste(name_by, collapse = ", "), " - not in availble TYPE")
    }
    tab = tab[id %in% cfg@bfc_id & TYPE %in% qtype]
    paths = as.character(tab$fpath)
    nam = apply(
        as.data.frame(sapply(name_by, function(x)tab[[x]], simplify = FALSE))
        , 1, function(y)paste(y, collapse = " "))
    names(paths) = nam
    paths
    
}

