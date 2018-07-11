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

