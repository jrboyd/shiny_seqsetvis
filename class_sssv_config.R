library(BiocFileCache)
setClass("ssv_config", list(
    bfc = "BiocFileCache", 
    bfc_id = "character", 
    color_var = "character", 
    colors = "character"
))
ssv_config = function(bfc, bfc_id, color_var, colors){
    new(Class = "ssv_config", 
        bfc = bfc_data, 
        bfc_id = rid, 
        color_var = "CELL", 
        colors = safeBrew(3))
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

cfg_get_color_by


rid = bfcfilter(bfc_data, c("hg38", "CTCF", "MCF10"))$rid
my_cfg = new(Class = "ssv_config", bfc_id = rid, bfc = bfc_data, color_var = "CELL", colors = safeBrew(3))

ssv_config(bfc = bfc_data, bfc_id = rid, color_var = "CELL", colors = safeBrew(3))

cfg_get_path(my_cfg, ftype = "bigWig")
cfg_get_path(my_cfg, ftype = "narrowPeak")
