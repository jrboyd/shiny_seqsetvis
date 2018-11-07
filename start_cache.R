library(BiocFileCache)
library(data.table)
# library(seqsetvis)
source("class_ssv_config.R")
source("functions_app.R")
bfc_data = BiocFileCache(".cache_data")
urls = seqsetvis::CTCF_in_10a_bigWig_urls
url_df = as.data.frame(matrix(unlist(strsplit(names(urls), "_")), ncol = 2, byrow = TRUE))
colnames(url_df) = c("CELL", "MARK")
# url_df$URL = CTCF_in_10a_bigWig_urls
url_df$TYPE = "bigWig"
url_df$GENOME = "hg38"

#GENOME and TYPE are required
REQ_CN = c("GENOME", "TYPE")
if(!all(REQ_CN %in% colnames(url_df))){
    stop("all of (", paste(REQ_CN, collapse = " "), ") must be in colnames of url_df")
}
todo = c(REQ_CN, setdiff(colnames(url_df), REQ_CN))
RNAMES = apply(    
    sapply(todo, function(td){
        k = which(colnames(url_df) == td)
        if(length(k) == 0){
            paste(td, "NA", sep = "=")
        }else{
            paste(td, url_df[,k], sep = "=")
        }
        
    }), 1, function(x)paste(x, collapse = ",")
)

cache_df = data.frame(RNAMES, urls, stringsAsFactors = FALSE)
for(i in seq_len(nrow(cache_df))){
   bfcrpath(bfc_data, cache_df$urls[i], rnames = cache_df$RNAMES[i]) 
}
###
urls = seqsetvis::CTCF_in_10a_narrowPeak_urls
url_df = as.data.frame(matrix(unlist(strsplit(names(urls), "_")), ncol = 2, byrow = TRUE))
colnames(url_df) = c("CELL", "MARK")
# url_df$URL = CTCF_in_10a_bigWig_urls
url_df$TYPE = "narrowPeak"
url_df$GENOME = "hg38"

#GENOME and TYPE are required
REQ_CN = c("GENOME", "TYPE")
if(!all(REQ_CN %in% colnames(url_df))){
    stop("all of (", paste(REQ_CN, collapse = " "), ") must be in colnames of url_df")
}
todo = c(REQ_CN, setdiff(colnames(url_df), REQ_CN))
RNAMES = apply(    
    sapply(todo, function(td){
        k = which(colnames(url_df) == td)
        if(length(k) == 0){
            paste(td, "NA", sep = "=")
        }else{
            paste(td, url_df[,k], sep = "=")
        }
        
    }), 1, function(x)paste(x, collapse = ",")
)

cache_df = data.frame(RNAMES, urls, stringsAsFactors = FALSE)
for(i in seq_len(nrow(cache_df))){
    bfcrpath(bfc_data, cache_df$urls[i], rnames = cache_df$RNAMES[i]) 
}

rid = bfcfilter(bfc_data, c("hg38", "CTCF", "MCF10"))$rid

my_cfg = ssv_config(bfc = bfc_data, bfc_id = rid, color_var = "CELL", colors = seqsetvis::safeBrew(3))

cfg_get_path(my_cfg, ftype = "bigWig")
cfg_get_path(my_cfg, ftype = "narrowPeak")

var1_cfg = my_cfg

var2_rid = c(bfcfilter(bfc_data, c("hg38", "CTCF", "MCF10AT1"))$rid,
        bfcfilter(bfc_data, c("hg38", "CTCF", "MCF10CA1"))$rid)
var2_cfg = ssv_config(bfc = bfc_data, bfc_id = var2_rid, color_var = "CELL", colors = seqsetvis::safeBrew(3)[2:3])
var3_cfg = ssv_config(bfc = bfc_data, bfc_id = rid, color_var = "CELL", colors = seqsetvis::safeBrew(3, pal = "set1"))

# file.remove(dir(".cache_configs/", full.names = TRUE))
# file.remove(".cache_configs/")


bfc_cfg = BiocFileCache(".cache_configs")
cfgs = list("AF 10a progression CTCF" = var1_cfg, "AF at1 and ca1 CTCF" = var2_cfg, "AF 10a progression CTCF-recolor" = var3_cfg)
for(i in seq_along(cfgs)){
    rid = names(cfgs)[i]
    if(nrow(bfcquery(bfc_cfg, query = rid)) < 1){
        fpath = bfcnew(bfc_cfg, rid)
        saveRDS(cfgs[[i]], file = fpath)
    }
}


cfgs_rname = bfcinfo(bfc_cfg)$rname
cfgs_list = lapply(cfgs_rname, function(rname){
    print(rname)
    readRDS(bfcrpath(bfc_cfg, rnames = rname))    
})
names(cfgs_list) = cfgs_rname

