library(BiocFileCache)
library(seqsetvis)
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
my_cfg = new(Class = "ssv_config", bfc_id = rid, bfc = bfc_data, color_var = "CELL", colors = safeBrew(3))

ssv_config(bfc = bfc_data, bfc_id = rid, color_var = "CELL", colors = safeBrew(3))

cfg_get_path(my_cfg, ftype = "bigWig")
cfg_get_path(my_cfg, ftype = "narrowPeak")

