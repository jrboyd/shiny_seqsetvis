
bfc_data = BiocFileCache(".cache_data")
urls = CTCF_in_10a_bigWig_urls
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
urls = CTCF_in_10a_narrowPeak_urls
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

library(data.table)
rnamel = strsplit(bfcinfo(bfc_data)$rname, ",")
names(rnamel) = bfcinfo(bfc_data)$rid
rnamel = lapply(rnamel, function(x)data.table(attrib = x))
bfc_dt = rbindlist(rnamel, use.names = TRUE, idcol = "id")
bfc_dt[, c("parameter", "value") := tstrsplit(attrib, "=")]
bfc_dt$attrib = NULL
bfc_dt = dcast(bfc_dt, id ~ parameter)
o = c(REQ_CN, setdiff(colnames(bfc_dt), REQ_CN))
bfc_dt = bfc_dt[, o, with = FALSE]
bfc_dt$fpath = sapply(bfc_dt$id, function(id){
    bfcpath(bfc_data, id)[1]
})

bfc_dt_disp = bfc_dt[, 
                   toupper(colnames(bfc_dt)) == colnames(bfc_dt), 
                   with = FALSE]
for(i in seq_len(ncol(bfc_dt_disp))){
    cn = colnames(bfc_dt_disp)[i]
    bfc_dt_disp[[cn]] = factor(bfc_dt_disp[[cn]])
}

