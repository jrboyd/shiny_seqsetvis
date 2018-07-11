bed_path = "~/ShinyApps/shiny_peak_data/beds/"
names(bed_path) = "intersectR"

user_roots = dir("/slipstream/home/", full.names = T) %>% paste0(. , "/ShinyData")
user_roots = subset(user_roots, dir.exists(user_roots))
names(user_roots) = dirname(user_roots) %>% basename()
qcframework_load <<- dir("/slipstream/galaxy/uploads/working/qc_framework", pattern = "^output", full.names = T)

names(qcframework_load) <- basename(qcframework_load)
roots_load_set = c(bed_path, user_roots, qcframework_load)
roots_load_set = roots_load_set[dir.exists(roots_load_set)]
# names(roots_load_set) <- basename(roots_load_set)
roots_load_bw <<- c("/slipstream/galaxy/production/galaxy-dist/static/UCSCtracks/", 
                    qcframework_load)
names(roots_load_bw) <- basename(roots_load_bw)
roots_load_bw = c(user_roots, roots_load_bw)
roots_load_bw = roots_load_bw[dir.exists(roots_load_bw)]

roots_save_set = c(bed_path, user_roots)
roots_save_set = roots_save_set[dir.exists(roots_save_set)]

shinyFiles2load = function(shinyF, roots){
    root_path = roots[shinyF$root]
    rel_path = paste0(unlist(shinyF$files), collapse = "/")
    file_path = paste0(root_path, "/", rel_path)
    return(file_path)
}

shinyFiles2save = function(shinyF, roots){
    root_path = roots[shinyF$root]
    rel_path = paste0(unlist(shinyF$name), collapse = "/")
    file_path = paste0(root_path, "/", rel_path)
    return(file_path)
}

appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"
btnCSS <- "
.my_plotbox .btn-box-tool:hover {
background: transparent !important;
border-style: none !important;
}
.my_plotbox .btn-box-tool:active {
background: transparent !important;
border-style: none !important;
}
.my_plotbox .box-header:hover {
background: #606c84 !important;
cursor: pointer !important;
}
.my_plotbox .box-header:active {
background: #aaaaaa !important;
}
.my_plotbox .box-tools:focus {
outline: none !important;
}
.my_plotbox .fa:focus {
outline: none !important;
}
.my_plotbox .btn-box-tool:active:focus {
outline: none !important;
}
"

boxCSS = "
.my_plotbox .loader, 
.my_plotbox .loader:before, 
.my_plotbox .loader:after {
height: 300px !important;
color: red !important;
background: red !important;
}
"
boxCSS = ""

tblCSS = "
.td:active {
user-select: none !important;
}
"