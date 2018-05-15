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

shiny_ssvPlotBox = function(box_title = "Main Plot", id = 1, plot_id = "plotTest", collapsed = TRUE){
    id = as.character(id)
    mybox = box(title = box_title, collapsible = TRUE, collapsed = collapsed, solidHeader = TRUE, status = "primary", 
                fluidRow(
                    column(width = 6,
                           withSpinner(plotOutput(plot_id, width = "280px", height = "280px"))),
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
    # mybox = as.character(mybox)
    # to_ins =  'data-widget="collapse"'
    # mybox = sub(to_ins, "", mybox)
    # mybox = sub('class="box-header"', paste('class="box-header"', to_ins), mybox)
    # HTML(mybox)
    mybox
}