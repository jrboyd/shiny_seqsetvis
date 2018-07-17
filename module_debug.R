server_debug = function(input, output, session){
    shinyjs::show(id = "debug")
    
    observeEvent(input$DT_configSelect_rows_selected,
                 {
                     showNotification(paste("configSelect selected:", paste(input$DT_configSelect_rows_selected, collapse = " ")))
                 })
    
    observeEvent(input$DT_configSelect_rows_all,
                 {
                     showNotification(paste("configSelect all:", paste(input$DT_configSelect_rows_all , collapse = " ")))
                 })
    
    observeEvent(input$DT_active_rows_selected,
                 {
                     showNotification(paste("active selected:", paste(input$DT_active_rows_selected, collapse = " ")))
                 })
    
    observeEvent(input$DT_active_rows_all,
                 {
                     showNotification(paste("active all:", paste(input$DT_active_rows_all , collapse = " ")))
                 })
    
    observeEvent(input$DT_filesAvailable_rows_selected,
                 {
                     showNotification(paste("cache selected:", paste(input$DT_filesAvailable_rows_selected, collapse = " ")))
                 })
    
    observeEvent(input$DT_filesAvailable_rows_all,
                 {
                     showNotification(paste("cache all:", paste(input$DT_filesAvailable_rows_all , collapse = " ")))
                 })
}

ui_debug = function(){
    div(id = "debug",
        p("debug out", id = "debugOut"),
        p("debug details", id = "debugDetails")
    )
}