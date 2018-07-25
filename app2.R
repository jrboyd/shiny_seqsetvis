shinyApp(
    ui = fluidPage(tags$h2("testing stuff"), 
                   sliderInput("slider1", "slider1", 0, 10, 1),
                   sliderInput("slider2", "slider2", 0, 10, 1),
                   textOutput("textOut1")
    ),
    server = function(input, output, session) {
        observeEvent(
            eventExpr = {
                input[[paste0("slider", 1)]]
                input[[paste0("slider", 2)]]
                for(i in 1:2){
                    input[[paste0("slider", i)]]
                }
            }, 
            handlerExpr = {
                showNotification(
                    paste("observeEvent",
                          input[[paste0("slider", 1)]],
                          input[[paste0("slider", 2)]]))
            }
        )
        
        observe(
            {
                # i = 1
                # j = 2
                # showNotification(
                #     paste("observe",
                #           input[[paste0("slider", i)]],
                #           input[[paste0("slider", j)]]))
                vals = sapply(1:2, function(i){
                    input[[paste0("slider", i)]]
                })
                showNotification(
                    paste("observe",
                          paste(vals, collapse = " ")
                    ))
            }
        )
        
        output$textOut1 = renderText({
            i = 1
            input[[paste0("slider", i)]]
        })
    }
)
