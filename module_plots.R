server_plots = function(get_colors, get_features, get_signals, input, output, session){
    
    output[["intBars"]] = renderPlot(width = 280, height = 280, {
        # req(rSelectedCfgDesc())
        req(get_colors())
        req(get_features())
        input$redrawPlot
        ssvFeatureBars(get_features(), bar_colors = get_colors()) + 
            guides(color = "none")
    })
    
    output$intEuler = renderPlot(width = 280, height = 280, {
        # req(rSelectedCfgDesc())
        req(get_colors())
        req(get_features())
        input$redrawPlot
        ssvFeatureEuler(get_features(), circle_colors = get_colors()) + 
            guides(color = "none", fill = "none")
    })
    
    output$intMemb = renderPlot(width = 280, height = 280, {
        # req(rSelectedCfgDesc())
        req(get_colors())
        req(get_features())
        input$redrawPlot
        ssvFeatureBinaryHeatmap(get_features(), raster_approximation = TRUE) + 
            guides(color = "none")
    })
    
    output$intVenn = renderPlot(width = 280, height = 280, {
        # req(rSelectedCfgDesc())
        req(get_colors())
        req(get_features())
        input$redrawPlot
        ssvFeatureVenn(get_features(), circle_colors = get_colors()) + 
            # scale_color_manual(values = safeBrew(3, pal = "Dark2")) +
            guides(color = "none", fill = "none")
    })
    
    output$intLineAgg = renderPlot(width = 280, height = 280, {
        # req(rSelectedCfgDesc())
        req(get_colors())
        req(get_signals())
        input$redrawPlot
        ssvSignalLineplotAgg(get_signals(), spline_n = 5) + 
            scale_color_manual(values = get_colors()) +
            theme_cowplot() + guides(color = "none")
    })
}

ui_plots_features = function(){
    fluidRow(
        shiny_ssvPlotBox("Bars", "intBars", collapsed = FALSE),
        shiny_ssvPlotBox("Euler", "intEuler"),
        shiny_ssvPlotBox("Membership Map", "intMemb"),
        shiny_ssvPlotBox("Venn", "intVenn")
    )
}

ui_plots_signals = function(){
    fluidRow(
        shiny_ssvPlotBox("Line - aggregated", "intLineAgg", collapsed = FALSE)       
    )
}