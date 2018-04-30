#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

header = dashboardHeader(
    
    dropdownMenu(
        type = "notifications", 
        icon = icon("question-circle"),
        badgeStatus = NULL,
        headerText = "See also:",
        
        notificationItem("shiny", icon = icon("file"),
                         href = "http://shiny.rstudio.com/"),
        notificationItem("shinydashboard", icon = icon("file"),
                         href = "https://rstudio.github.io/shinydashboard/")
    ),
    title = "seqsetvis"
)
body <- dashboardBody(
    fluidRow(
        tabBox(
            title = "First tabBox",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabset1", height = "250px",
            tabPanel("Tab1", "First tab content"),
            tabPanel("Tab2", "Tab content 2")
        ),
        tabBox(
            side = "right", height = "250px",
            selected = "Tab3",
            tabPanel("Tab1", "Tab content 1"),
            tabPanel("Tab2", "Tab content 2"),
            tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
        )
    ),
    fluidRow(
        tabBox(
            # Title can include an icon
            title = tagList(shiny::icon("gear"), "tabBox status"),
            tabPanel("Tab1",
                     "Currently selected tab from first box:",
                     verbatimTextOutput("tabset1Selected")
            ),
            tabPanel("Tab2", "Tab content 2")
        )
    )
)

sidebar = dashboardSidebar(
    sidebarMenuOutput("menu")
)

shinyApp(
    ui = dashboardPage(
        header,
        sidebar,
        body
    ),
    server = function(input, output) {
        # The currently selected tab from the first box
        output$tabset1Selected <- renderText({
            input$tabset1
        })
        
        output$menu <- renderMenu({
            sidebarMenu(
                menuItem("load", icon = icon("file-o", lib = "font-awesome"), badgeLabel = "!", badgeColor = "red"),
                menuItem("intersect", icon = icon("bar-chart", lib = "font-awesome"), badgeLabel = "!", badgeColor = "red"),
                menuItem("inspect", icon = icon("area-chart", lib = "font-awesome"), badgeLabel = "!", badgeColor = "red")
            )
        })
    }
)