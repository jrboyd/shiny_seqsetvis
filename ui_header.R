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