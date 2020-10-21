
library(shiny)

ui <- fluidPage(
  title = "Geography database",
  br(),
  sidebarLayout(
    sidebarPanel(
      tags$b("Sidebar Panel")
    ),

    mainPanel(
      tags$b("Main Panel")
    )
  )
)
