
library(shiny)

ui <- navbarPage(
  "Geography",

  navbarMenu("Tables",
    tabPanel("City", value = "city"),
    tabPanel("Country", value = "country"),
    tabPanel("Country Language", value = "country_language")
  ),

  tabPanel("Trivia", value = "trivia"),

  sidebarLayout(
    sidebarPanel(
      uiOutput("col_sel"),
      width = 3
    ),

    mainPanel(
      fluidRow(
        uiOutput("mk_row_button", inline = TRUE),
        uiOutput("edit_row_button", inline = TRUE),
        uiOutput("rm_row_button", inline = TRUE),
        uiOutput("reset_button", inline = TRUE)
      ),
      br(),
      fluidRow(
        DT::dataTableOutput("table")
      ),
      width = 9
    )
  ),
  theme = "bootstrap.css",
  id = "tab_name"
)
