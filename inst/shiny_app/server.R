
library(shiny)
library(geography)

load_data()

## Functions:

columns <- function (input, table) {

  if (!(all(input$col_sel %in% names(geo_data[[table]])))) {
    columns <- NULL
  } else {
    columns <- input$col_sel
  }
}

server <- function(input, output, session) {

  ## Reactive values:

  flag <- reactiveValues(update = 0)

  ## Reactive expressions:

  tab <- reactive(input$tab_name)

  ## Custom user interface:

  output$col_sel <- renderUI({
    if (tab() %in% ls(geo_data)) {
      checkboxGroupInput(
        inputId = "col_sel",
        label = "Select Columns",
        choices = names(geo_data[[tab()]]),
        selected = names(geo_data[[tab()]])[c(1, 2, 3, 4)]
      )
    } else if (tab() == "trivia") {
      #
    }
  })

  output$reset_button <- renderUI({
    if (tab() %in% ls(geo_data)) {
      actionButton("reset_button", "Reset Tables", icon("redo"))
    }
  })

  output$mk_row_button <- renderUI({
    if (tab() %in% ls(geo_data)) {
      actionButton("mk_row_button", "Add", icon("plus"))
    }
  })

  output$edit_row_button <- renderUI({
    if (tab() %in% ls(geo_data)) {
      actionButton("edit_row_button", "Edit", icon("edit"))
    }
  })

  output$rm_row_button <- renderUI({
    if (tab() %in% ls(geo_data)) {
      actionButton("rm_row_button", "Remove", icon("trash-alt"))
    }
  })

  ## Output:

  output$table <- DT::renderDataTable({
    flag$update

    if (tab() %in% ls(geo_data)) {
      DT::datatable(
        geo_data[[tab()]][columns(input, tab())],
        options = list(
          scrollX = TRUE,
          pageLength = 15,
          lengthChange = FALSE
        ),
        editable = FALSE,
        rownames = FALSE,
        class = "display white-space: nowrap"
      )
    } else if (tab() == "trivia") {
      #
    }
  })

  ## Handle events:

  observeEvent(
    input$reset_button,
    {
      load_data()
      flag$update <- flag$update + 1
    },
    ignoreInit = TRUE
  )

  rm_row_ui(
    "rm_row",
    reactive(input$rm_row_button),
    reactive(input$table_rows_selected)
  )

  update_rm <- rm_row_server(
    "rm_row",
    reactive(input$rm_row_button),
    reactive(input$table_rows_selected),
    tab, reactive(flag$update)
  )

  observe(flag$update <- update_rm())

  edit_row_ui(
    "edit_row",
    reactive(input$edit_row_button),
    reactive(input$table_rows_selected)
  )

  update_edit <- edit_row_server(
    "edit_row",
    reactive(input$edit_row_button),
    reactive(input$table_rows_selected),
    tab, reactive(flag$update)
  )

  observe(flag$update <- update_edit())

  mk_row_ui(
    "mk_row",
    reactive(input$mk_row_button),
    reactive(input$table_rows_selected)
  )

  update_mk <- mk_row_server(
    "mk_row",
    reactive(input$mk_row_button),
    reactive(input$table_rows_selected),
    tab, reactive(flag$update)
  )

  observe(flag$update <- update_mk())

}
