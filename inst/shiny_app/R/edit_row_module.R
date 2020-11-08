
edit_row_ui <- function (ns_name, edit_row_button, table_rows_selected) {
  ns <- NS(ns_name)

  observeEvent(
    edit_row_button(),
    {
      showModal(
        if (length(table_rows_selected()) != 1L) {
          modalDialog(
            title = "Warning",
            paste("Please select one row." ),
            easyClose = TRUE
          )
        } else {
          modalDialog(
            title = "Row Edition",
            fluidPage(
              htmlOutput(ns("key_entry")),
              uiOutput(ns("entries")),
              verbatimTextOutput(ns("msg"), placeholder = TRUE),
              actionButton(ns("submit_button"), "Submit"),
              actionButton(ns("dismiss_button"), "Dismiss")
            ),
            footer = NULL
          )
        }
      )
    },
    ignoreInit = TRUE
  )
}

edit_row_server <- function (ns_name, edit_row_button, table_rows_selected, tab, update) {

  moduleServer(
    ns_name,
    function (input, output, session) {
      ns <- session$ns

      key_name <- reactive(attributes(geo_data[[tab()]])$key)
      key_num <- reactive(which(names(geo_data[[tab()]]) == key_name()))
      field <- reactive(names(geo_data[[tab()]])[-key_num()])
      row_key <- reactive(geo_data[[tab()]][table_rows_selected(), key_name()])

      output$key_entry <- renderUI({
        HTML(paste0(
          "<div><b>", key_name(), "</b></div>",
          "<div style='color:red'>", row_key(), "</div>"
        ))
      })

      output$entries <- renderUI({
        lapply(
          field(),
          function (col_name) {
            textInput(
              inputId = ns(col_name), label = col_name,
              value = geo_data[[tab()]][table_rows_selected(), col_name]
            )
          }
        )
      })

      update_n <- eventReactive(
        input$submit_button,
        {
          value <- rep(NA, length(field()))

          for (i in 1:length(field())) {
            value[i] <- input[[field()[i]]]
          }

          res <- tryCatch(
            {
              update_row(tab(), row_key(), field(), value)
              0L
            },
            warning = function (w) {
              output$msg <- renderText({w$message})
              1L
            },
            error = function (e) {
              output$msg <- renderText({e$message})
              1L
            }
          )

          if (res == 0L) {
            removeModal()
            update() + 1
          } else {
            update()
          }
        },
        ignoreInit = TRUE
      )

      observeEvent(
        input$dismiss_button,
        {
          removeModal()
          update_n <- reactive(update())
        },
        ignoreInit = TRUE
      )

      return (update_n)
    }
  )
}
