
mk_row_ui <- function (ns_name, mk_row_button, table_rows_selected) {
  ns <- NS(ns_name)

  observeEvent(
    mk_row_button(),
    {
      showModal(
        if (length(table_rows_selected()) > 1L) {
          modalDialog(
            title = "Warning",
            paste("Please select one row preceding a new row." ),
            easyClose = TRUE
          )
        } else {
          modalDialog(
            title = "Row Key",
            fluidPage(
              uiOutput(ns("key_entry")),
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

mk_row_server <- function (
  ns_name, mk_row_button, table_rows_selected, tab, update
) {

  moduleServer(
    ns_name,
    function (input, output, session) {
      ns <- session$ns

      output$key_entry <- renderUI({
        textInput(
          inputId = ns("key"),
          label = attributes(geo_data[[tab()]])$key
        )
      })

      update_n <- eventReactive(
        input$submit_button,
        {
          field <- attributes(geo_data[[tab()]])$key
          value <- input$key

          res <- tryCatch(
            {
              if (identical(value, "")) {
                output$msg <- renderText({"Key value can not be empty."})
                3L
              } else {
                add_row(tab(), field, value)
                0L
              }
            },
            warning = function (w) {
              output$msg <- renderText({w$message})
              1L
            },
            error = function (e) {
              output$msg <- renderText({e$message})
              2L
            }
          )

          if (length(table_rows_selected()) == 0L) {
            geo_data[[tab()]] <- rbind(
              geo_data[[tab()]][nrow(geo_data[[tab()]]), ],
              geo_data[[tab()]][-nrow(geo_data[[tab()]]), ]
            )
          } else if (length(table_rows_selected()) == 1L) {
            geo_data[[tab()]] <- rbind(
              geo_data[[tab()]][1:table_rows_selected(), ],
              geo_data[[tab()]][nrow(geo_data[[tab()]]), ],
              geo_data[[tab()]][(table_rows_selected()+1):nrow(geo_data[[tab()]]), ]
            )
          }

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
