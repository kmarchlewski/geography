
rm_row_ui <- function (ns_name, rm_row_button, table_rows_selected) {
  ns <- NS(ns_name)

  observeEvent(
    rm_row_button(),
    {
      if (length(table_rows_selected()) < 1L) {
        showModal(
          modalDialog(
            title = "Warning",
            paste("Please select at least one row."),
            easyClose = TRUE
          )
        )
      }
    },
    ignoreInit = TRUE
  )
}

rm_row_server <- function (ns_name, rm_row_button, table_rows_selected, tab, update) {

  moduleServer(
    ns_name,
    function (input, output, session) {

      update_n <- eventReactive(
        rm_row_button(),
        {
          if (length(table_rows_selected()) >= 1L) {
            key_name <- attributes(geo_data[[tab()]])$key

            row_key <- geo_data[[tab()]][table_rows_selected(), key_name]

            for (i in 1:length(table_rows_selected())) {
              delete_row(tab(), row_key[i])
            }

            update() + 1
          } else {
            update()
          }
        },
        ignoreInit = TRUE
      )

      return (update_n)
    }
  )
}
