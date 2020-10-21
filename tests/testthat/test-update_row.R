
library(geography)
load_data()

test_that("update_row: arguments", {
  expect_error(
    update_row(1, 1L, "Name", "New City"),
    "\"table\" is not a character string."
  )

  expect_error(
    update_row("city_test", 1L, "Name", "New City"),
    "There is no \"table\" in \"geo_data\" environment."
  )

  expect_error(
    update_row("city", list(a = 1L), "Name", "New City"),
    "\"row_key\" is not a vector of length one."
  )

  expect_error(
    update_row("city", c(1L, 2L), "Name", "New City"),
    "\"row_key\" is not a vector of length one."
  )

  expect_error(
    update_row("city", "A", "Name", "New City"),
    "Wrong type of a \"row_key\" argument."
  )

  expect_error(
    update_row("city", -1L, "Name", "New City"),
    "There is no row with \"row_key\" id in \"table\"."
  )

  expect_error(
    update_row("city", 1L, c("Name", "District"), "New City"),
    "\"field\" and \"value\" vectors are of different length."
  )

  expect_error(
    update_row("city", 1L, c("Name", "ID"), c("New City", 2)),
    "You can not update a table key."
  )

})

test_that("update_row: return value", {
  expect_silent(update_row("city", 1L, "Name", "New City"))

  # return to a previous state of tables
  load_data()
})

test_that("update_row: side effect", {

  n_row <- nrow(geo_data[["city"]])
  types <- sapply(geo_data[["city"]], typeof)

  update_row("city", 1L, "Name", "New City")

  expect_identical(
    nrow(geo_data[["city"]]),
    n_row,
    "A row number is incorrect."
  )

  expect_identical(
    sapply(geo_data[["city"]], typeof),
    types,
    "Types in columns have changed after updating a row."
  )
})
