
library(geography)
load_data()

test_that("add_row: arguments", {
  expect_error(
    delete_row(1, 1L),
    "\"table\" is not a character string."
  )

  expect_error(
    delete_row("city_test", 1L),
    "There is no \"table\" in \"geo_data\" environment."
  )

  expect_error(
    delete_row("city", list(a = 1L)),
    "\"row_key\" is not a vector of length one."
  )

  expect_error(
    delete_row("city", c(1L, 2L)),
    "\"row_key\" is not a vector of length one."
  )

  expect_error(
    delete_row("city", "A"),
    "Wrong type of a \"row_key\" argument."
  )

  expect_error(
    delete_row("city", -1L),
    "There is no row with \"row_key\" id in \"table\"."
  )
})

test_that("add_row: return value", {
  expect_silent(delete_row("city", 1L))

  # return to a previous state of tables
  load_data()
})

test_that("add_row: side effect", {

  n_row <- nrow(geo_data[["city"]])
  types <- sapply(geo_data[["city"]], typeof)

  delete_row("city", 1L)

  expect_identical(
    nrow(geo_data[["city"]]),
    n_row - 1L,
    "A row is not removed."
  )

  expect_identical(
    sapply(geo_data[["city"]], typeof),
    types,
    "Types in columns have changed after removing a row."
  )
})
