
library(geography)
load_data()

test_that("add_row: arguments", {

  expect_error(
    add_row(1, c("ID", "Name"), c(5000, "Test City")),
    "\"table\" is not a character string."
  )

  expect_error(
    add_row("city_test", c("ID", "Name"), c(5000, "Test City")),
    "There is no \"table\" in \"geo_data\" environment."
  )

  expect_error(
    add_row("city", c(1, 2), c(5000, "Test City")),
    "\"field\" is not a character string."
  )

  expect_error(
    add_row("city", c("ID", "Name2"), c(5000, "Test City")),
    "Wrong value in \"field\"."
  )

  expect_error(
    add_row("city", c("ID"), c(5000, "Test City")),
    "\"field\" and \"value\" vectors are of different length."
  )

  expect_error(
    add_row("city", c("Population", "Name"), c(1000, "Test City")),
    "There is no \"key\" name in the \"field\" vector."
  )

  expect_error(
    add_row("city", c("ID", "Name"), c(1, "Test City")),
    "The \"key\" value is not unique."
  )
})

test_that("add_row: return value", {
  expect_silent(add_row("city", c("ID", "Name"), c(4080, "Test City")))

  # return to a previous state of tables
  load_data()
})

test_that("add_row: side effect", {

  n_row <- nrow(geo_data[["city"]])
  types <- sapply(geo_data[["city"]], typeof)

  add_row("city", c("ID", "Name"), c(4080, "Test City"))

  expect_identical(
    nrow(geo_data[["city"]]),
    n_row + 1L,
    "There is no new row in a table."
  )

  expect_identical(
    sapply(geo_data[["city"]], typeof),
    types,
    "Types in columns have changed after adding a new row."
  )

  expect_identical(
    length(unique(geo_data[["city"]]["ID"])),
    length(geo_data[["city"]]["ID"]),
    "Not all key values are unique after adding a new row."
  )

})
