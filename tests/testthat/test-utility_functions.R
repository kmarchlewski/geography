library(geography)
load_data()

test_that("sel_row function: arguments", {
  expect_error(
    sel_row(1, "Population < 2000"),
    "\"table\" is not a character string."
  )

  expect_error(
    sel_row("name", "Population < 2000"),
    "There is no \"table\" in \"geo_data\" environment."
  )

  expect_error(
    sel_row("city", "Population < 2000", 1),
    "Not all conditions in \"...\" are character strings."
  )
})

test_that("sel_row function: return value", {
  expect_type(sel_row("city", "Population < 2000"), "closure")
  expect_type((sel_row("city", "Population < 2000"))("city"), "logical")
})
