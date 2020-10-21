
library(geography)
load_data()

test_that("read_tab: arguments", {

  expect_error(read_tab(1, "Name"), "\"table\" is not a character string.")
  expect_error(read_tab("city", 1), "\"field\" is not a character string.")
  expect_error(read_tab("city", "Name", 1), "\"sel_fun\" is not a function.")
  expect_error(read_tab("city", "tname"), "Wrong value in \"field\".")

  expect_error(
    read_tab("tname", "Name"),
    "There is no \"table\" in \"geo_data\" environment."
  )

  expect_error(
    read_tab("city", "Name", function(x, y) {}),
    "\"sel_fun\" should take only \"table\" as an argument."
  )
})

test_that("read_tab: side effect", {
  expect_output(
    read_tab("city", c("Name", "Population")),
    "Name", "Population"
  )
})
