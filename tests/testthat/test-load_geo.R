
library(geography)

test_that("load_data: return value", {
  expect_silent(load_data())
})

test_that("load_data: side effect", {
  # load_data()

  expect_identical(
    find("geo_data"),
    "package:geography",
    " There is no \"geo_data\" environment in \'package:geography\'."
  )

  expect_identical(
    ls(envir = geo_data),
    c("city", "country", "country_language"),
    " There are no geographic tables in the \"geo_data\" environment."
  )
})
