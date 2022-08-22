test_that("Convesion of nanotime to plotly-time", {
  nanotime_to_plotlytime(
    nanotime::nanotime("2022-06-01T01:00:00.123456789+00:00"),
    tz = "Asia/Tokyo"
    ) %>%
    expect_equal("2022-06-01 10:00:00.123456789")
})

test_that("Conversion of plotly-time to nanotime", {
  lab <- nanosecond_to_label(
    c(1, 1e3, 1e6, 1e9, 1e9 * 60, 1e9 * 3600,
      1e9 * 86400, 1e9 * 2592000, 1e9 * 31536000)
    )

  expect_equal(
    lab,
    c("1.0ns", "1.0us", "1.0ms", "1.0s",
      "1.0m", "1.0h", "1.0d", "1.0m", "1.0y")
  )
})

test_that("Generate nano-second label", {
  lab <- numeric_to_label(
    c(1, 1e3, 1e6, 1e9, 1e12)
  )

  expect_equal(
    lab,
    c("1.0", "1.0K", "1.0M", "1.0G", "1.0T")
  )
})
