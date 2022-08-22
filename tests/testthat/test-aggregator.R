d <- tibble::tibble(
  x = seq(0,1e6),
  t = nanotime::nanotime(Sys.time()) + seq(0, 1e6) * 7e4,
  tp = Sys.time() + seq(0, 1e6) * 7,
  tch = format(t, "%Y-%m-%d %H:%M:%E9S"),
  y = (3 + sin(x / 200) + runif(1e6 + 1) / 10) * x / 1000,
  y_fct = cut(y, breaks = c(-Inf, seq(5, 30, 5), Inf))
)

test_that("Check regarding the abstract_aggregator", {
  abs_agg <- abstract_aggregator$new()
  expect_is(abs_agg, c("abstract_aggregator","R6"))

  d_agg <- abs_agg$aggregate(d$x, d$y, n_out = 100)
  expect_equal(d_agg, list(x = d$x[1:100], y = d$y[1:100]))

  expect_error(abs_agg$aggregate(d$x[1:100], d$y, n_out = 100))

  abs_agg_int <- abstract_aggregator$new(
    accepted_datatype = c("integer", "factor")
    )

  expect_error(abs_agg_int$aggregate(d$x, d$y, n_out = 100))

  d_replace_NA <- abs_agg$aggregate(
    d$x[c(1, 100:500)], d$y[c(1, 100:500)], n_out = 100
    )

  expect_equal(
    d_replace_NA,
    list(
      x = c(d$x[1], NA, d$x[101:198]),
      y = c(d$y[1], NA, d$y[101:198])
    )
  )


  d_insert_NA <- abs_agg$aggregate(
    d$x[c(1, 51:100)], d$y[c(1, 51:100)], n_out = 100
    )

  expect_equal(
    d_insert_NA,
    list(
      x = c(d$x[1], NA, d$x[51:100]),
      y = c(d$y[1], NA, d$y[51:100])
    )
  )

})



test_that("Check nth-point aggregator", {
  nth_agg <- nth_pnt_aggregator$new()
  d_agg <- nth_agg$aggregate(d$x[1:1e4], d$y[1:1e4], n_out = 100)

  expect_equal(length(d_agg), 2)
  expect_equal(length(d_agg$x), 100)
  expect_equal(d$y[d_agg$x + 1], d_agg$y)

})



test_that("Check min-max-ovlp aggregator", {
  minmax_ovlp_agg <- min_max_ovlp_aggregator$new()
  d_agg <- minmax_ovlp_agg$aggregate(d$x[1:1e4], d$y[1:1e4], n_out = 100)

  expect_equal(length(d_agg), 2)
  expect_equal(length(d_agg$x), 100)
  expect_equal(d$y[d_agg$x + 1], d_agg$y)
})


test_that("Check min-max aggregator", {
  minmax_agg <- min_max_aggregator$new()
  d_agg <- minmax_agg$aggregate(d$x[1:1e4], d$y[1:1e4], n_out = 100)

  expect_equal(length(d_agg), 2)
  expect_equal(length(d_agg$x), 100)
  expect_equal(d$y[d_agg$x + 1], d_agg$y)
})



test_that("Check LTTB aggregator", {
  LTTB_agg <- LTTB_aggregator$new()
  d_agg <- LTTB_agg$aggregate(d$x[1:1e4], d$y[1:1e4], n_out = 100)

  expect_equal(length(d_agg), 2)
  expect_equal(length(d_agg$x), 100)
  expect_equal(d$y[d_agg$x + 1], d_agg$y)
})

test_that("Check LTTB aggregator using time x", {
  LTTB_agg <- LTTB_aggregator$new()
  d_agg <- LTTB_agg$aggregate(d$t[1:1e4], d$y[1:1e4], n_out = 100)

  expect_equal(length(d_agg), 2)
  expect_equal(length(d_agg$x), 100)
})

test_that("Check LTTB aggregator using factor y", {
  LTTB_agg <- LTTB_aggregator$new()
  d_agg <- LTTB_agg$aggregate(d$x[1:1e4], d$y_fct[1:1e4], n_out = 100)

  expect_equal(length(d_agg), 2)
  expect_equal(length(d_agg$x), 100)
})


test_that("Efficient LTTB aggregator", {
  eLTTB_agg <- eLTTB_aggregator$new()
  d_agg <- eLTTB_agg$aggregate(d$x, d$y, n_out = 100)

  expect_equal(length(d_agg), 2)
  expect_equal(length(d_agg$x), 100)
})

