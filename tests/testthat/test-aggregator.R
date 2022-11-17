d <- tibble::tibble(
  x = seq(0,1e6),
  t = nanotime::nanotime(Sys.time()) + seq(0, 1e6) * 7e4,
  tp = Sys.time() + seq(0, 1e6) * 7,
  tch = format(t, "%Y-%m-%d %H:%M:%E9S"),
  y = (3 + sin(x / 200) + runif(1e6 + 1) / 10) * x / 1000,
  y_fct = cut(y, breaks = c(-Inf, seq(5, 30, 5), Inf))
)

d_sep <- d[c(1, 50:1e4, 3e4:1e5, (2e5+3e4):(8e5), 9e5:1e6),]

test_that("Check regarding the base aggregator", {
  bs_agg <- aggregator$new(interleave_gaps = TRUE)
  expect_is(bs_agg, c("aggregator","R6"))
})


test_that("Check regarding the null", {
  null_agg <- null_aggregator$new(interleave_gaps = TRUE)
  expect_is(null_agg, c("aggregator","R6"))

  d_agg <- null_agg$aggregate(d$x[1:100], d$y[1:100], n_out = 100)
  expect_equal(d_agg, list(x = d$x[1:100], y = d$y[1:100]))

  expect_error(null_agg$aggregate(d$x[1:100], d$y, n_out = 100))

  null_agg_int <- aggregator$new(
    accepted_datatype = c("integer", "factor")
  )

  expect_error(null_agg_int$aggregate(d$x, d$y, n_out = 100))

  d_replace_NA_begin <- null_agg$aggregate(
    d$x[c(1, 2, 100:500)], d$y[c(1,2, 100:500)], n_out = -1
  )

  expect_equal(
    d_replace_NA_begin,
    list(
      x = c(d$x[1], NA, d$x[100:500]),
      y = c(d$y[1], NA, d$y[100:500])
    )
  )


  null_agg_end <- null_aggregator$new(
    interleave_gaps = TRUE, NA_position = "end"
    )
  d_replace_NA_end <- null_agg_end$aggregate(
    d$x[c(1, 2, 100:500)], d$y[c(1,2, 100:500)], n_out = -1
  )

  expect_equal(
    d_replace_NA_end,
    list(
      x = c(d$x[c(1,2)], NA, d$x[101:500]),
      y = c(d$y[c(1,2)], NA, d$y[101:500])
    )
  )


  null_agg_both <- null_aggregator$new(
    interleave_gaps = TRUE, NA_position = "both"
  )
  d_replace_NA_both <- null_agg_both$aggregate(
    d$x[c(1, 2, 100:500)], d$y[c(1,2, 100:500)], n_out = -1
  )

  expect_equal(
    d_replace_NA_both,
    list(
      x = c(d$x[c(1)], NA, NA, d$x[101:500]),
      y = c(d$y[c(1)], NA, NA, d$y[101:500])
    )
  )

  d_insert_NA <- null_agg$aggregate(
    d$x[c(1, 51:100)], d$y[c(1, 51:100)]
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

  minmax_ovlp_agg_na <- min_max_ovlp_aggregator$new(interleave_gaps = TRUE)
  d_sep_agg <- minmax_ovlp_agg_na$aggregate(d_sep$x, d_sep$y, n_out = 100)
  sum(is.na(d_sep_agg$x))

})


test_that("Check min-max aggregator", {
  minmax_agg <- min_max_aggregator$new()
  d_agg <- minmax_agg$aggregate(d$x[1:1e4], d$y[1:1e4], n_out = 100)

  expect_equal(length(d_agg), 2)
  expect_equal(length(d_agg$x), 100)
  expect_equal(d$y[d_agg$x + 1], d_agg$y)

  minmax_agg_na <- min_max_aggregator$new(interleave_gaps = TRUE)
  d_sep_agg <- minmax_agg_na$aggregate(d_sep$x, d_sep$y, n_out = 100)
  sum(is.na(d_sep_agg$x))

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


test_that("Check Efficient LTTB aggregator", {
  eLTTB_agg <- eLTTB_aggregator$new()
  d_agg <- eLTTB_agg$aggregate(d$x, d$y, n_out = 100)

  expect_equal(length(d_agg), 2)
  expect_equal(length(d_agg$x), 100)
})

test_that("Check range stat aggregator", {
  rs_agg <- range_stat_aggregator$new()
  d_agg <- rs_agg$aggregate(d$x, d$y, n_out = 100)

  expect_equal(length(d_agg), 4)
  expect_equal(length(d_agg$x), 100)

  d_agg_pr <- do.call(rs_agg$as_plotly_range, d_agg)
  expect_equal(length(d_agg_pr), 1)
  expect_equal(length(d_agg_pr[[1]]), 6)

  rs_agg_na <- range_stat_aggregator$new(interleave_gaps = TRUE)
  d_sep_agg <- rs_agg_na$aggregate(d_sep$x, d_sep$y, n_out = 100)

  prng <- do.call(rs_agg_na$as_plotly_range, d_sep_agg)

  expect_equal(length(prng[[1]]), 6)

  drng <- purrr::map(prng, ~rs_agg_na$as_range(.x) %>% tibble::as_tibble()) %>%
    bind_rows()

  expect_equal(sum(!is.na(d_sep_agg$x)), nrow(drng))

  })

