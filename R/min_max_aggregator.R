#' Aggregation using local minimum and maximum values.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' Divide the data into small data ranges
#' and find the maximum and minimum values of each.
#' Note that many samples may be replaced with \code{NA},
#' if \code{interleave_gaps = TRUE} and the original data is increased or decreased
#' monotonically. Use \code{min_max_ovlp_aggregator} instead in that case.
#' \code{n_out} must be even number.
#' @examples
#' data(noise_fluct)
#' agg <- min_max_aggregator$new(interleave_gaps = TRUE)
#' d_agg <- agg$aggregate(noise_fluct$time, noise_fluct$f500, 1000)
#' plotly::plot_ly(x = d_agg$x, y = d_agg$y, type = "scatter", mode = "lines")
#'
min_max_aggregator <- R6::R6Class(
  "min_max_aggregator",
  inherit = aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param interleave_gaps,coef_gap,NA_position,...
    #' Arguments pass to the constructor of \code{aggregator} object.
    initialize = function(
      ...,
      interleave_gaps, coef_gap, NA_position
    ) {
      args <- c(as.list(environment()), list(...))
      do.call(super$initialize, args)
    }
  ),
  private = list(
    accepted_datatype = c("numeric", "integer", "character", "factor", "logical"),
    aggregate_exec = function(x, y, n_out) {
      n_minmax <- n_out / 2 - 1

      y_mat <- private$generate_matrix(
        y[2:(length(x) - 1)], n_minmax, remove_first_last = FALSE
      )
      y_mat_values <- apply(y_mat, 2, function(x) sum(!is.na(x)))

      idx_min <- 1 +
        purrr::map_int(1:n_minmax, ~which.min(y_mat[, .x])) +
        c(0, cumsum(y_mat_values)[1:(n_minmax - 1)])

      idx_max <- 1 +
        purrr::map_int(1:n_minmax, ~which.max(y_mat[, .x])) +
        c(0, cumsum(y_mat_values)[1:(n_minmax - 1)])

      idx <- c(1, idx_min, idx_max, length(x)) %>% sort()

      return(list(x = x[idx], y = y[idx]))

    },
    aggregate_db = function(x, y, n_out, db) {

      srcs_name <- basename(db) %>% stringr::str_remove("\\..*$")
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db)

        n_row <- dbGetQuery(
          con,
          "select count(*) as n_row from tmp_data"
        ) %>%
          dplyr::pull()

        n_out <- n_out %/% 2
        n_sample <- rep(n_row %/% n_out, n_out) + c(rep(1, n_row %% n_out), rep(0, n_out - n_row %% n_out))

        data_agg <- purrr::map2(
          n_sample, c(0, cumsum(n_sample)[-n_out]),
          function(i, offset) {
            data <- dbGetQuery(
              con,
              paste0(
                "select * from tmp_data limit ", i, "offset ", offset
              )
            )
            idx_min <- which(data["y"] == min(data["y"], na.rm = TRUE))
            idx_max <- which(data["y"] == max(data["y"], na.rm = TRUE))

            data[c(min(idx_min, idx_max), max(idx_min, idx_max)),]
          }
        ) %>%
          bind_rows()

        DBI::dbExecute(con, paste0("drop table tmp_data"))
      DBI::dbDisconnect(con)

      if (inherits(data_agg$x, "POSIXt")) {
        data_agg$x <- nanotime::as.nanotime(data_agg$x)
      }

      return(list(x = data_agg$x, y = data_agg$y))


    }
  )
)
