#' Aggregation that returns ranges of the data.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' A super class for describing aggregators that returns x, y, ylwr and yupr
#' values based on given x and y data.
#'
rng_aggregator <- R6::R6Class(
  "rng_aggregator",
  inherit = aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param ...
    #' Arguments pass to \code{aggregator$new}.
    initialize = function(...) {
      super$initialize(...)
    },

    #' @description
    #' Compute a plotly trace to illustrate the range of the data.
    #' @param x,y,ylwr,yupr Outputs of the sub class of \code{rng_aggregator}.
    #' @param opacity Numeric, optional. Opacity of the range fill.
    #' By default, 0.5.
    as_plotly_range = function(x, y, ylwr, yupr, opacity = 0.5) {

      prng <- list(
        x = c(x, rev(x)),
        y = c(ylwr, rev(yupr)),
        text = paste(
          paste0("x: ",    x),
          paste0("y: ",    y),
          paste0("ylwr: ", c(ylwr, rev(ylwr))),
          paste0("yupr: ", c(yupr, rev(yupr))),
          sep = "<br>"
        ),
        fill = "toself",
        opacity = opacity,
        hoveron = "points"
      )

      return(prng)
    },
    #' @description
    #' Compute x, y, ylwr and yupr from a plotly trace made by
    #' \code{self$as_plotly_range}.
    #' @param prng List of which length is 2.
    #' Both of 2 lists are named list that contains \code{x} and \code{y},
    #' where the one is for nominal values and the other is for range values.
    as_range = function(prng) {

      assertthat::assert_that(inherits(prng, "list"))
      assertthat::assert_that(length(prng) == 2)
      assertthat::assert_that(
        length(prng) == 2 &&
          "x" %in% names(prng[[1]]) && "y" %in% names(prng[[1]]) &&
          "x" %in% names(prng[[2]]) && "y" %in% names(prng[[2]]) &&
          length(prng[[1]]$x) == length(prng[[1]]$y) &&
          length(prng[[2]]$x) == length(prng[[2]]$y) &&
          (length(prng[[1]]$x) ==  2 * length(prng[[1]]$x) ||
             2 * length(prng[[1]]$x) == length(prng[[1]]$x)),
        msg = "There must be 2 lists,
        which contains x and y and represents nominal values and ranges."
      )

      if (length(prng[[1]]$x) ==  2 * length(prng[[1]]$x)) {
        prng_n <- prng[[1]]
        prng_r <- prng[[2]]
      } else {
        prng_n <- prng[[2]]
        prng_r <- prng[[1]]
      }

      rng <- list(
        x = prng_n$x,
        y = prng_n$y,
        ylwr = prng_r$y[seq(1, length(prng_n$x))],
        yupr = prng_r$y[seq(2 * length(prng_n$x), length(prng_n$x) + 1)]
      )

      return(rng)
    }


  ),
  private = list(
  )
)
