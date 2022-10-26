#' Show the aggregation functions
#'
#' @description
#' It displays all the aggregators registered in the package.
#' No arguments are necessary.
#' @export
#' @examples
#' list_aggregators()
list_aggregators <- function(){
  ls("package:shinyHugePlot") %>%
    stringr::str_subset("_aggregator$") %>%
    setdiff(c("aggregator", "rng_aggregator"))
}
