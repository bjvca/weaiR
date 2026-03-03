#' Construct a weai_result S3 object
#'
#' @param individual Data.frame of individual-level results.
#' @param aggregate List of aggregate results.
#' @param decomposition List of decomposition results.
#' @param subgroup Named list of subgroup results or NULL.
#' @param call The original function call.
#' @param config List of configuration parameters.
#' @return An object of class "weai_result".
#' @keywords internal
new_weai_result <- function(individual, aggregate, decomposition,
                            subgroup, call, config) {
  structure(
    list(
      individual = individual,
      aggregate = aggregate,
      decomposition = decomposition,
      subgroup = subgroup,
      call = call,
      config = config
    ),
    class = "weai_result"
  )
}
