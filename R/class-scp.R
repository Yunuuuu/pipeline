#' Single Cell Pipeline
#'
#' @param data A list of object used in the pipeline.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]>, all items must be a `step`
#' object. Can also provide as a list of steps directly.
#' @return A `Pipeline` object.
#' @name scp
scp <- function(data = list(), ...) {
    Pipeline$new(..., data = data)
}

