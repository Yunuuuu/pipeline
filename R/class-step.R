#' Build a `step` object
#'
#' @description
#' A `step` object define the command to run in the pipeline.
#'
#' Both `step` and `create_step` do the same thing, except that `step` function
#' also defuse the `call` argument as a quosure.
#'
#' @param id A scalar character indicates the identification of the step. Must
#' be unique across the `step_tree`.
#' @param call The call define the command to run. Function `step` will use
#'   <[`rlang::enquo()`]> to defuse this argument.
#' @param deps A character vector or `NULL` defines the upstream steps to run
#' before runing this step. `NULL` means no dependencies.
#' @param finished A scalar `logical` indicates whether this step has been
#' evaluated.
#' @param return A scalar `logical` indicates whether to keep the returned
#' value. if `FALSE`, the result evaluated from `call` won't be kept.
#' @param seed A scalar `logical` or `numeric`. a `logical` value indicates
#' whether to set seed when evaluated the expression in "call". if `TRUE`, the
#' call is evaluated with a seed (based on the hash of the call object).
#' Otherwise, the call is evaluated without seed. if `numeric`, seed will be set
#' by `set.seed(as.integer(seed))`.
#' @param ...  <[`dynamic-dots`][rlang::dyn-dots]> Other items to extend `step`
#'   object.
#' @return A `step` object.
#' @export
#' @name step
step <- function(id, call, deps = NULL, finished = FALSE, return = TRUE, seed = FALSE, ...) {
    create_step(
        id = id, call = rlang::enquo(call), deps = deps,
        finished = finished, return = return, seed = seed,
        ...
    )
}

#' @export
#' @rdname step
create_step <- function(id, call, deps = NULL, finished = FALSE, return = TRUE, seed = FALSE, ...) {
    # assert ...
    dots <- rlang::dots_list(..., .homonyms = "error")
    if (length(dots) && !all(has_names(dots))) {
        cli::cli_abort("all items in {.arg ...} must be named")
    }
    x <- list(
        id = id, call = call,
        deps = deps, finished = finished,
        return = return, seed = seed
    )
    x <- c(x, dots)
    validate_step(new_step(x))
}

# low-level constructor
# steps define the command the run in the analysis pipeline
#' @noRd
new_step <- function(x) {
    stopifnot(is.list(x))
    structure(x, class = c("step", "Pipeline"))
}

validate_step <- function(x) {
    step <- unclass(x)
    # assert id
    assert_class(step$id, is.character, class = "character", null_ok = FALSE)
    assert_length(step$id, 1L, null_ok = FALSE)
    reserved_names <- c(".data", ".pipeline", ".step")
    if (is.na(step$id) || step$id == "") {
        cli::cli_abort("{.arg step$id} can't use {.val } or {.code NA_character_}")
    } else if (step$id %in% reserved_names) {
        cli::cli_abort("{.arg id} can't use reserved names {.val {reserved_names}}")
    }

    # assert call
    assert_class(step$call, rlang::is_call, class = "call", null_ok = FALSE)

    # assert deps
    assert_class(step$deps, is.character, class = "character", null_ok = TRUE)

    # assert finished
    assert_class(step$finished, is.logical, class = "logical", null_ok = FALSE)
    assert_length(step$finished, 1L, null_ok = FALSE)
    if (is.na(step$finished)) {
        cli::cli_abort("{.arg step$finished} can't use {.code NA}")
    }

    # assert return
    assert_class(step$return, is.logical, class = "logical", null_ok = FALSE)
    assert_length(step$return, 1L, null_ok = FALSE)
    if (is.na(step$return)) {
        cli::cli_abort("{.arg step$return} can't use {.code NA_character_}")
    }

    # assert seed
    if (!(rlang::is_scalar_logical(step$seed) ||
        rlang::is_scalar_double(step$seed) ||
        rlang::is_scalar_integer(step$seed))) {
        cli::cli_abort("{.arg step$seed} must be a scalar {.cls logical} or {.cls numeric}")
    }
    if (is.na(step$seed)) {
        cli::cli_abort("{.arg step$seed} can't use {.code NA}")
    }
    x
}

#' @param x A `step` object from which to extract element(s) or in which to
#' replace element(s).
#' @param i The indices specifying elements to extract or replace
#' @param value The value to replace with.
#' @export
#' @rdname step
`[[.step` <- function(x, i) {
    NextMethod()
}

#' @export
#' @rdname step
`[[<-.step` <- function(x, i, value) {
    validate_step(new_step(NextMethod()))
}

#' @export
#' @rdname step
`$.step` <- `[[.step`

#' @export
#' @rdname step
`$<-.step` <- `[[<-.step`

#' @export
#' @rdname step
`[.step` <- function(x, i) {
    NextMethod()
}

#' @export
#' @rdname step
`[<-.step` <- function(x, i, value) {
    validate_step(new_step(NextMethod()))
}

#' Reports whether x is a `step` object
#' @param x An object to test
#' @keywords internal
#' @noRd
is_step <- function(x) inherits(x, "step")
