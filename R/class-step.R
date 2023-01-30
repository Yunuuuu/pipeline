#' Build a `step` object
#'
#' @description
#' A `step` object define the command to run in the pipeline.
#'
#' Both `step` and `create_step` do the same thing, except that `step` function
#' also defuse the `expression` argument as a quosure.
#'
#' @param id A scalar character indicates the identification of the step. Must
#' be unique across the `step_tree`.
#' @param expr The expression define the command to run. Function `step`
#'   will use <[`rlang::enquo()`]> to defuse this argument. Details see
#'   [is_expression][rlang::is_expression]
#' @param deps A character vector or `NULL` defines the upstream steps to run
#' before runing this step. `NULL` means no dependencies.
#' @param finished A scalar `logical` indicates whether this step has been
#' evaluated.
#' @param bind A scalar `logical` indicates whether to keep the returned value
#' (bind the symbol named with id to the result in the specific environment
#' usually the pipeline internal environment). if `FALSE`, the result evaluated
#' from `expr` won't be kept. The evaluation of the step expression in the
#' pipeline will always return `NULL`.
#' @param seed A scalar `logical` or `numeric`. a `logical` value indicates
#' whether to set seed when evaluated the "expression". if `TRUE`, the
#' expression is evaluated with a seed (based on the hash of the expression
#' object).  Otherwise, the expression is evaluated without seed. if `numeric`,
#' seed will be set by `set.seed(as.integer(seed))`.
#' @param ...  <[`dynamic-dots`][rlang::dyn-dots]> named values, other items to
#'   extend `step` object.
#' @return A `step` object.
#' @export
#' @name step
step <- function(id, expr, deps = NULL, finished = FALSE, bind = TRUE, seed = FALSE, ...) {
    create_step(
        id = id, expr = rlang::enquo(expr), deps = deps,
        finished = finished, bind = bind, seed = seed,
        ...
    )
}

#' @export
#' @rdname step
create_step <- function(id, expr, deps = NULL, finished = FALSE, bind = TRUE, seed = FALSE, ...) {
    x <- rlang::dots_list(
        id = id, expr = expr,
        deps = deps, finished = finished,
        bind = bind, seed = seed,
        ...,
        .homonyms = "error"
    )
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

    # assert names
    if (length(step) && !all(has_names(step))) {
        cli::cli_abort("all items in {.cls step} must be named")
    }

    # assert id
    assert_class(step$id, is.character, class = "character", null_ok = FALSE)
    assert_length(step$id, 1L, null_ok = FALSE)
    reserved_names <- c(".data", ".pipeline", ".step")
    if (is.na(step$id) || step$id == "") {
        cli::cli_abort("{.arg step$id} can't use {.val } or {.code NA_character_}")
    } else if (any(step$id == reserved_names)) {
        cli::cli_abort("{.arg id} can't use reserved names {.val {reserved_names}}")
    }

    # assert expr
    assert_class(step$expr, rlang::is_expression,
        class = "expression", null_ok = FALSE
    )

    # assert deps
    assert_class(step$deps, is.character, class = "character", null_ok = TRUE)

    # assert finished
    assert_class(step$finished, is.logical, class = "logical", null_ok = FALSE)
    assert_length(step$finished, 1L, null_ok = FALSE)
    if (is.na(step$finished)) {
        cli::cli_abort("{.arg step$finished} can't use {.code NA}")
    }

    # assert bind
    assert_class(step$bind, is.logical, class = "logical", null_ok = FALSE)
    assert_length(step$bind, 1L, null_ok = FALSE)
    if (is.na(step$bind)) {
        cli::cli_abort("{.arg step$bind} can't use {.code NA_character_}")
    }

    # assert seed
    if (!(length(step$seed) == 1L && (is.logical(step$seed) ||
        is.numeric(step$seed)))) {
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
