# step class
# user-friendly helper
#' Build a `step` object
#' A `step` object define the command to run in the pipeline.
#' @description
#' steps define the command the run in the analysis pipeline
#'  - step: user-friendly helper
#'  - new_step: low-level constructor
#' @param id A scalar character indicates the identification of the step. Must
#' be unique across the `step_tree`.
#' @param call The command to run. Notes: this will be enclosed by
#' <[`rlang::enquo()`]> to bundle the environment of users. If you use the step
#' in another environment other than the current environment. You should use
#' [new_step] or quote the expression firstly then use
#' <[`!!`][rlang::injection-operator]> to inject the call expression in [step].
#' @param deps A character vector defines the upstream steps to run before
#' runing this step.
#' @param finished A scalar `logical` indicates whether this step has been
#' evaluated.
#' @param return A scalar `logical` indicates whether to keep the returned
#' value. if `FALSE`, the result evaluated from `call` won't be kept.
#' @param seed A scalar `logical` or `numeric`. a `logical` value indicates
#' whether to set seed when evaluated the expression in "call". if `TRUE`, the
#' call is evaluated with a seed (based on the hash of the call object).
#' Otherwise, the call is evaluated without seed. if `numeric`, seed will be set
#' by `set.seed(as.integer(seed))`.
#' @return A `step` object.
#' @name step
#' @export
step <- function(id, call, deps = NULL, finished = FALSE, return = TRUE, seed = FALSE) {
    new_step(
        id = id,
        call = rlang::enquo(call),
        deps = deps,
        finished = finished,
        return = return,
        seed = seed
    )
}

# low-level constructor
# steps define the command the run in the analysis pipeline
#' @export
#' @rdname step
new_step <- function(id, call, deps = NULL, finished = FALSE, return = TRUE, seed = FALSE) {
    if (!rlang::is_scalar_character(id)) {
        cli::cli_abort("{.arg id} must be a scalar {.cls character}")
    } else if (identical(id, "") || identical(id, NA_character_)) {
        cli::cli_abort("{.arg id} can't be {.val \"\"} or {.val NA_character_}")
    }
    if (!rlang::is_call(call)) {
        cli::cli_abort("{.arg call} must be a {.cls call} object")
    }
    if (!(is.null(deps) || rlang::is_character(deps))) {
        cli::cli_abort("{.arg deps} must be a scalar {.cls character} or {.val NULL}")
    }
    if (!rlang::is_scalar_logical(finished)) {
        cli::cli_abort("{.arg finished} must be a scalar {.cls logical}")
    }
    if (!rlang::is_scalar_logical(return)) {
        cli::cli_abort("{.arg return} must be a scalar {.cls logical}")
    }
    if (!(rlang::is_scalar_logical(seed) ||
        rlang::is_scalar_double(seed) ||
        rlang::is_scalar_integer(seed))) {
        cli::cli_abort("{.arg seed} must be a scalar {.cls logical} or {.cls numeric}")
    }
    structure(
        list(
            id = id, call = call, deps = deps,
            finished = finished, return = return,
            seed = seed
        ),
        class = c("step", "Pipeline")
    )
}

#' @export
#' @param x A `step` object from which to extract element(s) or in which to
#' replace element(s).
#' @param name The indices specifying elements to extract or replace
#' @rdname step
`[[.step` <- function(x, name) {
    NextMethod()
}

#' @export
#' @rdname step
`[[<-.step` <- function(x, name, value) {
    step <- NextMethod()
    new_step(
        id = step$id,
        call = step$call, deps = step$deps,
        finished = step$finished, return = step$return
    )
}

#' @export
#' @rdname step
`$.step` <- `[[.step`

#' @export
#' @rdname step
`$<-.step` <- `[[<-.step`

#' @export
#' @rdname step
`[.step` <- function(x, name) {
    NextMethod()
}

#' @export
#' @rdname step
`[<-.step` <- function(x, name, value) {
    step <- NextMethod()
    new_step(
        id = step$id,
        call = step$call, deps = step$deps,
        finished = step$finished, return = step$return,
        seed = step$seed
    )
}

`+.step` <- function(x, y) {
    if (missing(y)) {
        cli::cli_abort(c(
            "Cannot use {.code +} with a single argument",
            "i" = "Did you accidentally put {.code +} on a new line?"
        ))
    }
    new_step_tree(x, y)
}

#' Reports whether x is an `step` object
#' @param x An object to test
#' @keywords internal
#' @noRd
is_step <- function(x) inherits(x, "step")

#' Create a step object based on a default step
#'
#' @param x A object to define the step, if NULL, will use the default value.
#' @param default A step object, should always use a standardized call in the
#'  step object.
#' @param ... Other arguments to define [step].
#' @return An step object
#' @keywords internal
#' @noRd
define_step <- function(x = NULL, default = NULL, ...) {
    if (is.null(x)) {
        step <- default
    } else if (is_step(x)) {
        step <- x
    } else if (is.list(x)) {
        default$call <- call_standardise(default$call)
        default$call <- rlang::call_modify(default$call, !!!x)
        step <- default
    } else if (rlang::is_call(x)) {
        default$call <- x
        step <- default
    } else {
        cli::cli_abort(c(
            "{.arg x} should be {.val NULL}, {.cls list}, {.cls step} or {.cls call}.",
            "x" = "You've supplied a {.cls {class(call)}}."
        ))
    }
    step <- modify_list(step,
        restrict = c("deps", "finished", "return", "seed"),
        ...
    )
    step
}
