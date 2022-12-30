# step class
# user-friendly helper
#' Build a `step` object
#' A `step` object define the command to run in the pipeline.
#' @param call The command to run. Notes: this will be enclosed by
#' <[`rlang::enquo()`]> to bundle the environment of users. If you use the step
#' in another environment other than the current environment. You should quote
#' the expression firstly then use <[`!!`][rlang::injection-operator]> to inject
#' the call expression.
#' @param deps A character vector defines the upstream steps to run before
#' runing this step.
#' @param finished A scalar `logical` indicates whether this step has been
#' evaluated.
#' @param return A scalar `logical`, `character`, or `NULL`. a `logical` value
#' indicates whether to keep the returned value. if `FALSE`, the result
#' evaluated from `call` won't be saved. Otherwise, the results will be saved.
#' if `TRUE` or `NULL`, the result name will be the same with the name in
#' `step_tree` object. Or a sclar `character` define the name.
#' @return A `step` object.
#' @name step
#' @export 
step <- function(call, deps = NULL, finished = FALSE, return = NULL) {
    new_step(
        call = rlang::enquo(call),
        deps = deps,
        finished = finished,
        return = return
    )
}

# low-level constructor
# steps define the command the run in the analysis pipeline
new_step <- function(call, deps = NULL, finished = FALSE, return = NULL) {
    if (!rlang::is_call(call)) {
        cli::cli_abort("{.arg call} should be a {.cls call} object")
    }
    if (!(is.null(deps) || rlang::is_character(deps))) {
        cli::cli_abort("{.arg deps} should be a scalar {.cls character} or {.val NULL}")
    }
    if (!rlang::is_scalar_logical(finished)) {
        cli::cli_abort("{.arg finished} should be a scalar {.cls logical}")
    }
    if (!(is.null(return) || rlang::is_scalar_character(return) || rlang::is_scalar_logical(return))) {
        # nolint
        cli::cli_abort("{.arg return} should be a scalar {.cls character} or {.cls character}, or {.val NULL}")
    }
    structure(
        list(
            call = call, deps = deps,
            finished = finished, return = return
        ),
        class = "step"
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
    step(
        call = step$call, deps = step$deps,
        finished = step$finished, return = step$return
    )
}

#' @export 
#' @rdname step
`$.step` <- function(x, name) {
    NextMethod()
}

#' @export 
#' @rdname step
`$<-.step` <- function(x, name, value) {
    step <- NextMethod()
    step(
        call = step$call, deps = step$deps,
        finished = step$finished, return = step$return
    )
}

#' @export 
#' @rdname step
`[.step` <- function(x, name) {
    NextMethod()
}

#' @export 
#' @rdname step
`[<-.step` <- function(x, name, value) {
    step <- NextMethod()
    step(
        call = step$call, deps = step$deps,
        finished = step$finished, return = step$return
    )
}


#' Reports whether x is an `step` object
#' @param x An object to test
#' @keywords internal
#' @noRd 
is_step <- function(x) inherits(x, "step")

#' Run step call command
#' Run step in a mask environment.
#' @param step A step object.
#' @param mask The environment used to implement data mask, Objects in `mask`
#' have priority over those in `envir`.
#' @param envir The environment in which to evaluate the `call` in step.
#' @keywords internal
#' @noRd 
run_step <- function(step, mask = NULL, envir = rlang::caller_env()) {
    mask <- rlang::new_data_mask(mask)
    mask$.data <- rlang::as_data_pronoun(mask)
    rlang::eval_tidy(step$call, data = mask, env = envir)
}

reset_step <- function(step) {
    step$finished <- FALSE
    step
}

finish_step <- function(step) {
    step$finished <- TRUE
    step
}
