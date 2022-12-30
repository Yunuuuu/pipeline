# step class
# user-friendly helper
step <- function(call, deps = NULL, finished = FALSE, return = NULL) {
    validate_step(new_step(
        call = rlang::enquo(call),
        deps = deps,
        finished = finished,
        return = return
    ))
}

# low-level constructor
# steps define the command the run in the analysis pipeline
new_step <- function(call, deps = NULL, finished = FALSE, return = NULL) {
    structure(
        list(
            call = call, deps = deps,
            finished = finished, return = return
        ),
        class = "step"
    )
}

validate_step <- function(x) {
    values <- unclass(x)
    if (!(rlang::is_character(values$deps) || is.null(values$deps))) {
        cli::cli_abort("{.arg deps} should be a scalar {.cls character} or {.val NULL}")
    }
    if (!rlang::is_scalar_logical(values$finished)) {
        cli::cli_abort("{.arg finished} should be a scalar {.cls logical}")
    }
    if (!(rlang::is_scalar_character(values$return) || is.null(values$return))) {
        cli::cli_abort("{.arg return} should be a scalar {.cls character} or {.val NULL}")
    }
    x
}

`[[.step` <- function(x, name) {
    NextMethod()
}

`[[<-.step` <- function(x, name, value) {
    step <- NextMethod()
    step(
        call = step$call, deps = step$deps,
        finished = step$finished, return = step$return
    )
}

`$.step` <- function(x, name) {
    NextMethod()
}
`$<-.step` <- function(x, name, value) {
    step <- NextMethod()
    step(
        call = step$call, deps = step$deps,
        finished = step$finished, return = step$return
    )
}

`[.step` <- function(x, name) {
    NextMethod()
}

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
is_step <- function(x) inherits(x, "step")

#' Reports whether x is an `step` object
#' @param step A step object.
#' @param mask The environment used to implement data mask, Objects in `mask`
#' have priority over those in `envir`.
#' @param envir The environment in which to evaluate the `call` in step.
#' @keywords internal
run_step <- function(step, mask = NULL, envir = rlang::caller_env()) {
    mask <- rlang::new_data_mask(mask)
    mask$.data <- rlang::as_data_pronoun(mask)
    rlang::eval_tidy(step$call, data = mask, env = envir)
}
