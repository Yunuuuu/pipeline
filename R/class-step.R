# step class
# user-friendly helper
#' Build a `step` object
#' A `step` object define the command to run in the pipeline.
#' @description
#' steps define the command the run in the analysis pipeline
#'  - step: user-friendly helper
#'  - new_step: low-level constructor
#' @param call The command to run. Notes: this will be enclosed by
#' <[`rlang::enquo()`]> to bundle the environment of users. If you use the step
#' in another environment other than the current environment. You should use
#' [new_step] or quote the expression firstly then use
#' <[`!!`][rlang::injection-operator]> to inject the call expression in [step].
#' @param deps A character vector defines the upstream steps to run before
#' runing this step.
#' @param finished A scalar `logical` indicates whether this step has been
#' evaluated.
#' @param return A scalar `logical`, `character`, or `NULL`. a `logical` value
#' indicates whether to keep the returned value. if `FALSE`, the result
#' evaluated from `call` won't be saved. Otherwise, the results will be saved.
#' if `TRUE` or `NULL`, the result name will be the same with the name in
#' `step_tree` object. Or a sclar `character` define the name.
#' @param seed  A scalar `logical`, `numeric`, or `NULL`. a `logical` value
#' indicates whether to set seed when evaluated the command in "call". if
#' `TRUE`, the call is evaluated with a seed (based on the hash of the call
#' object). if `numeric`, seed will be set by `set.seed(as.integer(seed))`.
#' Otherwise, the call is evaluated without seed.
#' @return A `step` object.
#' @name step
#' @export
step <- function(call, deps = NULL, finished = FALSE, return = NULL, seed = NULL) {
    new_step(
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
new_step <- function(call, deps = NULL, finished = FALSE, return = NULL, seed = NULL) {
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
        cli::cli_abort("{.arg return} should be a scalar {.cls character} or {.cls character}, or {.val NULL}")
    }
    if (isTRUE(seed)) {
        seed <- digest::digest2int(digest::digest(call, "crc32"), seed = 0L)
    } else if (rlang::is_scalar_double(seed) || rlang::is_scalar_integer(seed)) {
        seed <- as.integer(seed)
    } else if (!(is.null(seed) || isFALSE(seed))) {
        cli::cli_abort("{.arg seed} should be a scalar {.cls logical} or {.cls numeric}, or {.val NULL}")
    }
    structure(
        list(
            call = call, deps = deps,
            finished = finished, return = return,
            seed = seed
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
        finished = step$finished, return = step$return,
        seed = step$seed
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
        finished = step$finished, return = step$return,
        seed = step$seed
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
    if (rlang::is_scalar_integer(step$seed)) {
        old_seed <- rlang::env_get(globalenv(), ".Random.seed", default = NULL)
        if (is.null(old_seed)) {
            on.exit(rm(".Random.seed", envir = globalenv()))
        } else {
            on.exit(rlang::env_bind(globalenv(), .Random.seed = old_seed))
        }
        set.seed(step$seed)
    }
    mask <- rlang::new_data_mask(mask)
    mask$.data <- rlang::as_data_pronoun(mask)
    rlang::eval_tidy(step$call, data = mask, env = envir)
}

assert_step <- function(step) {
    if (!is_step(step)) {
        cli::cli_abort(c(
            "{.var step} must be a {.cls step} object",
            "x" = "You've supplied a {.cls {class(step)}}."
        ))
    }
}

reset_step <- function(step) {
    assert_step(step)
    step$finished <- FALSE
    step
}

finish_step <- function(step) {
    assert_step(step)
    step$finished <- TRUE
    step
}

is_finished <- function(step) {
    assert_step(step)
    isTRUE(step$finished)
}
