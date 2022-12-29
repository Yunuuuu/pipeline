# step class
# user-friendly helper
step <- function(name, call, deps = character(), finished = FALSE, return = name) {
    if (!rlang::is_scalar_character(name)) {
        cli::cli_abort("{.arg name} should be a scalar {.cls character}")
    }
    if (!rlang::is_scalar_logical(finished)) {
        cli::cli_abort("{.arg finished} should be a scalar {.cls logical}")
    }
    if (!(rlang::is_scalar_character(return) || is.null(return))) {
        cli::cli_abort("{.arg return} should be a scalar {.cls character} or {.val NULL}")
    }
    new_step(
        name = name,
        call = rlang::enquo(call),
        deps = as.character(deps),
        finished = finished,
        return = return
    )
}

`[[.step` <- function(x, name) {
    NextMethod()
}

`[[<-.step` <- function(x, name, value) {
    step <- NextMethod()
    step(step$name, step$call, step$deps, step$finished)
}

`$.step` <- function(x, name) {
    NextMethod()
}
`$<-.step` <- function(x, name, value) {
    step <- NextMethod()
    step(step$name, step$call, step$deps, step$finished)
}

`[.step` <- function(x, name) {
    NextMethod()
}

`[<-.step` <- function(x, name, value) {
    step <- NextMethod()
    step(step$name, step$call, step$deps, step$finished)
}


#' Reports whether x is an `step` object
#' @param x An object to test
#' @keywords internal
is_step <- function(x) inherits(x, "step")

# low-level constructor
# steps define the command the run in the analysis pipeline
new_step <- function(name, call, deps = NULL, finished = FALSE, return = name) {
    structure(
        list(
            name = name, call = call, deps = deps,
            finished = finished, return = return
        ),
        class = "step"
    )
}

# step_tree class
# low-level constructor
new_step_tree <- function(...) {
    step_list <- rlang::list2(...)
    if (!all(vapply(step_list, is_step, logical(1L)))) {
        cli::cli_abort(c(
            "all items in {.arg ...} should be {.cls step} object.",
            i = "try to use {.fn step} to create it"
        ))
    }
    structure(
        step_list,
        names = vapply(step_list, `[[`, character(1L), "name"),
        class = "step_tree"
    )
}

step_tree <- function(...) {
    new_step_tree(...)
}

`[[<-.step_tree` <- function(x, name, value) {
    new_step_tree(NextMethod())
}

`$<-.step_tree` <- function(x, name, value) {
    new_step_tree(NextMethod())
}

`[<-.step_tree` <- function(x, name, value) {
    new_step_tree(NextMethod())
}

get_step_deps <- function(step_tree) {
    lapply(unclass(step_tree), function(x) {
        deps <- x[["deps"]]
        if (!length(deps) || all(is.na(deps))) {
            return(NULL)
        }
        deps[!is.na(deps)]
    })
}

get_step_levels <- function(deps_list) {
    levels <- structure(
        vector("list", length(deps_list)),
        names = names(deps_list)
    )
    define_level <- function(name, deps_list) {
        value <- levels[[name]]
        if (is.null(value)) {
            deps <- deps_list[[name]]
            if (is.null(deps)) {
                value <- 1L
            } else {
                deps_levels <- vapply(deps, define_level, integer(1L),
                    deps_list = deps_list, USE.NAMES = FALSE
                )
                value <- max(deps_levels) + 1L
            }
            levels[[name]] <<- value
        }
        value
    }
    for (name in names(levels)) {
        define_level(name = name, deps_list = deps_list)
    }
    levels
}

run_step <- function(step, mask = NULL, envir = rlang::caller_env()) {
    mask <- rlang::new_data_mask(mask)
    mask$.data <- rlang::as_data_pronoun(mask)
    rlang::eval_tidy(step$call, data = mask, env = envir)
}
