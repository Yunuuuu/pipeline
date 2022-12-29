# step class
# user-friendly helper
step <- function(name, call, deps = character(), finished = FALSE) {
    if (!rlang::is_scalar_character(name)) {
        cli::cli_abort("{.arg name} should be a scalar {.cls character}")
    }
    if (!rlang::is_scalar_character(finished)) {
        cli::cli_abort("{.arg finished} should be a scalar {.cls logical}")
    }
    new_step(
        name = name,
        call = rlang::enquo(call),
        deps = as.character(deps),
        finished = finished
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
new_step <- function(name, call, deps = NULL, finished = FALSE) {
    structure(
        list(name = name, call = call, deps = deps, finished = finished),
        class = "step"
    )
}

# step_tree class
# low-level constructor
new_step_tree <- function(..., step_list) {
    step_list <- new_step_list(..., step_list = step_list)
    if (!all(vapply(step_list, is_step, logical(1L)))) {
        cli::cli_abort(c(
            "all items in {.arg step_list} should be  a {.cls step} object.",
            i = "try to use {.fn new_step} to create it"
        ))
    }
    structure(
        step_list,
        names = vapply(step_list, `[[`, character(1L), "name"),
        class = "step_tree"
    )
}

#' Create a list of steps
#' just for internal usage, for users, it's better to use new_step_tree
#' @keywords internal
#' @noRd
new_step_list <- function(..., step_list) {
    # for items in ..., we use the name as the step name and the value as the
    # call of step
    dots_list <- rlang::enquos(...)
    dots_list <- imap(dots_list, new_step)

    # for items in step_list, they must be a list of `step` object
    c(step_list, dots_list)
}

step_tree <- function(..., step_list) {
    new_step_tree(..., step_list = step_list)
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

# See ?as.dendrogram for the store of tree object
extract_deps <- function(step_tree) {
    deps_list <- lapply(unclass(step_tree), function(x) {
        deps <- x[["deps"]]
        if (!length(deps) || all(is.na(deps))) {
            return(NA_character_)
        }
        deps
    })
    deps <- data.table::data.table(
        from = unlist(deps_list, recursive = FALSE, use.names = FALSE),
        to = rep(names(deps_list), each = lengths(deps_list))
    )
}

run_step <- function(step, mask_envir = NULL, envir = rlang::caller_env()) {
    mask <- rlang::new_data_mask(mask_envir)
    mask$.data <- rlang::as_data_pronoun(mask)
    rlang::eval_tidy(step$call, data = mask, env = envir)
}
