# step_tree class
# low-level constructor
new_step_tree <- function(...) {
    step_list <- rlang::list2(...)
    structure(step_list, class = "step_tree")
}

validate_step_tree <- function(step_tree) {
    values <- unclass(step_tree)
    if (!all(vapply(values, is_step, logical(1L)))) {
        cli::cli_abort(c(
            "all items in {.cls step_tree} should be {.cls step} object.",
            i = "try to use {.fn step} to create it"
        ))
    }
    ids <- names(values)
    if (is.null(ids) || any(is.na(ids) || ids == "")) {
        cli::cli_abort("all items in {.cls step_tree} should be named.")
    }
    dup_ids <- ids[duplicated(ids)]
    if (length(dup_ids)) {
        cli::cli_abort(c(
            "Duplicated {.cls step} in {.cls step_tree}",
            i = "Duplicated {.field names}: {dup_ids]"
        ))
    }
    step_tree
}

step_tree <- function(...) {
    validate_step_tree(new_step_tree(...))
}

`[[.step_tree` <- function(x, name, value) {
    NextMethod()
}

`[[<-.step_tree` <- function(x, name, value) {
    step_tree(!!!NextMethod())
}

`$.step_tree` <- function(x, name, value) {
    NextMethod()
}

`$<-.step_tree` <- function(x, name, value) {
    step_tree(!!!NextMethod())
}

`[.step_tree` <- function(x, name) {
    step_tree(!!!NextMethod())
}

`[<-.step_tree` <- function(x, name, value) {
    step_tree(!!!NextMethod())
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

#' if a dependence is not in `names(deps_list)`, which occured when the
#' dependence is not in the `step_tree`, then an `NA` value is returned.
#'
get_step_levels <- function(deps_list) {
    levels <- structure(
        vector("list", length(deps_list)),
        names = names(deps_list)
    )
    define_level <- function(name, deps_list) {
        if (!name %in% names(levels)) {
            value <- NA_integer_
        } else {
            value <- levels[[name]]
            if (is.null(value)) {
                deps <- deps_list[[name]]
                if (is.null(deps)) {
                    value <- 1L
                } else {
                    deps_levels <- vapply(deps, define_level, integer(1L),
                        deps_list = deps_list, USE.NAMES = FALSE
                    )
                    value <- max(deps_levels, na.rm = FALSE) + 1L
                }
            }
        }
        levels[[name]] <<- value
        value
    }
    for (name in names(levels)) {
        define_level(name = name, deps_list = deps_list)
    }
    unlist(levels, recursive = FALSE, use.names = TRUE)
}
