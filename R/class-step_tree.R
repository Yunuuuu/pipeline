#' @title step_tree class
#'
#' @name step_tree
NULL

#' low-level constructor
#' @noRd
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
    if (is.null(ids) || any(is.na(ids) | ids == "")) {
        cli::cli_abort("all items in {.cls step_tree} should be named.")
    }
    dup_ids <- ids[duplicated(ids)]
    if (length(dup_ids)) {
        cli::cli_abort(c(
            "duplicated {.cls step} in {.cls step_tree}",
            i = "duplicated {.field names}: {dup_ids]"
        ))
    }
    step_tree
}

step_tree <- function(...) {
    validate_step_tree(new_step_tree(...))
}

#' Reports whether x is an `step_tree` object
#' @param x An object to test
#' @keywords internal
#' @noRd
is_step_tree <- function(x) inherits(x, "step_tree")

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

#' @description if provided a non-step_tree object, induce error.
#' @keywords internal
#' @noRd
assert_step_tree <- function(step_tree) {
    if (!is_step_tree(step_tree)) {
        cli::cli_abort(c(
            "{.var step_tree} must be a {.cls step_tree} object",
            "x" = "You've supplied a {.cls {class(step_tree)}}."
        ))
    }
}

#' @return a list of character vector, if a step has no dependencies, NA will be
#' returned.
#' @keywords internal
#' @noRd
extract_step_deps <- function(step_list) {
    lapply(step_list, function(x) {
        deps <- x[["deps"]]
        if (!length(deps) || all(is.na(deps))) {
            return(NA_character_)
        }
        deps[!is.na(deps)]
    })
}

build_deps_graph <- function(step_deps) {
    edge_df <- data.frame(
        from = unlist(step_deps, use.names = FALSE),
        to = rep(names(step_deps), lengths(step_deps))
    )
    nodes <- unique(c(edge_df$from, edge_df$to))
    nodes <- nodes[!is.na(nodes)]
    node_df <- data.frame(nodes = nodes)

    # omit edges with NA value
    edge_df <- edge_df[!is.na(edge_df$from), , drop = FALSE]

    igraph::graph_from_data_frame(
        d = edge_df, directed = TRUE,
        vertices = node_df
    )
}

#' @keywords internal
#' @param step_deps A list of dependencies for each steps.
#' @return a named interger, names indicates the step name, and value indicates
#' the levels of the step.
#' @noRd
define_step_levels <- function(deps_graph, step_name) {
    root <- igraph::V(deps_graph)[
        igraph::degree(deps_graph, mode = "in") == 0L
    ]
    if (step_name %in% root) {
        return(1L)
    } else {
        paths <- igraph::all_simple_paths(
            deps_graph,
            from = root,
            to = step_name
        )
        return(max(lengths(paths)))
    }
}

add_deps_graph_attrs <- function(deps_graph, step_list) {
    nodes <- names(igraph::V(deps_graph))

    # add is_finished attribute to nodes
    is_finished <- vapply(nodes, function(node) {
        # nolint
        step <- step_list[[node]]
        if (is_step(step)) {
            return(step$finished)
        } else if (is.null(step)) {
            return(NA)
        }
    }, logical(1L), USE.NAMES = FALSE)
    is_existed <- nodes %in% names(step_list) # nolint
    levels <- vapply(nodes, define_step_levels, # nolint
        integer(1L),
        deps_graph = deps_graph
    )
    for (attr in c("is_finished", "is_existed", "levels")) {
        deps_graph <- igraph::set_vertex_attr(
            deps_graph,
            name = attr,
            value = rlang::env_get(rlang::current_env(), nm = attr)
        )
    }
    deps_graph
}

deprecated_define_step_levels <- function(step_deps) {
    levels <- structure(
        vector("list", length(step_deps)),
        names = names(step_deps)
    )

    #' 1. if a dependence is not in `names(step_deps)`, which occured when the
    #'    dependence is not in the `step_tree`, then an `NA` value is returned.
    #' 2. if a step has a dependency `NULL`, it should be the first level.
    #' 3. Otherwise, the level of a step should be the max level among its
    #'    parent step nodes adding 1L.
    define_level <- function(name) {
        if (!name %in% names(levels)) {
            # `name` may contain the dependencies (parent step node) not in the
            # step tree, in this way, we just return NA_integer_ value for the
            #  the definition of this child step node.
            value <- NA_integer_
        } else {
            value <- levels[[name]]
            # if NULL, level is not defined for this step
            # we define the level based on its dependencies
            if (is.null(value)) {
                deps <- step_deps[[name]]
                if (identical(length(deps), 1L) && is.na(deps)) {
                    value <- 1L
                } else {
                    deps_levels <- vapply(deps, define_level, integer(1L),
                        step_deps = step_deps, USE.NAMES = FALSE
                    )
                    value <- max(deps_levels, na.rm = FALSE) + 1L
                }
                levels[[name]] <<- value
            }
        }
        # always return value since we recursively use `define_level` function.
        # in this way, we can recursively in depth across the dependencies tree.
        value
    }
    for (name in names(levels)) {
        define_level(name = name, step_deps = step_deps)
    }
    unlist(levels, recursive = FALSE, use.names = TRUE)
}

find_deps_graph <- function(target, deps_graph) {
    igraph::subcomponent(deps_graph, v = target, mode = "in")
}

run_step_tree <- function(step_tree, targets = NULL, refresh = FALSE, force = TRUE, mask = NULL, envir = rlang::caller_env()) {
    assert_step_tree(step_tree)
    step_list <- unclass(step_tree)
    step_deps <- extract_step_deps(step_list)

    # targets are the step nodes we run until
    # if NULL, all steps without child steps
    targets <- rlang::enquo(targets)
    if (is.null(rlang::quo_get_expr(targets))) {
        targets <- names(step_deps)[
            !names(step_deps) %in%
                unlist(step_deps, recursive = FALSE, use.names = FALSE)
        ]
    } else {
        targets <- tidyselect::eval_select(targets, data = step_list)
        targets <- names(step_tree)[targets]
    }

    # build dependencies graph
    deps_graph <- build_deps_graph(step_deps = step_deps)
    deps_graph <- add_deps_graph_attrs(
        step_deps = step_deps, step_list = step_list
    )

    # extract the deps_graph of each targets step
    targets_deps_graph_list <- structure(
        lapply(targets, find_deps_graph, deps_graph = deps_graph),
        names = targets
    )

    if (anyNA(step_levels)) {
        cli::cli_abort(c(
            "Some steps's dependencies won't fond in {.arg step_tree}",
            i = "Missed dependant steps: {names(step_levels)[is.na(step_levels)]}"
        ))
    }
}

#' Create a `match_level` object
#' @param name The step name of which to matched step level
#' @keywords internal
#' @noRd
match_level <- function(name) {
    if (anyNA(name)) {
        cli::cli_abort("{.val NA} is not allowed in {.arg name}.")
    }
    if (!is.character(name)) {
        cli::cli_abort(c(
            "{.var name} must be a atomic {.cls character}.",
            "x" = "You've supplied a {.cls {class(name)}}."
        ))
    }
    structure(name, class = "match_level")
}

#' Reports whether x is an `match_level` object
#' @param x An object to test
#' @keywords internal
#' @noRd
is_match_level <- function(x) inherits(x, "match_level")
