#' @title step_tree class
#'
#' @name step_tree
NULL

#' low-level constructor
#' @noRd
new_step_tree <- function(...) {
    step_list <- rlang::list2(...)
    if (!all(vapply(step_list, is_step, logical(1L)))) {
        cli::cli_abort(
            "all items must be {.cls step} object."
        )
    }
    structure(
        step_list,
        names = vapply(step_list, "[[", character(1L), "id"),
        class = c("step_tree", "Pipeline")
    )
}

validate_step_tree <- function(step_tree) {
    ids <- names(step_tree)
    dup_ids <- unique(ids[duplicated(ids)])
    if (length(dup_ids)) {
        cli::cli_abort(c(
            "all ids in {.var step_tree} must be unique",
            x = "duplicated names: {dup_ids]"
        ))
    }
    step_tree
}

step_tree <- function(...) {
    rlang::check_dots_unnamed()
    validate_step_tree(new_step_tree(...))
}

#' Reports whether x is an `step_tree` object
#' @param x An object to test
#' @keywords internal
#' @noRd
is_step_tree <- function(x) inherits(x, "step_tree")

`[[.step_tree` <- function(x, id) {
    NextMethod()
}

`[[<-.step_tree` <- function(x, id, value) {
    step_tree(!!!NextMethod())
}

`$.step_tree` <- `[[.step_tree`
`$<-.step_tree` <- `[[<-.step_tree`

`[.step_tree` <- function(x, id) {
    step_tree(!!!NextMethod())
}

`[<-.step_tree` <- function(x, id, value) {
    step_tree(!!!NextMethod())
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

build_step_graph_helper <- function(step_list, add_attrs = FALSE) {
    step_deps <- extract_step_deps(step_list)
    edge_df <- data.frame(
        from = unlist(step_deps, use.names = FALSE),
        to = rep(names(step_deps), lengths(step_deps))
    )
    nodes <- unique(c(edge_df$from, edge_df$to))
    nodes <- nodes[!is.na(nodes)]
    node_df <- data.frame(nodes = nodes)

    # omit edges with NA value
    edge_df <- edge_df[!is.na(edge_df$from), , drop = FALSE]

    step_graph <- igraph::graph_from_data_frame(
        d = edge_df, directed = TRUE,
        vertices = node_df
    )

    # add node attributes
    if (isTRUE(add_attrs)) {
        step_graph <- add_step_graph_attrs(
            step_graph = step_graph,
            step_list = step_list,
            step_deps = step_deps
        )
    }
    step_graph
}

add_step_graph_attrs <- function(step_graph, step_list, step_deps) {
    nodes <- names(igraph::V(step_graph))

    # add is_finished attribute to nodes
    is_finished <- vapply(nodes, function(node) { # nolint styler: off
        step <- step_list[[node]]
        if (is_step(step)) {
            return(step$finished)
        } else if (is.null(step)) {
            return(NA)
        }
    }, logical(1L), USE.NAMES = FALSE)
    is_existed <- nodes %in% names(step_list) # nolint

    # define step levels
    levels <- define_step_levels(step_deps = step_deps)[nodes] # nolint

    for (attr in c("is_finished", "is_existed", "levels")) {
        step_graph <- igraph::set_vertex_attr(
            step_graph,
            name = attr,
            value = rlang::env_get(rlang::current_env(), nm = attr)
        )
    }
    step_graph
}

#' Calculate the step levels.
#' @param ids The step name to get the level
#' @param step_deps A list of dependencies for each steps.
#' @return A named interger of the same length of names indicates the levels of
#' the step.
#' @noRd
#' @keywords internal
define_step_levels <- function(ids = NULL, step_deps) {
    if (is.null(ids)) {
        ids <- names(step_deps)
    } else {
        missed_ids <- setdiff(ids, names(step_deps))
        if (length(missed_ids)) {
            cli::cli_abort(
                "Can't find {.field {missed_ids}} in {.var step_tree}"
            )
        }
    }
    levels <- structure(vector("list", length(ids)), names = ids)

    #' 1. if a dependence is not in `names(step_deps)`, which occured when the
    #'    dependence is not in the `step_tree`, then an `NA` value is returned.
    #' 2. if a step has a dependency `NULL`, it should be the first level.
    #' 3. Otherwise, the level of a step should be the max level among its
    #'    parent step nodes adding 1L.
    define_level <- function(id) {
        if (!id %in% names(step_deps)) {
            # `name` may contain the dependencies (parent step node) not in the
            # step tree, in this way, we just return NA_integer_ value for the
            #  the definition of this child step node.
            value <- NA_integer_
        } else {
            value <- levels[[id]]
            # if NULL, level is not defined for this step
            # we define the level based on its dependencies
            if (is.null(value)) {
                deps <- step_deps[[id]]
                # if dependency is NA, which means no dependencies, this
                # indicates this step is the root, 1L should be returned.
                if (identical(length(deps), 1L) && is.na(deps)) {
                    value <- 1L
                } else {
                    deps_levels <- vapply(deps, define_level, integer(1L),
                        USE.NAMES = FALSE
                    )
                    value <- max(deps_levels, na.rm = FALSE) + 1L
                }
                levels[[id]] <<- value
            }
        }
        # always return the value, in this way, we can recursively in depth
        # across the dependencies tree.
        value
    }
    for (id in ids) {
        define_level(id = id)
    }
    unlist(levels, recursive = FALSE, use.names = TRUE)
}
