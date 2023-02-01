#' Run step
#'
#' @param step A step object.
#' @param mask The environment used as data mask.
#' @param pipeline The pipeline object.
#' @param envir The environment in which to evaluate step.
#' @noRd
eval_step <- function(step, mask, pipeline = NULL, envir = parent.frame()) {
    # if there is any seed, we set seed and then restore the existed seed in the
    # globalenv()
    if (isTRUE(step$seed) || is.numeric(step$seed)) {
        if (isTRUE(step$seed)) {
            seed <- digest::digest2int(
                digest::digest(step$expr, "crc32"),
                seed = 0L
            )
        } else {
            seed <- as.integer(step$seed)
        }
        old_seed <- rlang::env_get(globalenv(), ".Random.seed", default = NULL)
        if (is.null(old_seed)) {
            on.exit(rm(
                list = ".Random.seed", envir = globalenv(), inherits = FALSE
            ))
        } else {
            on.exit(assign(
                x = ".Random.seed", value = old_seed, pos = globalenv(),
                inherits = FALSE
            ))
        }
        set.seed(seed)
    }

    # use the environment attached in private as the mask
    mask <- rlang::new_data_mask(mask)
    mask$.data <- rlang::as_data_pronoun(mask)

    # install .pipeline referring to the Pipeline itself (R6 Pipeline)
    # A R6 object is just a environment, we can use it directly as a pronoun
    if (!is.null(pipeline)) {
        mask$.pipeline <- rlang::as_data_pronoun(pipeline)
    }

    # install .step referring to the step itself
    mask$.step <- rlang::as_data_pronoun(unclass(step))

    # then we run the command attached with this step
    rlang::eval_tidy(step$expr, data = mask, env = envir)
}

#' @param step_param User provided step components to modify the default step
#' items.
#' @param default The default components for this step.
#' @noRd
build_step <- function(id, expr, step_param, default,
                       arg = rlang::caller_arg(step_param),
                       call = rlang::parent.frame()) {
    if (!all(has_names(step_param))) {
        cli::cli_abort(
            "All items in {.arg {arg}} must be named",
            call = call
        )
    }
    step_param <- modify_list(default, step_param)
    rlang::inject(create_step(id = id, expr = expr, !!!step_param))
}

#' @param x A function argument from other function..
#' @return A quosure object if it is not a missing defused argument, otherwise,
#' a symbol of the argument name.
#' @noRd
quo_or_symbol <- function(x) {
    x_symbol <- substitute(x)
    x_quo <- rlang::eval_bare(
        rlang::expr(rlang::enquo(!!x_symbol)),
        env = rlang::parent.frame()
    )
    if (rlang::quo_is_missing(x_quo)) x_symbol else x_quo
}

#' @keywords internal
#' @noRd
sub_step_graph_helper <- function(step_graph, to = NULL, from = NULL, ids = NULL) {
    args <- c("ids", "to", "from")
    supplied_args <- args[
        !vapply(list(ids, to, from),
            is.null, logical(1L),
            USE.NAMES = FALSE
        )
    ]
    arg_len <- length(supplied_args)
    if (arg_len == 0L) {
        return(step_graph)
    } else if (arg_len > 1L) {
        cli::cli_abort(c(
            "Only one of {.arg {args}} or none can be supplied",
            "i" = "You have supplied {arg_len}: {.arg {supplied_args}}"
        ))
    } else {
        ids <- switch(supplied_args,
            ids = ids,
            to = igraph::subcomponent(
                step_graph,
                v = to, mode = "in"
            ),
            from = igraph::subcomponent(
                step_graph,
                v = from, mode = "out"
            )
        )
        return(igraph::subgraph(step_graph, vids = ids))
    }
}

#' @keywords internal
#' @noRd
build_step_graph_helper <- function(step_list, add_attrs = FALSE) {
    if (!length(step_list)) {
        return(igraph::make_empty_graph(directed = TRUE))
    }
    step_deps <- lapply(step_list, function(x) {
        deps <- unique(x$deps)
        deps <- deps[!is.na(deps) & deps != ""]
        if (!length(deps)) {
            return(NA_character_)
        }
        deps
    })
    edge_df <- data.frame(
        from = unlist(step_deps, use.names = FALSE),
        to = rep(names(step_deps), lengths(step_deps))
    )

    # extract all node names
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
    step_levels <- define_step_levels(
        ids = names(step_deps),
        step_deps = step_deps
    )
    step_levels <- step_levels[nodes]
    for (attr in c("is_finished", "is_existed", "step_levels")) {
        step_graph <- igraph::set_vertex_attr(
            step_graph,
            name = attr,
            value = get(x = attr, pos = environment(), inherits = FALSE)
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
    levels <- structure(vector("list", length(ids)), names = ids)

    #' 1. if a dependence is not in `names(step_deps)`, which occured when the
    #'    dependence is not in the `step_tree`, then an `NA` value is returned.
    #' 2. if a step has a dependency `NULL`, it should be the first level.
    #' 3. Otherwise, the level of a step should be the max level among its
    #'    parent step nodes adding 1L.
    define_level <- function(id) {
        if (any(id == names(step_deps))) {
            value <- levels[[id]]
            # if NULL, level is not defined for this step
            # we define the level based on its dependencies
            if (is.null(value)) {
                deps <- step_deps[[id]]
                # if dependency is NA, which means no dependencies, this
                # indicates this step is the root, 1L should be returned.
                if (length(deps) == 1L && is.na(deps)) {
                    value <- 1L
                } else {
                    deps_levels <- vapply(deps, define_level, integer(1L),
                        USE.NAMES = FALSE
                    )
                    value <- max(deps_levels, na.rm = FALSE) + 1L
                }
                levels[[id]] <<- value
            }
        } else {
            # `step` may contain the dependencies (parent step node) not in the
            # step tree, in this way, we just return NA_integer_ value for  this
            # child step node.
            value <- NA_integer_
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

# this will also modify the underlying call, substitute the result
# symbol of the "from" step to the result symbol of "to" step.
# if you just want to modify deps but not change the underlying call
# object, try to use `$modify_step` method.
# switch_step_dep = function(id, from, to, force = FALSE) {
#     assert_scalar_chr(id)
#     private$assert_id_exist(id)
#     step <- private$step_tree[[id]]
#     symbol_list <- vector("list", 2L)
#     # check from and to arguments
#     for (i in 1:2) {
#         arg <- c("from", "to")[[i]]
#         name <- list(from, to)[[i]]
#         private$check_id(name, exist = FALSE)
#         if (!name %in% step$deps) {
#             if (isTRUE(force)) {
#                 symbol_list[[i]] <- name
#             } else {
#                 cli::cli_abort(c(
#                     "{.arg {arg}} must exist in the {.var step_tree]",
#                     i = "You can force to proceed by enable {.arg force} where {.arg {arg}} will be regarded as the step returned result symbol" # nolint
#                 ))
#             }
#         } else {
#             symbol_list[[i]] <- self$get_return_name(id = name)
#             if (is.null(symbol_list[[i]])) {
#                 cli::cli_abort("{.arg {arg}} has no returned result symbol")
#             }
#         }
#     }
#     # change the dependency
#     step$deps <- union(setdiff(step$deps, from), to)
#     # change the call by transforming the symbol returned by "from" step
#     # into symbol returned by "to" step
#     step$expr <- rlang::set_expr(
#         step$expr,
#         value = change_expr(
#             step$expr,
#             from = rlang::sym(symbol_list[[1L]]),
#             to = rlang::sym(symbol_list[[2L]])
#         )
#     )
#     private$step_tree[[id]] <- step
#     invisible(self)
# }
