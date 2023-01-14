#' R6 Class Representing Pipeline
#'
#' @description A Pipeline object, which is bound with a step_tree and a
#'   environment, provides methods to run the steps in the attached environment.
#'   In this way, we can bind all steps used in a pipeline and their dependant
#'   evaluation frame in a object.
#' @param id A scalar character of the step name.
#' @param step A [step] object.
#' @param reset If `TRUE`, will label all downstream steps (depend on current
#' step) as unfinished.
#' @param refresh If `TRUE`, the step will be evaluated no matter wether the
#' step has been finished or not. Otherwise the step will only be evaluated if
#' it has never been evaluated once, in which case, the result will be obtained
#' directly from the last evaluated result.
#' @param ...  <[`dynamic-dots`][rlang::dyn-dots]> all items must be a `step`
#'   object without name. steps must be unique with no duplicated ids. Can also
#'   provide as a list of steps directly.
#' @param to The step to start the search to create the step dependencies graph.
#'   If `to` is specified, all steps from which the (to) step is reachable are
#'   extracted. Only one or none of "to", "from", "ids" can be specified.
#' @param from The step to start the search to create the step dependencies
#'   graph. If `from` is specified, all steps reachable from the (from) step are
#'   extracted. Only one or none of "to", "from", "ids" can be specified.
#' @param ids Should be an atomic character, the steps used to form the step
#' dependencies graph, this is passed into the `vids` argument of
#' [subgraph][igraph::subgraph]. Only one or none of "to", "from", "ids" can be
#' specified. If all "ids", "from" and "to" arguments are `NULL`, the whole
#' step_tree in the pipeline will be used to create the dependencies graph.
Pipeline <- R6::R6Class("Pipeline",
    public = list(
        #' @description
        #' Create a new person object.
        #' @param data A data list to build the attached environment which is
        #'   used to evaluate the variable in the step expression object. One
        #'   can also use `$env_bind` method to add new variable.
        #' @return A new `Pipeline` object.
        initialize = function(..., data = list()) {
            private$envir <- rlang::new_environment(data = data)
            self$set_step_tree(...)
            invisible(self)
        },

        #' @description
        #' Set the step tree in the `Pipeline` object
        set_step_tree = function(...) {
            private$step_tree <- step_tree(...)
            invisible(self)
        },

        #' @description
        #' Get a single step in the `Pipeline` step_tree.
        #' @return A step object.
        get_step = function(id) {
            assert_class(id, is.character, class = "character", null_ok = FALSE)
            assert_length(id, 1L, null_ok = FALSE)
            private$assert_ids_exist(id)
            private$step_tree[[id]]
        },

        #' @description
        #' Get multiple steps in the `Pipeline` step_tree.
        #' @param ids A atomic character of the step name. if `NULL`, all steps
        #'   will be extracted.
        #' @return A step_tree object.
        get_steps = function(ids = NULL) {
            assert_class(ids, is.character,
                class = "character", null_ok = TRUE
            )
            if (!is.null(ids)) {
                private$assert_ids_exist(ids)
                private$step_tree[ids]
            } else {
                private$step_tree
            }
        },

        #' @description
        #' Add a new step in the `Pipeline` step_tree.
        add_step = function(step, reset = TRUE) {
            id <- step$id
            if (id %in% names(private$step_tree)) {
                cli::cli_warn(c(
                    "!" = "Override the existed {.field step}: {.val {id}}"
                ))
            }
            private$step_tree[[id]] <- step
            if (isTRUE(reset)) {
                private$reset_downstream_internal(id = id)
            }
            invisible(self)
        },
        #' @description
        #' Add new steps in the `Pipeline` step_tree.
        #' @param ...  <[`dynamic-dots`][rlang::dyn-dots]> all items must be a
        #'   `step` object without name. Any duplicated steps (with duplicated
        #'   id) are only included once, the latter one will override the
        #'   former. Can also provide as a list of steps directly.
        add_steps = function(..., reset = TRUE) {
            rlang::check_dots_unnamed()
            dots <- rlang::dots_list(..., .named = NULL)
            if (length(dots) == 1L && !is_step(dots[[1L]]) && is.list(dots[[1L]])) {
                dots <- dots[[1L]]
            }
            for (i in seq_along(dots)) {
                self$add_step(dots[[i]], reset = reset)
            }
            invisible(self)
        },

        #' @description
        #' Remove steps in the `Pipeline` step_tree.
        #' @param ids An atomic character, the steps to remove from the
        #' `Pipeline`.
        remove_steps = function(ids, reset = TRUE) {
            assert_class(ids, is.character,
                class = "character",
                null_ok = FALSE
            )
            private$assert_ids_exist(ids)
            for (id in ids) {
                if (isTRUE(reset)) {
                    private$reset_downstream_internal(id = id)
                }
                private$step_tree[[id]] <- NULL
            }
            invisible(self)
        },

        #' @description Label the step as unfinished. If downstream is `TRUE`,
        #'   will also label all steps depending on this step as unfinished.
        #' @param downstream If `TRUE`, will label all downstream steps (depend
        #' on current step) as unfinished.
        reset_step = function(id, downstream = TRUE) {
            assert_class(id, is.character, class = "character", null_ok = FALSE)
            assert_length(id, 1L, null_ok = FALSE)
            private$assert_ids_exist(id)
            private$reset_step_internal(id = id)
            if (downstream) {
                private$reset_downstream_internal(id = id)
            }
            invisible(self)
        },

        #' @description
        #' Label all steps in the step_tree as unfinished.
        reset_step_tree = function() {
            ids <- names(private$step_tree)
            for (id in ids) {
                private$reset_step_internal(id = id)
            }
            invisible(self)
        },

        #' @description
        #' Label the step as finished.
        finish_step = function(id) {
            assert_class(id, is.character, class = "character", null_ok = FALSE)
            assert_length(id, 1L, null_ok = FALSE)
            private$assert_ids_exist(id)
            private$finish_step_internal(id)
            invisible(self)
        },
        #' @description
        #' Modify the step components.
        #' @param ... <[`dynamic-dots`][rlang::dyn-dots]>, A named value paris
        #'   indicates the components to replace in the `step`. `NULL` will be
        #'   kept, use [`zap()`] to remove a component from the step.
        modify_step = function(id, ..., reset = TRUE) {
            assert_class(id, is.character, class = "character", null_ok = FALSE)
            assert_length(id, 1L, null_ok = FALSE)
            private$assert_ids_exist(id)
            step <- private$step_tree[[id]]
            dots <- check_dots_named(...)
            step <- validate_step(new_step(modify_list(unclass(step), dots)))
            private$step_tree[[id]] <- step
            if (isTRUE(reset)) {
                private$reset_downstream_internal(id = id)
            }
            invisible(self)
        },

        #' @description
        #' If the expression in the step is a call object, this provide a
        #' convenient way to modify the call argument.
        #' @param ... <[`dynamic-dots`][rlang::dyn-dots]>, Named or unnamed
        #'   expressions (constants, names or calls) used to modify the call.
        #'   Use [`zap()`] to remove arguments. Empty arguments are preserved.
        modify_call = function(id, ..., reset = TRUE) {
            assert_class(id, is.character, class = "character", null_ok = FALSE)
            assert_length(id, 1L, null_ok = FALSE)
            private$assert_ids_exist(id)
            step <- private$step_tree[[id]]
            if (!rlang::is_call(step$expression)) {
                cli::cli_abort(c(
                    "the expression of the step must be a {.cls call} object",
                    "x" = "You've supplied a step with a {.cls {typeof(step$expression)}} expression"
                ))
            }
            dots <- rlang::dots_list(...)
            if (length(dots)) {
                step$expression <- rlang::set_expr(
                    step$expression,
                    call_standardise(step$expression)
                )
                step$expression <- rlang::set_expr(
                    step$expression,
                    rlang::call_modify(step$expression, !!!dots)
                )
                private$step_tree[[id]] <- step
                if (isTRUE(reset)) {
                    private$reset_downstream_internal(id = id)
                }
            }
            invisible(self)
        },

        #' @description Build step dependencies network as an graph object
        #' @param add_attrs If `TRUE`, add "is_finished", "is_existed" and
        #'   "levels" as the graph vertex attributes. Default: `FALSE`
        #' @return A igraph object.
        get_step_graph = function(to = NULL, from = NULL, ids = NULL, add_attrs = FALSE) {
            # assert to argument
            assert_class(to, is.character, class = "character", null_ok = TRUE)
            assert_length(to, 1L, null_ok = TRUE)
            if (!is.null(to)) {
                private$assert_ids_exist(id)
            }

            # assert from argument
            assert_class(from, is.character,
                class = "character", null_ok = TRUE
            )
            assert_length(from, 1L, null_ok = TRUE)
            if (!is.null(from)) {
                private$assert_ids_exist(from)
            }

            # assert ids argument
            assert_class(ids, is.character,
                class = "character", null_ok = TRUE
            )
            if (!is.null(ids)) {
                private$assert_ids_exist(ids)
            }

            # build step graph
            private$build_step_graph(
                to = to, from = from, ids = ids,
                add_attrs = add_attrs
            )
        },

        #' @description
        #'  Plot the step dependencies tree.
        #' @param layout Gives the layout of the graphs.
        #' @param ... Other arguments passed to [`plot`][igraph::plot.igraph].
        plot_step_graph = function(to = NULL, from = NULL, ids = NULL, layout = igraph::layout_as_tree, ...) {
            step_graph <- self$get_step_graph(
                to = to, from = from, ids = ids, add_attrs = TRUE
            )
            plot(step_graph, layout = layout, ...)
        },

        #' @description
        #' Running the step
        #'
        #' @param id Name of the step object.
        #' @param envir The environment in which to evaluate the `expression` in
        #'   the step.
        run_step = function(id, refresh = FALSE, reset = FALSE, envir = rlang::caller_env()) {
            assert_class(id, is.character, class = "character", null_ok = FALSE)
            assert_length(id, 1L, null_ok = FALSE)
            private$assert_ids_exist(id)
            private$run_step_internal(
                id = id, refresh = refresh,
                reset = reset,
                envir = envir
            )
        },
        #' @description
        #' Running the steps until the target step
        #' @param targets <[`tidy-select`][tidyselect::language]>, A set of
        #'   targeted steps until which to run.
        #' @param envir The environment in which to evaluate the `expression` in
        #'   the step.
        run_targets = function(targets = NULL, refresh = FALSE, reset = FALSE, envir = rlang::caller_env()) {
            # build dependencies graph
            step_graph <- private$build_step_graph(add_attrs = TRUE)
            attrs <- igraph::vertex_attr(step_graph)[c("name", "levels")]
            step_levels <- structure(attrs$levels, names = attrs$name)

            # targets are the steps we want to run until if NULL, all steps
            # without child steps will be used
            step_list <- unclass(private$step_tree)
            targets <- rlang::enquo(targets)
            if (rlang::quo_is_null(targets)) {
                targets <- names(igraph::V(step_graph))[
                    igraph::degree(step_graph, mode = "out") == 0L
                ]
            } else {
                targets <- tidyselect::eval_select(targets, data = step_list)
                targets <- names(step_list)[targets]
            }
            results_list <- structure(
                vector("list", length(targets)),
                names = targets
            )

            for (target in targets) {
                # extract the deps_graph of each targets step
                target_deps <- igraph::subcomponent(step_graph,
                    v = target, mode = "in"
                )
                target_deps <- names(target_deps)

                # Output targets information
                cli_par_id <- cli::cli_par()
                cli::cli_rule(center = "Runing target: {.field {target}}")
                cli::cli_inform(
                    "Including {.val {length(target_deps)}} step{?s}"
                )

                # check if all dependencies exist in step_tree
                missing_ids <- setdiff(target_deps, names(private$step_tree))
                if (length(missing_ids)) {
                    cli::cli_abort(c(
                        "Can't find all {.field steps} in the {.var step_tree}",
                        x = "Missing {.val {length(missing_ids)}} id{?s}: {.val {missing_ids}}"
                    ))
                }

                # since levels is integer, we simply factor it so split can sort
                # the list in the order of the levels
                step_ids_list <- split(
                    target_deps,
                    factor(step_levels[target_deps])
                )
                lid <- cli::cli_ul()
                results_list[[target]] <- imap(step_ids_list, function(ids, level) {
                    cli_list(
                        label = sprintf("level %s", level),
                        items = ids,
                        add_num = TRUE
                    )
                    lapply(ids, function(id) {
                        private$run_step_internal(
                            id = id, refresh = refresh,
                            reset = reset,
                            envir = envir
                        )
                    })
                })
                cli::cli_end(lid)
                cli::cli_end(cli_par_id)
            }
            invisible(results_list)
        },

        # methods for the operation in the attached environment
        #' @description
        #' Get single variable from the environment
        #' @param nm Name of binding, a string.
        env_get = function(nm) {
            rlang::env_get(env = private$envir, nm = nm, inherit = FALSE)
        },
        #' @description
        #' Get multiple variable from the environment
        #' @param nms Names of bindings, a character vector.
        env_get_list = function(nms) {
            rlang::env_get_list(env = private$envir, nms = nms, inherit = FALSE)
        },
        #' @description
        #' Bind symbols to object in the attached environment.
        #' @param ... <[`dynamic-dots`][rlang::dyn-dots]>, Named objects
        #'   (env_bind()). Use zap() to remove bindings.
        env_bind = function(...) {
            rlang::env_bind(.env = private$envir, ...)
            invisible(self)
        },
        #' @description
        #' Remove bindings from the attached environment.
        #' @param nms A character vector of binding names to remove.
        env_unbind = function(nms) {
            rlang::env_unbind(.env = private$envir, nms = nms)
            invisible(self)
        },
        #' @description
        #' Move variable from a environment to the attached environment
        #' @param variable A single symbol.
        env_move = function(variable) {
            variable <- rlang::enquo(variable)
            if (rlang::quo_is_symbol(variable)) {
                nm <- rlang::as_name(variable)
            } else {
                cli::cli_abort(
                    "Provided {.arg variable} must be defused as a simple {.cls symbol}."
                )
            }
            self$env_bind(!!nm := rlang::eval_tidy(variable))
            rlang::env_unbind(rlang::quo_get_env(variable), nms = nm)
            invisible(self)
        },
        #' @description Names of symbols bound in the attached environment
        env_names = function() rlang::env_names(private$envir)
    ),

    # all object used to run step command should live in `envir`
    # all methods defined in private environment are not necessary to check the
    # arguments.
    private = list(
        step_tree = NULL, envir = NULL,

        ## For the usage of asserting the public method arguments
        assert_ids_exist = function(ids, arg = rlang::caller_arg(ids), call = rlang::caller_env()) {
            missing_ids <- setdiff(ids, names(private$step_tree))
            if (length(missing_ids)) {
                cli::cli_abort(c(
                    "Provided {.arg {arg}} must exist in the {.var step_tree}",
                    x = "Missing ids: {.val {missing_ids}}"
                ), call = call)
            }
        },

        ## step utils
        ### label step as finished
        finish_step_internal = function(id) {
            private$step_tree[[id]]$finished <- TRUE
        },

        ### label step (including downstream steps depend on it) as unfinished
        reset_step_internal = function(id) {
            private$step_tree[[id]]$finished <- FALSE
        },
        reset_downstream_internal = function(id) {
            downstream_steps <- private$build_step_graph(
                from = id, add_attrs = FALSE
            )
            reset_ids <- setdiff(names(igraph::V(downstream_steps)), id)
            for (reset_id in reset_ids) {
                if (reset_id %in% names(private$step_tree)) {
                    private$reset_step_internal(reset_id)
                }
            }
        },

        ### run the step and return the result
        run_step_internal = function(id, refresh = FALSE, reset = FALSE, envir = rlang::caller_env()) {
            step <- private$step_tree[[id]]
            # if this step has been finished, and refresh is FALSE
            # Just return the value from the environment
            if (step$finished && !isTRUE(refresh)) {
                if (step$return) {
                    return(rlang::env_get(private$envir, nm = id))
                } else {
                    return(NULL)
                }
            }
            result <- eval_step(step,
                mask = private$envir,
                pipeline = self,
                envir = envir
            )

            if (step$return) {
                self$env_bind(!!id := result)
            } else {
                result <- NULL
            }
            private$finish_step_internal(id)

            if (isTRUE(reset)) {
                private$reset_downstream_internal(id = id)
            }
            result
        },
        ### Build step dependencies network as an graph object
        build_step_graph = function(to = NULL, from = NULL, ids = NULL, add_attrs = FALSE) {
            if (!length(private$step_tree)) {
                return(NULL)
            }
            step_list <- unclass(private$step_tree)
            step_graph <- build_step_graph_helper(
                step_list,
                add_attrs = add_attrs
            )
            sub_step_graph(step_graph, to = to, from = from, ids = ids)
        }
    )
)

`+.Pipeline` <- function(x, y) {
    if (missing(y)) {
        cli::cli_abort(c(
            "Cannot use {.code +} with a single argument",
            "i" = "Did you accidentally put {.code +} on a new line?"
        ))
    }
    # Get the name of what was passed in as y, and pass along so that it
    # can be displayed in error messages
    yname <- deparse(substitute(y)) # nolint

    if (inherits(x, "Pipeline") && R6::is.R6(x)) {
        if (is.list(y)) {
            x$add_steps(!!!y, reset = TRUE)
        } else if (is_step_tree(y)) {
            x$add_steps(!!!unclass(y), reset = TRUE)
        } else if (is_step(y)) {
            x$add_step(y, reset = TRUE)
        } else {
            cli::cli_abort(
                "Can't add {.var {yname}} to a {.cls Pipeline} R6 object."
            )
        }
    } else if (is_step(x) || is_step_tree(x)) {
        cli::cli_abort(c(
            "Cannot add {.cls step} objects together",
            "i" = "Did you forget to add this object to a {.cls Pipeline} R6 object?"
        ))
    }
    x
}
