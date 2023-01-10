#' R6 Class Representing Pipeline
#'
#' @description A Pipeline object, which is bound with a step_tree and a
#'   environment, provides methods to run a the steps in the attached
#'   environment.
#' @param id A scalar character of the step name.
#' @param step A [step] object.
#' @param reset A scalar logical value indicates if labelling all downstream
#' steps as unfinished.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]>, all items must be a `step`
#' object.
Pipeline <- R6::R6Class("Pipeline",
    public = list(
        #' @description
        #' Create a new person object.
        #' @param ...  <[`dynamic-dots`][rlang::dyn-dots]> `step` object used to
        #' create `Pipeline` step tree. Must be unnamed.
        #' @param data A data list used to evaluate the variable in the step
        #' call object.
        #' @return A new `Pipeline` object.
        initialize = function(..., data = list()) {
            private$envir <- rlang::new_environment(data = data)
            self$env_bind(self = self)
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
        #' Get the step in the `Pipeline` step_tree.
        get_step = function(id) {
            assert_scalar_chr(id)
            private$assert_id_exist(id)
            private$step_tree[[id]]
        },
        #' @description
        #' Change the step in the `Pipeline` step_tree.
        set_step = function(step, reset = TRUE) {
            private$step_tree[[step$id]] <- step
            if (isTRUE(reset)) {
                private$reset_step_internal(id = id, downstream = TRUE)
            }
            invisible(self)
        },
        #' @description
        #' Change the steps in the `Pipeline` step_tree.
        set_steps = function(..., reset = TRUE) {
            rlang::check_dots_unnamed()
            dots_list <- rlang::dots_list(...)
            for (i in seq_along(dots_list)) {
                self$set_step(dots_list[[i]], reset = reset)
            }
            invisible(self)
        },
        #' @description
        #' Add a new step in the `Pipeline` step_tree.
        add_step = function(step, reset = TRUE) {
            id <- step$id
            if (id %in% names(private$step_tree)) {
                cli::cli_abort(c(
                    "Already existed {.field step}: {.val {id}}",
                    "i" = "use {.fn set_step} method if you want to override it." # nolint
                ))
            }
            self$set_step(step = step, reset = reset)
            invisible(self)
        },
        #' @description
        #' Add new steps in the `Pipeline` step_tree.
        add_steps = function(..., reset = TRUE) {
            rlang::check_dots_unnamed()
            dots_list <- rlang::dots_list(...)
            for (i in seq_along(dots_list)) {
                self$add_step(dots_list[[i]], reset = reset)
            }
            invisible(self)
        },
        #' @description
        #' Label the step as unfinished. If downstream is `TRUE`, will also
        #' label all steps depending on this step as unfinished.
        #' @param downstream A logical value indicates whether resetting
        #'   downstream steps.
        reset_step = function(id, downstream = TRUE) {
            assert_scalar_chr(id)
            private$assert_id_exist(id)
            private$reset_step_internal(id = id, downstream = downstream)
            invisible(self)
        },
        #' @description
        #' Label the step as finished.
        finish_step = function(id) {
            assert_scalar_chr(id)
            private$assert_id_exist(id)
            private$finish_step_internal(id)
            invisible(self)
        },
        #' @description
        #' Modify the step.
        #' @param ... <[`dynamic-dots`][rlang::dyn-dots]>, A named list with
        #'   components to replace corresponding components in the `step`.
        #'   `NULL` will be kept in the step object, use [`zap()`] to remove a
        #'   component in the step.
        modify_step = function(id, ..., reset = TRUE) {
            assert_scalar_chr(id)
            private$assert_id_exist(id)
            step <- private$step_tree[[id]]
            step <- new_step(modify_list(unclass(step), ...))
            private$step_tree[[id]] <- step
            if (isTRUE(reset)) {
                private$reset_step_internal(id = id, downstream = TRUE)
            }
            invisible(self)
        },

        #' @description
        #' Modify the step call components.
        #' @param ... <[`dynamic-dots`][rlang::dyn-dots]>, Named or unnamed
        #'   expressions (constants, names or calls) used to modify the call.
        #'   Use [`zap()`] to remove arguments. Empty arguments are preserved.
        modify_call = function(id, ..., reset = TRUE) {
            assert_scalar_chr(id)
            private$assert_id_exist(id)
            step <- private$step_tree[[id]]
            dots_list <- rlang::dots_list(...)
            if (length(dots_list)) {
                step$call <- rlang::set_expr(
                    step$call,
                    call_standardise(step$call)
                )
                step$call <- rlang::set_expr(
                    step$call,
                    rlang::call_modify(step$call, !!!dots_list)
                )
                private$step_tree[[id]] <- step
                if (isTRUE(reset)) {
                    private$reset_step_internal(id = id, downstream = TRUE)
                }
            }
            invisible(self)
        },
        #' @description
        #'  Plot the step dependencies tree.
        #' @param to,from The step to start the search to create the step
        #'   dependencies graph. If `to` is specified, all steps from which step
        #'   (to) is reachable are extracted. If `from` is specified, all steps
        #'   reachable from step (from) are extracted. If both are specified,
        #'   only `to` is used.
        #' @param layout Gives the layout of the graphs.
        #' @param ... Other arguments passed to [`plot`][igraph::plot.igraph].
        plot_step_graph = function(to = NULL, from = NULL, layout = igraph::layout_as_tree, ...) {
            step_graph <- private$build_step_graph(
                to = to, from = from, add_attrs = TRUE
            )
            plot(step_graph, layout = layout, ...)
        },

        #' @description
        #' Running the step
        #'
        #' @param id Name of the step object.
        #' @param refresh A scalar logical value indicates if we should run step
        #' even it has been finished.
        #' @param envir The environment in which to evaluate the `call` in the
        #'   step.
        run_step = function(id, refresh = FALSE, reset = TRUE, envir = rlang::caller_env()) {
            assert_scalar_chr(id)
            private$assert_id_exist(id)
            private$run_step_internal(
                id = id, refresh = refresh,
                reset = reset,
                envir = envir
            )
        },
        #' @description
        #' Running the steps until the target step
        #' @param targets <[`tidy-select`][tidyselect::language]>, A set of
        #'   target steps until which to run.
        #' @param refresh A scalar logical value indicates if we should run step
        #' even it has been finished.
        #' @param envir The environment in which to evaluate the `call` in the
        #'   step.
        run_targets = function(targets = NULL, refresh = FALSE, reset = TRUE, envir = rlang::caller_env()) {
            step_list <- unclass(private$step_tree)

            # build dependencies graph
            step_graph <- private$build_step_graph(add_attrs = TRUE)
            attrs <- igraph::vertex_attr(step_graph)[c("name", "levels")]
            step_levels <- structure(attrs$levels, names = attrs$name)

            # targets are the steps we want to run until if NULL, all steps
            # without child steps will be used
            targets <- rlang::enquo(targets)
            if (is.null(rlang::get_expr(targets))) {
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
                private$check_step_deps(target_deps)

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
            rlang::env_unbind(rlang::quo_get_env(variable), nms = nm)
            self$env_bind(!!nm := rlang::eval_tidy(variable))
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
        is_id_exist = function(id) {
            id %in% names(private$step_tree)
        },
        assert_id_exist = function(id, arg = rlang::caller_arg(id), call = rlang::caller_env()) {
            missing_id <- setdiff(id, names(private$step_tree))
            if (length(missing_id)) {
                cli::cli_abort(
                    c(
                        "{.arg {arg}} must exist in the {.var step_tree}",
                        x = "Missing ids: {.val {missing_id}}"
                    ),
                    call = call
                )
            }
        },

        ## step utils
        ### label step as finished
        finish_step_internal = function(id) {
            private$step_tree[[id]]$finished <- TRUE
        },

        ### label step (including downstream steps depend on it) as unfinished
        reset_step_internal = function(id, downstream = TRUE) {
            if (isTRUE(downstream)) {
                downstream_steps <- private$build_step_graph(
                    from = id, add_attrs = FALSE
                )
                reset_ids <- names(igraph::V(downstream_steps))
            } else {
                reset_ids <- id
            }
            for (reset_id in reset_ids) {
                if (private$is_id_exist(reset_id)) {
                    private$step_tree[[reset_id]]$finished <- FALSE
                }
            }
        },

        ### run the step and return the result
        run_step_internal = function(id, refresh = FALSE, reset = TRUE, envir = rlang::caller_env()) {
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
                self = self, private = private,
                envir = envir
            )

            if (step$return) {
                self$env_bind(!!id := result)
            } else {
                result <- NULL
            }

            if (isTRUE(reset)) {
                private$reset_step_internal(
                    id = id, downstream = TRUE
                )
            }
            private$finish_step_internal(id)
            result
        },

        # abort if any dependency is non-exist in step_tree
        check_step_deps = function(deps = NULL) {
            if (is.null(deps)) {
                deps <- extract_step_deps(unclass(private$step_tree))
                deps <- unlist(deps, recursive = FALSE, use.names = FALSE)
            }
            missing_deps <- setdiff(deps, names(private$step_tree))
            if (length(missing_deps)) {
                cli::cli_abort(c(
                    "Can't find all dependencies in the {.var step_tree}",
                    x = "Missing {.val {length(missing_deps)}} dependenc{?y/ies}: {.val {missing_deps}}"
                ))
            }
        },

        # build step dependencies network as an graph object
        build_step_graph = function(to = NULL, from = NULL, add_attrs = FALSE) {
            step_list <- unclass(private$step_tree)
            if (is.null(step_list)) {
                return(NULL)
            }
            step_graph <- build_step_graph_helper(
                step_list,
                add_attrs = add_attrs
            )

            if (is.null(to) && is.null(from)) {
                return(step_graph)
            } else {
                if (!is.null(from) && !is.null(to)) {
                    cli::cli_warn(c(
                        "Both {.arg from} and {.arg to} are setted.",
                        "!" = "Will only use {.arg to}."
                    ))
                }
                if (!is.null(to)) {
                    graph_ids <- igraph::subcomponent(step_graph,
                        v = to, mode = "in"
                    )
                } else if (!is.null(from)) {
                    graph_ids <- igraph::subcomponent(step_graph,
                        v = from, mode = "out"
                    )
                }
                return(igraph::subgraph(step_graph, vids = graph_ids))
            }
        }
    )
)
