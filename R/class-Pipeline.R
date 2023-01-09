# R pipeline
Pipeline <- R6::R6Class("Pipeline",
    public = list(
        initialize = function(..., data = list(), envir = NULL) {
            if (is.null(envir)) {
                private$envir <- rlang::new_environment(data = data)
            } else if (rlang::is_environment(envir)) {
                private$envir <- envir
            } else {
                cli::cli_abort("{.arg envir} must be a {.cls environment} or {.val NULL}")
            }
            self$set_step_tree(...)
        },

        #' methods for step_tree operation
        set_step_tree = function(...) {
            private$step_tree <- step_tree(...)
            invisible(self)
        },
        get_step = function(id) {
            assert_scalar_chr(id)
            private$assert_id_exist(id)
            private$step_tree[[id]]
        },
        set_step = function(id, ..., reset = TRUE) {
            assert_scalar_chr(id)
            private$step_tree[[id]] <- step(...)
            if (isTRUE(reset)) {
                private$reset_step(id = id, downstream = TRUE)
            }
            invisible(self)
        },
        add_step = function(id, ..., reset = TRUE) {
            assert_scalar_chr(id)
            if (id %in% names(private$step_tree)) {
                cli::cli_abort(c(
                    "Already existed {.field step}",
                    i = "check {.arg id} argument or use {.fn set_step} method if you want to override it." # nolint
                ))
            }
            private$step_tree[[id]] <- step(...)
            if (isTRUE(reset)) {
                private$reset_step(id = id, downstream = TRUE)
            }
            invisible(self)
        },
        set_steps = function(..., reset = TRUE) {
            step_tree <- step_tree(...)
            for (id in names(step_tree)) {
                private$step_tree[[id]] <- step_tree[[id]]
                if (isTRUE(reset)) {
                    private$reset_step(id = id, downstream = TRUE)
                }
            }
            invisible(self)
        },
        add_steps = function(..., reset = TRUE) {
            step_tree <- step_tree(...)
            dup_ids <- intersect(names(step_tree), names(private$step_tree))
            if (length(dup_ids)) {
                cli::cli_abort(c(
                    "Already existed {.field steps}: {dup_ids}",
                    i = "use {.fn set_steps} method if you want to override them."
                ))
            }
            for (id in names(step_tree)) {
                private$step_tree[[id]] <- step_tree[[id]]
                if (isTRUE(reset)) {
                    private$reset_step(id = id, downstream = TRUE)
                }
            }
            invisible(self)
        },
        reset_step = function(id, downstream = TRUE) {
            assert_scalar_chr(id)
            private$assert_id_exist(id)
            private$reset_step(id = id, downstream = downstream)
            invisible(self)
        },
        finish_step = function(id) {
            assert_scalar_chr(id)
            private$assert_id_exist(id)
            private$finish_step(id)
            invisible(self)
        },
        modify_step = function(id, deps, finished, return, seed, call, reset = TRUE) {
            assert_scalar_chr(id)
            private$assert_id_exist(id)
            step <- private$step_tree[[id]]
            if (!missing(deps)) {
                step["deps"] <- list(deps)
            }
            if (!missing(finished)) {
                step["finished"] <- list(finished)
            }
            if (!missing(return)) {
                step["return"] <- list(return)
            }
            if (!missing(seed)) {
                step["seed"] <- list(seed)
            }
            if (!missing(call)) {
                step["call"] <- list(call)
            }
            private$step_tree[[id]] <- step
            if (isTRUE(reset)) {
                private$reset_step(id = id, downstream = TRUE)
            }
            invisible(self)
        },

        # modify call only change the call element of the step
        modify_call = function(id, ..., reset = TRUE) {
            assert_scalar_chr(id)
            private$assert_id_exist(id)
            step <- private$step_tree[[id]]
            step$call <- rlang::set_expr(
                step$call,
                call_standardise(step$call)
            )
            dots_list <- rlang::enquos(...)
            if (length(dots_list)) {
                step$call <- rlang::set_expr(
                    step$call,
                    rlang::call_modify(step$call, !!!dots_list)
                )
                private$step_tree[[id]] <- step
                if (isTRUE(reset)) {
                    private$reset_step(id = id, downstream = TRUE)
                }
            }
            invisible(self)
        },

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
        #     step$call <- rlang::set_expr(
        #         step$call,
        #         value = change_expr(
        #             step$call,
        #             from = rlang::sym(symbol_list[[1L]]),
        #             to = rlang::sym(symbol_list[[2L]])
        #         )
        #     )
        #     private$step_tree[[id]] <- step
        #     invisible(self)
        # },

        plot_step_graph = function(to = NULL, from = NULL, layout = igraph::layout_as_tree, ...) {
            step_graph <- private$build_step_graph(
                to = to, from = from, add_attrs = TRUE
            )
            plot(step_graph, layout = layout, ...)
        },

        #' Running step command in the attached environment
        #' @param id The name of step object.
        #' @param refresh A scalar logical value indicates if run step if it has
        #' been finished.
        #' @param reset A scalar logical value indicates.
        #' @param envir The environment in which to evaluate the `call` attached
        #'   in step.
        #' @keywords internal
        #' @noRd
        run_step = function(id, refresh = FALSE, reset = TRUE, envir = rlang::caller_env()) {
            assert_scalar_chr(id)
            private$assert_id_exist(id)
            private$run_step(
                id = id, refresh = refresh,
                reset = reset,
                envir = envir
            )
        },
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
                        private$run_step(
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

        #' methods for the operation in the attached environment
        #' Get single variable from the environment
        env_get = function(nm) {
            rlang::env_get(env = private$envir, nm = nm, inherit = FALSE)
        },
        #' Get multiple variable from the environment
        env_get_list = function(nms) {
            rlang::env_get_list(env = private$envir, nms = nms, inherit = FALSE)
        },
        env_bind = function(...) {
            rlang::env_bind(.env = private$envir, ...)
            invisible(self)
        },
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
        env_names = function() rlang::env_names(private$envir)
    ),

    # all object used to run step command should live in `envir`
    # By passing data into `data` argument of `eval_tidy`, we can implement data
    # mask
    private = list(
        step_tree = NULL, envir = NULL,
        # add methods for the usage of public methods
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
        finish_step = function(id) {
            private$step_tree[[id]]$finished <- TRUE
        },
        reset_step = function(id, downstream = TRUE) {
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
        run_step = function(id, refresh = FALSE, reset = TRUE, envir = rlang::caller_env()) {
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

            # if there is any seed, we set seed
            if (isTRUE(step$seed) || rlang::is_scalar_integer(step$seed) || rlang::is_scalar_double(step$seed)) {
                if (isTRUE(step$seed)) {
                    seed <- digest::digest2int(
                        digest::digest(step$call, "crc32"),
                        seed = 0L
                    )
                } else {
                    seed <- as.integer(step$seed)
                }
                old_seed <- rlang::env_get(
                    globalenv(), ".Random.seed",
                    default = NULL
                )
                if (is.null(old_seed)) {
                    on.exit(rm(".Random.seed", envir = globalenv()))
                } else {
                    on.exit(
                        rlang::env_bind(globalenv(), .Random.seed = old_seed)
                    )
                }
                set.seed(seed)
            }

            # use the environment attached in private as the mask
            mask <- rlang::new_data_mask(private$envir)
            mask$.data <- rlang::as_data_pronoun(mask)

            # then we run the command attached with this step
            result <- rlang::eval_tidy(step$call, data = mask, env = envir)

            if (step$return) {
                self$env_bind(!!id := result)
            } else {
                result <- NULL
            }
            if (isTRUE(reset)) private$reset_step(id = id, downstream = TRUE)
            private$finish_step(id)
            result
        },
        # abort if dependencies are non-exist in step_tree
        check_step_deps = function(deps = NULL) {
            if (is.null(deps)) {
                deps <- extract_step_deps(unclass(private$step_tree))
                deps <- unlist(deps, recursive = FALSE, use.names = FALSE)
            }
            missing_deps <- setdiff(deps, names(private$step_tree))
            if (length(missing_deps)) {
                cli::cli_abort(c(
                    "Can't find all dependencies in {.var step_tree}",
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
                    graph_ids <- igraph::subcomponent(
                        step_graph,
                        v = to, mode = "in"
                    )
                } else if (!is.null(from)) {
                    graph_ids <- igraph::subcomponent(
                        step_graph,
                        v = from, mode = "out"
                    )
                }
                return(igraph::subgraph(step_graph, vids = graph_ids))
            }
        }
    )
)
