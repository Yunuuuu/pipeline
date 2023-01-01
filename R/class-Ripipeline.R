# R interactive pipeline
Ripipeline <- R6::R6Class("Ripipeline",
    public = list(
        initialize = function(main = NULL, ...) {
            private$envir <- rlang::new_environment(list(main = main))
            self$set_step_tree(...)
        },

        #' methods for step_tree operation
        get_step = function(id) {
            private$check_id(id, exist = FALSE)
            private$step_tree[[id]]
        },
        set_step_tree = function(...) {
            private$step_tree <- step_tree(...)
            invisible(self)
        },
        set_step = function(id, ..., reset = TRUE) {
            private$check_id(id, exist = FALSE)
            private$step_tree[[id]] <- step(...)
            if (isTRUE(reset)) {
                self$reset_step(id = id, downstream = TRUE)
            }
            invisible(self)
        },
        add_step = function(id, ..., reset = TRUE) {
            private$check_id(id, exist = FALSE)
            if (id %in% names(private$step_tree)) {
                cli::cli_abort(c(
                    "Already existed {.field step}",
                    i = "check {.arg id} argument or use {.fn set_step} method if you want to override it." # nolint
                ))
            }
            private$step_tree[[id]] <- step(...)
            if (isTRUE(reset)) {
                self$reset_step(id = id, downstream = TRUE)
            }
            invisible(self)
        },
        set_steps = function(..., reset = TRUE) {
            step_tree <- step_tree(...)
            for (id in names(step_tree)) {
                private$step_tree[[id]] <- step_tree[[id]]
                if (isTRUE(reset)) {
                    self$reset_step(id = id, downstream = TRUE)
                }
            }
            invisible(self)
        },
        add_steps = function(..., reset = TRUE) {
            step_tree <- step_tree(...)
            dup_names <- intersect(names(step_tree), names(private$step_tree))
            if (length(dup_names)) {
                cli::cli_abort(c(
                    "Already existed {.field steps}: {dup_names}",
                    i = "use {.fn set_steps} method if you want to override them."
                ))
            }
            for (id in names(step_tree)) {
                private$step_tree[[id]] <- step_tree[[id]]
                if (isTRUE(reset)) {
                    self$reset_step(id = id, downstream = TRUE)
                }
            }
            invisible(self)
        },
        # modify call only change the call element of the step
        # No need to update the step_graph
        modify_call = function(id, ..., call = NULL, reset = TRUE) {
            private$check_id(id, exist = TRUE)
            step <- private$step_tree[[id]]
            call <- rlang::enquo(call)
            if (!is.null(rlang::get_expr(call))) {
                step$call <- call
                if (isTRUE(reset)) {
                    self$reset_step(id = id, downstream = TRUE)
                }
            } else {
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
                    if (isTRUE(reset)) {
                        self$reset_step(id = id, downstream = TRUE)
                    }
                }
            }
            private$step_tree[[id]] <- step
            invisible(self)
        },
        modify_step = function(id, deps, finished, return, seed, call = NULL, ..., reset = TRUE) {
            private$check_id(id, exist = TRUE)
            self$modify_call(
                id = id, ...,
                call = !!rlang::enquo(call),
                reset = FALSE
            )
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
            private$step_tree[[id]] <- step
            if (isTRUE(reset)) {
                self$reset_step(id = id, downstream = TRUE)
            }
            invisible(self)
        },
        reset_step = function(id, downstream = TRUE) {
            private$check_id(id, exist = TRUE)
            if (isTRUE(downstream)) {
                downstream_steps <- self$build_step_graph(
                    from = id, add_attrs = FALSE
                )
                reset_ids <- names(igraph::V(downstream_steps))
            } else {
                reset_ids <- id
            }
            for (reset_id in reset_ids) {
                private$step_tree[[reset_id]]$finished <- FALSE
            }
            invisible(self)
        },
        finish_step = function(id) {
            private$check_id(id, exist = TRUE)
            private$step_tree[[id]]$finished <- TRUE
            invisible(self)
        },
        # this will also modify the underlying call, substitute the result
        # symbol of the "from" step to the result symbol of "to" step.
        # if you just want to modify deps but not change the underlying call
        # object, try to use `$modify_step` method.
        switch_step_dep = function(id, from, to, force = FALSE) {
            private$check_id(id, exist = TRUE)
            step <- private$step_tree[[id]]
            symbol_list <- vector("list", 2L)
            # check from and to arguments
            for (i in 1:2) {
                arg <- c("from", "to")[[i]]
                name <- list(from, to)[[i]]
                private$check_id(name, exist = FALSE)
                if (!name %in% step$deps) {
                    if (isTRUE(force)) {
                        symbol_list[[i]] <- name
                    } else {
                        cli::cli_abort(c(
                            "{.arg {arg}} must exist in the {.var step_tree]",
                            i = "You can force to proceed by enable {.arg force} where {.arg {arg}} will be regarded as the step returned result symbol" # nolint
                        ))
                    }
                } else {
                    symbol_list[[i]] <- self$get_return_name(id = name)
                    if (is.null(symbol_list[[i]])) {
                        cli::cli_abort("{.arg {arg}} has no returned result symbol")
                    }
                }
            }

            # run switch
            step$deps <- union(setdiff(step$deps, from), to)

            step$call <- rlang::set_expr(
                step$call,
                value = change_expr(
                    step$call,
                    from = rlang::sym(symbol_list[[1L]]),
                    to = rlang::sym(symbol_list[[2L]])
                )
            )
            private$step_tree[[id]] <- step
            invisible(self)
        },
        get_return_name = function(id) {
            private$check_id(id, exist = TRUE)
            step <- private$step_tree[[id]]
            if (is.null(step$return) || isTRUE(step$return)) {
                id
            } else if (rlang::is_scalar_character(step$return)) {
                step$return
            } else {
                NULL
            }
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
        #' Get multiple variable from the environment
        env_bind = function(name = NULL, value) {
            if (is.null(name)) {
                name <- rlang::try_fetch(
                    rlang::as_name(rlang::ensym(value)),
                    error = function(cnd) {
                        cli::cli_abort(c(
                            "Provided {.arg value} is not a simple {.cls symbol}.",
                            x = conditionMessage(cnd),
                            i = "try to set {.arg name} argument manually."
                        ))
                    }
                )
            }
            rlang::env_bind(.env = private$envir, !!name := value)
            invisible(self)
        },
        env_names = function() {
            rlang::env_names(private$envir)
        },

        #' following define dependencies methods
        #' Report if dependencies exist in step_tree
        check_step_deps = function(deps = NULL) {
            if (is.null(deps)) {
                deps <- extract_step_deps(unclass(private$step_tree))
                deps <- unlist(deps, recursive = FALSE, use.names = FALSE)
            }
            missed_deps <- setdiff(deps, names(private$step_tree))
            if (length(missed_deps)) {
                cli::cli_abort(c(
                    "Can't find all dependencies in {.var step_tree}",
                    x = "Missing {.val {length(missed_deps)}} dependenc{?y/ies}: {.val {missed_deps}}"
                ))
            }
            invisible(self)
        },

        #' build step dependencies network as an graph object
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
        },
        plot_step_graph = function(to = NULL, from = NULL, layout = igraph::layout_as_tree, ...) {
            step_graph <- self$build_step_graph(
                to = to, from = from, add_attrs = TRUE
            )
            plot(step_graph, layout = layout, ...)
        },

        #' @param id The name of step object.
        #' @param refresh A scalar logical value indicates if run step if it has
        #' been finished.
        #' @param reset A scalar logical value indicates.
        #' @param envir The environment in which to evaluate the `call` attached
        #'   in step.
        #' @keywords internal
        #' @noRd
        run_step = function(id, refresh = FALSE, reset = TRUE, envir = rlang::caller_env()) {
            step <- private$step_tree[[id]]
            if (is.null(step$return) || isTRUE(step$return)) {
                return <- id
            } else if (rlang::is_scalar_character(step$return)) {
                return <- step$return
            }
            if (isTRUE(step$finished) && isFALSE(refresh)) {
                if (!isFALSE(step$return)) {
                    return(rlang::env_get(private$envir, nm = return))
                } else {
                    return(NULL)
                }
            }
            if (isTRUE(step$seed) || rlang::is_scalar_integer(step$seed) || rlang::is_scalar_double(step$seed)) {
                if (isTRUE(step$seed)) {
                    seed <- digest::digest2int(
                        digest::digest(call, "crc32"),
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
            mask <- rlang::new_data_mask(private$envir)
            mask$.data <- rlang::as_data_pronoun(mask)
            result <- rlang::eval_tidy(step$call, data = mask, env = envir)
            if (!isFALSE(step$return)) {
                self$env_bind(name = return, value = result)
            } else {
                result <- NULL
            }
            if (isTRUE(reset)) self$reset_step(id = id, downstream = TRUE)
            self$finish_step(id)
            result
        },
        run_targets = function(targets = NULL, refresh = FALSE, reset = TRUE, envir = rlang::caller_env()) {
            step_list <- unclass(private$step_tree)

            # build dependencies graph
            step_graph <- self$build_step_graph(add_attrs = TRUE)
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
                self$check_step_deps(target_deps)

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
                        self$run_step(
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
        }
    ),
    # all object used to run step command should live in `data`
    # By passing data into `data` argument of `eval_tidy`, we can implement data
    # mask
    private = list(
        step_tree = NULL, envir = NULL,
        check_id = function(id, exist = TRUE) {
            assert_scalar_chr(id)
            if (exist && !id %in% names(private$step_tree)) {
                cli::cli_abort("{.arg id} must exist in the {.var step_tree}")
            }
        }
    )
)
