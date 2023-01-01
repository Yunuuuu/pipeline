# R interactive pipeline
Ripipeline <- R6::R6Class("Ripipeline",
    public = list(
        initialize = function(main = NULL, ...) {
            private$envir <- rlang::new_environment(list(main = main))
            self$set_step_tree(...)
        },

        #' methods for step_tree operation
        set_step_tree = function(...) {
            private$step_tree <- step_tree(...)
            private$step_graph <- NULL
            invisible(self)
        },
        set_step = function(id, ...) {
            if (!rlang::is_scalar_character(id)) {
                cli::cli_abort("{.arg id} must be a scalar {.cls character}")
            }
            private$step_tree[[id]] <- step(...)
            private$step_graph <- NULL
            invisible(self)
        },
        add_step = function(id, ...) {
            if (!rlang::is_scalar_character(id)) {
                cli::cli_abort("{.arg id} must be a scalar {.cls character}")
            }
            if (id %in% names(private$step_tree)) {
                cli::cli_abort(c(
                    "Already existed {.field step}",
                    i = "check {.arg id} argument or use {.fn set_step} method if you want to override it." # nolint
                ))
            }
            private$step_tree[[id]] <- step(...)
            private$step_graph <- NULL
            invisible(self)
        },
        set_steps = function(...) {
            step_tree <- step_tree(...)
            for (id in names(step_tree)) {
                private$step_tree[[id]] <- step_tree[[id]]
            }
            private$step_graph <- NULL
            invisible(self)
        },
        add_steps = function(...) {
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
            }
            private$step_graph <- NULL
            invisible(self)
        },
        modify_step = function(id, call = NULL, ...) {
            if (!rlang::is_scalar_character(id)) {
                cli::cli_abort(c(
                    "{.arg id} must be a scalar {.cls character}",
                    "x" = "You've supplied a {.cls {class(id)}}."
                ))
            } else if (!id %in% names(step_tree)) {
                cli::cli_abort("{.arg id} must in the {.arg step_tree}")
            }
            step <- private$step_tree[[id]]
            if (is.list(call)) {
                step$call <- rlang::call_modify(step$call, !!!call)
            } else if (rlang::is_expression(call)) {
                step$call <- call
            } else {
                cli::cli_abort(c(
                    "{.arg call} must be a {.cls list} or {.arg expression}",
                    "x" = "You've supplied a {.cls {class(call)}}."
                ))
            }
            private$step_tree[[id]] <- modify_list(step,
                restrict = c("deps", "finished", "return", "seed"),
                ...
            )
            invisible(self)
        },
        reset_step = function(id, downstream = TRUE) {
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
            private$step_tree[[id]]$finished <- TRUE
            invisible(self)
        },

        #' methods for the operation in the attached environment
        #' Get single variable from the environment
        get = function(nm) {
            rlang::env_get(env = private$envir, nm = nm, inherit = FALSE)
        },
        #' Get multiple variable from the environment
        get_list = function(nms) {
            rlang::env_get_list(env = private$envir, nms = nms, inherit = FALSE)
        },
        #' Get multiple variable from the environment
        bind = function(name = NULL, value) {
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

        #' following define dependencies methods
        #' Report if dependencies exist in step_tree
        check_step_deps = function(deps = NULL) {
            if (is.null(deps)) {
                deps <- extract_step_deps(unclass(self$step_tree))
                deps <- unlist(deps, recursive = FALSE, use.names = FALSE)
            }
            missed_deps <- setdiff(deps, names(self$step_tree))
            if (length(missed_deps)) {
                cli::cli_abort(c(
                    "Can't find all dependencies in {.var step_tree}",
                    x = "Missing {.val {length(missed_deps)}} dependenc{?y/ies}: {missed_deps}"
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
            step_graph <- private$step_graph
            if (is.null(step_graph)) {
                step_graph <- build_step_graph_helper(
                    step_list,
                    add_attrs = add_attrs
                )
                private$step_graph <- step_graph
            }

            if (!is.null(from) && !is.null(to)) {
                cli::cli_warn(c(
                    "Both {.arg from} and {.arg to} are setted.",
                    "!" = "Will only use {.arg to}."
                ))
            }
            if (is.null(to) && is.null(from)) {
                return(step_graph)
            } else if (!is.null(to)) {
                return(igraph::subcomponent(step_graph, v = to, mode = "in"))
            } else if (!is.null(from)) {
                return(igraph::subcomponent(step_graph, v = from, mode = "out"))
            }
        },
        plot_step_tree = function(layout = igraph::layout_as_tree, ...) {
            plot(self$build_step_graph(from = id, add_attrs = TRUE),
                layout = layout, ...
            )
        },

        #' @param id The name of step object.
        #' @param refresh A scalar logical value indicates if run step if it has
        #' been finished.
        #' @param reset A scalar logical value indicates.
        #' @param envir The environment in which to evaluate the `call` attached
        #'   in step.
        #' @keywords internal
        #' @noRd
        run_step = function(id, refresh = FALSE, reset = FALSE, envir = rlang::caller_env()) {
            step <- private$step_tree[[id]]
            if (is.null(step$return) || isTRUE(step$return)) {
                return <- id
            } else if (rlang::is_scalar_character(step$return)) {
                return <- step$return
            }
            if (isTRUE(step$finished) && isFALSE(refresh)) {
                return(rlang::env_get(private$envir, nm = return))
            }
            if (rlang::is_scalar_integer(step$seed)) {
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
                set.seed(step$seed)
            }
            mask <- rlang::new_data_mask(private$envir)
            mask$.data <- rlang::as_data_pronoun(mask)
            result <- rlang::eval_tidy(step$call, data = mask, env = envir)
            if (!isFALSE(step$return)) {
                self$bind(name = return, value = result)
            }
            if (isTRUE(reset)) self$reset_step(id = id, downstream = TRUE)
            self$finish_step(id)
            result
        },
        run_targets = function(targets = NULL, refresh = FALSE, reset = FALSE, envir = rlang::caller_env()) {
            step_list <- unclass(private$step_tree)

            # build dependencies graph
            step_graph <- self$build_step_graph(add_attrs = FALSE)

            # targets are the steps we want to run until if NULL, all steps
            # without child steps will be used
            targets <- rlang::enquo(targets)
            if (is.null(rlang::quo_get_expr(targets))) {
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
                target_deps <- igraph::subcomponent(
                    step_graph,
                    v = target, mode = "in"
                )
                attrs <- igraph::vertex_attr(target_deps)[c("name", "levels")]
                cli::cli_inform(
                    "Running {.val {length(attrs$name)}} step{?s} for {.field {target}} target"
                )

                # check if all dependencies exist in step_tree
                self$check_step_deps(attrs$name)

                # run targeted dependencies
                cli::cli_inform(
                    "Running {.val {length(attrs$name)}} step{?s} for target: {.field {target}}"
                )

                # since levels is integer, we simply factor it so split can sort
                # the list in the order of the levels
                step_ids_list <- split(attrs$name, factor(attrs$levels))
                results_list[[target]] <- lapply(step_ids_list, function(ids) {
                    lapply(ids, function(id) {
                        self$run_step(
                            id = id, refresh = refresh,
                            reset = reset,
                            envir = envir
                        )
                    })
                })
            }
            invisible(results_list)
        }
    ),
    # all object used to run step command should live in `data`
    # By passing data into `data` argument of `eval_tidy`, we can implement data
    # mask
    private = list(step_tree = NULL, envir = NULL, step_graph = NULL)
)
