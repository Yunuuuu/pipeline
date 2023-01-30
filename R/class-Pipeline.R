#' R6 Class Representing Pipeline
#'
#' @description A Pipeline object, is bound with two environment, of which one
#' for the collections of steps (step_collections) and another for the
#' accommodation of objects to run steps or the objects returned by the steps.
#'
#' In this way, we can bind all steps used in a pipeline and their dependant
#'   evaluation frame in one object.
#' @param id A scalar character of the step name.
#' @param step A [step] object.
#' @param reset If `TRUE`, will label all downstream steps (depend on current
#' step) as unfinished.
#' @param refresh If `TRUE`, the step will be evaluated no matter wether the
#' step has been finished or not. Otherwise the step will only be evaluated if
#' it has never been evaluated once, in which case, the result will be obtained
#' directly from the last evaluated result.
#' @param ...  <[`dynamic-dots`][rlang::dyn-dots]> all items must be a `step`
#'   object. Names won't make any sense. Any duplicated steps (with duplicated
#'   id) are only included once, the latter one will override the former. Can
#'   also provide as a list of steps directly.
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
#' step_collections in the pipeline will be used to create the dependencies
#' graph.
Pipeline <- R6::R6Class("Pipeline",
    public = list(
        #' @description
        #' Create a new person object.
        #' @param data A data list to build the attached environment which is
        #'   used to evaluate the variable in the step expression object. One
        #'   can also use `$env_bind` method to add new variable.
        #' @return A new `Pipeline` object.
        initialize = function(..., data = list()) {
            private$envir <- new.env(hash = TRUE, parent = emptyenv())
            private$env_bind_internal(!!!data)
            self$set_step_collections(...)
        },

        #' @description
        #' Set the step tree in the `Pipeline` object
        set_step_collections = function(...) {
            private$step_collections <- new.env(
                hash = TRUE, parent = emptyenv()
            )
            self$add_steps(..., reset = FALSE)
            invisible(self)
        },

        #' @description
        #' Get a single step in the `Pipeline` step_collections.
        #' @return A step object.
        get_step = function(id) {
            assert_class(id, is.character, class = "character", null_ok = FALSE)
            assert_length(id, 1L, null_ok = FALSE)
            private$assert_ids_exist(id)
            private$get_step_internal(id)
        },

        #' @description
        #' Get multiple steps in the `Pipeline` step_collections.
        #' @param ids An atomic character of the step name. if `NULL`, all steps
        #'   will be extracted.
        #' @return A list of step object.
        get_step_list = function(ids = NULL) {
            assert_class(ids, is.character,
                class = "character", null_ok = TRUE
            )
            if (is.null(ids)) {
                ids <- private$get_ids()
            } else {
                private$assert_ids_exist(ids)
            }
            private$get_step_list_internal(ids)
        },

        #' @description
        #' Add new steps into the `Pipeline` step_collections.
        add_steps = function(..., reset = TRUE) {
            dots <- rlang::dots_list(
                ...,
                .named = NULL, .homonyms = "keep",
                .ignore_empty = "all"
            )
            if (length(dots) == 1L && !is_step(dots[[1L]]) && is.list(dots[[1L]])) {
                dots <- dots[[1L]]
            }
            if (!all(vapply(dots, is_step, logical(1L), USE.NAMES = FALSE))) {
                cli::cli_abort("all items in {.arg ...} must be {.cls step} object")
            }
            ids <- vapply(dots, "[[", character(1L), "id", USE.NAMES = FALSE)
            overrided_ids <- intersect(ids, private$get_ids())
            if (length(overrided_ids)) {
                cli::cli_warn(c(
                    "Override {.val {length(overrided_ids)}} {.field step{?s}}",
                    "!" = "Replaced step ids: {.val {overrided_ids}}"
                ))
            }
            names(dots) <- ids
            list2env(dots, envir = private$step_collections)
            if (length(ids) && isTRUE(reset)) {
                private$set_downstream_state(ids = ids, value = FALSE)
            }
            invisible(self)
        },

        #' @description
        #' Remove steps in the `Pipeline` step_collections.
        #' @param ids An atomic character, the steps to remove from the
        #' `Pipeline`.
        remove_steps = function(ids, reset = TRUE) {
            assert_class(ids, is.character,
                class = "character",
                null_ok = FALSE
            )
            private$assert_ids_exist(ids)
            if (isTRUE(reset)) {
                private$set_downstream_state(ids = ids, value = FALSE)
            }
            rm(list = ids, pos = private$step_collections, inherits = FALSE)
            invisible(self)
        },

        #' @description Label the step as unfinished. If downstream is `TRUE`,
        #'   will also label all steps depending on this step as unfinished.
        #' @param ids An atomic character, the steps to reset.
        #' @param downstream If `TRUE`, will label all downstream steps (depend
        #' on current step) as unfinished.
        reset_steps = function(ids, downstream = TRUE) {
            assert_class(ids, is.character,
                class = "character", null_ok = FALSE
            )
            private$assert_ids_exist(ids)
            private$set_steps_state(ids = ids, value = FALSE)
            if (isTRUE(downstream)) {
                private$set_downstream_state(ids = ids, value = FALSE)
            }
            invisible(self)
        },
        #' @description Label all steps in the `step_collections` as unfinished.
        reset_step_collections = function() {
            private$set_steps_state(ids = private$get_ids(), value = FALSE)
            invisible(self)
        },

        #' @description
        #' Label the step as finished.
        finish_steps = function(ids) {
            assert_class(ids, is.character,
                class = "character", null_ok = FALSE
            )
            private$assert_ids_exist(ids)
            private$set_steps_state(ids, value = TRUE)
            invisible(self)
        },

        #' @description
        #' Modify the step components.
        #' @param ... <[`dynamic-dots`][rlang::dyn-dots]>, A named value paris
        #'   indicates the components to replace in the `step`. `NULL` will be
        #'   kept, use [`zap()`] to remove a component from the step.
        modify_step = function(id, ..., reset = TRUE) {
            step <- self$get_step(id)
            dots <- check_dots_named(...)
            step <- validate_step(new_step(modify_list(unclass(step), dots)))
            private$step_collections[[id]] <- step
            if (isTRUE(reset)) {
                private$set_downstream_state(ids = id, value = FALSE)
            }
            invisible(self)
        },

        #' @description
        #' If the expression in the step is a `call` object, this provide a
        #' convenient way to modify the call argument.
        #' @param ... <[`dynamic-dots`][rlang::dyn-dots]>, Named or unnamed
        #'   expressions (constants, names or calls) used to modify the call.
        #'   Use [`zap()`] to remove arguments. Empty arguments are preserved.
        modify_call = function(id, ..., reset = TRUE) {
            step <- self$get_step(id)
            if (!rlang::is_call(step$expr)) {
                cli::cli_abort(c(
                    "the expression of the step must be a {.cls call} object",
                    x = "You've supplied a step with a {.cls {typeof(step$expr)}} expression"
                ))
            }
            dots <- rlang::dots_list(...)
            if (length(dots)) {
                step$expr <- rlang::set_expr(
                    step$expr,
                    call_standardise(step$expr)
                )
                step$expr <- rlang::set_expr(
                    step$expr,
                    rlang::call_modify(step$expr, !!!dots)
                )
                private$step_collections[[id]] <- step
                if (isTRUE(reset)) {
                    private$set_downstream_state(ids = id, value = FALSE)
                }
            }
            invisible(self)
        },

        #' @description Build step dependencies network as an graph object
        #' @param add_attrs If `TRUE`, add "is_finished", "is_existed" and
        #'   "step_levels" as the graph vertex attributes. Default: `FALSE`
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
            step_graph <- private$build_step_graph(add_attrs)
            private$sub_step_graph(
                step_graph = step_graph,
                to = to, from = from, ids = ids
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
        #' @param envir The environment in which to evaluate the `expression` in
        #'   the step.
        run_step = function(id, refresh = FALSE, envir = caller_env()) {
            assert_class(id, is.character, class = "character", null_ok = FALSE)
            assert_length(id, 1L, null_ok = FALSE)
            private$assert_ids_exist(id)
            private$run_step_internal(
                id = id, refresh = refresh,
                envir = envir
            )
        },
        #' @description
        #' Running the steps until the target step
        #' @param targets <[`tidy-select`][tidyselect::language]>, A set of
        #'   targeted steps until which to run.
        #' @param envir The environment in which to evaluate the `expression` in
        #'   the step.
        run_targets = function(targets = NULL, refresh = FALSE, envir = caller_env()) {
            # build dependencies graph
            step_graph <- private$build_step_graph(add_attrs = TRUE)
            attrs <- igraph::vertex_attr(step_graph)[c("name", "step_levels")]
            step_levels <- structure(attrs$step_levels, names = attrs$name)

            # targets are the steps we want to run until if NULL, all steps
            # without child steps will be used
            step_list <- private$get_step_list_internal(private$get_ids())
            targets <- rlang::enquo(targets)
            if (rlang::quo_is_null(targets)) {
                targets <- names(igraph::V(step_graph))[
                    igraph::degree(step_graph, mode = "out") == 0L
                ]
            } else {
                targets <- tidyselect::eval_select(targets, data = step_list)
                targets <- names(step_list)[targets]
            }
            results_list <- vector("list", length(targets))
            names(results_list) <- targets

            for (target in targets) {
                # extract the deps_graph of each targets step
                target_deps <- private$sub_step_graph(
                    step_graph = step_graph, to = target
                )
                target_deps <- names(igraph::V(target_deps))

                # Output targets information
                cli_par_id <- cli::cli_par()
                cli::cli_rule(center = "Runing target: {.field {target}}")
                cli::cli_inform(
                    "Including {.val {length(target_deps)}} step{?s}"
                )

                # check if all dependencies exist in step_collections
                missing_ids <- setdiff(target_deps, private$get_ids())
                if (length(missing_ids)) {
                    cli::cli_abort(c(
                        "Can't find all {.field steps} in the {.field step_collections}",
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
            get(nm, pos = private$envir, inherits = FALSE)
        },
        #' @description
        #' Get multiple variable from the environment
        #' @param nms Names of bindings, a character vector.
        env_get_list = function(nms) {
            mget(nms, envir = private$envir, inherits = FALSE)
        },
        #' @description
        #' Bind symbols to object in the attached environment.
        #' @param ... <[`dynamic-dots`][rlang::dyn-dots]>, Named objects
        #'   (env_bind()). Use zap() to remove bindings.
        env_bind = function(...) {
            private$env_bind_internal(...)
            invisible(self)
        },
        #' @description
        #' Remove bindings from the attached environment.
        #' @param nms A character vector of binding names to remove.
        env_unbind = function(nms) {
            rm(list = nms, pos = private$envir, inherits = FALSE)
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
            private$env_bind_internal(!!nm := rlang::eval_tidy(variable))
            rm(list = nm, pos = rlang::quo_get_env(variable), inherits = FALSE)
            invisible(self)
        },
        #' @description Names of symbols bound in the attached environment
        env_names = function() ls(pos = private$envir, all.names = TRUE)
    ),

    # all object used to run step command should live in `envir`
    # all methods defined in private environment are not necessary to check the
    # arguments.
    private = list(
        step_collections = NULL, envir = NULL,

        ## For the usage of asserting the public method arguments
        assert_ids_exist = function(ids, arg = rlang::caller_arg(ids), call = caller_env()) {
            missing_ids <- setdiff(ids, private$get_ids())
            if (length(missing_ids)) {
                cli::cli_abort(c(
                    "Provided {.arg {arg}} must exist in the {.field step_collections}",
                    x = "Missing ids: {.val {missing_ids}}"
                ), call = call)
            }
        },

        ## environmetn utils
        env_bind_internal = function(...) {
            rlang::env_bind(.env = private$envir, ...)
        },

        ## step collections utils
        get_ids = function() {
            ls(pos = private$step_collections, all.names = TRUE)
        },
        get_step_internal = function(id) {
            private$step_collections[[id]]
        },
        get_step_list_internal = function(ids) {
            mget(ids,
                envir = private$step_collections,
                inherits = FALSE
            )
        },

        ### run the step and return the result
        run_step_internal = function(id, refresh = FALSE, envir = caller_env()) {
            step <- private$get_step_internal(id)
            # if this step has been finished, and refresh is FALSE
            # Just return the value from the environment
            if (step$finished && !isTRUE(refresh)) {
                if (step$bind) {
                    if (exists(id, where = private$envir, inherits = FALSE)) {
                        return(private$envir[[id]])
                    } else {
                        cli::cli_abort(c(
                            "Can't find the result of step {.val {id}} though it has been finished.",
                            i = "Try to set {.code refresh = TRUE}"
                        ))
                    }
                } else {
                    return(NULL)
                }
            }
            result <- eval_step(step,
                mask = private$envir,
                pipeline = self,
                envir = envir
            )

            if (step$bind) {
                private$env_bind_internal(!!id := result)
            } else {
                result <- NULL
            }
            private$set_step_state(id, value = TRUE)
            result
        },

        ### label step as finished or unfinished
        set_step_state = function(id, value) {
            private$step_collections[[id]]$finished <- value
        },

        ### label steps or downstream steps (depend on it) as finished or unfinished
        set_steps_state = function(ids, value) {
            for (id in ids) {
                private$set_step_state(id = id, value = value)
            }
        },
        set_downstream_state = function(ids, value) {
            step_graph <- private$build_step_graph(add_attrs = FALSE)
            downstream_ids <- lapply(ids, function(id) {
                downstream_steps <- private$sub_step_graph(
                    step_graph,
                    from = id
                )
                setdiff(names(igraph::V(downstream_steps)), id)
            })
            downstream_ids <- unlist(
                downstream_ids,
                recursive = FALSE, use.names = FALSE
            )
            private$set_steps_state(ids = unique(downstream_ids), value = value)
        },

        ### Build step dependencies network as an graph object
        build_step_graph = function(add_attrs = FALSE) {
            step_list <- private$get_step_list_internal(private$get_ids())
            step_graph <- build_step_graph_helper(
                step_list,
                add_attrs = add_attrs
            )
        },
        sub_step_graph = function(step_graph, to = NULL, from = NULL,
                                  ids = NULL) {
            sub_step_graph_helper(
                step_graph,
                to = to, from = from, ids = ids
            )
        }
    )
)

`+.Pipeline` <- function(x, y) {
    if (missing(y)) {
        cli::cli_abort(c(
            "Cannot use {.code +} with a single argument",
            i = "Did you accidentally put {.code +} on a new line?"
        ))
    }
    # Get the name of what was passed in as y, and pass along so that it
    # can be displayed in error messages
    yname <- deparse(substitute(y)) # nolint

    if (inherits(x, "Pipeline") && R6::is.R6(x)) {
        if (is.list(y) || is_step(y)) {
            x$add_steps(y, reset = TRUE)
        } else {
            cli::cli_abort(
                "Can't add {.var {yname}} to a {.cls Pipeline} R6 object."
            )
        }
    } else if (is_step(x)) {
        cli::cli_abort(c(
            "Cannot add {.cls step} objects together",
            "i" = "Did you forget to add this object to a {.cls Pipeline} R6 object?"
        ))
    }
    x
}
