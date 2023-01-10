#' Run step
#'
#' @param step A step object.
#' @param self The public environment.
#' @param private The private environment.
#' @param envir The environment in which to evaluate step.
#' @noRd
eval_step <- function(step, self, private, envir) {
    # if there is any seed, we set seed and then restore the existed seed in the
    # globalenv()
    if (isTRUE(step$seed) || rlang::is_scalar_integer(step$seed) || rlang::is_scalar_double(step$seed)) {
        if (isTRUE(step$seed)) {
            seed <- digest::digest2int(
                digest::digest(step$call, "crc32"),
                seed = 0L
            )
        } else {
            seed <- as.integer(step$seed)
        }
        old_seed <- rlang::env_get(globalenv(), ".Random.seed", default = NULL)
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
    rlang::eval_tidy(step$call, data = mask, env = envir)
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
# }
