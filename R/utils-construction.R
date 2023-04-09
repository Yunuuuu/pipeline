#' @param x A function argument from other function..
#' @return A quosure object if it is not a missing defused argument, otherwise,
#' a symbol of the argument name.
#' @noRd
quo_or_symbol <- function(x) {
    x_symbol <- substitute(x)
    x_quo <- rlang::eval_bare(
        rlang::expr(rlang::enquo(!!x_symbol)),
        env = parent.frame()
    )
    if (rlang::quo_is_missing(x_quo)) x_symbol else x_quo
}

#' @param step_param User provided step components to modify the default step
#' items.
#' @param default The default components for this step.
#' @noRd
build_step <- function(id, expr, step_param, default,
                       arg = rlang::caller_arg(step_param),
                       call = parent.frame()) {
    if (!rlang::is_named2(step_param)) {
        cli::cli_abort("All items in {.arg {arg}} must be named", call = call)
    }
    step_param <- modify_list(default, step_param)
    rlang::inject(pipeline::create_step(id = id, expr = expr, !!!step_param))
}
