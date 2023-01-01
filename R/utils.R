`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

has_names <- function(x) {
    nms <- names(x)
    if (is.null(nms)) {
        return(rep(FALSE, length(x)))
    }
    !is.na(nms) & nms != ""
}

#' @export
rlang::zap

#' Report if an argument is a specific class
#' @keywords internal
#' @noRd
assert_class <- function(x, class, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    if (!inherits(x, class)) {
        cli::cli_abort(
            c(
                "{.arg {arg}} must be a {.cls {class}} object",
                "x" = "You've supplied a {.cls {class(x)}}."
            ),
            call = call
        )
    }
}

#' Report if an argument is a specific class
#' @keywords internal
#' @noRd
assert_scalar_chr <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    if (!rlang::is_scalar_character(x)) {
        cli::cli_abort(
            c(
                "{.arg {arg}} must be a scalar {.cls character}",
                "x" = "You've supplied a length {.val length(x)} {.cls {class(x)}}."
            ),
            call = call
        )
    }
}

#' implement similar purrr::imap function
#' @note this function won't keep the names of .x in the final result
#' @keywords internal
#' @noRd
imap <- function(.x, .f, ...) {
    .mapply(
        rlang::as_function(.f),
        dots = list(.x, names(.x) %||% as.character(seq_along(.x))),
        MoreArgs = list(...)
    )
}

#' Rename elements in a list, data.frame or vector
#'
#' This is akin to `dplyr::rename` and `plyr::rename`. It renames elements given
#' as names in the `replace` vector to the values in the `replace` vector
#' without touching elements not referenced.
#'
#' @param x A data.frame or a named vector or list
#' @param replace A named character vector. The names identifies the elements in
#' `x` that should be renamed and the values gives the new names.
#'
#' @return `x`, with new names according to `replace`
#'
#' @keywords internal
#' @noRd
rename <- function(x, replace) {
    current_names <- names(x)
    old_names <- names(replace)
    missing_names <- setdiff(old_names, current_names)
    if (length(missing_names) > 0L) {
        replace <- replace[!old_names %in% missing_names]
        old_names <- names(replace)
    }
    names(x)[match(old_names, current_names)] <- as.vector(replace)
    x
}

modify_list <- function(x, restrict = NULL, ...) {
    dots_list <- rlang::list2(...)
    dots_list <- dots_list[has_names(dots_list)]
    if (is.null(restrict)) {
        restrict <- names(dots_list)
    } else {
        restrict <- intersect(restrict, names(dots_list))
    }
    for (name in dots_list) {
        x[name] <- dots_list[name]
    }
    x
}

call_standardise <- function (call, env = rlang::caller_env()) {
    expr <- rlang::get_expr(call)
    env <- rlang::get_env(call, env)
    fn <- rlang::eval_bare(rlang::node_car(expr), env)
    if (rlang::is_primitive(fn)) {
        call
    } else {
        matched <- rlang::call_match(expr, fn, defaults = TRUE)
        rlang::set_expr(call, matched)
    }
}
