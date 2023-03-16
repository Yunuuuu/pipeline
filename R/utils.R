`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

#' @importFrom rlang zap
#' @export
rlang::zap

#' Report if an argument is a specific class
#'
#' @keywords internal
#' @noRd
assert_class <- function(x, is_class, class, null_ok = FALSE, arg = rlang::caller_arg(x), call = parent.frame()) {
    message <- "{.cls {class}} object"
    if (null_ok) {
        message <- paste(message, "or {.code NULL}", sep = " ")
    }
    message <- sprintf("{.arg {arg}} must be a %s", message)
    # class sometimes can also contain the `NULL`
    if (is.null(x) && !is_class(x)) {
        if (!null_ok) {
            cli::cli_abort(c(message,
                "x" = "You've supplied a {.code NULL}"
            ), call = call)
        }
    } else if (!is_class(x)) {
        cli::cli_abort(c(message,
            "x" = "You've supplied a {.cls {class(x)}} object"
        ), call = call)
    }
}

#' Report if an argument has specific length
#' @keywords internal
#' @noRd
assert_length <- function(x, length, null_ok = FALSE, arg = rlang::caller_arg(x), call = parent.frame()) {
    length <- as.integer(length)
    if (length == 1L) {
        message <- "{.field scalar} object"
    } else {
        message <- "length {.val {length}} object"
    }
    if (null_ok) {
        message <- paste(message, "or {.code NULL}", sep = " ")
    }
    message <- sprintf("{.arg {arg}} must be a %s", message)
    is_right_length <- length(x) == length
    if (is.null(x) && !is_right_length) {
        if (!null_ok) {
            cli::cli_abort(c(message,
                "x" = "You've supplied a {.code NULL}"
            ), call = call)
        }
    } else if (!is_right_length) {
        cli::cli_abort(c(message,
            "x" = "You've supplied a length {.val {length(x)}} object"
        ), call = call)
    }
}

#' Implement similar purrr::imap function
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
    if (length(missing_names)) {
        replace <- replace[!old_names %in% missing_names]
        old_names <- names(replace)
    }
    names(x)[match(old_names, current_names)] <- as.vector(replace)
    x
}

# `NULL` passed to ... will also be kept
#' @param x A list to modify
#' @param replace A named list with components to replace (or add) corresponding
#' components in `x`.
#' @noRd
modify_list <- function(x, replace) {
    for (name in names(replace)) {
        value <- replace[[name]]
        if (rlang::is_zap(value)) {
            x[[name]] <- NULL
        } else {
            x[name] <- list(value)
        }
    }
    x
}

check_dots_named <- function(..., call = parent.frame()) {
    dots <- rlang::dots_list(..., .named = NULL, .homonyms = "error")
    if (!rlang::is_named2(dots)) {
        cli::cli_abort(
            "All elements in {.arg ...} must be named",
            call = call
        )
    }
    dots
}

# call object utils ----------------------------------------
call_standardise <- function(call, env = parent.frame()) {
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

# find_expr_deps <- function(expr) {
#     # substitute "~" with "base::identity"
#     expr <- rlang::call2(rlang::expr(substitute), expr,
#         env = rlang::expr(list(`~` = base::identity)),
#         .ns = "base"
#     )
#     expr <- rlang::eval_bare(expr, env = rlang::base_env())
#     codetools::findGlobals(rlang::new_function(args = NULL, body = expr))
# }

change_expr <- function(expr, from, to) {
    switch(expr_type(expr),

        # special cases
        # for missing argument in pairlist
        missing = rlang::missing_arg(),

        # seems like this will always live in the end of anonymous function call
        # we just return as it is
        integer = expr,

        # Base cases
        constant = ,
        symbol = if (identical(expr, from)) to else expr,

        # Recursive cases
        call = as.call(lapply(expr, change_expr, from = from, to = to)),
        pairlist = as.pairlist(lapply(expr, change_expr, from = from, to = to)),
        cli::cli_abort("Don't know how to handle type {.cls {expr_type(expr)}}")
    )
}

expr_type <- function(x) {
    if (rlang::is_missing(x)) {
        "missing"
    } else if (rlang::is_syntactic_literal(x)) {
        "constant"
    } else if (is.symbol(x)) {
        "symbol"
    } else if (is.call(x)) {
        "call"
    } else if (is.pairlist(x)) {
        "pairlist"
    } else {
        typeof(x)
    }
}

# environment helper
# Wrapper around list2env with a NULL check. In R <3.2.0, if an empty unnamed
# list is passed to list2env(), it errors. But an empty named list is OK. For
# R >=3.2.0, this wrapper is not necessary.
# @param empty_to_null Controls what to do when x is NULL or empty list.
#   If TRUE, return NULL. If FALSE, return an empty list.
list2env2 <- function(x, envir = NULL, parent = emptyenv(),
                      hash = (length(x) > 100),
                      size = max(29L, length(x))) {
    if (is.null(envir)) {
        envir <- new.env(hash = hash, parent = parent, size = size)
    }
    if (length(x) == 0L) {
        return(envir)
    }
    list2env(x, envir)
}

# cli output helper
cli_list <- function(label, items, sep = ": ", add_num = TRUE) {
    items <- cli::cli_vec(items, list("vec-trunc" = 3L))
    message <- "{.field {label}}{sep}{.val {items}}"
    if (add_num) {
        message <- paste(message, "({length(items)} item{?s})", sep = " ")
    }
    cli::cli_li(message)
}
