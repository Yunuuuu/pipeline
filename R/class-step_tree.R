#' @title Build a `step_tree` class
#' @description
#' A `step_tree` is a list of steps, the element names are always the same with
#' the step ids.
#' @param ...  <[`dynamic-dots`][rlang::dyn-dots]> all items must be `step`
#'   object, names in ... don't make sense since we always use the step id as
#'   the names. steps must be unique with no duplicated ids. Can also provide as
#'   a list of steps directly.
#' @return A `step_tree` object.
#' @name step_tree
#' @export
step_tree <- function(...) {
    dots <- rlang::dots_list(..., .named = NULL)
    if (identical(length(dots), 1L) && !is_step(dots[[1L]]) && is.list(dots[[1L]])) {
        dots <- dots[[1L]]
    }
    new_step_tree(dots)
}

#' low-level constructor
#' @noRd
new_step_tree <- function(step_list) {
    if (!all(vapply(step_list, is_step, logical(1L), USE.NAMES = FALSE))) {
        cli::cli_abort("all elements must be {.cls step} object")
    }
    ids <- vapply(step_list, "[[", character(1L), "id", USE.NAMES = FALSE)
    dup_ids <- unique(ids[duplicated(ids)])
    if (length(dup_ids)) {
        cli::cli_abort(c(
            "all ids in {.var step_tree} must be unique",
            x = "duplicated ids: {dup_ids]"
        ))
    }
    structure(step_list,
        names = ids,
        class = c("step_tree", "Pipeline")
    )
}

#' @param x A `step_tree` object from which to extract element(s) or in which to
#' replace element(s).
#' @param id The id specifying step to extract or replace.
#' @param value The value to replace with. Must be a `step` object or a list of
#'   `step` object.
#' @export
#' @rdname step_tree
`[[.step_tree` <- function(x, id) {
    NextMethod()
}

#' @export
#' @rdname step_tree
`[[<-.step_tree` <- function(x, id, value) {
    new_step_tree(NextMethod())
}

#' @export
#' @rdname step_tree
`$.step_tree` <- `[[.step_tree`


#' @export
#' @rdname step_tree
`$<-.step_tree` <- `[[<-.step_tree`

#' @export
#' @rdname step_tree
`[.step_tree` <- function(x, id) {
    new_step_tree(NextMethod())
}

#' @export
#' @rdname step_tree
`[<-.step_tree` <- function(x, id, value) {
    new_step_tree(NextMethod())
}

#' Reports whether x is a `step_tree` object
#' @param x An object to test
#' @keywords internal
#' @noRd
is_step_tree <- function(x) inherits(x, "step_tree")
