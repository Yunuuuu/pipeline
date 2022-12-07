get_markers <- function(name) {
    cell_marker_dataset[[name]]
}

cell_marker_dataset <- structure(
    rlang::env(),
    class = "cell_marker_dataset"
)

#' @param x an `cell_marker_dataset` object
#' @param ... Not used currently
#' @export
#' @name cell_marker_dataset
print.cell_marker_dataset <- function(x, ...) {
    cat(sprintf(
        "A total of %d cell markers sets", rlang::env_length(x)
    ), sep = "\n")
}

# convention: name should be the surname of the first author followed by the
# years of the published article connected with "_"
new_marker_set <- function(name, main, ..., reference) {
    marker_set <- c(list(main = main), list(...))
    # every elements in ... should be named
    check_names(marker_set)
    if (name %in% rlang::env_names(cell_marker_dataset)) {
        stop("Existing cell markers found in datasets", call. = FALSE)
    }
    cell_marker_dataset[[name]] <- structure(
        marker_set,
        class = "marker_set",
        reference = reference
    )
}

check_names <- function(x) {
    x_type <- typeof(x)
    if (identical(x_type, "character")) {
        # No need names for character
        is_right <- TRUE
    } else if (identical(x_type, "list")) {
        # for a list, we should check all elements have names,
        # and then recall this function to check every elments
        is_right <- has_names(x) && all(vapply(x, check_names, logical(1L)))
    } else {
        stop("all elements should be NULL or a list", call. = FALSE)
    }
    if (!is_right) {
        stop("all elements or sub-elements should be named", call. = FALSE)
    }
    is_right
}

has_names <- function(x) {
    x_names <- names(x)
    if (is.null(x_names) || any(x_names == "")) FALSE else TRUE
}

#' @param x an `marker_set` object
#' @param ... Not used currently
#' @name marker_set
#' @export
print.marker_set <- function(x, ...) {
    cat(strwrap(
        sprintf("Marker sets derived from: %s", attr(x, "reference")),
        indent = 0, exdent = 2
    ), sep = "\n")
    cat("\n")
    cat("Main cell types", sep = "\n")
    .mapply(
        wrap_cat,
        list(
            label = names(x$main),
            items = x$main
        ),
        MoreArgs = list(indent = 2, exdent = 4)
    )
    subelemnts <- x[setdiff(names(x), "main")]
    cat(sprintf(
        "Including markers for %d %s",
        length(subelemnts),
        if (identical(length(subelemnts), 1L)) "sub-group" else "sub-groups"
    ), sep = "\n")
    .mapply(
        wrap_cat,
        list(
            label = names(subelemnts),
            items = lapply(subelemnts, names)
        ),
        MoreArgs = list(indent = 2, exdent = 4)
    )
}

wrap_cat <- function(label, items, sep = ": ", collapse = ", ", indent = 0L, exdent = 2L) {
    total <- length(items)

    ext <- if (identical(total, 0L)) {
        "none"
    } else if (identical(total, 1L)) {
        items
    } else if (total <= 6L) {
        paste(
            paste(items[-length(items)], collapse = collapse),
            "and", items[[length(items)]],
            sep = " "
        )
    } else {
        paste(
            paste(
                paste(items[1:3], collapse = collapse),
                "...",
                paste(items[(total - 1L):total], collapse = collapse),
                sep = ", "
            ),
            sprintf("(%d total)", total),
            sep = " "
        )
    }
    cat(strwrap(
        paste(label, ext, sep = sep),
        indent = indent, exdent = exdent
    ), sep = "\n")
}
