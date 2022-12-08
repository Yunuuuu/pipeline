get_markers <- function(name) {
    manual_cell_marker_dataset[[name]]
}

manual_cell_marker_dataset <- structure(
    rlang::new_environment(),
    class = "manual_cell_marker_dataset"
)

#' @param x an `manual_cell_marker_dataset` object
#' @param ... Not used currently
#' @export
#' @name manual_cell_marker_dataset
print.manual_cell_marker_dataset <- function(x, ...) {
    cli::cli_text("A total of {rlang::env_length(x)} cell markers sets")
}

# convention: name should be the surname of the first author followed by the
# years of the published article connected with "_"
new_marker_set <- function(name, main, ..., reference) {
    marker_set <- c(list(main = main), list(...))
    # every elements in ... should be named
    check_names(marker_set)
    if (name %in% rlang::env_names(manual_cell_marker_dataset)) {
        cli::cli_abort("Existing cell markers found in datasets")
    }
    manual_cell_marker_dataset[[name]] <- structure(
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
    } else if (!is.null(x)) {
        cli::cli_abort("all elements should be {.field NULL}, or a {.cls list} or a {.cls chracter}")
    }
    if (!is_right) {
        cli::cli_abort("all elements or sub-elements should be named")
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
    cli::cli_text("Cell marker set derived from: {.url {attr(x, \"reference\")}}")
    cli::cli_rule("marker set details")
    main_par_id <- cli::cli_par()
    cli::cli_text("Main cell types")
    main_item_lid <- cli::cli_ul()
    .mapply(
        cli_list,
        list(
            label = names(x$main),
            items = x$main
        ),
        MoreArgs = list(add_num = FALSE)
    )
    cli::cli_end(id = main_item_lid)
    cli::cli_end(id = main_par_id)
    subelemnts <- x[setdiff(names(x), "main")]
    if (length(subelemnts)) {
        cli::cli_ul()
        cli::cli_text("Including markers for {length(subelemnts)} sub-group{?s}")
        .mapply(
            cli_list,
            list(
                label = names(subelemnts),
                items = lapply(subelemnts, names)
            ),
            MoreArgs = NULL
        )
        cli::cli_end()
    }

    x
}

cli_list <- function(label, items, sep = ": ", add_num = TRUE) {
    items <- cli::cli_vec(items, list("vec-trunc" = 3L))
    message <- "{.field {label}}{sep}{.val {items}}"
    if (add_num) {
        message <- paste(message, "({length(items)} item{?s})", sep = " ")
    }
    cli::cli_li(message)
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
