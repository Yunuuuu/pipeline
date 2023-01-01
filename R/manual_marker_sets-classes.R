#' Get marker set from `manual_datasets()`
#'
#' The package collected some marker sets from research articles in
#' `manual_datasets()`, this function just provide a method to extract the
#' specific marker set from the datasets
#'
#' @param name The name of the marker set to extract. `name` usually is the
#' surname of the first author followed by the years of the published article
#' connected with "_".
#' @return A `marker_set` object.
#' @export
#' @name get_markers
get_markers <- function(name) {
    manual_cell_marker_datasets[[name]]
}

#' cell markers datasets collected from research articles
#'
#' We manullay collected some marker sets from research articles,
#' `manual_datasets` function just returns the database with a class of
#' `manual_cell_marker_datasets` which has a specific `print` method.
#' @return An `manual_cell_marker_datasets` object.
#' @export
#' @name manual_datasets
manual_datasets <- function() {
    manual_cell_marker_datasets
}

#' @param x An `manual_cell_marker_datasets` object.
#' @param ... Not used currently
#' @export
#' @rdname manual_datasets
print.manual_cell_marker_datasets <- function(x, ...) {
    cli::cli_text("A total of {rlang::env_length(x)} cell markers set{?s}")
}

manual_cell_marker_datasets <- structure(
    rlang::new_environment(),
    class = "manual_cell_marker_datasets"
)

# convention: name should be the surname of the first author followed by the
# years of the published article connected with "_"
new_marker_set <- function(name, ..., reference) {
    marker_set <- rlang::list2(...)
    # every elements in ... should be named
    # validate_marker_set(marker_set) ? cannot use devtools::load_all() when
    # enable this line.
    
    if (rlang::env_has(manual_cell_marker_datasets, nms = name)) {
        cli::cli_abort("Existed cell marker set found in datasets")
    }
    rlang::env_bind(
        .env = manual_cell_marker_datasets,
        !!name := structure(
            marker_set,
            class = "marker_set",
            reference = reference
        )
    )
}

#' @param x An `marker_set` object
#' @param ... Not used currently
#' @export
#' @rdname get_markers
print.marker_set <- function(x, ...) {
    cli::cli_text("Cell marker set derived from: {.url {attr(x, \"reference\")}}")
    cli::cli_rule("marker set details")
    # cat main markers if it exits
    if ("main" %in% names(x)) {
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
    }
    # cat other markers
    other_items <- x[setdiff(names(x), "main")]
    if (length(other_items)) {
        cli::cli_ul()
        cli::cli_text("Including markers for {length(other_items)} group{?s}")
        .mapply(
            cli_list,
            list(
                label = names(other_items),
                items = lapply(other_items, names)
            ),
            MoreArgs = NULL
        )
        cli::cli_end()
    }
    invisible(x)
}

cli_list <- function(label, items, sep = ": ", add_num = TRUE) {
    items <- cli::cli_vec(items, list("vec-trunc" = 3L))
    message <- "{.field {label}}{sep}{.val {items}}"
    if (add_num) {
        message <- paste(message, "({length(items)} item{?s})", sep = " ")
    }
    cli::cli_li(message)
}

validate_marker_set <- function(x) {
    x_type <- typeof(x)
    if (identical(x_type, "character")) {
        # No need names for character
        is_right <- TRUE
    } else if (identical(x_type, "list")) {
        # for a list, we should check all elements have names,
        # and then recall this function to check every elments
        is_right <- list_has_element_names(x) &&
            all(vapply(x, validate_marker_set, logical(1L)))
    } else if (!is.null(x)) {
        cli::cli_abort("all elements should be {.val NULL}, or a {.cls list} or a {.cls chracter}")
    }
    if (!is_right) {
        cli::cli_abort("all elements or sub-elements should be named")
    }
    is_right
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
