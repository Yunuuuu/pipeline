#' Run expressions on a gridengine system
#' @param ... Expressions to defuse.
#' @param node Node to run the expressions.
#' @param name A string define the job name.
#' @param wd Path define the working directory.
#' @param resource_list A named characters define the given resources.
#' @export 
qsub <- function(..., node = NULL, name = NULL, wd = getwd(), resource_list = character()) {
    qsub_core <- Sys.getenv("QSUB", unset = "", names = FALSE)
    if (!nzchar(qsub_core)) {
        qsub_core <- Sys.which("qsub")
        if (!nzchar(qsub_core)) {
            stop("Cannot find `qsub` command", call. = FALSE)
        }
    }
    itnernal_args <- c(
        "-V", sprintf("-wd %s", wd),
        sprintf("-S %s", file.path(R.home("bin"), "R"))
    )
    if (!is.null(name)) {
        itnernal_args <- c(itnernal_args, sprintf("-N %s", name))
    }
    if (rlang::has_name(resource_list, "node")) {
        resource_list <- rename(resource_list, c(node = "h"))
    }
    if (!is.null(node)) {
        resource_list <- resource_list[names(resource_list) != "h"]
        resource_list <- c(h = node, resource_list)
    }
    resource_has_name <- has_names(resource_list)
    resource_list <- c(
        paste(names(resource_list[resource_has_name]),
            resource_list[resource_has_name],
            sep = "="
        ),
        resource_list[!resource_has_name]
    )
    resource_list <- paste(resource_list, collapse = ",")
    itnernal_args <- c(itnernal_args, sprintf("-l %s", resource_list))
    rscript_file <- tempfile(fileext = ".R")
    if (file.exists(rscript_file)) {
        file.create(rscript_file, showWarnings = FALSE)
    }
    insert_exprs_into_rscript(..., file = rscript_file)
    system2(path.expand(qsub_core), args = c(itnernal_args, rscript_file))
}

#' @keywords internal
insert_exprs_into_rscript <- function(..., file) {
    exprs <- rlang::enexprs(...)
    exprs_chr <- lapply(exprs, rlang::expr_deparse)
    exprs_chr <- unlist(exprs_chr, recursive = FALSE, use.names = FALSE)
    file_con <- file(file)
    writeLines(exprs_chr, con = file_con, sep = "\n")
    close(file_con)
}
