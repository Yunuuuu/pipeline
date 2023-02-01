#' Run expressions on a gridengine system
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]>, Expressions to defuse.
#' @param node Node to run the expressions.
#' @param name A string define the job name.
#' @param wd Path define the working directory.
#' @param resource_list A named characters define the given resources.
#' @param wait A logical (not `NA`) indicating whether the R interpreter should
#' wait for the command to finish, or run it asynchronously. When running the
#' command asynchronously, no output will be displayed on the Rgui console in
#' Windows (it will be dropped, instead).
#' @return If `wait = TRUE` the value is the exit status returned by the
#' command, and if `wait = FALSE` it is `0` (the conventional success value). An
#' error code (`0` for success), given the [invisible] attribute (so needs to be
#' printed explicitly). If the command could not be run for any reason, the
#' value is `127` and a warning is issued (as from R 3.5.0).
#' @export
qsub <- function(..., node = NULL, name = NULL, wd = getwd(), resource_list = character(), wait = FALSE) {
    qsub_core <- Sys.getenv("QSUB", unset = "", names = FALSE)
    if (!nzchar(qsub_core)) {
        qsub_core <- Sys.which("qsub")
        if (!nzchar(qsub_core)) {
            cli::cli_abort("Cannot find {.code qsub} command")
        }
    }
    itnernal_args <- c(
        "-V", sprintf("-wd %s", wd),
        sprintf("-S %s", file.path(R.home("bin"), "Rscript"))
    )
    if (!is.null(name)) {
        itnernal_args <- c(itnernal_args, sprintf("-N %s", name))
    }
    if (rlang::has_name(resource_list, "node")) {
        resource_list <- rename(resource_list, c(node = "h"))
    }
    if (!is.null(node)) {
        resource_list <- resource_list[names(resource_list) !=
            "h"]
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
    if (length(resource_list)) {
        resource_list <- paste(resource_list, collapse = ",")
        itnernal_args <- c(itnernal_args, sprintf("-l %s", resource_list))
    }
    rscript_file <- tempfile(pattern = "r_qsub_", fileext = ".R")
    if (!file.exists(rscript_file)) {
        file.create(rscript_file, showWarnings = FALSE)
    }
    insert_exprs_into_rscript(..., file = rscript_file)
    cli::cli_inform("Submitting job with {.code qsub}")
    system2(
        path.expand(qsub_core),
        args = c(itnernal_args, rscript_file),
        wait = wait, stdout = FALSE, stderr = FALSE
    )
}

#' @keywords internal
insert_exprs_into_rscript <- function(..., file) {
    exprs <- rlang::enexprs(...)
    exprs_chr <- lapply(exprs, function(expr) {
        deparse(rlang::quo_squash(expr, warn = FALSE), width.cutoff = 500L)
    })
    exprs_chr <- unlist(exprs_chr, recursive = FALSE, use.names = FALSE)
    file_con <- file(file)
    writeLines(exprs_chr, con = file_con, sep = "\n")
    close(file_con)
}
