#' Run expressions on a gridengine system
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]>, Expressions to defuse.
#' Multiple expressions are run sequentially in the order the exist in `...`.
#' @param node Node to run the expressions.
#' @param name A string define the job name.
#' @param wd Path define the working directory.
#' @param resources A named characters define the given resources.
#' @param wait A logical (not `NA`) indicating whether the R interpreter should
#' wait for the command to finish, or run it asynchronously.
#' @param globals (optional) a scalar logical, a character vector, or a named
#' list to control how globals are handled. For details, see section 'Globals
#' used by expressions'
#' @param global_on_missing (character string) Action to take when non-existing
#' global variables ("globals" or "unknowns") are identified when the
#' expressions are created. If "error", an error is generated immediately. If
#' "ignore", no action is taken and an attempt to evaluate the expression will
#' be made. The latter is useful when there is a risk for false-positive globals
#' being identified, e.g. when expressions contain non-standard evaluation
#' (NSE). (Default: "ignore").
#' @param packages (optional) a character vector specifying packages
#' to be attached in the qsub R session.
#' @section Globals used by expressions:
#' Global objects (short _globals_) are objects (e.g. variables and
#' functions) that are needed in order for the expressions to be
#' evaluated while not being local objects. For example, in
#' \preformatted{
#'   a <- 42
#'   f <- qsub(b <- 2, a * b)
#' }
#' variable `a` is a global of assignment `f` whereas
#' `b` is a local variable.
#' In order for the expresssions to be resolved successfully (and correctly),
#' all globals need to be gathered when the expresssions are created such that
#' they are available whenever and wherever the expresssions is resolved in the
#' qsub R session.
#'
#' The default behavior (`globals = TRUE`), is that globals are automatically
#' identified and gathered.  More precisely, globals are identified via code
#' inspection of the expressions and their values are retrieved with environment
#' `envir` as the starting point (basically via `get(global, envir = envir,
#' inherits = TRUE)`).
#' _In most cases, such automatic collection of globals is sufficient
#' and less tedious and error prone than if they are manually specified_.
#'
#' However, for full control, it is also possible to explicitly specify
#' exactly which the globals are by providing their names as a character
#' vector.
#' In the above example, we could use
#' \preformatted{
#'   a <- 42
#'   f <- qsub(b <- 2, a * b, globals = "a")
#' }
#'
#' Yet another alternative is to explicitly specify also their values
#' using a named list as in
#' \preformatted{
#'   a <- 42
#'   f <- qsub(b <- 2, a * b, globals = list(a = a))
#' }
#' or
#' \preformatted{
#'   f <- qsub(b <- 2, a * b, globals = list(a = 42))
#' }
#'
#' Specifying globals explicitly avoids the overhead added from automatically
#' identifying the globals and gathering their values.  Furthermore, if we know
#' that the expressions does not make use of any global variables, we can
#' disable the automatic search for globals by using
#' \preformatted{
#'   f <- qsub({ a <- 42; b <- 2; a * b }, globals = FALSE)
#' }
#'
#' Expressions often make use of functions from one or more packages.  As
#' long as these functions are part of the set of globals, the package will make
#' sure that those packages are attached when the expressions is resolved in the
#' qsub R session.  Because there is no need for such globals to be frozen or
#' exported, the package will not export them, which reduces the amount of
#' transferred objects.
#' For example, in
#' \preformatted{
#'   x <- rnorm(1000)
#'   f <- qsub({ median(x) })
#' }
#' variable `x` and `median()` are globals, but only `x`
#' is exported whereas `median()`, which is part of the \pkg{stats}
#' package, is not exported.  Instead it is made sure that the \pkg{stats}
#' package is on the search path when the expressions are evaluated in the qsub
#' R session.
#' Effectively, the above becomes
#' \preformatted{
#'   x <- rnorm(1000)
#'   f <- qsub({
#'     library("stats")
#'     median(x)
#'   })
#' }
#' To manually specify this, one can either do
#' \preformatted{
#'   x <- rnorm(1000)
#'   f <- qsub({
#'     median(x)
#'   }, globals = list(x = x, median = stats::median)
#' }
#' or
#' \preformatted{
#'   x <- rnorm(1000)
#'   f <- qsub({
#'     library("stats")
#'     median(x)
#'   }, globals = list(x = x))
#' }
#' Both are effectively the same.
#'
#'
#' Although rarely needed, a combination of automatic identification and manual
#' specification of globals is supported via attributes `add` (to add
#' false negatives) and `ignore` (to ignore false positives) on value
#' `TRUE`.  For example, with
#' `globals = structure(TRUE, ignore = "b", add = "a")` any globals
#' automatically identified except `b` will be used in addition to
#' global `a`.
#'
#' @return If `wait = TRUE` the value is the exit status returned by the
#' command, and if `wait = FALSE` it is `0` (the conventional success value). An
#' error code (`0` for success), given the [invisible] attribute (so needs to be
#' printed explicitly). If the command could not be run for any reason, the
#' value is `127` and a warning is issued (as from R 3.5.0).
#' @export
qsub <- function(..., node = NULL, name = NULL, wd = getwd(), resources = character(), wait = FALSE, globals = TRUE, global_on_missing = c("ignore", "error"), packages = NULL) {
    qsub_core <- Sys.getenv("QSUB", unset = "", names = FALSE)
    if (!nzchar(qsub_core)) {
        qsub_core <- Sys.which("qsub")
        if (!nzchar(qsub_core)) {
            cli::cli_abort("Cannot find {.code qsub} command")
        }
    }
    qsub_args <- c(
        "-V", sprintf("-wd %s", wd),
        sprintf("-S %s", file.path(R.home("bin"), "Rscript"))
    )
    if (!is.null(name)) {
        qsub_args <- c(qsub_args, sprintf("-N %s", name))
    }
    assert_class(resources, is.character, "character", null_ok = TRUE)
    if (rlang::has_name(resources, "node")) {
        resources <- rename(resources, c(node = "h"))
    }
    if (!is.null(node)) {
        resources <- resources[names(resources) != "h"]
        resources <- c(h = node, resources)
    }
    resource_has_name <- rlang::have_name(resources)
    resources <- c(
        paste(names(resources[resource_has_name]),
            resources[resource_has_name],
            sep = "="
        ),
        resources[!resource_has_name]
    )
    if (length(resources)) {
        resources <- paste(resources, collapse = ",")
        qsub_args <- c(qsub_args, sprintf("-l %s", resources))
    }
    rscript_file <- set_cache_file(pattern = "r_qsub_", ext = ".R")
    insert_exprs_into_rscript(...,
        file = rscript_file,
        globals = globals,
        packages = packages,
        global_on_missing = global_on_missing
    )
    cli::cli_inform(c(
        "Submitting job with {.code qsub}",
        "i" = "Rscript file path: {.path {rscript_file}}"
    ))
    system2(
        path.expand(qsub_core),
        args = c(qsub_args, rscript_file),
        wait = wait, stdout = FALSE, stderr = FALSE
    )
}

#' @keywords internal
insert_exprs_into_rscript <- function(..., file, globals, packages, global_on_missing) {
    exprs <- lapply(rlang::enexprs(...), function(expr) {
        rlang::quo_squash(expr, warn = TRUE)
    })
    exprs_len <- length(exprs)
    if (exprs_len > 1L) {
        expr <- rlang::call2("{", !!!exprs)
    } else if (exprs_len == 1L) {
        expr <- exprs[[1L]]
    }
    gp <- get_globals_and_pkgs(
        expr,
        envir = parent.frame(),
        globals = globals,
        global_on_missing = global_on_missing
    )
    exprs_chr <- lapply(exprs, function(expr) {
        deparse(expr, width.cutoff = 500L, backtick = TRUE)
    })
    exprs_chr <- unlist(exprs_chr, recursive = FALSE, use.names = FALSE)

    globals <- gp$globals
    pkgs <- gp$packages

    # merge user-specied packages
    assert_class(packages, is.character, "character", null_ok = TRUE)
    if (length(packages) > 0 || length(pkgs) > 0L) {
        pkgs <- unique(c(pkgs, packages))
    }

    # use current working dir as the cache_dir?
    # the temporary files will be removed
    # cache_dir <- getwd()

    # transfer global variabls into qsub R session
    if (length(globals) > 0L) {
        global_file <- set_cache_file(pattern = "globals_", ext = ".qs")
        qs::qsave(globals, global_file)
        exprs_chr <- c(global_tmpl(global_file), exprs_chr)
    }

    # library pakcage in the qsub R session
    if (length(pkgs) > 0L) {
        pkg_file <- set_cache_file(pattern = "packages_", ext = ".qs")
        qs::qsave(pkgs, pkg_file)
        exprs_chr <- c(pkg_tmpl(pkg_file), exprs_chr)
    }

    file_con <- file(file)
    writeLines(exprs_chr, con = file_con, sep = "\n")
    close(file_con)
}

#' @keywords internal
global_tmpl <- function(global_file) {
    c(
        "cli::cli_inform(\"Importing globals\")",
        sprintf(
            "invisible(list2env(x = qs::qread(\"%s\"), envir = environment(NULL)))",
            global_file
        ),
        sprintf(
            "cli::cli_inform(\"Removing globals tempfile: {.file %s}\")",
            basename(global_file)
        ),
        sprintf("invisible(file.remove(\"%s\"))", global_file),
        "cli::cli_inform(\"\")"
    )
}

#' @keywords internal
pkg_tmpl <- function(pkg_file) {
    c(
        "cli::cli_inform(\"librarying packages\")",
        sprintf(
            "for (pkg in qs::qread(\"%s\")) library(pkg, character.only = TRUE)",
            pkg_file
        ),
        sprintf(
            "cli::cli_inform(\"Removing packages tempfile: {.file %s}\")",
            basename(pkg_file)
        ),
        sprintf("invisible(file.remove(\"%s\"))", pkg_file),
        "cli::cli_inform(\"\")"
    )
}

#' @keywords internal
get_globals_and_pkgs <- function(exprs, globals, global_on_missing = c("ignore", "error"), envir = parent.frame()) {
    global_on_missing <- match.arg(global_on_missing)
    must_exist <- switch(global_on_missing,
        ignore = FALSE,
        error = TRUE
    )
    if (is.character(globals)) {
        globals <- globals::globalsByName(
            globals,
            envir = envir,
            mustExist = must_exist
        )
    } else if (is.logical(globals)) {
        assert_length(globals, 1L, null_ok = FALSE)
        ## Any manually added globals?
        add <- attr(globals, "add", exact = TRUE)
        if (!is.null(add)) {
            if (is.character(add)) {
                add <- globals::globalsByName(
                    add,
                    envir = envir,
                    mustExist = must_exist
                )
            } else if (is.list(add)) {
                # Global object is also a list so it's no need to check if add
                # is a Global object
                add <- globals::as.Globals(add)
            } else {
                cli::cli_abort(c(
                    "Attribute {.field add} of {.arg globals} must be an atomic {character} or a named {.cls list} or a {.cls Globals} object", # nolint
                    "x" = "You have supplied a {.cls {typeof(add)}} object."
                ))
            }
        }

        ## Any manually dropped/ignored globals?
        ignore <- attr(globals, "ignore", exact = TRUE)
        if (!is.null(ignore)) {
            assert_class(ignore, is.character, "character")
        }
        if (globals) {
            globals <- globals::globalsOf(
                ## Passed to globals::findGlobals()
                exprs,
                envir = envir,
                substitute = FALSE,
                ## Passed to globals::findGlobals() via '...'
                dotdotdot = "return",
                method = "ordered",
                unlist = TRUE,
                ## Passed to globals::globalsByName()
                mustExist = must_exist,
                recursive = TRUE
            )
        } else {
            globals <- globals::as.Globals(list())
        }

        ## Drop 'ignore' globals?
        ## FIXME: This should really be implemented in globals::globalsOf()
        if (!is.null(ignore)) {
            if (any(ignore %in% names(globals))) {
                globals <- globals[setdiff(names(globals), ignore)]
            }
        }
        ## Append 'add' globals?
        if (inherits(add, "Globals") && length(add) > 0L) {
            globals <- unique(c(globals, add))
        }
    } else if (is.list(globals)) {
        globals <- globals::as.Globals(globals)
    } else {
        # Global object is also a list so it's no need to check if globals
        # is a Global object
        cli::cli_abort(c(
            "{.arg globals} must be an atomic {.cls character} or a scalar {.cls logical} or a named {.cls list} or a {.cls Globals} object", # nolint
            "x" = "You have supplied a {.cls {typeof(globals)}} object"
        ))
    }

    if (length(globals) > 0L) {
        ## Append packages associated with globals
        pkgs <- globals::packagesOf(globals)

        ## Drop all globals which are located in one of
        ## the packages in 'pkgs'.  They will be available
        ## since those packages are attached.
        where <- attr(globals, "where", exact = TRUE)

        names <- names(globals)
        keep <- rep_len(TRUE, length(globals))
        names(keep) <- names
        for (name in names) {
            pkg <- environmentName(where[[name]])
            pkg <- sub("^package:", "", pkg)
            if (any(pkg == pkgs)) {
                ## Only drop exported objects
                if (exists(name, where = asPkgEnvironment(pkg), inherits = FALSE)) {
                    keep[name] <- FALSE
                }
            }
        }
        globals <- globals[keep]

        ## Now drop globals that are primitive functions or
        ## that are part of the base packages, which now are
        ## part of 'pkgs' if needed.
        globals <- globals::cleanup(globals)
    } else {
        pkgs <- NULL
    }

    ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ## Any packages to export?
    ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if (length(pkgs) > 0L) {
        ## Never attach the 'base' package, because that is always
        ## available for all R sessions / implementations.
        pkgs <- setdiff(pkgs, "base")
        ## Local functions

        ## Record which packages in 'pkgs' that are loaded and
        ## which of them are attached (at this point in time).
        ## isLoaded <- is.element(pkgs, loadedNamespaces())
        isAttached <- is.element(pkgs, attachedPackages())
        pkgs <- pkgs[isAttached]
    }
    list(globals = globals, packages = pkgs)
}

asPkgEnvironment <- function(pkg) {
    name <- sprintf("package:%s", pkg)
    if (!name %in% search()) {
        return(emptyenv())
    }
    as.environment(name)
}

attachedPackages <- function() {
    pkgs <- search()
    pkgs <- grep("^package:", pkgs, value = TRUE)
    pkgs <- sub("^package:", "", pkgs)
    pkgs
}
