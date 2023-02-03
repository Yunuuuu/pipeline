#' @keywords internal
set_cache_dir <- function() {
    cache_dir <- tools::R_user_dir(.packageName, which = "cache")
    if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
    }
    cache_dir
}

#' @keywords internal
set_cache_file <- function(pattern, ext) {
    cache_file <- tempfile(
        pattern = pattern,
        tmpdir = set_cache_dir(),
        fileext = ext
    )
    if (!file.exists(cache_file)) {
        file.create(cache_file, showWarnings = FALSE)
    }
    cache_file
}
