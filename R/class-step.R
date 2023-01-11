#' Build a `step` object
#' @description
#' A `step` object define the command to run in the pipeline.
#'  - step: user-friendly helper, the `call` will be defused as a quosure.
#'  - new_step: low-level constructor, the `call` will used as is.
#' @param id A scalar character indicates the identification of the step. Must
#' be unique across the `step_tree`.
#' @param call The command to run. Notes: this will be enclosed by
#' <[`rlang::enquo()`]> to bundle the environment of users. If you use the step
#' in another environment other than the current environment. You should use
#' [new_step] or quote the expression firstly then use
#' <[`!!`][rlang::injection-operator]> to inject the call expression in [step].
#' @param deps A character vector or `NULL` defines the upstream steps to run
#' before runing this step. `NULL` means no dependencies.
#' @param finished A scalar `logical` indicates whether this step has been
#' evaluated.
#' @param return A scalar `logical` indicates whether to keep the returned
#' value. if `FALSE`, the result evaluated from `call` won't be kept.
#' @param seed A scalar `logical` or `numeric`. a `logical` value indicates
#' whether to set seed when evaluated the expression in "call". if `TRUE`, the
#' call is evaluated with a seed (based on the hash of the call object).
#' Otherwise, the call is evaluated without seed. if `numeric`, seed will be set
#' by `set.seed(as.integer(seed))`.
#' @param ...  <[`dynamic-dots`][rlang::dyn-dots]> Other items to extend `step`
#'   object.
#' @return A `step` object.
#' @name step
#' @export
step <- function(id, call, deps = NULL, finished = FALSE, return = TRUE, seed = FALSE, ...) {
    new_step(
        id = id,
        call = rlang::enquo(call),
        deps = deps,
        finished = finished,
        return = return,
        seed = seed,
        ...
    )
}

# low-level constructor
# steps define the command the run in the analysis pipeline
#' @export
#' @rdname step
new_step <- function(id, call, deps = NULL, finished = FALSE, return = TRUE, seed = FALSE, ...) {
    # assert id
    assert_class(id, is.character, class = "character", null_ok = FALSE)
    assert_length(id, 1L, null_ok = FALSE)
    if (is.na(id) || id == "") {
        cli::cli_abort("{.arg id} can't use {.val } or {.code NA_character_}")
    } else if (id == "self") {
        cli::cli_abort("{.arg id} can't use reserved names {.val self}")
    }

    # assert call
    assert_class(call, rlang::is_call, class = "call", null_ok = FALSE)

    # assert deps
    assert_class(deps, is.character, class = "character", null_ok = TRUE)

    # assert finished
    assert_class(finished, is.logical, class = "logical", null_ok = FALSE)
    assert_length(finished, 1L, null_ok = FALSE)
    if (is.na(finished)) {
        cli::cli_abort("{.arg finished} can't use {.code NA}")
    }

    # assert return
    assert_class(return, is.logical, class = "logical", null_ok = FALSE)
    assert_length(return, 1L, null_ok = FALSE)
    if (is.na(return)) {
        cli::cli_abort("{.arg return} can't use {.code NA_character_}")
    }

    # assert seed
    if (!(rlang::is_scalar_logical(seed) ||
        rlang::is_scalar_double(seed) ||
        rlang::is_scalar_integer(seed))) {
        cli::cli_abort("{.arg seed} must be a scalar {.cls logical} or {.cls numeric}")
    }
    if (is.na(seed)) {
        cli::cli_abort("{.arg seed} can't use {.code NA}")
    }

    # assert ...
    dots_list <- rlang::dots_list(..., .homonyms = "error")
    if (!all(has_names(dots_list))) {
        cli::cli_abort("all items in {.arg ...} must be named")
    }
    structure(
        rlang::dots_list(
            id = id, call = call, deps = deps,
            finished = finished, return = return,
            seed = seed,
            !!!dots_list
        ),
        class = c("step", "Pipeline")
    )
}

#' @export
#' @param x A `step` object from which to extract element(s) or in which to
#' replace element(s).
#' @param i The indices specifying elements to extract or replace
#' @param value The value to replace with.
#' @rdname step
`[[.step` <- function(x, i) {
    NextMethod()
}

#' @export
#' @rdname step
`[[<-.step` <- function(x, i, value) {
    rlang::inject(new_step(!!!NextMethod()))
}

#' @export
#' @rdname step
`$.step` <- `[[.step`

#' @export
#' @rdname step
`$<-.step` <- `[[<-.step`

#' @export
#' @rdname step
`[.step` <- function(x, i) {
    NextMethod()
}

#' @export
#' @rdname step
`[<-.step` <- function(x, i, value) {
    rlang::inject(new_step(!!!NextMethod()))
}

#' Reports whether x is a `step` object
#' @param x An object to test
#' @keywords internal
#' @noRd
is_step <- function(x) inherits(x, "step")
