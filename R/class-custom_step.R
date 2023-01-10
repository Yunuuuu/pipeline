step_unbind <- function(nms, deps = NULL) {
    new_custom_step(
        id = "unbind", call = NULL, 
        deps = deps, finished = FALSE, return = FALSE, seed = FALSE,
        nms = as.character(nms)
    )
}

new_custom_step <- function(...) {
    step <- new_step(...)
    class(step) <- c("custom_step", class(step))
    step
}

is_custom_step <- function(x) inherits(x, "custom_step")
