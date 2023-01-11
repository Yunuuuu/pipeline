step_unbind <- function(nms, deps = NULL) {
    nms <- rlang::enquo(nms)
    new_step(
        id = "unbind",
        call = rlang::expr(self$env_unbind(nms = !!nms)),
        deps = deps, finished = FALSE, return = FALSE, seed = FALSE
    )
}
