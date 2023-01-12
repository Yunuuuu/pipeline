step_unbind <- function(nms, deps = NULL) {
    nms <- rlang::enquo(nms)
    create_step(
        id = "unbind",
        call = rlang::expr(.pipeline$env_unbind(nms = !!nms)),
        deps = deps, finished = FALSE, return = FALSE, seed = FALSE
    )
}
