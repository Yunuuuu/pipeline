test_that("Pipeline class works", {
    pipeline <- Pipeline$new()
    expect_true(R6::is.R6(pipeline))

    # test get_step_list with NULL works well
    expect_identical(
        pipeline$get_step_list(),
        structure(list(), names = character())
    )

    # test add_steps add length zero element
    pipeline$add_steps()
    expect_identical(
        pipeline$get_step_list(),
        structure(list(), names = character())
    )
    # test add_steps add steps directly
    pipeline$add_steps(
        step("a", 1L, deps = c("b", "c")),
        step("b", "b1", deps = "c")
    )
    expect_identical(
        pipeline$get_step("a"),
        step("a", 1L, deps = c("b", "c"))
    )
    expect_identical(
        pipeline$get_step("b"),
        step("b", "b1", deps = "c")
    )

    # test add_steps add a list of steps
    expect_warning(pipeline$add_steps(list(
        step("a", 2L, deps = c("b", "c")),
        step("b", "b2", deps = "c")
    )))
    expect_identical(
        pipeline$get_step("a"),
        step("a", 2L, deps = c("b", "c"))
    )
    expect_identical(
        pipeline$get_step("b"),
        step("b", "b2", deps = "c")
    )

    # test set_step_collections works well
    step_list_init <- list(
        step("a", paste(1, 2), deps = c("b", "c")),
        step("b", c(c, 1, 2), deps = "c"),
        step("c", c(1, 2), bind = TRUE),
        step("d", "d", deps = NULL),
        step("e", 1, "f")
    )
    names(step_list_init) <- vapply(
        step_list_init, "[[", character(1L), "id",
        USE.NAMES = FALSE
    )
    pipeline$set_step_collections(step_list_init)
    expect_identical(pipeline$get_step_list(), step_list_init)

    # test get_step_list with specified ids works well
    expect_identical(
        pipeline$get_step_list(c("a", "b")),
        step_list_init[c("a", "b")]
    )
    expect_error(pipeline$get_step_list(letters))

    # test remove_steps works well
    expect_error(pipeline$remove_steps(c("f", "h", "g")))
    expect_error(pipeline$remove_steps(NULL))
    pipeline$remove_steps("e")
    expect_identical(
        pipeline$get_step_list(),
        step_list_init[c("a", "b", "c", "d")]
    )

    # test finish_steps works well
    pipeline$finish_steps("a")
    expect_s3_class(pipeline$get_step("a"), "step")
    expect_true(pipeline$get_step("a")$finished)

    # test reset_steps works well
    pipeline$reset_steps("a")
    expect_s3_class(pipeline$get_step("a"), "step")
    expect_false(pipeline$get_step("a")$finished)

    # test reset_steps works well
    pipeline$finish_steps(c("a", "b", "c"))
    pipeline$reset_step_collections()
    expect_false(all(
        vapply(pipeline$get_step_list(), "[[", logical(1L), "finished")
    ))

    # test modify_step works well
    expect_error(pipeline$modify_step("e"))
    expect_error(pipeline$modify_step("d", 1L))
    pipeline$modify_step("d", deps = "a", seed = 1L)
    expect_identical(
        pipeline$get_step("d"),
        step("d", "d", deps = "a", seed = 1L)
    )
})
