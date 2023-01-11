test_that("step_tree works", {
    step_tree1 <- step_tree(
        a_name = step("a", paste(1, 2), deps = c("b", "c")),
        b_name = step("b", c(c, 1, 2), deps = "c"),
        c_name = step("c", c(1, 2), return = TRUE),
        d_name = step("d", "d", deps = NULL),
        e_name = step("e", 1, "f")
    )
    expect_identical(names(step_tree1), letters[1:5])
    expect_s3_class(step_tree1, "step_tree")
    expect_s3_class(step_tree1$a, "step")
    expect_s3_class(step_tree1[["a"]], "step")
    expect_s3_class(step_tree1["a"], "step_tree")

    step_tree1$f <- step("f", 2)
    expect_s3_class(step_tree1, "step_tree")
    expect_identical(names(step_tree1), letters[1:6])


    step_tree1[["g"]] <- step("g", 2)
    step_tree1[["g"]] <- list(step("g", 2))
    expect_s3_class(step_tree1, "step_tree")
    expect_identical(names(step_tree1), letters[1:7])

    step_tree1["h"] <- list(step("h", 2))
    expect_s3_class(step_tree1, "step_tree")
    expect_identical(names(step_tree1), letters[1:8])

    step_tree1["i"] <- list(step("j", 2))
    expect_s3_class(step_tree1, "step_tree")
    expect_identical(names(step_tree1), letters[c(1:8, 10)])

    expect_error(suppressWarnings(step_tree1["i"] <- step("i", 2)))

})
