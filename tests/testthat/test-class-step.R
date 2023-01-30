test_that("`step` id argument works", {
    expect_error(step(1L, paste(1, 2)))
    expect_error(step("", paste(1, 2)))
    expect_error(step(NA_character_, paste(1, 2)))
    expect_error(step(".data", paste(1, 2)))
    expect_error(step(".pipeline", paste(1, 2)))
    expect_error(step(".step", paste(1, 2)))
    expect_error(step(c("a", "b"), paste(1, 2)))
    expect_s3_class(step("a", paste(1, 2)), "step")
})

test_that("`create_step` expression argument works", {
    expect_s3_class(create_step("a", 2), "step")
    expect_s3_class(create_step("a", rlang::expr(a)), "step")
})

test_that("`step` deps argument works", {
    expect_error(step("a", paste(1, 2), deps = 1L))
    expect_error(step("a", paste(1, 2), deps = FALSE))
    expect_s3_class(step("a", paste(1, 2), deps = NULL), "step")
    expect_s3_class(step("a", paste(1, 2), deps = NA_character_), "step")
    expect_s3_class(step("a", paste(1, 2), deps = ""), "step")
    expect_s3_class(step("a", paste(1, 2), deps = "dep1"), "step")
})

test_that("`step` finished argument works", {
    expect_error(step("a", paste(1, 2), finished = NULL))
    expect_error(step("a", paste(1, 2), finished = 1L))
    expect_error(step("a", paste(1, 2), finished = ""))
    expect_error(step("a", paste(1, 2), finished = NA))
    expect_s3_class(step("a", paste(1, 2), finished = TRUE), "step")
})

test_that("`step` return argument works", {
    expect_error(step("a", paste(1, 2), return = NULL))
    expect_error(step("a", paste(1, 2), return = 1L))
    expect_error(step("a", paste(1, 2), return = ""))
    expect_error(step("a", paste(1, 2), return = NA))
    expect_s3_class(step("a", paste(1, 2), return = TRUE), "step")
})

test_that("`step` seed argument works", {
    expect_error(step("a", paste(1, 2), seed = NULL))
    expect_error(step("a", paste(1, 2), seed = ""))
    expect_error(step("a", paste(1, 2), seed = NA))
    expect_s3_class(step("a", paste(1, 2), seed = TRUE), "step")
    expect_s3_class(step("a", paste(1, 2), seed = 1), "step")
    expect_s3_class(step("a", paste(1, 2), seed = 2L), "step")
})

test_that("`step` index works", {
    step1 <- step("a", paste(1, 2), seed = TRUE)
    expect_s3_class(step1, "step")
    expect_identical(step1$id, "a")
    expect_true(rlang::is_quosure(step1$expr))
    expect_true(step1$seed)

    step1$id <- "b"
    expect_s3_class(step1, "step")
    expect_identical(step1$id, "b")
    expect_error(step1$id <- 1L)

    step1[["id"]] <- "c"
    expect_s3_class(step1, "step")
    expect_identical(step1$id, "c")
    expect_error(step1[["id"]] <- 1L)
    expect_error(step1[["id"]] <- list(NULL))

    step1["id"] <- "d"
    expect_s3_class(step1, "step")
    expect_identical(step1$id, "d")
    expect_error(step1["id"] <- 1L)

    step1["id"] <- list("e")
    expect_s3_class(step1, "step")
    expect_identical(step1$id, "e")
    expect_error(step1["id"] <- list(1L))
    expect_error(step1["id"] <- list(NULL))
})
