
test_that("identity renderer can render all geometries", {
  renderer <- IdentityRenderer$new()
  args <- list(
    x = "x", y = "y", feature = "feature", label = "label",
    piece = "piece", extra = "extra"
  )
  arg_names <- names(args)

  expect_identical(
    do.call(renderer$render_path, args)[arg_names],
    args
  )

  expect_identical(
    do.call(renderer$render_points, args)[arg_names],
    args
  )

  expect_identical(
    do.call(renderer$render_polygon, args)[arg_names],
    args
  )

  expect_identical(
    do.call(renderer$render_text, args)[arg_names],
    args
  )

  expect_equivalent(
    renderer$render_null(),
    list()
  )

  expect_equivalent(
    renderer$render_stack(1, 2, 3),
    list(1, 2, 3)
  )


  panel <- Panel$new()
  expect_equivalent(
    renderer$render_panel(panel, 1, 2, 3),
    list(panel = panel, panel = list(1, 2, 3))
  )

  expect_equivalent(
    renderer$render_panels(NULL, 1, 2, 3)$panels,
    list(1, 2, 3)
  )

  expect_null(renderer$render_panels(NULL, 1, 2, 3)$graphic)
})

test_that("identity renderer returns position scales for the x and y aesthetics", {
  renderer <- IdentityRenderer$new()
  expect_is(renderer$default_scale(1, "x"), "ScaleContinuousPosition")
  expect_is(renderer$default_scale(letters[1], "y"), "ScaleDiscretePosition")
})

test_that("identity renderer contains a default scale that has an identity mapping", {
  renderer <- IdentityRenderer$new()
  scale <- renderer$default_scale(NULL, "aesthetic")
  expect_identical(scale$map(c(1, 2, 3)), c(1, 2, 3))
  expect_identical(scale$aesthetics, "aesthetic")
})
