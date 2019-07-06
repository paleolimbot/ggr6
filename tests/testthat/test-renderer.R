
test_that("identity renderer can render all geometries", {
  renderer <- PlotRendererIdentity$new()
  args <- list(coords = "coords", group = "group", label = "label", subgroup = "subgroup", extra = "extra")
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
    renderer$render_stack(list(1, 2, 3)),
    list(1, 2, 3)
  )
})
