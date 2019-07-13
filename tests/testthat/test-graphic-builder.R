
test_that("builder can build a basic plot", {

  tbl <- tibble(x = 6:10, y = 1:5, col = rep(c("a", "b"), length.out = 5))

  graphic <- Graphic$new()
  graphic$layers$add(
    Layer$new(tbl, geom = GeomPoint$new())
  )

  builder <- GraphicBuilder$new(graphic)

  # initializer
  expect_is(builder$build_init(), "GraphicBuilder")
  expect_equal(ncol(builder$plot_data), 1)
  expect_equal(nrow(builder$plot_data), 1)
  expect_length(builder$panels, 1)
  expect_is(builder$panels[[1]], "Panel")

  # keep panel for checking scales
  panel <- builder$panels[[1]]

  prepared <- builder$prepare_data_and_scales(builder$plot_data)
  expect_identical(prepared[[1]], tbl)
  expect_setequal(panel$scales$aesthetics(), c("x", "y", "col"))

  computed <- builder$compute_statistics(prepared)
  expect_identical(computed[[1]], tbl)

  computed_pos <- builder$compute_positions(computed)
  expect_identical(computed_pos[[1]], tbl)

  mapped_np <- builder$map_non_position_scales(computed_pos)
  expect_identical(mapped_np[[1]], tbl)

  finished <- builder$finish_data(mapped_np)
  expect_identical(finished[[1]], tbl)

  # check the whole process
  builder$build()
  expect_identical(builder$plot_data[[1]], tbl)
})
