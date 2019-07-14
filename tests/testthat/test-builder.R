
test_that("builder can build an empty plot", {
  builder <- Builder$new(Graphic$new())
  expect_silent(builder$build_init())
  expect_silent(builder$build())
  expect_is(builder$render(), "rendered_panels")
})

test_that("builder can build a basic plot", {

  tbl <- tibble(x = 6:10, y = 1:5, col = rep(c("a", "b"), length.out = 5))

  graphic <- Graphic$new()
  graphic$layers$add(
    Layer$new(tbl, geom = GeomPoint$new())
  )

  builder <- Builder$new(graphic)

  # initializer
  expect_is(builder$build_init(), "Builder")
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

  # check the whole build process
  builder$build()
  expect_identical(builder$plot_data[[1]], tbl)

  # make sure the plot can be rendered
  rendered <- builder$render()
  expect_is(rendered, "rendered_panels")
})

test_that("Stat and geom defaults are processed during plot build", {
  tbl <- tibble(x = 6:10, y = 1:5, col = rep(c("a", "b"), length.out = 5))

  GeomPointDefault <- R6Class(
    "GeomPointDefault", inherit = GeomPoint,
    public = list(
      default_aesthetic_values = function(renderer) {
        list(default_geom_aesthetic = 1L)
      }
    )
  )

  StatIdentityDefault <- R6Class(
    "StatIdentityDefault", inherit = StatIdentity,
    public = list(
      default_aesthetic_values = function(renderer) {
        list(default_stat_aesthetic = 2L)
      }
    )
  )

  graphic <- Graphic$new()
  layer <- Layer$new(
    tbl,
    geom = GeomPointDefault$new(),
    stat = StatIdentityDefault$new()
  )

  graphic$layers$add(layer)
  builder <- Builder$new(graphic)
  builder$build()
  built_data <- builder$plot_data[[1]]
  expect_true("default_stat_aesthetic" %in% colnames(built_data))
  expect_true("default_geom_aesthetic" %in% colnames(built_data))
  expect_true(all(built_data$default_geom_aesthetic == 1))
  expect_true(all(built_data$default_stat_aesthetic == 2))
})

test_that("Stat and geom overrides are processed during plot build", {
  tbl <- tibble(x = 6:10, y = 1:5, col = rep(c("a", "b"), length.out = 5))

  graphic <- Graphic$new()
  layer <- Layer$new(
    tbl,
    geom = GeomPoint$new(list(set_geom_aesthetic = 1L)),
    stat = StatIdentity$new(list(set_stat_aesthetic = 2L))
  )

  graphic$layers$add(layer)
  builder <- Builder$new(graphic)
  builder$build()
  built_data <- builder$plot_data[[1]]
  expect_true("set_stat_aesthetic" %in% colnames(built_data))
  expect_true("set_geom_aesthetic" %in% colnames(built_data))
  expect_true(all(built_data$set_geom_aesthetic == 1))
  expect_true(all(built_data$set_stat_aesthetic == 2))
})
