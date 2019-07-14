
test_that("GeomBlank returns NULL", {
  df <- tibble(x = 1)
  geom <- GeomBlank$new()
  renderer <- RendererIdentity$new()

  expect_is(
    geom$render_panel(df, NULL, renderer),
    "rendered_null"
  )
})

test_that("Geom default panel calculation works", {

  GeomGroupedPoint <- R6Class(
    "GeomGrouped", inherit = Geom,

    public = list(
      render_group = function(data, panel, renderer) {
        renderer$render_points(data$x, 0)
      }
    )
  )

  df <- tibble(x = 1:5, group = rep(1:2, length.out = 5))
  geom <- GeomGroupedPoint$new()
  renderer <- RendererIdentity$new()

  rendered <- geom$render_panel(df, NULL, renderer)
  expect_is(rendered, "rendered_stack")
  expect_length(rendered, 2)
  expect_is(rendered[[1]], "rendered_points")
  expect_equal(rendered[[1]]$x, c(1, 3, 5))
  expect_is(rendered[[2]], "rendered_points")
  expect_equal(rendered[[2]]$x, c(2, 4))
})
