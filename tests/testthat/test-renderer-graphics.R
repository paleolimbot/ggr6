
context("renderer-graphics")

test_that("graphics renderer can render all geometry types", {

  f <- function() {
    graphic <- Graphic$new()
    renderer <- RendererGraphics$new()
    scale_x <- ScaleContinuousPosition$new("x")$set_limits(c(-1, 11))
    scale_y <- ScaleContinuousPosition$new("y")$set_limits(c(-1, 11))
    scales <- ScaleList$new()$add(scale_x)$add(scale_y)
    panel <- Panel$new(scales = scales)

    renderer$render_panels(
      graphic,

      renderer$render_panel(
        panel,

        renderer$render_null(),
        renderer$render_points(x = c(1, 3), y = c(1, 3)),
        renderer$render_path(x = c(1, 3, 4, 4), y = c(1, 3, 4, 5), group = c(1, 1, 2, 2)),
        renderer$render_polygon(
          x = c(0, 1, 1, 0,   9, 10, 10, 9),
          y = c(0, 0, 1, 1,   9, 9, 10, 10),
          group = c(1, 1, 1, 1, 2, 2, 2, 2)
        ),
        renderer$render_text(x = c(6, 8), y = c(6, 8), label = c("six", "eight"))
      )
    )
  }

  vdiffr::expect_doppelganger("graphics renderer basic test", f)
})

test_that("graphics renderer can render a basic plot", {
  f <- function() {
    graphic <- Graphic$new()
    renderer <- RendererGraphics$new()

    tbl <- tibble(x = 6:10, y = 1:5, col = rep(c("a", "b"), length.out = 5))
    graphic$layers$add(
      Layer$new(tbl, geom = GeomPoint$new())
    )

    graphic$scales$add(
      ScaleDiscrete$new("col")$
        set_palette_factory(scales::hue_pal())$
        set_na_value("grey50")
    )

    builder <- Builder$new(graphic, renderer)
    builder$render()
  }

  vdiffr::expect_doppelganger("graphics renderer built graphic", f)
})

test_that("the graphics legend guide only accepts named arguments to the constructor", {
  expect_error(GuideGraphicsLegend$new(a = 1, b = 2, 3), "must be named")
  expect_silent(GuideGraphicsLegend$new())
})

test_that("the graphics legend guide can be trained with layer defaults", {
  layers <- LayerList$new()$add(Layer$new(tibble(col = c("a", "b")), geom = GeomPoint$new()))
  col_scale <- ScaleDiscrete$new("col")$
    set_palette_factory(scales::hue_pal())$
    train(c("a", "b"))

  guide <- GuideGraphicsLegend$new()
  guide$train(col_scale)
  guide$train_layers(layers, RendererGraphics$new())

  expect_identical(guide$layer_defaults$pt.lwd, 1)
})

test_that("the graphics legend can be rendered", {
  col_scale <- ScaleDiscrete$new("col")$
    set_palette_factory(scales::hue_pal())$
    train(c("one", "two", "three"))

  guide <- GuideGraphicsLegend$new(pch = 16)$train(col_scale)

  f = function() {
    plot(1:5)
    guide$render(LayerList$new(), Panel$new(), RendererGraphics$new())
  }

  vdiffr::expect_doppelganger("basic graphics legend", f)
})
