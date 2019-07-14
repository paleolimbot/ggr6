
context("renderer-graphics")

test_that("graphics renderer can render all geometry types", {

  f <- function() {
    graphic <- Graphic$new()
    renderer <- RendererGraphics$new()
    scale_x <- ScaleSimpleContinuous$new("x")$set_limits(c(-1, 11))
    scale_y <- ScaleSimpleContinuous$new("y")$set_limits(c(-1, 11))
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
