
test_that("Graphic objects can be initiated without arguments", {
  expect_true(is.R6(Graphic$new()))
})

test_that("Layer objects be be added using add_layer()", {
  graphic <- Graphic$new()$add_layer(Layer$new())
  expect_equal(graphic$layers$size(), 1)
  expect_error(graphic$add_layer(NULL), class = "bad_r6_type")
})

test_that("Scale objects be be added using add_scale()", {
  graphic <- Graphic$new()$add_scale(ScaleNull$new())
  expect_equal(graphic$scales$size(), 1)
  expect_error(graphic$add_scale(NULL), class = "bad_r6_type")
})

test_that("Graphic$add_scale() does not replace existing scales", {
  graphic <- Graphic$new()$add_scale(ScaleNull$new("x"))
  scale2 <- ScaleNull$new(c("x", "x2"))
  expect_error(graphic$add_scale(scale2), "already exists")
  expect_silent(graphic$replace_scale(scale2))
  expect_identical(graphic$scales$scale("x"), scale2)
})
