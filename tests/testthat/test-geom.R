
test_that("GeomBlank returns NULL", {
  df <- tibble(x = 1)
  expect_identical(GeomBlank$new()$compute_layer(), NULL)
})
