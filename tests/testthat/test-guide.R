
test_that("GuideNull has the correct key when trained", {
  guide <- GuideNull$new()

  scale <- ScaleContinuous$new(c("aes1", "aes2"))$set_breaks(1:3)
  expect_identical(guide$train(scale), guide)
  expect_identical(colnames(guide$key), c(".breaks", ".labels", "aes1", "aes2"))
  expect_equal(nrow(guide$key), 0)
})

test_that("GuideLegend returns censored breaks in key", {
  guide <- GuideLegend$new()

  scale <- ScaleContinuous$new(c("aes1", "aes2"))$
    set_breaks(1:5)$
    set_limits(c(1.5, 4.5))
  expect_identical(guide$train(scale), guide)
  expect_identical(colnames(guide$key), c(".breaks", ".labels", "aes1", "aes2"))
  expect_identical(guide$key$.breaks, 2:4)
})
