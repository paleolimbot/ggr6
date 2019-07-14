
test_that("not_implemented() throws a subclassed condition", {
  expect_error(not_implemented(), class = "not_implemented")
})

test_that("invalid_state() throws a subclassed condition", {
  expect_error(invalid_state(), class = "invalid_state")
})

test_that("is_discrete() return TRUE for character, factor, and logical", {
  expect_true(is_discrete(TRUE))
  expect_true(is_discrete("character"))
  expect_true(is_discrete(factor("char")))
  expect_false(is_discrete(1))
})

test_that("assert_r6() works as expected", {
  Cls <- R6Class("Cls")
  Cls2 <- R6Class("Cls2")
  expect_error(assert_r6(NULL, "Cls"), class = "bad_r6_type")
  expect_error(assert_r6(Cls2$new(), "Cls"), class = "bad_r6_type")

  cls_inst <- Cls$new()
  expect_identical(assert_r6(cls_inst, "Cls"), cls_inst)
})
