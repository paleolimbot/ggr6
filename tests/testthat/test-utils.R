
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
