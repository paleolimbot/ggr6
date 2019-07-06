
test_that("not_implemented() throws a subclassed condition", {
  expect_error(not_implemented(), class = "not_implemented")
})

test_that("invalid_state() throws a subclassed condition", {
  expect_error(invalid_state(), class = "invalid_state")
})
