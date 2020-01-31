
test_that("StatIdentity returns identical data", {
  df <- tibble(x = 1)
  expect_identical(df, StatIdentity$new()$compute_panel(df, Panel$new()))
})

test_that("Stat default panel calculation works", {

  StatLength <- R6Class(
    "StatLength", inherit = Stat,

    public = list(
      compute_group = function(data, panel, renderer) {
        tibble(length = nrow(data))
      }
    )
  )

  df <- tibble(x = 1:5, group = rep(1:2, length.out = 5))
  stat <- StatLength$new()
  panel <- Panel$new()
  renderer <- IdentityRenderer$new()

  computed <- stat$compute_panel(df, panel, renderer)
  expect_identical(computed, tibble(group = c(1L, 2L), length = c(3L, 2L)))
})
