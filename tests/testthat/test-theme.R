
test_that("default theme just returns the default value", {
  expect_null(theme("anything", NULL))
  expect_identical(theme("really, anything", "a value"), "a value")
  expect_error(theme("key with no default"), class = "no_theme_key")
})

test_that("theme can be temporarily set", {
  test_theme <- CascadingTheme$new()$set_values("first" = 1)
  expect_null(theme("first", NULL))
  expect_identical(with_theme(test_theme, theme("first")), 1)
  expect_error(with_theme(test_theme, theme("second")), class = "no_theme_key")
  expect_null(with_theme(test_theme, theme("second", NULL)))
  expect_null(theme("first", NULL))
})

test_that("with_theme() can be nested", {
  test_theme <- CascadingTheme$new()$set_values("first" = 1)
  test_theme2 <- CascadingTheme$new()$set_values("second" = 2)

  expect_null(theme("first", NULL))
  expect_null(theme("second", NULL))

  expect_identical(with_theme(test_theme, theme("first")), 1)
  expect_error(with_theme(test_theme, theme("second")), class = "no_theme_key")
  expect_identical(
    with_theme(
      test_theme,
      with_theme(test_theme2, theme("second"))
    ),
    2
  )

  expect_error(
    with_theme(
      test_theme,
      with_theme(test_theme2, theme("first"))
    ),
    class = "no_theme_key"
  )

  expect_null(theme("first", NULL))
  expect_null(theme("second", NULL))
})
