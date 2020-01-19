
test_that("base inheritance caclulation works as expected", {
  inherit <- list(
    bass = "fish",
    fish = "animal",
    striped_bass = "bass",
    guppie = "fish"
  )

  expect_identical(
    calc_inheritance("striped_bass", inherit),
    c("animal", "fish", "bass", "striped_bass")
  )
  expect_identical(calc_inheritance("guppie", inherit), c("animal", "fish", "guppie"))
  expect_identical(calc_inheritance("plant", inherit), "plant")

  bad_inherit <- c(inherit, list(animal = "guppie"))
  expect_error(calc_inheritance("fish", bad_inherit), class = "circular_reference")
})

test_that("cascading elements are combined correctly", {

  expect_identical(cascade("parent", "child"), "child")
  expect_identical(
    cascade("parent", ~paste0(.x, sep, "child"), list(data = list(sep = "->"))),
    "parent->child"
  )
  expect_identical(
    cascade("parent", rlang::quo(paste0(.x, sep, "child")), list(data = list(sep = "->"))),
    "parent->child"
  )
  expect_identical(cascade(4, sqrt), 2)

  element_one <- cascading_element(key2 = "one_two", key3 = "one_three")
  element_two <- cascading_element(key1 = "two_one", key2 = "two_two", key3 = ~paste0(.x, "->two_three"))
  one_two <- cascade(element_one, element_two)
  expect_identical(one_two$key3, "one_three->two_three")
  expect_identical(one_two$key1, "two_one")
  expect_identical(one_two$key2, "two_two")
})
