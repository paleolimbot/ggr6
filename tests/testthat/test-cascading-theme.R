
test_that("nodes and values can be validated", {
  expect_identical(
    CascadingTheme$new()$
      set_node_validators(chr_node = assert_chr_scalar)$
      set_nodes(chr_node = "default character")$
      value("chr_node"),
    "default character"
  )

  expect_error(
    CascadingTheme$new()$
      set_node_validators(chr_node = assert_chr_scalar)$
      set_nodes(chr_node = 1)$
      value("chr_node"),
    class = "node_validation_error"
  )

  ct <- expect_silent(
    CascadingTheme$new()$
      set_value_validators(chr_node = assert_chr_scalar)$
      set_nodes(chr_node = 1)
  )

  expect_error(
    ct$value("chr_node"),
    class = "value_validation_error"
  )
})

test_that("empty cascading theme can be created, queried, and compiled", {
  empty <- CascadingTheme$new()
  expect_length(empty$compile(), 0)
  expect_identical(empty$keys(), character(0))
  expect_is(empty$compile(), "compiled_theme")
  expect_is(empty$compile(), "cascading_element")

  expect_identical(empty$node("bad_key", NULL), NULL)
  expect_error(empty$node("bad_key"), "No such node")

  expect_identical(empty$value("bad_key", NULL), NULL)
  expect_error(empty$value("bad_key"), "No such value")

  expect_identical(empty$value_base("bad_key", NULL), NULL)
  expect_error(empty$value_base("bad_key"), "No such node")

  expect_identical(empty$value_validator("bad_key", NULL), NULL)
  expect_error(empty$value_validator("bad_key"), "No validator")

  expect_identical(empty$node_validator("bad_key", NULL), NULL)
  expect_error(empty$node_validator("bad_key"), "No validator")

  expect_error(empty$inheritance("bad_key"), "no such node")
})

test_that("inheritance is correct for a cascading theme", {
  cascading_theme <- CascadingTheme$new()$
    set_nodes(
      animal = "I'm an animal",
      fish = ~paste(parent, "I'm a fish!", sep = "=>"),
      bass = ~paste(parent, "I'm a bass!", sep = "=>"),
      striped_bass = ~paste(parent, "I'm a striped_bass!", sep = "=>"),
      guppie = ~paste(parent, "I'm a guppie!", sep = "=>"),
      plant = "I'm unrelated to fish"
    )$
    set_tree(
      bass = "fish",
      fish = "animal",
      striped_bass = "bass",
      guppie = "fish"
    )

  expect_identical(
    cascading_theme$value("striped_bass"),
    "I'm an animal=>I'm a fish!=>I'm a bass!=>I'm a striped_bass!"
  )

  expect_identical(
    cascading_theme$inheritance("striped_bass"),
    c("animal", "fish", "bass", "striped_bass")
  )
  expect_identical(cascading_theme$inheritance("guppie"), c("animal", "fish", "guppie"))
  expect_identical(cascading_theme$inheritance("plant"), "plant")

  expect_error(
    CascadingTheme$new()$set_tree(child = "parent"),
    "Bad references in tree"
  )

  expect_error(
    CascadingTheme$new()$set_tree("not named"),
    "must have unique keys"
  )

  expect_error(
    CascadingTheme$new()$set_tree(is_named = c("bad", "value")),
    "character vectors of length 1"
  )

  expect_length(CascadingTheme$new()$set_tree()$tree, 0)

  zappable <- CascadingTheme$new()$
    set_nodes(will_be_zapped = NULL, another_node = NULL)$
    set_tree(will_be_zapped = "another_node")
  expect_identical(zappable$inheritance("will_be_zapped"), c("another_node", "will_be_zapped"))
  expect_length(
    zappable$set_tree(will_be_zapped = NULL)$tree,
    0
  )
})

test_that("special cascade() cases are handled the same by $value() and $compile()", {
  cascading_theme <- CascadingTheme$new()$
    set_nodes(
      key_zap = rlang::zap(),
      key_box = rlang::as_box(rlang::zap()),
      key_normal = "just a value",
      key_function = function(...) "function_value"
    )

  expect_identical(cascading_theme$value("key_normal"), "just a value")

  expect_error(cascading_theme$value("key_zap"), "No such value")

  expect_identical(cascading_theme$value("key_box"), rlang::zap())
  expect_identical(cascading_theme$value_base("key_box"), rlang::as_box(rlang::zap()))

  expect_identical(cascading_theme$value("key_function"), "function_value")

  theme <- cascading_theme$compile()
  expect_length(theme, 3)
  expect_identical(theme$key_normal, "just a value")
  expect_identical(theme$key_box, rlang::zap())
  expect_identical(theme$key_function, "function_value")
})

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

test_that("simple elements cascade correctly", {
  expect_identical(cascade("parent", "child"), "child")
  expect_identical(cascade("parent"), "parent")
  expect_identical(cascade(child = "child"), "child")
})

test_that("formulas, functions, and quosures cascade correctly", {
  expect_identical(
    cascade("parent", ~paste0(parent, sep, "child"), list(data = list(sep = "->"))),
    "parent->child"
  )
  expect_identical(
    cascade("parent", rlang::quo(paste0(parent, sep, "child")), list(data = list(sep = "->"))),
    "parent->child"
  )
  expect_identical(cascade(4, sqrt), 2)

  # two functions, two quosures, two formulas
  expect_identical(
    cascade(
      function(parent = "NA") parent,
      function(parent) paste(parent, "child", sep = "->"),
      list(data = list(sep = "->"))
    ),
    "NA->child"
  )

  expect_identical(
    cascade(
      rlang::quo("parent"),
      rlang::quo(paste0(parent, sep, "child")),
      list(data = list(sep = "->"))
    ),
    "parent->child"
  )

  expect_identical(
    cascade(
      ~"parent",
      ~paste0(parent, sep, "child"),
      list(data = list(sep = "->"))
    ),
    "parent->child"
  )
})

test_that("cascading_elements cascade correctly", {
  element_one <- cascading_element(key2 = "one_two", key3 = "one_three")
  element_two <- cascading_element(
    key1 = "two_one", key2 = "two_two",
    key3 = ~paste0(parent, "->two_three")
  )

  one_two <- cascade(element_one, element_two)
  expect_identical(one_two$key3, "one_three->two_three")
  expect_identical(one_two$key1, "two_one")
  expect_identical(one_two$key2, "two_two")

  expect_error(cascade(element_two, element_one), "'parent' not found")

  expect_identical(cascade(element_one, "not an element"), "not an element")
  expect_identical(cascade("not an element", element_one), element_one)

  # check zapping
  element_three <- cascading_element(key2 = rlang::zap(), key3 = function(...) rlang::zap())
  expect_length(cascade(element_one, element_three), 0)
  # no reverse zapping
  expect_identical(cascade(element_three, element_one), element_one)
})

test_that("cascading element constructor works as expected", {
  expect_length(cascading_element(), 0)
  expect_length(cascading_element(key = "value"), 1)
  expect_identical(cascading_element(key = "value")$key, "value")

  # errors for bad values
  expect_error(cascading_element("value"), "unique names")
  expect_error(cascading_element(key = "value", "another value"), "unique names")
  expect_error(cascading_element(key = "value", key = "another value"), "unique names")
  expect_error(new_cascading_element(NULL), "must be a list")
})

test_that("cascading element subclassing works", {
  expect_is(cascading_element(), "cascading_element")
  expect_true(is_cascading_element(cascading_element()))

  complex_subclass <- cascading_element(.subclass = c("subclass2", "subclass1"))
  expect_is(complex_subclass, "subclass2")
  expect_is(complex_subclass, "subclass1")
  expect_is(complex_subclass, "cascading_element")

  # no repeating cascading_element class
  expect_identical(
    class(cascading_element(.subclass = c("subclass_name", "cascading_element"))),
    c("subclass_name", "cascading_element")
  )
})
