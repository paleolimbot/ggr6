
test_that("r6doc can document a class by name", {
  expect_identical(r6doc_class(Scale), r6doc("Scale"))
})

test_that("r6doc_class can document a class", {
  Cls <- R6Class("Cls", public = list(fun1 = function(a, b) NULL, fun2 = function() NULL))
  doc <- r6doc_class(Cls)
  # cat(doc)
  expect_is(doc, "character")
  expect_length(doc, 1)
})

test_that("r6doc_class can use superclass documentation", {
  Cls <- R6Class("Cls", public = list(fun1 = function(a, b) NULL, fun2 = function() NULL))
  ClsSub <- R6Class(
    "ClsSub", inherit = Cls,
    public = list(
      fun2 = function() {
        "A docstring"
        NULL
      },
      fun3 = function(c) NULL
    )
  )

  doc <- r6doc_class(ClsSub, super = TRUE)
  # cat(doc)
  expect_is(doc, "character")
  expect_length(doc, 1)
  expect_match(doc, "Cls\\$fun1")
})

test_that("r6methods can resolve superclass methods", {
  Cls <- R6Class("Cls", public = list(fun1 = function(a, b) NULL, fun2 = function() NULL))
  ClsSub <- R6Class(
    "ClsSub", inherit = Cls,
    public = list(
      fun2 = function() {
        "A docstring"
        NULL
      },
      fun3 = function(c) NULL
    )
  )

  cls_methods <- r6methods(Cls)
  expect_identical(cls_methods$method_name, c("fun1", "fun2", "clone"))
  cls_sub_methods <- r6methods(ClsSub)
  expect_identical(cls_sub_methods$method_name, c("fun2", "fun3", "clone", "fun1"))
})

test_that("normalize_whitespace() works", {
  docstring <- "
               @param aesthetics A list of aesthetics this scale will transform
                 and map.
               "

  normalized <- normalize_whitespace(docstring)
  expect_length(normalized, 1)
  expect_false(grepl(normalized, "^\\s+"))
  expect_false(grepl(normalized, "\\s+$"))
  expect_length(strsplit(normalized, "\n")[[1]], 2)
})

test_that("r6docstring extracts a docstring from a function", {
  f <- function() {
    "
    @param aesthetics A list of aesthetics this scale will transform
      and map.
    "
    invisible(NULL)
  }

  doc <- r6docstring(f)
  expect_is(doc, "character")
  expect_length(doc, 1)
})

test_that("r6docstring doesn't extract docstrings from functions that only contain a string", {
  expect_identical(r6docstring(function() "a string"), "")
  expect_identical(r6docstring(function() {"a string"}), "")
})

test_that("r6usage errors when a method does not exist", {
  expect_error(r6usage("not_a_method", "not_a_class"), "Can't create")
  Cls <- R6Class("Cls")
  expect_error(r6usage("not_a_method", Cls), "No such method")
})

test_that("r6usage genreates a valid function call for usage", {
  Cls <- R6Class("Cls", public = list(a_method = function(a, b = 2, c = 3) NULL))
  usage_str <- r6usage("a_method", Cls)
  expect_is(usage_str, "character")
  expect_length(usage_str, 1)
  expect_true(is.call(parse(text = usage_str)[[1]]))
})

