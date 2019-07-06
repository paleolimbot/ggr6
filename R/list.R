
List <- R6Class(
  "List",
  public = list(
    lst = NULL,

    initialize = function(lst = list()) {
      self$lst <- unclass(as.list(lst))
    },
    add = function(item) {
      self$set(self$size() + 1, item)
    },
    get = function(index) {
      self$lst[[index]]
    },
    set = function(index, item) {
      self$lst[[index]] <- item
      invisible(self)
    },
    size = function() {
      length(self$lst)
    }
  )
)
