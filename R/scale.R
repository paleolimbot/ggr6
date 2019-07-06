
Scale <- R6Class(
  "Scale",
  public = list(

  )
)

ScaleList <- R6Class(
  "ScaleList", inherit = List,
  public = list(
    set = function(index, item) {
      if (!is.R6(item) || !inherits(item, "Scale")) {
        abort("`item` must be a Scale instance.")
      }

      super$set(index, item)
    }
  )
)
