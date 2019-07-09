
ScaleSimpleDiscrete <- R6Class(
  "ScaleSimpleDiscrete", inherit = ScaleSimple,

  public = list(

    initialize = function(aesthetics = character(0)) {
      super$initialize(aesthetics)
      self$set_range(scales::DiscreteRange$new())
      self$set_trans(discrete_identity_trans())
    },

    map = function(x) {
      not_implemented()
    }
  )
)
