
Stat <- R6Class(
  "Stat",
  public = list(
    compute_panel = function(data_trans, panel, renderer) {
      not_implemented() # nocov
    }
  )
)

StatIdentity <- R6Class(
  "StatIdentity", inherit = Stat,
  public = list(
    compute_panel = function(data_trans, panel, renderer) {
      data_trans
    }
  )
)
