
#' Panels
#'
#' Panel objects are the [Geom]-, [Stat]-, and [Position]-facing
#' representation of the [Coord], [Scale], and [Guide] objects.
#' Panels are created by the [Facet] and finalized by the [Coord] at the
#' end of the build process. Currently they also train the [Guide] objects,
#' but this is against the philosphy of the Panel (a vessel in which information
#' relevant to the Panel is contained) and should be moved to the [Builder].
#'
#' @eval r6doc("Panel")
#'
#' @export
Panel <- R6Class(
  "Panel",
  public = list(
    coord = NULL,
    scales = NULL,
    guides = NULL,

    initialize = function(coord = CoordIdentity$new(), scales = ScaleList$new()) {
      "
      Create a Panel with a [Coord] and [ScaleList].
      "
      self$coord <- coord
      self$scales <- scales
      self$guides <- GuideList$new()
    },

    train_guides = function(layers, renderer) {
      "
      This method should be moved to the [Builder].
      "
      guides <- self$scales$guides()
      guides$merge_all()
      guides$train_layers(layers, renderer)
      self$guides <- guides

      invisible(self)
    },

    position_scales = function() {
      "
      Returns a [ScaleList] for each position aesthetic defined by
      the [Coord].
      "
      self$scales$filter_by_aesthetics(self$coord$aesthetics)
    },

    non_position_scales = function() {
      "
      Returns a [ScaleList] of scales representing non-position
      aesthetics.
      "
      self$scales$discard_by_aesthetics(self$coord$aesthetics)
    },

    transform = function(data) {
      "
      Calls the [Coord]'s `$transform()` method.
      "
      self$coord$transform(data, self$scales)
    }
  )
)
