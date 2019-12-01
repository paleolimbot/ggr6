
#' Panels
#'
#' Panel objects are the [Geom]-, [Stat]-, and [Position]-facing
#' representation of the [Coord], [Scale], and [Guide] objects.
#' Panels are created by the [Facet] and finalized by the [Coord] at the
#' end of the build process. Currently they also train the [Guide] objects,
#' but this is against the philosphy of the Panel (a vessel in which information
#' relevant to the Panel is contained) and should be moved to the [Builder].
#'
#' @export
Panel <- R6Class(
  "Panel",
  public = list(
    coord = NULL,
    scales = NULL,
    guides = NULL,

    #' @details Create a Panel with a [Coord] and [ScaleList].
    initialize = function(coord = CoordIdentity$new(), scales = ScaleList$new()) {
      self$coord <- coord
      self$scales <- scales
      self$guides <- GuideList$new()
    },

    #' @details
    #' Returns a [ScaleList] for each position aesthetic defined by
    #' the [Coord].
    position_scales = function() {
      self$scales$filter_by_aesthetics(self$coord$aesthetics)
    },

    #' @details Returns a [ScaleList] of scales representing non-position
    #' aesthetics.
    non_position_scales = function() {
      self$scales$discard_by_aesthetics(self$coord$aesthetics)
    },

    #' @details Calls the [Coord]'s `$transform()` method.
    transform = function(data) {
      self$coord$transform(data, self$scales)
    }
  )
)
