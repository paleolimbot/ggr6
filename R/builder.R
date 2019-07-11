
PlotBuilder <- R6Class(
  "PlotBuilder",

  public = list(
    build = function(plot_spec = PlotSpec$new(), renderer = PlotRendererIdentity$new()) {

      plot_spec$facet$init_panels(plot_spec$coord, plot_spec$scales)

    }
  )
)
