#' Create series list input for noma_ts_plot
#'
#' @param name navn på serie
#' @param plot_type hva slags plot-type man vil lage. Kan ta verdiene "dot", "line", "dashed line", "bar", "stacked bar"
#' @param legendname hva skal legendname være
#' @param color valg av farge
#'
#' @export
#'
#' @examples
#' \dontrun{series_list = list(create_list("last_3m", "line", "NOK 3m terminpunkter",
#'                             create_list("last_2m", "bar", "NOK 2m terminpunkter"),
#'                             create_list("last_1m", "dashed line", "NOK 1m terminpunkter"))}
create_list = function(name, plot_type, legendname = name, color = NULL) {

  if(plot_type == "dot") {
    type = "scatter"
    mode = "markers"
    barmode = NULL
    style = list(color = color)
    marker = NULL
  }


  if(plot_type == "line") {

    type = "scatter"
    mode = "lines"
    barmode = NULL
    style = list(color = color)
    marker = NULL

  }

  if(plot_type == "dashed line") {

    type = "scatter"
    mode = "lines"
    barmode = NULL
    style = list(dash = "dash", color = color)
    marker = NULL

  }

  if(plot_type == "bar") {

    type = "bar"
    mode = NULL
    barmode = "group"
    style = NULL
    marker = list(color = color)

  }

  if(plot_type == "stacked bar") {

    type = "bar"
    mode = NULL
    barmode = "stack"
    style = NULL
    marker = list(color = color)

  }


  s = list("name" = name,
           "type" = type,
           "mode" = mode,
           "style" = style,
           "marker" = marker,
           "legendname" = legendname,
           "barmode" = barmode,
           "color" = color)

  return(s)


}
