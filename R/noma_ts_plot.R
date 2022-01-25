

#' Plot tidsseriedata
#'
#' @param df dataframe med tidsseriedata, dvs. en datokolonne, mange kolonner med variabler (non-tidy).
#' @param series_list liste med spesifikasjoner for serier som skal plottes.
#' @param xaxis_title navn på x-akse
#' @param yaxis_title navn på y-akse
#' @param plot_title tittel på graf
#' @param legend_options alternativer for legend
#'
#' @importFrom plotly plot_ly add_trace layout add_annotations
#' @import magrittr
#' @importFrom dplyr last
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' \dontrun{p = noma_ts_plot(df, series_list)}
noma_ts_plot = function(df,
                     series_list,
                     xaxis_title = "",
                     yaxis_title = "",
                     plot_title = "",
                     legend_options = list(showlegend = T,
                                           legend_position_x = NULL,
                                           legend_position_y = NULL)) {

  if(!is.list(series_list[[1]])) {
    series_list = list(series_list)
  }


  series_names = sapply(series_list, `[[`, 1)

  df = df[c("date", series_names)]


  #Check if there are any series specified as stacked bar
  barmode = unlist(unique(sapply(series_list, `[[`, "barmode")))

  if((length(barmode) > 1) & ("stack" %in% barmode)) {
    barmode = "stack"
    warning("Flere verdier oppgitt for barmode. Overskriver til barmode = 'stack'")
  }



  fig = plot_ly(df, colors = nb_colors)

  for (s in series_list) {

    fig = add_trace(fig,
                    x = ~date,
                    y = df[[s$name]],
                    type = s$type,
                    mode = s$mode,
                    name = s$legendname,
                    line = s$style,
                    marker = s$marker,
                    text = s$legendname,
                    hovertemplate = paste( "<b>%{text}</b><br>",
                                           "Verdi: %{y:.2f}<br>",
                                           "Dato: %{x|%d-%m-%Y}<br>",
                                           "<extra></extra>")) %>%

      add_annotations(x = max(df[["date"]]),
                      y = last(df[[s$name]]),
                      text = paste(round(last(df[[s$name]]),2)),
                      xanchor = "left",
                      showarrow = F,
                      font = list(color = s$color)
      )
  }



  fig = fig %>% layout(xaxis = list(title = xaxis_title),
                       yaxis = list(title = yaxis_title),
                       title = list(text = plot_title,
                                    xanchor = "left",
                                    x = 0.05,
                                    y = 0.99),
                       barmode = barmode,
                       showlegend = legend_options$showlegend,
                       legend = list(x = legend_options$legend_position_x,
                                     y = legend_options$legend_position_y))


  return(fig)


}




