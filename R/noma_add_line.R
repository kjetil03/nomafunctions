
#' Legg til linje i plot
#'
#' @param p plot man skal legge til linje i. Det bør være generert med noma_tidy_plot (eller noma_ts_plot (ikke testa p.t.))
#' @param df dataframe med data
#' @param xcol navn på x-aksekollonne
#' @param ycol navn på y-aksekollonne
#' @param p_xcol navn på x-aksekollonne i p, dvs. plottet man legger til en linje i
#' @param name navn på linje som skal vises i legend.
#' @param color farge på linje
#'
#' @importFrom plotly add_trace
#' @importFrom lubridate wday
#' @import magrittr
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' \dontrun{#' #Legge til linje fra datasett med navn bloom_data i plot ved navn p:
#' p = noma_add_line(p,
#'                   bloom_data,
#'                  "date",
#'                  "us0003m_index_px_last",
#'                   p_xcol =
#'                   "SettlementDate",
#'                   name = "Libor")}
noma_add_line = function(p, df, xcol, ycol, p_xcol, name = ycol, color = NULL) {


  p_min_xcol = min(environment(p[["x"]][["visdat"]][[1]])[["data"]][[p_xcol]])
  p_max_xcol = max(environment(p[["x"]][["visdat"]][[1]])[["data"]][[p_xcol]])

  df = df[(df[[xcol]] >= p_min_xcol & df[[xcol]] <= p_max_xcol), ]


  # if(length(df[[xcol]][lubridate::wday(df[[xcol]], week_start = 1) > 5]) > 0) {
  #   
  #   rangebreaks = NULL
  #   
  #   
  # } else {
  #   
  #   rangebreaks = list(
  #     list(bounds=list("sat", "mon")))
  #   
  # }

  p = p %>% add_trace(x = df[[xcol]],
                      y = df[[ycol]],
                      type = "scatter",
                      mode = "lines",
                      name = name,
                      data = df,
                      inherit = FALSE,
                      #colors = colors,
                      line = list(color = color),
                      text = name,
                      hovertemplate = paste( "<b>%{text}</b><br>",
                                             "Verdi: %{y:.2f}<br>",
                                             "Dato: %{x|%d-%m-%Y}<br>",
                                             "<extra></extra>"))

  return(p)


}
