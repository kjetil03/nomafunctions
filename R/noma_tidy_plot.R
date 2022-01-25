
#' Plot tidy data
#'
#' @param df dataframe med data
#' @param xcol navn på x-akse-kolonne (f.eks. datokolonne)
#' @param ycol nanv på y-akse-kolonne (f.eks. volumkolonne)
#' @param sorting_var sorteringsvariabel
#' @param plot_type hva slags plot-type man vil lage. Kan ta verdiene "dot", "line", "dashed line", "bar", "stacked bar", "stacked area", "stacked percent area"
#' @param xaxis_title tittel på x-akse
#' @param yaxis_title tittel på y-akse
#' @param plot_title tittel på plot
#' @param marker_size kolonne som styrer størrelse på bobler i boblediagram
#' @param xname navn på x-variabel i hovertext
#' @param yname navn på y-variabel i hovertext
#' @param xrange range for x-akse. Spesifisert som vektor med min- og maksverdi
#' @param yrange range for y-akse. Spesifisert som vektor med min- og maksverdi
#' @param legendtext tekst på legend. Hovedsakelig relevant om man setter sorting_var  = NULL. Kan Da kontrollere hva
#'                   som vises i legend for den ene serien som plottes
#' @param sizename navn på variabel som styrer størrelse på bobler i hovertext
#' @param colors farger. Vektor med farger
#' @param legend_options alternativer for
#' @param custom_hover_text egendefinert spesifikasjon av hovertekst. Nyttig særlig for dot-plots,
#'                          hvor man vil ha mye info i hovertext for hver dot.
#'
#' @importFrom plotly plot_ly add_trace layout add_annotations config
#' @import magrittr
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' \dontrun{p = df %>% noma_tidy_plot(., "date", "volume", "tenor", "stacked bar")
#'
#'          #Med custom_hover_text:
#'          p = noma_tidy_plot(k,
#'            xcol = "SettlementDate",
#'            ycol = "InterestRate",
#'            sorting_var = "IssuerName",
#'            plot_type = "dot",
#'            xname = "Dato",
#'            yname = "Rente",
#'            marker_size = "PrincipalAmount",
#'            legend_options = list(showlegend = T),
#'            sizename = "Utstedt volum (mill USD)",
#'            plot_title = "Rente på nye utstedelser i 3m CP/CD vs Libor",
#'            colors = nb_colors,
#'            custom_hover_text = paste("Utsteder: ", k$ParentName,
#'                                    "<br> Rente", k$InterestRate,
#'                                    "<br> Løpetid (dager): ", k$TimeToMaturity,
#'                                     "<br> Volum (mill usd):", round(k$PrincipalAmount/10^6,2),
#'                                     "<br> Produkttype", k$ProductType)) }
noma_tidy_plot = function(df,
                          xcol,
                          ycol,
                          sorting_var,
                          plot_type,
                          xaxis_title = "",
                          yaxis_title = "",
                          plot_title = "",
                          plot_subtitle = "",
                          marker_size = NULL,
                          xname = xcol,
                          yname = ycol,
                          xrange = c(NULL, NULL),
                          yrange = c(NULL, NULL),
                          legendtext = NULL,
                          sizename = marker_size,
                          colors = NULL,
                          legend_options = list(showlegend = T,
                                                legend_position_x = NULL,
                                                legend_position_y = NULL),
                          custom_hover_text = NULL) {


  if(plot_type == "dot") {
    type = "scatter"
    mode = "markers"
    barmode = NULL
    if(is.null(marker_size)) {
      size = NULL
    } else {
      size = df[[marker_size]]
    }

    marker = list(sizemode = "diameter")

    style = NULL
    fill = ""
    stackgroup = NULL
    hoveron = NULL
    groupnorm = NULL
  }


  if(plot_type == "line") {

    type = "scatter"
    mode = "lines"
    barmode = NULL
    marker = NULL
    style = NULL
    size = NULL
    fill = NULL
    stackgroup = NULL
    hoveron = NULL
    groupnorm = NULL

  }

  if(plot_type == "dashed line") {

    type = "scatter"
    mode = "lines"
    barmode = NULL
    style = list(dash = "dash")
    marker = NULL
    size = NULL
    fill = NULL
    stackgroup = NULL
    hoveron = NULL
    groupnorm = NULL

  }

  if(plot_type == "bar") {

    type = "bar"
    mode = NULL
    barmode = "group"
    style = NULL
    marker = NULL
    size = NULL
    fill = NULL
    stackgroup = NULL
    hoveron = NULL
    groupnorm = NULL

  }

  if(plot_type == "stacked bar") {

    type = "bar"
    mode = NULL
    barmode = "stack"
    style = NULL
    marker = NULL
    size = NULL
    fill = NULL
    stackgroup = NULL
    hoveron = NULL
    groupnorm = NULL

  }

  if(plot_type == "stacked area"){

    type = "scatter"
    mode = 'line'
    stackgroup = "one"
    style = NULL
    marker = NULL
    size = NULL
    fill = df[[sorting_var]]
    barmode = NULL
    hoveron = "points"
    groupnorm = NULL



  }

  if(plot_type == "stacked percent area"){

    type = "scatter"
    mode = 'line'
    stackgroup = "one"
    style = NULL
    marker = NULL
    size = NULL
    fill = df[[sorting_var]]
    barmode = NULL
    hoveron = "points"
    groupnorm = 'percent'



  }


  #Dobbeltsjekk at df ikke er gruppert
  df = ungroup(df)
  #Denne løser en veldig skjelden bug, der aksene blir feil pga. utelukking av helger

  #df = df[lubridate::wday(df[[xcol]], week_start = 1) < 6, ]





  #Gjør om x-kolonnen til character. Det gjør plotting lettere
  #df[[xcol]] = as.character(df[[xcol]])

  #Overskriv legendtext hvis sorting_var er spesifisert
  if(!is.null(sorting_var) & !is.null(legendtext)) {
    legendtext = NULL
    warning("sorting_var ikke satt til NULL. Parameter legendtext overstyres")

  }


  if(is.null(sorting_var)) {

    if(is.null(custom_hover_text)) {

      text = df[[ycol]]
      hoverinfo = NULL
      hoverlabel = NULL

      if(is.null(marker_size)) {

        hovertemplate = paste0(
          yname, ": ", "%{y:.2f}<br>",
          xname, ": ", "%{x|%d-%m-%Y}<br>",
          "<extra></extra>"
        )

      } else {

        hovertemplate = paste0(
          yname, ": ", "%{y:.2f}<br>",
          xname, ": ", "%{x|%d-%m-%Y}<br>",
          sizename,": ", "%{marker.size:,.2f}",
          "<extra></extra>"
        )

      }

    } else {

      hovertemplate = NULL
      hoverinfo = 'text'
      hoverlabel = list(align = "left")
      text = custom_hover_text

    }

    if(is.null(legendtext)) {
      legendtext = plot_title
    } else {
      legendtext = legendtext
    }


    p = plot_ly(df,
                x = ~df[[xcol]],
                y = ~df[[ycol]],
                type = type,
                mode = mode,
                line = style,
                size = size,
                name = legendtext,
                sizes = c(5, 25),
                marker = marker,
                fill = fill,
                colors = colors,
                #color = ~df[[sorting_var]],
                stackgroup = stackgroup,
                text = ~text,
                textposition = "none",
                hovertemplate = hovertemplate,
                hoverinfo = hoverinfo,
                hoverlabel = hoverlabel)

  } else {


    if(is.null(custom_hover_text)) {

      text = df[[sorting_var]]
      hoverinfo = NULL
      hoverlabel = NULL

      if(is.null(marker_size)) {

        hovertemplate = paste0(
          "<b>%{text}</b><br>",
          yname, ": ", "%{y:.2f}<br>",
          xname, ": ", "%{x|%d-%m-%Y}<br>",
          "<extra></extra>")

      } else {

        hovertemplate = paste0(
          "<b>%{text}</b><br>",
          yname, ": ", "%{y:.2f}<br>",
          xname, ": ", "%{x|%d-%m-%Y}<br>",
          sizename, ": ", "%{marker.size:,.2f}",
          "<extra></extra>")

      }

    } else {

      hovertemplate = NULL
      hoverinfo = 'text'
      hoverlabel = list(align = "left")
      text = custom_hover_text

    }






    p = plot_ly(df,
                x = ~df[[xcol]],
                y = ~df[[ycol]],
                type = type,
                mode = mode,
                line = style,
                color = ~df[[sorting_var]],
                colors = colors,
                size = size,
                sizes = c(5, 25),
                marker = marker,
                fill = fill,
                legendgroup = ~df[[sorting_var]],
                text = ~text,
                textposition = "none",
                hovertemplate = hovertemplate,
                hoverinfo = hoverinfo,
                hoverlabel = hoverlabel,
                stackgroup = stackgroup,
                hoveron = hoveron,
                groupnorm = groupnorm

    )

  }


  if(plot_type == "stacked percent area") {

    ticksuffix = '%'

  } else {
    ticksuffix = NULL
  }



  #Sjekk om datasettet inneholder data på helgedager og sett rangebreaks deretter:

  if(length(df[[xcol]][lubridate::wday(df[[xcol]], week_start = 1) > 5]) > 0) {

    rangebreaks = NULL


  } else {

    rangebreaks = list(
      list(bounds=list("sat", "mon")))

  }


  p = p %>%
    layout(barmode = barmode,
           xaxis = list(title = xaxis_title,
                        autotick = T,
                        range = xrange,
                        showgrid = F,
                        #dtick = floor(length(unique(df[[xcol]]))/2)
                        rangebreaks = rangebreaks
           ),
           yaxis = list(title = yaxis_title,
                        range = yrange,
                        showgrid = F,
                        ticksuffix = ticksuffix),
           showlegend = legend_options$showlegend,
           legend = list(x = legend_options$legend_position_x,
                         y = legend_options$legend_position_y,
                         bgcolor = "rgba(0,0,0,0)"),
           title =  list(text = paste0(plot_title,
                                       '<br>',
                                       '<sup>',
                                       plot_subtitle),
                         xanchor = "left",
                         x = 0.05,
                         y = 0.95)) %>%
    config(locale = "no")



  return(p)

}
