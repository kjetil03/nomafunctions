#' Fyll ut datasett
#'
#' \code{expose_vec} fyller ut datoaksen for et datasett med ufullstendig datoakse.
#' Verdier på datoer uten data i opprinnelig datasett blir satt til NA. NB: datoaksen blir
#' satt til daglig datoakse. Det kan gi store datasett
#'
#' @param df Dataframe med
#' @param vars vektor med variabelnavn
#' @param datecol navn på datokolonne
#'
#'
#' @importFrom rlang sym
#' @importFrom tidyr complete expand
#' @encoding UTF-8
#'
#' @export
#'
#' @examples
#' \dontrun{df = df %>% expose_vec(c("var1", "var2"), datecol = "date")}


expose_vec = function(df, vars, datecol) {

  names(df)[names(df) == datecol] = "date"

  x = lapply(vars, sym)

  df = complete(df, date = seq(min(date), max(date), by = "day"), nesting(!!!x))

  names(df)[names(df) == "date"] = datecol

  return(df)

}
