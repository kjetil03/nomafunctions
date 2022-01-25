
#' Utvid datasett
#'
#' @param df dataframe
#' @param ... navn på variabler
#'
#' @importFrom rlang exprs
#' @importFrom tidyr complete expand nesting
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples
#' \dontrun{df = df %>% expose_dots(var1, var2, var3)}
expose_dots = function(df, ...) {

  d = rlang::exprs(...)
  df = complete(df, date = seq(min(date), max(date), by = "day"), nesting(!!!d))

}
