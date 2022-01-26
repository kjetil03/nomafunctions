


#' Plot aggregated fx swap data
#'
#' @param df Data frame med data som skal plottes
#' @param counterparties Velg hvilke motparter som skal plottes mot
#' @param series hvilken sluttserie skal vises? Tar verdiene "netto", "innlan" og "utlan"
#' @param tenors Hvilke løpetider skal vises? Default er alle i datasettet som brukes som input
#' @param currencies Hvilke valutaer skal inkluderes? Default er alle
#' @param grouping Hvilken variabel skal grupperes over? Default er løpetid ("tenor")
#' @param date_start startdato for graf
#' @param date_end sluttdato for graf
#' @param yrange egendefinert y-range (optional)
#'
#' @importFrom dplyr mutate group_by filter select rename summarise
#' @importFrom tidyr gather pivot_wider
#' @import magrittr
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' agglist <- noma_agg_plot_fx(fx, "FOREIGN", "netto", c("3m", "6m"))
#' agglist$p 
#' }


noma_agg_plot_fx = function(df,
                     counterparties = c("FOREIGN", "DOMESTIC"), 
                     series = "netto",
                     tenors = levels(as.factor(df$tenor)),
                     currencies = levels(as.factor(df$FxFrgnCcy)),
                     grouping = "tenor",
                     date_start = "2020-03-01",
                     date_end = Sys.Date()-1, 
                     yrange = NULL) { 
  
  
  df = df %>% 
    filter(counterparty %in% counterparties,
           tenor %in% tenors,
           FxFrgnCcy %in% currencies) %>% 
    calculate_outstanding("FxSpotValDt",
                          "MtrtyDt",
                          "TxNmnlAmt",
                          c("TxTp", grouping)) %>% 
    mutate(outstanding = outstanding/10^9) %>% 
    pivot_wider(names_from = "TxTp", values_from = "outstanding") %>% 
    group_by(across(grouping)) %>% 
    mutate(netto = BUYI -SELL) %>% 
    rename(innlan = BUYI,
           utlan = SELL) %>% 
    gather(id, value, - c(date, all_of(grouping))) %>% 
    mutate(value= if_else(is.na(value), 0, value))
  
  
  p_all_outstanding <- df %>% 
    filter(id == series,
           date >= date_start,
           date <= date_end) %>%  
    noma_tidy_plot("date", 
                   "value", 
                   grouping, 
                   plot_type = "stacked bar", 
                   colors = nb_colors, 
                   yrange = yrange) 
  
  
  df_sum = df %>% filter(id == series) %>% 
    group_by(date) %>% 
    summarise(Sum = sum(value))
  
  
  p_all_outstanding = p_all_outstanding %>% 
    noma_add_line(df_sum, "date", "Sum", "date", color = "black")
  
  return(list(p = p_all_outstanding,
              df = df,
              df_sum = df_sum))
  
}
