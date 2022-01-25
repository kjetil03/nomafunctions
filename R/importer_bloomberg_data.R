#' Hent data fra bloomberg
#'
#'
#' @param start_date start-dato for uttrekk
#' @param fields hvilke felter man vil ha. Default er bid, ask og last
#' @param tickers hvilke serier man skal hente ut.
#' @param include_non_trading_days Skal man inkludere helger og andre dager det ikke handles på? T eller F
#' @param fill_na Skal man fylle NA med forrige verdi? T eller F
#'
#'
#' @importFrom Rblpapi blpConnect bdh
#' @importFrom purrr reduce
#' @importFrom dplyr left_join
#' @import magrittr
#' @importFrom tidyselect everything
#' @importFrom tidyr fill
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' \dontrun{df = importer_bloomberg_data(start_date = "2020-01-01",
#'                                       fields = c("px_last", "px_bid"),
#'                                       tickers = c("nok curncy", "nok3m curncy"))}
#'
#'
importer_bloomberg_data = function(start_date,
                           fields = c("PX_LAST"),
                           tickers,
                           include_non_trading_days = T,
                           fill_na = T)

  {



  #Koble til bloomberg
  blpConnect()

  #Hent data med bdh
  datalist <- bdh(tickers,
                  fields,
                  start.date = as.Date(start_date),
                  end.date = NULL,
                  include.non.trading.days = include_non_trading_days)

  #Fiks navn på kolonner i datasett
  if(length(tickers) == 1) {

    names(datalist)[names(datalist) != "date"] = make.names(tolower(tickers))

  } else {

    for(i in names(datalist)) {

      colnames = tolower(names(datalist[[i]]))
      colnames[colnames != "date"] = paste(gsub(" ", "_", i), colnames[colnames != "date"], sep = "_")

      names(datalist[[i]]) = colnames

    }

  datalist = datalist %>% reduce(., .f = left_join, by = "date")


  }


  if(fill_na) {

    datalist = datalist %>% fill(., everything())

  }



  return(datalist)


}


