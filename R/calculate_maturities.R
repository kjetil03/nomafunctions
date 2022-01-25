#' Beregn forfall fra gitt startdato
#'
#' @param df dataframe
#' @param start_date start-dato man vil beregne forfall fra
#' @param settlementdate navn på kolonne med oppgjørsdato
#' @param maturitydate navn på kolonne med forfallsdato
#' @param transaction_volume navn på kolonne med transaksjonsvolum
#' @param grouping_variables vektor med variabelnavn man vil gruppere etter
#' @param type char med type serie man vil velge. "volume" for forfall i volum per dag,
#'             "cummulative volume" for kummulativt volum per dag,
#'             "percent of outstanding" for å regne om volum per dag til prosent av utestående
#'             volum. "cummulative percent of outstanding" for å regne ut kummulativ prosentvise
#'             forfall av uteståend volum
#'
#' @import magrittr
#' @importFrom dplyr filter group_by summarise
#' @importFrom bizdays create.calendar
#'
#' @encoding  UTF-8
#' @export
#'
#' @examples
#' \dontrun{m  = calculate_maturities(df,
#'                                    "2020-01-01",
#'                                    "SettlementDate",
#'                                    "MaturityDate",
#'                                    "PrincipalAmount",
#'                                    "Maturity") }
calculate_maturities = function(df,
                                start_date,
                                settlementdate,
                                maturitydate,
                                transaction_volume,
                                grouping_variables,
                                type = "volume",
                                freq = "d") {



  #Sjekk at div input er spesifisert riktig og stopp program hvis ikke
  if(!freq %in% c("d", "b", "w", "m", "q", "y")) {
    stop(paste("freq er spesifisert feil. Velg freq i",  paste(c("d", "b", "w", "m", "q", "y"), collapse = ",")))
  }

  temp = df[grouping_variables]
  temp = temp[is.na(temp)]

  if(length(temp) >0) {
    stop(paste("Det er NA i en av grupperingsvariablene. Dette er ikke tillat. Fiks dette og prøv igjen."))
  }

  ####Hvis man ikke sorterer på noen variabler ####

  if(is.null(grouping_variables)) {

    matured = df %>%
      rename(settlementdate = all_of(settlementdate),
             date = all_of(maturitydate),
             volume = all_of(transaction_volume))

    matured = matured %>%
      filter(settlementdate < start_date, date >= start_date) %>%
      group_by(date) %>%
      summarise(matured = sum(volume))


    if(type == "volume") {
      matured = matured

    }

    if(type == "cummulative volume"){

      matured = matured %>% complete(date = seq(min(date), max(date), by = "day"))

      matured[is.na(matured)] = 0

      matured = matured %>%
        mutate(cumsum_matured = cumsum(matured)) %>%
        ungroup()



    }


    if(type %in% c("percent of outstanding",
                   "cummulative percent of outstanding")) {

      outstanding = df %>%
        calculate_outstanding(
          settlementdate,
          maturitydate,
          transaction_volume,
          grouping_variables = NULL,
          freq = "d") %>%
        filter(date == start_date) %>%
        summarise(sum = sum(outstanding))


      matured = matured %>%
        mutate(outstanding = outstanding$sum) %>%
        mutate(percent_matured = matured/outstanding*100) %>%
        ungroup()


      if(type == "percent of outstanding") {

        matured = matured

      } else {

        matured = matured %>% complete(date = seq(min(date), max(date), by = "day"))

        matured[is.na(matured)] = 0

        matured = matured %>%
          mutate(percent_matured_cumsum = cumsum(percent_matured)) %>%
          ungroup()


      }
    }
  }



  ###### Hvis man sorterer på en eller flere variabler #####
  if(!is.null(grouping_variables)) {

    matured = df %>%
      rename(settlementdate = all_of(settlementdate),
             date = all_of(maturitydate),
             volume = all_of(transaction_volume))

    matured = matured %>%
      filter(settlementdate < start_date, date >= start_date) %>%
      group_by(date, across(all_of(grouping_variables))) %>%
      summarise(matured = sum(volume)) %>%
      ungroup()


    if(type == "volume") {

      matured = matured

    }

    if(type == "cummulative volume") {

      matured = matured %>%
        expose_vec(grouping_variables, datecol = "date")

      matured[is.na(matured)] = 0

      matured = matured %>%
        group_by(across(all_of(grouping_variables))) %>%
        mutate(cumsum_matured = cumsum(matured)) %>%
        ungroup()


      # matured = matured %>%
      #   arrange(date) %>%
      #   group_by(across(all_of(grouping_variables))) %>%
      #   mutate(cumsum_matured = cumsum(matured))


    }


    if(type %in% c("percent of outstanding",
                   "cummulative percent of outstanding")) {

      outstanding = df %>%
        calculate_outstanding(
          settlementdate,
          maturitydate,
          transaction_volume,
          grouping_variables = NULL,
          freq = "d") %>%
        filter(date == start_date) %>%
        summarise(sum = sum(outstanding))


      matured = matured %>%
        mutate(outstanding = outstanding$sum) %>%
        mutate(percent_matured = matured/outstanding*100) %>%
        ungroup()

      if(type == "percent of outstanding") {

        #Unødvendig, men for tydelighetens skyld
        matured = matured


      } else {

        matured = matured %>%
          expose_vec(grouping_variables, datecol = "date")
        matured[is.na(matured)] = 0

        matured = matured %>%
          group_by(across(all_of(grouping_variables))) %>%
          mutate(percent_matured_cumsum = cumsum(percent_matured)) %>%
          ungroup()


      }
    }
  }




  #   if(type == "volume") {
  #
  #     matured = matured
  #
  #     } else {
  #
  #       if(type == "cummulative volume") {
  #         matured = matured %>%
  #           arrange(date) %>%
  #           muate(cumsum_matured = cumsum(matured))
  #
  #       } else {
  #
  #     outstanding = df %>%
  #       calculate_outstanding(
  #         settlementdate,
  #         maturitydate,
  #         transaction_volume,
  #         grouping_variables = NULL,
  #         freq = "d") %>%
  #       filter(date == start_date) %>%
  #       summarise(sum = sum(outstanding))
  #
  #
  #     matured = matured %>%
  #       mutate(outstanding = outstanding$sum) %>%
  #       mutate(percent_matured = matured/outstanding*100) %>%
  #       ungroup()
  #
  #
  #     if(type == "percent of outstanding") {
  #
  #
  #       matured = matured
  #
  #       } else {
  #
  #         matured = matured %>% complete(date = seq(min(date), max(date), by = "day"))
  #
  #         matured[is.na(matured)] = 0
  #
  #         matured = matured %>%
  #           mutate(percent_matured_cumsum = cumsum(percent_matured)) %>%
  #           ungroup()
  #
  #
  #       }
  #
  #     }
  #
  #
  #   } else {
  #
  #   matured = df %>%
  #     rename(settlementdate = all_of(settlementdate),
  #            date = all_of(maturitydate),
  #            volume = all_of(transaction_volume))
  #
  #   matured = matured %>%
  #     filter(settlementdate < start_date, date >= start_date) %>%
  #     group_by(date, across(all_of(grouping_variables))) %>%
  #     summarise(matured = sum(volume))
  #
  #
  #   if(type == "raw") {
  #
  #     matured = matured
  #
  #
  #   } else {
  #
  #     outstanding = df %>%
  #       calculate_outstanding(
  #         settlementdate,
  #         maturitydate,
  #         transaction_volume,
  #         grouping_variables = NULL,
  #         freq = "d") %>%
  #       filter(date == start_date) %>%
  #       summarise(sum = sum(outstanding))
  #
  #
  #     matured = matured %>%
  #       mutate(outstanding = outstanding$sum) %>%
  #       mutate(percent_matured = matured/outstanding*100) %>%
  #       ungroup()
  #
  #     if(type == "percent of outstanding") {
  #
  #       #Unødvendig, men for tydelighetens skyld
  #       matured = matured
  #
  #
  #     } else {
  #
  #       matured = matured %>%
  #         expose_vec(grouping_variables, datecol = "date")
  #       matured[is.na(matured)] = 0
  #
  #       matured = matured %>%
  #         group_by(across(all_of(grouping_variables))) %>%
  #         mutate(percent_matured_cumsum = cumsum(percent_matured)) %>%
  #         ungroup()
  #
  #
  #     }
  #
  #   }
  #
  #
  # }


  #Sett opp datoliste
  min_date <- as.Date("1941-01-01")
  max_date <- max(matured$date)

  #Bruker norske helligdager for å lage business days (kunne valgt amerikanske og, men
  #antar at det er norsk datoakse vi er mest interesserte i)

  helligdager = import_holidays()
  calendar = create.calendar("Norwegian_holidays",
                             helligdager$EventDate,
                             weekdays = c("saturday", "sunday"))


  list_dates <- list(
    "d" = seq(min_date, max_date, by = 1),
    "b" = bizseq(min(helligdager$EventDate), max(helligdager$EventDate), calendar),
    "w" = seq(min_date+1, max_date, by = 7),
    "m" = seq(min_date, max_date, by = "months")-1,
    "q" = seq(min_date, max_date, by = "quarters")-1,
    "y" = seq(min_date, max_date, by = "years")-1
  )

  #filtrer på datoer i datoliste avhengig av frekvens
  matured = matured %>% filter(date %in% list_dates[[freq]])

  return(matured)

}
