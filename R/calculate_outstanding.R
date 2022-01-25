#' Beregn utestående volum
#'
#' @param df dataframe med transaksjonsdata
#' @param settlementdate navn på kolonne som inneholder oppgjørsdato
#' @param maturitydate navn på kolonne som inneholder forfallsdato
#' @param transactionvolume navn på kolonne som inneholder transaksjonsvolum
#' @param grouping_variables vektor av variabler man skal sortere etter. Må være ett eller flere kolonnenavn i df.
#' @param freq frekvens man vil ha sluttresultatet på. Tar en av verdiene i "d", "b", "w", "m", "q", "y" for hhv.
#'    daglig, business day, ukentlig, månedlig, kvartalsvis og årlig frekvens
#'
#' @importFrom dplyr select rename mutate bind_rows arrange group_by summarise ungroup filter across all_of
#' @importFrom tidyr complete expand
#' @importFrom bizdays bizseq create.calendar
#' @import magrittr
#'
#' @export
#'
#' @examples
#' \dontrun{dtcc_data = dtcc_data %>%  calculate_outstanding_amounts(settlementdate = "settlementdate",
#'                                        maturitydate = "maturitydate",
#'                                        transactionvolume = "principalamount",
#'                                        grouping_variables = c("producttype", "name"),
#'                                        freq = "w") }
calculate_outstanding <- function(df,
                                  settlementdate,
                                  maturitydate,
                                  transactionvolume,
                                  grouping_variables,
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


  #Steg 1: Legg utstedelser og forfall i hver sin df med samme
  #datoakse og slå dem sammen

  new = df %>% select(-all_of(maturitydate)) %>%
    rename(date = all_of(settlementdate),
           new = all_of(transactionvolume)) %>%
    mutate(matured = 0)

  matured = df %>% select(-all_of(settlementdate)) %>%
    rename(date = all_of(maturitydate),
           matured = all_of(transactionvolume)) %>%
    mutate(new = 0)


  outstanding = bind_rows(new, matured)

  #Regn ut hvor mye som utstedes og forfaller per dag

  #Hvis det ikke grupperes på noe:
  if(is.null(grouping_variables)) {
    outstanding = outstanding %>%
      arrange(date) %>%
      group_by(date) %>%
      summarise(sum_new = sum(new), sum_matured = sum(matured))  %>%
      ungroup()

    #Utvid datasettet slik at det har data for alle dager og kategorier
    #(viktig for fine barplots) og fyll inn 0 på dager uten data
    outstanding = outstanding %>% complete(date = seq(min(date), max(date), by = "day"))


    outstanding[is.na(outstanding)] = 0

    #summer opp utestående som kumulativ utstedelse minus kumulativ forfall
    outstanding = outstanding  %>%
      mutate(cumsum_new = cumsum(sum_new),
             cumsum_matured = cumsum(sum_matured),
             outstanding = (cumsum_new - cumsum_matured)) %>%
      ungroup() %>%
      select(-c(sum_new, sum_matured, cumsum_new, cumsum_matured))

  } else {

    #Hvis det grupperes på en eller flere variabler
    outstanding = outstanding %>%
      arrange(date) %>%
      group_by(date, across(all_of(grouping_variables))) %>%
      summarise(sum_new = sum(new), sum_matured = sum(matured))  %>%
      ungroup()

    #Utvid datasettet slik at det har data for alle dager og kategorier
    #(viktig for fine barplots) og fyll inn 0 på dager uten data
    outstanding = outstanding %>%
      expose_vec(grouping_variables, datecol = "date") #se dokumentasjon for funksjon

    outstanding[is.na(outstanding)] = 0

    #summer opp utestående som kumulativ utstedelse minus kumulativ forfall
    outstanding = outstanding  %>%
      group_by(across(all_of(grouping_variables))) %>%
      mutate(cumsum_new = cumsum(sum_new),
             cumsum_matured = cumsum(sum_matured),
             outstanding = (cumsum_new - cumsum_matured)) %>%
      ungroup() %>%
      select(-c(sum_new, sum_matured, cumsum_new, cumsum_matured))

  }


  #Sett opp datoliste
  min_date <- as.Date("1941-01-01")
  max_date <- max(new$date)

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
  outstanding = outstanding %>% filter(date %in% list_dates[[freq]])

  return(outstanding)

}
