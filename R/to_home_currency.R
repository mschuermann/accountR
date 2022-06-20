# A function that downloads exchange currency to a specific date and returns a
# new column with the exchange rate in the selected currency

#' Converts foreign currencies in a data frame to a chosen currency by the user
#'
#' \code{FX_rate_convert} downloads foreign exchange courses from the European Central Bank's API to convert foreign currencies to a chosen currency by the user and documents the exchange rate used.
#'
#' @param data \emph{a data frame} including columns indicating the following:
#'   foreign exchange course identifier and the amounts to be translated into the new currency.
#'
#' @param FC_column \emph{a column name} in the data frame indicating the foreign exchange course identifier. See \code{priceR::currencies()} for supported currency codes.
#'
#' @param amount \emph{a column name} in the data frame indicating the amounts which should be translated into another currency.
#'
#' @param new_currency \emph{a currency code} indicating which currency the amounts should be translate into. See \code{priceR::currencies()} for supported currency codes.
#'
#' @param report_date \emph{a date} to which the amounts should be translated into another currency. The default is the system date. The format must be
#'   "YYYY-MM-DD".
#'
#' @return a dataframe which has two additional columns: \describe{
#'   \item{\strong{FX_rate}}{the applied exchange rate} \item{\strong{translated_amount}}{the amount translated to the chosen currency} }
#'
#' @examples
#' currencies <- data.frame(
#'   currency = c("USD", "DKK", "USD", NA),
#'   price = c(100, 329, 23, 799),
#'   date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-03-01"))
#' )
#'
#' FX_rate_convert(data = currencies, amount = "price", FC_column = "currency", new_currency = "EUR")
#' FX_rate_convert(data = currencies, amount = "price", FC_column = "currency", new_currency = "USD", report_date = "2022-06-30")
#'
FX_rate_convert <- function(data,
                            FC_column,
                            amount,
                            new_currency,
                            report_date = as.Date(Sys.Date())) {
  data[[FC_column]] <-
    tidyr::replace_na(data[[FC_column]], new_currency)
  official_currency_codes <- priceR::currencies()
  stopifnot(unique(data[[FC_column]] %in% official_currency_codes$code))
  Symbols <- base::unique(data[[FC_column]])
  exchange_rates <- base::data.frame()
  for (currency in Symbols) {
    exchange_rate <- priceR::historical_exchange_rates(
      from = currency,
      to = new_currency,
      start_date = report_date,
      end_date = report_date
    )
    exchange_rate <- base::paste(as.double(unlist(exchange_rate)))
    FX_rate <- base::as.numeric(exchange_rate[2])
    exchange_rates <-
      base::rbind(exchange_rates, data.frame(currency, FX_rate))
  }

  data <-
    base::merge(
      x = data,
      y = exchange_rates,
      by = FC_column,
      all.x = TRUE
    )
  data <-
    dplyr::mutate(data, translated_amount = (as.numeric(data[[amount]]) * as.numeric(unlist(FX_rate))))
  data$translated_amount <- round(data$translated_amount, digits = 2)
  return(data)
}
