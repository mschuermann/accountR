# A function that downloads exchange currency to a specific date and returns a new column with the exchange rate in the selected currency

FX_rate_convert <- function(data,
                    FC_column,
                    amount,
                    HC_currency,
                    report_date = as.Date(Sys.Date())) {
  data[[FC_column]] <- tidyr::replace_na(data[[FC_column]],HC_currency)
  Symbols <- base::unique(data[[FC_column]])
  exchange_rates <- base::data.frame()
  for (currency in Symbols) {
  exchange_rate <- priceR::historical_exchange_rates(from = currency,
                                              to = HC_currency,
                                              start_date = report_date,
                                              end_date = report_date)
  exchange_rate <- base::paste(as.double(unlist(exchange_rate)))
  FX_rate <- base::as.numeric(exchange_rate[2])
  exchange_rates <- base::rbind(exchange_rates, data.frame(currency, FX_rate))
  }

  data <- base::merge(x=data, y=exchange_rates, by=FC_column, all.x = TRUE)
  data <- dplyr::mutate(data, new=(as.double(data[[amount]]) * as.double(unlist(FX_rate))))
  return(data)
}
