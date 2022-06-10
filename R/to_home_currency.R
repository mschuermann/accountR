# A function that downloads exchange currency to a specific date and returns a new column with the exchange rate in the selected currency

currencies <- data.frame(
  currency = c("USD", "DKK", "USD"),
  price = c(10, 11, 12),
  date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01"))
)

FX_rate <- function(df,
                    FC_column,
                    HC_currency,
                    report_date = as.Date(Sys.Date())) {
  Symbols <- unique(df[[FC_column]])
  if(NA %in% Symbols) {replace_na(data=Symbols, replace=HC_currency)}
  Symbols = as.character(paste0(Symbols, "/", HC_currency))
  exchange_rates <- getSymbols(Symbols = Symbols, src= "oanda", from = report_date, to = report_date)
  return(exchange_rates)
}

result1 <- FX_rate(df = currencies, FC_column = "currency", HC_currency = "EUR")


Symbols = as.character(paste0("USD", "/", "EUR"))
exchange_rates <- getSymbols(Symbols = "USD/EUR", src="oanda", from = "2018-01-01", to = "2018-02-01")
exchange_rates <- loadSymbols(Symbols = "USD/EUR", src="oanda", from = "2022-01-03", to = "2022-01-02")
showSymbols()
str(exchange_rates)
FX_rate(df = df, FC_column = df$currency, HC_currency = "EUR")

# future feature: you could maybe choose which formula to use (ECB, Oanda, etc?)
#langfristige forderungen / kurzfristige forderungen?

