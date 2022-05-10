# A function that downloads exchange currency to a specific date and returns a new column with the exchange rate in the selected currency
library(dplyr)
library(quantmod)

df <- data.frame(
  currency = c("USD", "DKK", "USD", NA),
  price = c(10, 11, 12, 13),
  date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01"))
)

FX_rate <- function(df, FC_column, HC_currency, date) {
  quantmod::oanda.currencies
  exchange_rates <- getSymbols.oanda(as.String(concat(FC_column,"/",HC_currency)), from=date, to=date)
}

FX_rate(df=df, FC_column=df$currency, HC_currency = "EUR", date="31/12/2021")

