# Context: Testing the function FX_rate_convert()

test_that("downloads the correct foreign currency and calculates correct multiplication", {
  test_file_currency <- data.frame(
    currency = c("USD", "DKK", "USD", NA),
    price = c(100, 329, 23, 799),
    date = c("2020-01-01", "2020-02-01", "2020-03-01", "2020-03-01")
  )
  test_result_currency <- FX_rate_convert(
    data = test_file_currency,
    FC_column = "currency",
    amount = "price",
    new_currency = "EUR",
    report_date = "2022-05-30"
  )
  expect_vector(test_result_currency$translated_amount, c(44.23, 799.00, 92.73, 21.33))
  expect_vector(test_result_currency$FX_rate, c(0.134437, 1.000000, 0.927273, 0.927273))
})

test_that("correct dimensions in the dataframe", {
  test_file_currency <- data.frame(
    currency = c("USD", "DKK", "USD", NA),
    price = c(100, 329, 23, 799),
    date = c("2020-01-01", "2020-02-01", "2020-03-01", "2020-03-01")
  )
  test_result_currency <- FX_rate_convert(
    data = test_file_currency,
    FC_column = "currency",
    amount = "price",
    new_currency = "EUR",
    report_date = "2022-05-30"
  )
  expect_equal(nrow(test_result_currency), 4)
  expect_equal(ncol(test_result_currency), 5)
})

test_that("currency code not available in priceR should result in error", {
  test_file_currency <- data.frame(
    currency = c("USD", "DKK", "USD", "XYZ"),
    price = c(100, 329, 23, 799),
    date = c("2020-01-01", "2020-02-01", "2020-03-01", "2020-03-01")
  )
  expect_error(FX_rate_convert(
    data = test_file_currency,
    FC_column = "currency",
    amount = "price",
    new_currency = "EUR",
    report_date = "2022-05-30"
  ))
})
