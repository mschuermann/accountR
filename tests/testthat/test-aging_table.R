# Context: Testing within the function "aged_analysis"

test_file_1 <- data.frame(
  due = c("2022-03-30", "2022-04-02", "2022-05-23"),
  name = c("Alfa", "Bravo", "Charlie"),
  amount = c(100, 200, 300)
)

test_result_1 <- aged_analysis(data = test_file_1, due_date = "due", report_date = "2022-06-30")

test_that("Days overdue is numeric", {
  expect_equal(class(test_result_1$days_overdue), "numeric")
})

test_that("Due Date is a Date", {
  expect_equal(class(test_result_1$due), "Date")
})

test_that("Resulting object is a data.frame", {
  expect_equal(class(test_result_1), "data.frame")
})

category_exp <- c("90 - Inf", "60 - 90", "30 - 60")
test_that("Category is correctly named", {
  expect_equal(test_result_1$category, category_exp)
})

test_that("Only two columns added", {
  expect_equal(ncol(test_result_1), 5)
})

test_that("No rows added", {
  expect_equal(nrow(test_result_1), 3)
})

test_result_2 <- aged_analysis(data = test_file_1, due_date = "due", report_date = "2022-06-30", categories = c(0, 50, 100))
category_exp_2 <- c("50 - 100", "0 - 50")
test_that("Defined categories work", {
  expect_equal(unique(test_result_2$category), category_exp_2)
})

test_file_2 <- data.frame(
  due = c("N/A", "2022-04-02", "2022-05-23"),
  name = c("Alfa", "Bravo", "Charlie"),
  amount = c(100, 200, 300)
)

test_that("N/A in the due date leads to an error", {
  expect_error(aged_analysis(data = test_file_2, due_date = "due", report_date = "2022-06-10"))
})

# Context: Testing within the function "aging_report"

test_result_3 <- aging_report(data = test_result_1, open_amount = "amount", customer = "name", invoice_number = "name")

test_that("Error given when aged_analysis() has not been used", {
  expect_error(aging_report(data = test_file_1, open_amount = "amount", customer = "name", invoice_number = "name"))
})

colnames_expect <- c("Customer", "30 - 60", "60 - 90", "90 or more", "Total")
test_that("Column names are correct", {
  expect_equal(colnames(test_result_3), colnames_expect)
})

test_that("Number of columns is correct", {
  expect_equal(ncol(test_result_3), 5)
})

test_that("Number of rows is correct", {
  expect_equal(nrow(test_result_3), 3)
})

test_that("Aging table is correctly calculated", {
  expect_equal(test_result_3$Customer, c("Alfa", "Bravo", "Charlie"))
  expect_equal(test_result_3$`30 - 60`, c(0, 0, 300))
  expect_equal(test_result_3$`60 - 90`, c(0, 200, 0))
  expect_equal(test_result_3$`90 or more`, c(100, 0, 0))
  expect_equal(test_result_3$Total, c(100, 200, 300))
})

test_that("Resulting object is a data.frame", {
  expect_equal(class(test_result_3), "data.frame")
})
