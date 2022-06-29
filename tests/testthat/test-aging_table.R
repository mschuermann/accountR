# Context: Testing within the function "aged_analysis"

test_file_1 <- data.frame(
  due = c("2022-03-30", "2022-04-02", "2022-05-23"),
  name = c("Alfa", "Bravo", "Charlie"),
  amount = c(100, 200, 300)
)

test_that("Function stops if it is not a data frame", {
  test_vector <- c(150, 329)
  expect_error(aged_analysis(data = test_vector))
})

test_result_1 <- aged_analysis(data = test_file_1, due_date = "due", report_date = "2022-06-30")

test_that("If there is a NA in Due Date: Stop", {
  test <- data.frame(
    due = c("NA", "2022-04-02", "2022-05-23"),
    name = c("Alfa", "Bravo", "Charlie"),
    amount = c(100, 200, 300)
  )
  expect_error(aged_analysis(data = test, due_date = "due", report_date = "2022-06-30"))
})

test_that("If there is a character in Due Date: Stop", {
  test <- data.frame(
    due = c("4 April 2022", "2022-04-02", "2022-05-23"),
    name = c("Alfa", "Bravo", "Charlie"),
    amount = c(100, 200, 300)
  )
  expect_error(aged_analysis(data = test, due_date = "due", report_date = "2022-06-30"))
})

test_that("If there is a number in Due Date: Stop", {
  test <- data.frame(
    due = c(15602, "2022-04-02", "2022-05-23"),
    name = c("Alfa", "Bravo", "Charlie"),
    amount = c(100, 200, 300)
  )
  expect_error(aged_analysis(data = test, due_date = "due", report_date = "2022-06-30"))
})

test_that("Due Date is a Date", {
  expect_equal(class(test_result_1$due), "Date")
})

test_that("Days overdue is correctly calculated", {
  expect_equal(test_result_1$days_overdue, c(92, 89, 38))
})

test_that("Column for Days Overdue is called days_overdue", {
  expect_true("days_overdue" %in% colnames(test_result_1))
})

test_that("Days overdue is numeric", {
  expect_equal(class(test_result_1$days_overdue), "numeric")
})

test_that("Column for categories is called category", {
  expect_true("category" %in% colnames(test_result_1))
})

category_exp <- c("90 - Inf", "60 - 90", "30 - 60")
test_that("Category is correctly named", {
  expect_equal(test_result_1$category, category_exp)
})

test_that("Resulting object is a data.frame", {
  expect_equal(class(test_result_1), "data.frame")
})

test_that("Only two columns added", {
  expect_equal(ncol(test_result_1), 5)
})

test_that("No rows added", {
  expect_equal(nrow(test_result_1), 3)
})

test_that("No NAs in the calculations", {
  expect_equal(sum(is.na(test_result_1$days_overdue)), 0)
})

test_that("Defined categories work", {
  test_result_2 <- aged_analysis(data = test_file_1, due_date = "due", report_date = "2022-06-30", categories = c(0, 50, 100))
  category_exp_2 <- c("50 - 100", "0 - 50")
  expect_equal(unique(test_result_2$category), category_exp_2)
})

test_that("Different length of categories", {
  test_result <- aged_analysis(data = test_file_1, due_date = "due", report_date = "2022-06-30", categories = c(5, 10))
  expectation <- "10 - Inf"
  expect_equal(unique(test_result$category), expectation)
})

test_that("Unsorted category", {
  test_result <- aged_analysis(data = test_file_1, due_date = "due", report_date = "2022-06-30", categories = c(90, 5, 30))
  expectation <- c("90 - Inf", "30 - 90")
  expect_equal(unique(test_result$category), expectation)
})

# Context: Testing within the function "aging_report"
test_file_1 <- data.frame(
  due = c("2022-03-30", "2022-04-02", "2022-05-23"),
  name = c("Alfa", "Bravo", "Charlie"),
  amount = c(100, 200, 300)
)
test_result_1 <- aged_analysis(data = test_file_1, due_date = "due", report_date = "2022-06-30")
test_result_3 <- aging_report(data = test_result_1, open_amount = "amount", customer = "name", invoice_number = "name")

test_that("Function stops if it is not a data frame", {
  test_vector <- c(150, 329)
  expect_error(aging_report(data = test_vector))
})

test_that("NAs in the category column are leading to an error", {
  test_result_1 <- aged_analysis(data = test_file_1, due_date = "due", report_date = "2022-06-30")
  test_result_1[2,5] <- "N/A"
  expect_error(aging_report(data = test_result_1, open_amount = "amount", customer = "name", invoice_number = "name"))
})

test_that("NAs in the category column are leading to an error", {
  test_result_1 <- aged_analysis(data = test_file_1, due_date = "due", report_date = "2022-06-30")
  test_result_1[2,3] <- "test"
  expect_error(aging_report(data = test_result_1, open_amount = "amount", customer = "name", invoice_number = "name"))
})

test_that("Error given when there is no column given indicating the number of overdue days per observation", {
  expect_error(aging_report(data = test_file_1, open_amount = "amount", customer = "name", invoice_number = "name"))
})

test_that("Error given when there is no column given indicating the number of overdue days per observation when it has a different name", {
  test_result_1 <- aged_analysis(data = test_file_1, due_date = "due", report_date = "2022-06-30")
  test_result_1 <- dplyr::rename(test_result_1, cat_test = category)
  expect_error(aging_report(data = test_result_1, open_amount = "amount", customer = "name", invoice_number = "name"))
})

test_that("Column names are correct", {
  colnames_expect <- c("Customer", "30 - 60", "60 - 90", "90 or more", "Total")
  expect_equal(colnames(test_result_3), colnames_expect)
})

test_that("Number of columns is correct", {
  expect_equal(ncol(test_result_3), dplyr::n_distinct(test_result_1$category)+2)
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

