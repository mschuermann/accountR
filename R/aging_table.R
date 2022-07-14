# PURPOSE: To create an Accounts Receivables Aged Analysis

#' Calculates the number of outstanding days of invoices and assigns them to
#' chosen categories of overdue days
#'
#' \code{aged_analysis} is the first part of a two-step workflow to create a
#' Accounts Receivable Aged Analysis, widely used from accountants, auditors and
#' management to monitor accounts receivables. It categorizes the invoices
#' based on how long invoices have been outstanding.
#'
#' First the user should use \code{aged_analysis} to calculate the days
#' outstanding and categorize them to the chosen bins of days overdue. By
#' default, the following categories will be used: \enumerate{ \item Not overdue
#' yet ("-Inf - 0") \item Overdue between 1 and 30 days ("1 - 30") \item
#' Overdue between 30 and 60 days ("31 - 60") \item Overdue between 60 and 90
#' days ("61 - 90") \item Overdue more than 90 days ("91 - Inf").}
#'
#' It is recommended to use \code{\link{aging_report}} as the next step.
#'
#' @param data \emph{a data frame} including columns indicating the following:
#'   debtors ID/name, invoice number, invoice amount, and invoice due date.
#'   Usually this is going to be a Debtors Open Item Report, including all
#'   invoices that have not been (fully) paid to a specific date.
#' @param due_date \emph{a column name} in the data frame indicating the due
#'   date. Column name should be assigned to the parameter in quotation marks,
#'   e.g. "Due_Date".
#' @param report_date \emph{a report date} which is the benchmark if an invoice
#'   is overdue or not, usually the same date that the Debtors Open Item Report
#'   has been generated from. The default is the system date. The format must be
#'   "YYYY-MM-DD".
#' @param categories \emph{a vector of numbers} listing the days to be used as
#'   overdue bins. By default it is 0 days, 30 days, 60 days and 90 days. Format
#'   must be numeric values only, e.g. c(0, 30, 60, 90).
#'
#' @return a dataframe which has two additional columns: \describe{
#'   \item{\strong{days_overdue}}{indicating the number of days between
#'   report_date and due_date} \item{\strong{category}}{indicating the bin the
#'   days_overdue have been sorted in based on user's categories vector} }
#'
#' @examples
#' example <- accountR::demo_file
#' accountR:::aged_analysis(example, "due date", "2022-06-30")
#' accountR:::aged_analysis(example, "due date", categories = c(0, 60, 120), "2022-06-30")
#'
#' @export
aged_analysis <- function(data,
                          due_date,
                          report_date,
                          categories = c(0, 30, 60, 90)) {

  # check for missing dates in due date column, if there are: stop and give out error message
  data[[due_date]] <- as.Date(data[[due_date]])

  report_date <- as.Date(report_date)

  stopifnot(inherits(data, "data.frame")) # should be a data frame

  data[[due_date]][data[[due_date]] == -99] <- NA # in case NAs are displayed as -99
  data[[due_date]][as.character(data[[due_date]]) == "N/A"] <- NA # in case NAs are displayed as "N/A"
  data[[due_date]][as.character(data[[due_date]]) == "N A"] <- NA # in case NAs are displayed as "N A"
  data[[due_date]][as.character(data[[due_date]]) == "Not Available"] <- NA # in case NAs are displayed as "Not Available"
  data[[due_date]][as.character(data[[due_date]]) == "not available"] <- NA # in case NAs are displayed as "not available"
  data[[due_date]][as.character(data[[due_date]]) == "Not available"] <- NA # in case NAs are displayed as "Not available"

  stopifnot(sum(is.na(data[[due_date]])) == 0) # no NAs in the due date column allowed
  stopifnot(inherits(data[[due_date]], "Date")) # due dates should be formatted as dates

  categories <- append(categories, -Inf, 1)
  categories <- append(categories, Inf)
  cat_labels <- c()
  for (i in 1:length(categories)-1) {
    categories <- sort(unique(categories))
    add_one <- as.numeric(categories[i])+1
    name_cat <- paste(add_one,"-",categories[i+1])
    cat_labels[i] <- as.character(name_cat)
  }

  # calculating the number of days overdue from due date to report date
  data <- dplyr::mutate(data, days_overdue = report_date - as.Date(data[[due_date]]))
  data$days_overdue <- as.numeric(data$days_overdue)

  # calculating the categories of days overdue (e.g. due between more than 30 and less or equal to 60 days)
  data <- dplyr::mutate(data, category = cut(
    as.numeric(dplyr::all_of(days_overdue)),
    dplyr::all_of(categories),
    labels = cat_labels
  ))

  data <- data %>% dplyr::mutate_if(is.factor, as.character)

  return(data)
}



#' Creates a Accounts Receivable Aged Analysis Report
#'
#' \code{aging_report} is the second part of a two-step workflow to created a
#' Accounts Receivable Aged Analysis, widely used from accountants, auditors
#' and management to monitor accounts receivables. It categorizes the invoices
#' based on how long invoices have been outstanding. For the first step, please
#' refer to \code{\link{aged_analysis}}.
#'

#' @param data \emph{a data frame} including columns indicating the following:
#'   debtors ID/name, invoice number, invoice amount, and invoice due date.
#'   Usually this is going to be a Debtors Open Item Report, including all
#'   invoices that have not been (fully) paid to a specific date.
#' @param open_amount \emph{a column name} in the data frame indicating the open
#'   amount of the invoice. Column name should be assigned to the parameter in
#'   quotation marks, e.g. "Net_Amount". It is recommended to check if the open
#'   amounts are all in the same currency.
#' @param customer \emph{a column name} in the data frame indicating the ID or
#'   name of the customer that the invoice belongs to. Column name should be
#'   assigned to the parameter in quotation marks, e.g. "Customer". Customer
#'   names or IDs should be unique, so if there are multiple debtors with the
#'   same name, it might be useful to use the customer ID instead.
#' @param invoice_number \emph{a column name} in the data frame indicating the
#'   invoice number. Column name should be assigned to the parameter in
#'   quotation marks, e.g. "Invoice_Number".
#' @param category \emph{a column name} in the data frame indicating the
#'   category of days the invoice is in. Column name should be assigned to the parameter in
#'   quotation marks, e.g. "Categories". By default, the column name is "category" because it would be the standard output of \code{\link{aged_analysis}}.
#' @param include.credit.notes \emph{a logical}, by default set to TRUE. This
#'   logical indicates if amounts that are negative (usually representing credit
#'   notes) should be excluded or not. It might be useful to exclude credit
#'   notes if the aged report is used to calculate the bad debt expenses. It
#'   might be useful to include credit notes if the aged report is used to
#'   monitor on a customer level how much of the balance is overdue. This
#'   depends on the usage of the report and should be evaluated on an individual
#'   basis of the user.
#'
#' @return a data frame which has the following contents: \describe{
#'   \item{\strong{Customer names.}}{Each customer only appears once in the
#'   list.} \item{\strong{A column for each category chosen in
#'   \code{\link{aged_analysis}}}}{Indicating the amount which is due for a
#'   specific customer in this duration.}
#'   \item{\strong{Total}}{Indicating the total amount which is due for this customer.}}
#' @examples
#' example <- accountR::demo_file
#' example <- accountR:::aged_analysis(example, "due date", "2022-06-30")
#' accountR:::aging_report(example, "amount", "company name", "invoice no")
#' @export
aging_report <- function(data, # the dataframe from the previous step
                         open_amount, # the column which includes the open amount
                         customer, # the column which includes the customer name / ID
                         invoice_number, # the column which includes the invoice number
                         category = "category", # the column including the desired categories
                         include.credit.notes = TRUE) {
  days_overdue <- NULL
  if (include.credit.notes == FALSE) {
    data <- data[!(data[[open_amount]] < 0)]
    warning("Negative amounts in the data frame, i.e. credit notes, have been excluded.")
  }
  stopifnot(inherits(data, "data.frame")) # should be a data frame
  stopifnot(inherits(data[[open_amount]], "numeric")) # amount should be numeric

  data[[category]][data[[category]] == -99] <- NA # in case NAs are displayed as -99
  data[[category]][as.character(data[[category]]) == "N/A"] <- NA # in case NAs are displayed as "N/A"
  data[[category]][as.character(data[[category]]) == "N A"] <- NA # in case NAs are displayed as "N A"
  data[[category]][as.character(data[[category]]) == "Not Available"] <- NA # in case NAs are displayed as "Not Available"
  data[[category]][as.character(data[[category]]) == "not available"] <- NA # in case NAs are displayed as "not available"
  data[[category]][as.character(data[[category]]) == "Not available"] <- NA # in case NAs are displayed as "Not available"

  stopifnot(sum(is.na(data[[category]])) == 0) # no NAs in the due date column allowed

  if (!(category %in% colnames(data))) {
    stop("Please use aged_analysis() in the first step to assign the category of number of outstanding days or assign the equivalent column name in the variables.")
  }

  category_names <- unique(data[[category]])

  df_1 <-
    tidyr::pivot_wider(data,
      names_from = category,
      values_from = dplyr::all_of(open_amount),
      values_fill = 0,
      values_fn = list(open_amount = sum)
    )

  drop <- c(invoice_number, "days_overdue")
  keep <- c(customer, category_names)

  df_2 <- df_1[, !(names(df_1) %in% drop)]
  df_2 <- df_1[, (names(df_1) %in% keep)]
  colnames(df_2)[1] <- "Customer"

  df_2 <- stats::aggregate(. ~ Customer, df_2, sum)
  df_2 <- df_2 %>%
    dplyr::select(stringr::str_sort(colnames(df_2), numeric = TRUE)) %>%
    dplyr::select(dplyr::starts_with("-Inf"), tidyselect::everything()) %>%
    dplyr::select(Customer, tidyselect::everything())
  df_2 <- df_2 %>%
    dplyr::mutate(Total = base::
    rowSums(dplyr::select_if(., base::is.numeric)))

  names(df_2) <- sub("-Inf -", "Less than", names(df_2), fixed = TRUE)
  names(df_2) <- sub("- Inf", "or more", names(df_2), fixed = TRUE)

  return(df_2)
}

