library(devtools)

# Usually the input data file looks like this:
# A data frame with the following columns:
## Name Debitor / Creditor
## Invoice Number
## Currency of Invoice
## Total Invoice Amount (gross)
## Total Invoice Amount (net)
## Open Invoice Amount (gross)
## Open Invoice Amount (net)
## Invoice Date
## Due Date

library(dplyr)
library(tidyr)

library(readxl) # only for testing
test_file <- read_xlsx("Test_File_1.xlsx") # only for testing

calc_days_overdue <- function(df, due_date, report_date = Sys.Date(), categories = c(0, 30, 60, 90), include.infinitive = TRUE) {
  # check for missing dates in due date column, if there are: stop and give out error message
  stopifnot(sum(is.na(df[[due_date]])) == 0)

  # including the infinitive values (not due more than X days, due more than X days) to the categories
  if (include.infinitive == TRUE) {
    categories <- append(categories, -Inf, 1)
    categories <- append(categories, Inf)
  }

  # calculating the number of days overdue from due date to report date
  df <- mutate(df, days_overdue = report_date - as.Date(df[[due_date]]))
  df$days_overdue <- as.numeric(df$days_overdue)

  # calculating the categories of days overdue (e.g. due between 30 and 60 days)
  df <- mutate(df, category = cut(as.numeric(days_overdue), categories))

  # label the categories of overdue dates
  # category_labels<-NULL
  # for (i in 1:length(categories)-1) {
  #   category_labels[i] <-
  #     paste(c(as.character(categories[i])," to ",as.character(categories[i+1])), collapse="")
  # }
  # category_labels <- append(category_labels, paste("Less than",categories[1]),0)
  # category_labels <- append(category_labels, paste("More than",categories[length(categories)]))
  # levels(df$category) <- category_labels
  # df$category <- levels(df$category)[df$category]

  # renaming the categories without the first and last character which are ( or [ and ) or ] to avoid special characters
  df$category <- gsub("^.|.$", "", df$category)
  df$category <- gsub(", ", "_to_", df$category)
  df$category <- gsub(",", "_to_", df$category)
  return(df)
}

test_file_1 <- calc_days_overdue(test_file, due_date = "Due Date")

###### two-step version of creating an aging table

aging_table <- function(df, # the dataframe from the previous step
                        open_amount, # the column which includes the open amount
                        customer_column, # the column which includes the customer name / ID
                        invoice_number, # the column which includes the invoice number
                        include.credit.notes = TRUE) {
  if (include.credit.notes == FALSE) {
    df <- df[!(df[[open_amount]] < 0)]
    warning("Negative amounts in the data frame, i.e. credit notes, have been excluded.")
  }

  df_1 <-
    pivot_wider(df,
      names_from = category,
      values_from = open_amount,
      values_fill = 0,
      values_fn = list(open_amount = sum),
      names_prefix = "Category_"
    )


  drop <- c(invoice_number, "days_overdue")
  category_names <- colnames(df_1[, grepl("Category_", names(df_1))])
  keep <- c(customer_column, category_names)

  df_2 <- df_1[, !(names(df_1) %in% drop)]
  df_2 <- df_1[, (names(df_1) %in% keep)]
  colnames(df_2)[1] <- "Customer"

  df_2 <- aggregate(. ~ Customer, df_2, sum)
  return(df_2)
}

aging_table(test_file_1, open_amount = "Net_Amount", customer_column = "Customer", invoice_number = "Invoice_Number")

###### next function. create the aging table itself

create_aging_table <- function(df, # the dataframe
                               open_amount, # the column which includes the open amount
                               customer_column, # the column which includes the customer name / ID
                               invoice_number, # the column which includes the invoice number
                               due_date, # the column which includes the due date of the invoice
                               report_date = Sys.Date(), # date to which the days overdue will be calculated
                               categories = c(0, 30, 60, 90), # the number of categories to be built in days
                               include.infinitive = TRUE,
                               include.credit.notes = TRUE) {
  df <- calc_days_overdue(
    df = df,
    due_date = due_date,
    report_date = as.Date(report_date),
    categories = categories,
    include.infinitive = include.infinitive
  )

  df <- aging_table(
    df = df,
    open_amount = all_of(open_amount),
    customer_column = customer_column,
    invoice_number = invoice_number,
    include.credit.notes = include.credit.notes
  )

  # message("Aging Table based on ", df)
  # message("The Cut-Off Date for the aging table is: ", report_date)
  # if (include.credit.notes == TRUE) {
  #   message("Credit notes are included in the report.")
  # }
  #   else message("Credit notes have been excluded from the report.")
}
aging_1 <- create_aging_table(test_file, open_amount = "Net_Amount", report_date = "2022-01-20", customer_column = "Customer", invoice_number = "Invoice_Number", due_date = "Due Date")
# format of report date has to be "YYYY-MM-DD"
# format of categories has to be a vector of numeric values
# format of columns has to be given as character string
