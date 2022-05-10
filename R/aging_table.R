library(devtools)
library(tidyverse)

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

# Necessary are only those:
## Name Debitor / Creditor
## Currency of Invoice (only if not in the same currency)
## Open Invoice Amount (net)
## Due Date

library(readxl)
test_file <- read_xlsx("Test_File_1.xlsx")

calc_days_overdue <- function(df, due_date, categories = c(0,30,60,90), include.infinitive = TRUE) {
  if (include.infinitive == TRUE) {
  categories <- append(categories, -Inf, 1)
  categories <- append(categories, Inf)
  }
  df %>%
    mutate(days_overdue = Sys.Date() - as.Date(due_date)) %>%
    mutate(category = cut(as.numeric(days_overdue), categories))
}

test_file_1 <- calc_days_overdue(test_file, due_date = test_file$`Due Date`)

create_aging_table <- function(df, open_amount, customer_column, invoice_number, due_date, categories = c(0,30,60,90), include.infinitive = TRUE) {

  #df %>%
  #  rename("amount" := {{open_amount}})
  keep <- c(as.character(customer_column),"days_overdue", "amount", as.character(invoice_number))
  drop <- c(as.character(invoice_number), "days_overdue")

  df <- calc_days_overdue(df =df, due_date = due_date, categories= categories, include.infinitive = include.infinitive)

  df_1 <- as.data.frame(df)
  df_2 <- df_1 %>%
    pivot_wider(
      names_from = category,
      values_from = amount,
      values_fill = list(amount = 0),
      values_fn = list(amount = sum))

  df_3 <- df_2[, !(names(df_2) %in% drop)] %>%
    group_by(customer_column) %>%
    summarise_all(sum)
}

create_aging_table(test_file_1, `Net_Amount`, `Customer`, `Invoice_Number`)
create_aging_table(test_file, test_file$Net_Amount, test_file$Customer, test_file$Invoice_Number, test_file$`Due Date`)
