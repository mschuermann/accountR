# PURPOSE: Payment Reminders for Overdue Invoices


# customer_report <- function(data, customer_column, cutoff = 90) {
#
#   all_customers <- unique(data[[customer_column]])
#   for (id in all_customers) {
#     customers_report <- data %>%
#       filter(data[[customer_column]] == id) %>%
#       filter(days_overdue >= cutoff) %>%
#       arrange(desc(days_overdue))
#   }
#   reports <- as.list()
#   reports
# }
#
# customer_report(data = test_file_1, customer_column = "Customer")
