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

# Necessary are only those:
## Name Debitor / Creditor
## Currency of Invoice (only if not in the same currency)
## Open Invoice Amount (net)
## Due Date

