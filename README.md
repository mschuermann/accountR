
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="vignettes/Logo_accountR.jpg" width="25%" />

# accountR

<!-- badges: start -->
<!-- badges: end -->

The goal of accountR is to provide useful tools for accounting and
financial reporting. The tools allow users to convert data frames that
include values of multiple currencies automatically into one
user-defined currency at a specific reporting date. Additionally, it
provides functions to generate a Trade Receivables Aged Analysis report
which can be used to calculate the Allowance for Doubtful Accounts or
other tasks of the Accounts Receivable Management, such as monitoring of
payment collections.

## Installation

You can install the development version of accountR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mschuermann/accountR")
```

## Example for Currency Conversion

This is a basic example which shows you how to convert a data frame full
of different currencies into one currency:

``` r
library(accountR)

data <- accountR::demo_file
result <- 
  FX_rate_convert(data, "currency", "amount", "EUR", "2022-06-30")
#> For full currency exchange rate API documentation visit:
#>  https://exchangerate.host/#/#docs
#>  (this message will only appear once per session)

head(result)
#>   currency company name country customer id invoice no invoice date amount
#> 1      CAD            H  Canada       10019   INV03220   2022-07-11  44577
#> 2      CAD            H  Canada       10019   INV03258   2022-07-13  98807
#> 3      CAD            H  Canada       10019   INV03249   2022-07-12  75809
#> 4      CAD            H  Canada       10019   INV03233   2022-07-12  46431
#> 5      CAD            G  Canada       10018   INV02930   2022-05-20  81670
#> 6      CAD            H  Canada       10019   INV03058   2022-07-01  25716
#>     due date  FX_rate translated_amount
#> 1 2022-09-09 0.741648          33060.44
#> 2 2022-09-11 0.741648          73280.01
#> 3 2022-09-10 0.741648          56223.59
#> 4 2022-09-10 0.741648          34435.46
#> 5 2022-07-19 0.741648          60570.39
#> 6 2022-08-30 0.741648          19072.22
```

For further information and details please have a look into the accountR
vignette.

## Example for the Trade Receivables Aged Analysis report

This is a common example on how to generate a Trade Receivables Aged
Analysis report:

``` r
example_overdue <- aged_analysis(result, "due date", "2022-08-15")
example_report <- aging_report(example_overdue, "translated_amount", "company name", "invoice no")
head(example_report)
#>   Customer Less than 0 0 - 30  30 - 60  60 - 90   Total
#> 1        A    944070.0  75254     0.00  92171.2 1111495
#> 2        B    707494.0      0     0.00  51180.0  758674
#> 3        C    819466.0  11776 38213.00  96240.0  965695
#> 4        D    957180.9      0     0.00 173669.7 1130851
#> 5        E    918706.0  91301 76167.02 400631.7 1486806
#> 6        F    402013.0      0     0.00  92622.0  494635
```

For further information and details please have a look into the accountR
vignette.

# About the Package

If you have found issues or bugs, please let me know here:
<https://github.com/mschuermann/accountR/issues>

If you want to contribute, submit a suggestion for a useful tool that
should be added here or anything, feel free to reach out by email
(schrmanm@tcd.ie) or on [Twitter](https://twitter.com/schrmanm) or post
a suggestion in [here](https://github.com/mschuermann/accountR/issues)
or … whatever makes most sense to you - suggestions and contributions
are very welcome!
