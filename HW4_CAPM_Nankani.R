# ------------------------------------------------------------
# ISyE 4803 Financial Data Analysis
# Homework 4 — CAPM
# Neal Nankani
# Summer 2026
# ------------------------------------------------------------

# Clear objects left over from earlier R sessions
rm(list = ls())

# ------------------------------------------------------------
# Part A: Confirm required files are available
# ------------------------------------------------------------

required_files <- c(
  "m_sp500ret_3mtcm.txt",
  "m_logret_10stocks.txt"
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    paste(
      "Missing required file(s):",
      paste(missing_files, collapse = ", ")
    )
  )
}

cat("Working directory:\n")
print(getwd())

cat("\nFiles in the homework folder:\n")
print(list.files())

cat("\nBoth required data files were found.\n")

# ------------------------------------------------------------
# Part A: Import the market and stock data
# ------------------------------------------------------------

# The market file begins with a comment line marked by %.
# comment.char = "%" tells R to ignore that line.
market_data <- read.table(
  "m_sp500ret_3mtcm.txt",
  header = TRUE,
  sep = "",
  comment.char = "%",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Read the monthly returns for the 10 stocks
stock_data <- read.table(
  "m_logret_10stocks.txt",
  header = TRUE,
  sep = "",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Remove the empty lines at the bottom of the stock file
stock_data <- stock_data[complete.cases(stock_data), ]
# ------------------------------------------------------------
# Part A: Create monthly excess returns
# ------------------------------------------------------------

# The 3mTCM values are annual percentage rates.
# Convert annual percent to an approximate monthly decimal rate.
risk_free <- market_data[["3mTCM"]] / 100 / 12

# Monthly market excess return
market_excess <- market_data[["sp500"]] - risk_free

# Check the first six converted values
cat("\nFirst six monthly risk-free rates:\n")
print(head(risk_free))

cat("\nFirst six market excess returns:\n")
print(head(market_excess))

# ------------------------------------------------------------
# Verify that the market and stock observations align by month
# ------------------------------------------------------------

# Convert the two different date formats to year-month labels
market_month <- format(
  as.Date(paste0("01-", market_data$Date), format = "%d-%b-%y"),
  "%Y-%m"
)

stock_month <- format(
  as.Date(stock_data$Date, format = "%m/%d/%Y"),
  "%Y-%m"
)

# Confirm matching row counts and months
stopifnot(
  nrow(market_data) == nrow(stock_data),
  length(risk_free) == nrow(stock_data),
  length(market_excess) == nrow(stock_data),
  identical(market_month, stock_month)
)

cat("\nAll 156 monthly observations align correctly.\n")

# ------------------------------------------------------------
# Part A: Create excess returns for all 10 stocks
# ------------------------------------------------------------

# Keep only the 10 return columns; remove Date
stock_returns <- stock_data[, -1]

# Subtract each month's risk-free rate from every stock return
stock_excess <- sweep(
  stock_returns,
  MARGIN = 1,
  STATS = risk_free,
  FUN = "-"
)

cat("\nDimensions of stock-excess-return data:\n")
print(dim(stock_excess))

cat("\nStock names:\n")
print(names(stock_excess))

cat("\nFirst six stock excess-return observations:\n")
print(head(stock_excess))

# ------------------------------------------------------------
# Extract the AAPL alpha and beta results
# ------------------------------------------------------------

aapl_coefficients <- summary(aapl_fit)$coefficients

aapl_alpha <- aapl_coefficients["(Intercept)", "Estimate"]
aapl_alpha_p <- aapl_coefficients["(Intercept)", "Pr(>|t|)"]

aapl_beta <- aapl_coefficients["market_excess", "Estimate"]
aapl_beta_p <- aapl_coefficients["market_excess", "Pr(>|t|)"]

cat("\nAAPL alpha:", aapl_alpha, "\n")
cat("AAPL alpha p-value:", aapl_alpha_p, "\n")
cat("AAPL beta:", aapl_beta, "\n")
cat("AAPL beta p-value:", aapl_beta_p, "\n")

if (aapl_alpha_p > 0.05) {
  cat("AAPL conclusion: Fail to reject alpha = 0; CAPM is consistent.\n")
} else {
  cat("AAPL conclusion: Reject alpha = 0; CAPM is rejected.\n")
}
