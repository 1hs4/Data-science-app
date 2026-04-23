#!/usr/bin/env Rscript

# One-time setup for shinyapps.io credentials.
# Usage:
#   Rscript setup_shinyapps.R <account_name> <token> <secret>

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 3) {
  stop(
    "Usage: Rscript setup_shinyapps.R <account_name> <token> <secret>",
    call. = FALSE
  )
}

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Package 'rsconnect' is not installed.", call. = FALSE)
}

account <- args[[1]]
token <- args[[2]]
secret <- args[[3]]

rsconnect::setAccountInfo(
  name = account,
  token = token,
  secret = secret
)

message("shinyapps.io account configured successfully for: ", account)
