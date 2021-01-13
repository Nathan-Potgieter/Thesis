pacman::p_load(tidyverse, tidyquant)

# saving date
date <- Sys.Date()

# Getting top 100 by market cap tickers as of 2021/01/12
Tickers <- tq_index("SP500") %>%
    arrange(symbol) %>%
    mutate(symbol = case_when(symbol == "BRK.B" ~ "BRK-B",
                              symbol == "BF.B" ~ "BF-B",
                              TRUE ~ as.character(symbol))) %>%
    slice_max(order_by = weight, n = 100) %>% pull(symbol)

# function to get data
get_data <- function(ticker = "AAPL", from) {
    df <- tq_get(ticker, from = from) %>% mutate(symbol = rep(ticker, length(date)))
    return(df)
}

# Building data set, from_date to current date
from_date <- as.Date("2016-01-01")
SNP_top100_data <-
    Tickers %>% map_dfr(~get_data(ticker = .x, from = from_date))

# Checking for NA's: There arn't any
any(is.na(SNP_top100_data))

# Calculating Returns and separating by date
SNP_top100_data <-
SNP_top100_data %>% select(date, symbol, adjusted) %>%
    arrange(date) %>% group_by(symbol) %>%
    mutate(return = adjusted/lag(adjusted) - 1) %>%
    filter(date > first(date) & date <= as.Date("2021-01-01"))

# Saving data frame
save(SNP_top100_data, file = "data/SNP_top100_data.rda")


# Selecting a random index of 50
set.seed(428653)
indx <- sample(1:100, size = 50)


# Calculating COV/CORR using fitHeavyTail
emp_corr <-
SNP_top100_data %>% select(date, symbol, return) %>%
    spread(symbol, return) %>% select(-date) %>%
    .[,indx] %>%   # selecting random subset
    fitHeavyTail::fit_mvt() %>% .$cov %>% cov2cor()
save(emp_corr, file = "data/emp_corr.rda")


# Cleaning CORR with covfactormodel package
install.packages('covFactorModel')
pacman::p_load(covFactorModel)





