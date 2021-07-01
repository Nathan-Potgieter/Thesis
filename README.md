# The Effect of Correlation Structure on the Performance of Risk Optimised Portfolios

================
Nathan Potgieter

## Purpose

This document serves as the README for Nathan Potgieter’s Stellenbosch
University Masters thesis. This works sets out to perform Monte Carlo
simulations of five different financial market types, each with a unique
correlation structure, and then evaluate the performance of numerous
portfolio optimisation routines in each market type. This is done in
order to determine how a markets correlation structure impacts the
performance of portfolio optimisers. Ideally a set of heuristics will be
developed that can aid portfolio managers in their decision of which
portfolio optimiser to use.

## Creating Correlation Structure Types

This section sets out to design five distinct 50 by 50 market
correlation matrices, ranging from highly unrealistic, and purely
theoretical, to more realistic hierarchically clustered matrices.

``` r
# Loading required packages
library('pacman')
p_load(MCmarket, tidyverse, ggcorrplot)

#----------------------------------------------
# First look at the diagonal correlation matrix
# This seems far to simple and boring; Check with nico
#----------------------------------------------
corr_1 <- diag(50)
eigen_1 <- eigen(corr_1) # all eigenvalues are 1 and all eigenvectors contain only zero's
corr_1 %>% ggcorrplot(hc.order = TRUE, title = "Diagonal Matrix")
```

<img src="README_files/figure-gfm/making markets-1.png" width="80%" height="80%" style="display: block; margin: auto;" />

``` r
#---------------------------------------------
# Correlation matrix with no clusters
#---------------------------------------------
corr_2 <- gen_corr(D = 50, Clusters = "none")
eigen_2 <- eigen(corr_2)
corr_2 %>% ggcorrplot(title = "No Clusters")
```

<img src="README_files/figure-gfm/making markets-2.png" width="80%" height="80%" style="display: block; margin: auto;" />

``` r
#---------------------------------------------
# Correlation matrix with 5 clusters
#---------------------------------------------
corr_3 <- gen_corr(D = 50, Clusters = "non-overlapping", Num_Clusters = 5)
eigen_3 <- eigen(corr_3)
corr_3 %>% ggcorrplot(hc.order = TRUE, title = "Five Clusters")
```

<img src="README_files/figure-gfm/making markets-3.png" width="80%" height="80%" style="display: block; margin: auto;" />

``` r
#---------------------------------------------
# Correlation matrix with 10, 5 and 2 overlapping clusters
#---------------------------------------------
corr_4 <- gen_corr(D = 50, Clusters = "overlapping", Num_Clusters = c(10,5,2), Num_Layers = 3)
eigen_4 <- eigen(corr_4)
corr_4 %>% ggcorrplot(hc.order = TRUE, title = "Overlapping Clusters")
```

<img src="README_files/figure-gfm/making markets-4.png" width="80%" height="80%" style="display: block; margin: auto;" />

``` r
#-----------------------------------------------
# Some empirical correlation matrices - from S&P500
# See ??corr_mats for info on how dataset was compiled
# First looking at "normal market"
#-----------------------------------------------
corr_5 <- corr_mats$cor_normal[[2]]
eigen_5 <- eigen(corr_5)
corr_5 %>% ggcorrplot(hc.order = TRUE, title = "Normal Market") +
    theme_bw() +
  theme(axis.text.x=element_text(angle=90),
        axis.title = element_blank(),
        axis.ticks = element_blank())
```

<img src="README_files/figure-gfm/making markets-5.png" width="80%" height="80%" style="display: block; margin: auto;" />

``` r
#----------------------------------------------
# "stressed market"
#----------------------------------------------
corr_6 <- corr_mats$cor_stressed[[1]]
eigen_6 <- eigen(corr_6)
corr_6 %>% ggcorrplot(hc.order = TRUE, title = "Stressed Market")  +
    theme_bw() +
  theme(axis.text.x=element_text(angle=90),
        axis.title = element_blank(),
        axis.ticks = element_blank())
```

<img src="README_files/figure-gfm/making markets-6.png" width="80%" height="80%" style="display: block; margin: auto;" />

``` r
#----------------------------------------------
# "rally market"
#----------------------------------------------
corr_7 <- corr_mats$cor_rally[[1]]
eigen_7 <- eigen(corr_7)
corr_7 %>% ggcorrplot(hc.order = TRUE, title = "Rally Market")  +
    theme_bw() +
  theme(axis.text.x=element_text(angle=90),
        axis.title = element_blank(),
        axis.ticks = element_blank())
```

<img src="README_files/figure-gfm/making markets-7.png" width="80%" height="80%" style="display: block; margin: auto;" />

``` r
#------------------------------------------
# Data set of eigenvalues
#------------------------------------------
eigens <- tibble(corr_1 = eigen_1$values,
                 corr_2 = eigen_2$values,
                 corr_3 = eigen_3$values,
                 corr_4 = eigen_4$values,
                 corr_5 = eigen_5$values,
                 corr_6 = eigen_6$values,
                 corr_7 = eigen_7$values)
rm(eigen_1, eigen_2, eigen_3, eigen_4, eigen_5, eigen_6, eigen_7)

# Printing Eigenvalues in a table
head(eigens, n = 10)
```

# A tibble: 10 x 7

corr\_1 corr\_2 corr\_3 corr\_4 corr\_5 corr\_6 corr\_7 <dbl> <dbl>
<dbl> <dbl> <dbl> <dbl> <dbl> 1 1 15.9 9.10 21.7 18.0 20.4 9.42 2 1 10.4
9.10 6.73 3.28 2.54 4.66 3 1 6.28 9.10 1.9 2.20 1.73 2.19 4 1 3.92 9.10
1.90 1.51 1.55 2.03 5 1 2.59 9.10 1.09 1.37 1.33 1.67 6 1 1.81 0.1 1.07
1.30 1.19 1.46 7 1 1.32 0.1 0.4 1.20 1.14 1.40 8 1 1.01 0.1 0.4 1.18
1.10 1.27 9 1 0.793 0.1 0.4 1.06 1.05 1.22 10 1 0.640 0.1 0.4 0.999 1.03
1.20

## Simulating Markets

``` r
#clearing memory and Viewing current limit
gc()
memory.size(max = NA)

# Setting number of markets and length of progress bar.
N <- 50
pb <- dplyr::progress_estimated(N)


set.seed(9543712)
market_1 <-
    1:N %>%
    map_dfr(~sim_market_with_progress(corr = diag(20),
                                      k = 252,
                                      mv_dist = "t",
                                      mv_df = 3,
                                      marginal_dist = "norm",
                                      marginal_dist_model = list(mu = 0.02,
                                                                 sd = 0.1),
                                      ts_model = NULL),
            .id = "Universe")

# Saving dataframe
save(market_1, file = "data/market_1.rda")
save(market_1, file = "data/market_1.Rdata")
rm(market_1)
```

## Portfolio Optimisation

``` r
pacman::p_load(lubridate, fitHeavyTail, RiskPortfolios, tbl2xts, rmsfuns)
load("data/market_1.rda")

opt_port_return <- function(data, method, cov_calc_period = 100) {
  
  data_exante <- data %>%
    select(date, Asset, Return) %>% 
    spread(Asset, Return) %>% 
    arrange(date) %>% 
    filter(date<first(date) %m+% days(cov_calc_period))
  
  # Calculating covar mat
  cov <- data_exante %>% 
  select(-date) %>% 
  fit_mvt() %>% .$cov
  
  # Calculating min var weights and making xts 
  weights <- data %>% 
    filter(date == first(date)) %>% 
    mutate(weight = case_when(
      method == "naive" ~ 1/n(),
      
      method == "minvol" ~ optimalPortfolio(Sigma = cov, 
                                            control = list(type = "minvol", constraint = "lo")),
      method == "invvol" ~ optimalPortfolio(Sigma = cov, 
                                            control = list(type = "invvol", constraint = "lo")),
      method == "erc" ~ optimalPortfolio(Sigma = cov, 
                                         control = list(type = "erc", constraint = "lo")),
      method == "maxdiv" ~ optimalPortfolio(Sigma = cov, 
                                      control = list(type = "maxdiv", constraint = "lo"))
      )) %>% 
    select(date, Asset, weight) %>% 
    tbl_xts(cols_to_xts = "weight", spread_by = "Asset")
  
  # Creating expost portfolio dataset
  data_expost_xts <- data %>%
    select(date, Asset, Return) %>% 
    spread(Asset, Return) %>% 
    arrange(date) %>% 
    filter(date>first(date) %m+% days(cov_calc_period)) %>% 
    tbl_xts()
  
  # Calculating portfolio returns
  rmsfuns::Safe_Return.portfolio(data_expost_xts, weights = weights) %>% 
    xts_tbl()
}


# Calculating portfolio returns across universes

naive_port_m1 <- market_1 %>% 
  split(market_1$Universe) %>%
  map_dfr(~opt_port_return(.x, method = "naive", cov_calc_period = 100), .id = "Universe")

min_var_port_m1 <- market_1 %>% 
  split(market_1$Universe) %>%
  map_dfr(~opt_port_return(.x, method = "minvol"), .id = "Universe")

invvol_port_m1 <- market_1 %>% 
  split(market_1$Universe) %>%
  map_dfr(~opt_port_return(.x, method = "invvol"), .id = "Universe")

erc_port_m1 <- market_1 %>% 
  split(market_1$Universe) %>%
  map_dfr(~opt_port_return(.x, method = "erc"), .id = "Universe")

maxdiv_port_m1 <- market_1 %>% 
  split(market_1$Universe) %>%
  map_dfr(~opt_port_return(.x, method = "maxdiv"), .id = "Universe")
```

## Portfolio Analytics

``` r
pacman::p_load(PerformanceAnalytics)
universal_portfolio_analytics <- function(portfolio_return_tbl,
                                          method) {
  
  if(!method %in% c("sd", "DownsideDeviation", 
                    "VaR", "CVaR", "SharpRatio", 
                    "AverageDrawdown", "maxDrawdown")) stop("Please Provide a valid method argument")
    xts_list <-
    portfolio_return_tbl %>% 
    split(portfolio_return_tbl$Universe) %>% 
    map(~select(.data = .x, date, portfolio.returns)) %>% 
    map(~tbl_xts(.x)) 
  
  case_when(
    method == "sd" ~ map(xts_list, ~sd(.x)),
    method == "DownsideDeviation" ~ map(xts_list, ~DownsideDeviation(.x)),
    method == "VaR" ~ map(xts_list, ~VaR(.x)),
    method == "CVaR" ~ map(xts_list, ~CVaR(.x)),
    method == "SharpRatio" ~ map(xts_list, ~SharpeRatio(.x)),
    method == "AverageDrawdown" ~ map(xts_list, ~AverageDrawdown(.x)),
    method == "maxDrawdown" ~ map(xts_list, ~maxDrawdown(.x))
  ) %>% 
  map_dfr(~as_tibble(t(.x))) 
}

list_all_portfolio_analytics <- function(portfolio_return_tbl) {
  list(
  StandardDeviation = universal_portfolio_analytics(portfolio_return_tbl, method = "sd"),
  DownsideDeviation = universal_portfolio_analytics(portfolio_return_tbl, method = "DownsideDeviation"), 
  VaR = universal_portfolio_analytics(portfolio_return_tbl, method = "VaR"), 
  CVaR = universal_portfolio_analytics(portfolio_return_tbl, method = "CVaR"), 
  SharpRatio = universal_portfolio_analytics(portfolio_return_tbl, method = "SharpRatio"),
  AverageDrawdown = universal_portfolio_analytics(portfolio_return_tbl, method = "AverageDrawdown"),
  maxDrawdown = universal_portfolio_analytics(portfolio_return_tbl, method = "maxDrawdown")
)
}

# All Portfolio analytics for each portfolio type
# naive_portfolio_analytics <- list_all_portfolio_analytics(naive_port_m1)
# min_var_portfolio_analytics <- list_all_portfolio_analytics(min_var_port_m1)
# invvol_portfolio_analytics <- list_all_portfolio_analytics(invvol_port_m1)
# erc_portfolio_analytics <- list_all_portfolio_analytics(erc_port_m1)
# maxdiv_portfolio_analytics <- list_all_portfolio_analytics(maxdiv_port_m1)

port_analytics <- list(
  StandardDeviation = tibble(
    naive_portfolio = universal_portfolio_analytics(naive_port_m1, method = "sd"),
    min_var_portfolio = universal_portfolio_analytics(min_var_port_m1, method = "sd"),
    invvol_portfolio = universal_portfolio_analytics(invvol_port_m1, method = "sd"),
    erc_portfolio = universal_portfolio_analytics(erc_port_m1, method = "sd"),
    maxdiv_portfolio = universal_portfolio_analytics(maxdiv_port_m1, method = "sd")
           ),
  DownsideDeviation = tibble(
    naive_portfolio = universal_portfolio_analytics(naive_port_m1, method = "DownsideDeviation"),
    min_var_portfolio = universal_portfolio_analytics(min_var_port_m1, method = "DownsideDeviation"),
    invvol_portfolio = universal_portfolio_analytics(invvol_port_m1, method = "DownsideDeviation"),
    erc_portfolio = universal_portfolio_analytics(erc_port_m1, method = "DownsideDeviation"),
    maxdiv_portfolio = universal_portfolio_analytics(maxdiv_port_m1, method = "DownsideDeviation")
  ),
  VaR = tibble(
    naive_portfolio = universal_portfolio_analytics(naive_port_m1, method = "VaR"),
    min_var_portfolio = universal_portfolio_analytics(min_var_port_m1, method = "VaR"),
    invvol_portfolio = universal_portfolio_analytics(invvol_port_m1, method = "VaR"),
    erc_portfolio = universal_portfolio_analytics(erc_port_m1, method = "VaR"),
    maxdiv_portfolio = universal_portfolio_analytics(maxdiv_port_m1, method = "VaR")
  ),
  CVaR = tibble(
    naive_portfolio = universal_portfolio_analytics(naive_port_m1, method = "CVaR"),
    min_var_portfolio = universal_portfolio_analytics(min_var_port_m1, method = "CVaR"),
    invvol_portfolio = universal_portfolio_analytics(invvol_port_m1, method = "CVaR"),
    erc_portfolio = universal_portfolio_analytics(erc_port_m1, method = "CVaR"),
    maxdiv_portfolio = universal_portfolio_analytics(maxdiv_port_m1, method = "CVaR")
  ),
  SharpRatio = tibble(
    naive_portfolio = universal_portfolio_analytics(naive_port_m1, method = "SharpRatio"),
    min_var_portfolio = universal_portfolio_analytics(min_var_port_m1, method = "SharpRatio"),
    invvol_portfolio = universal_portfolio_analytics(invvol_port_m1, method = "SharpRatio"),
    erc_portfolio = universal_portfolio_analytics(erc_port_m1, method = "SharpRatio"),
    maxdiv_portfolio = universal_portfolio_analytics(maxdiv_port_m1, method = "SharpRatio")
  ),
  AverageDrawdown = tibble(
    naive_portfolio = universal_portfolio_analytics(naive_port_m1, method = "AverageDrawdown"),
    min_var_portfolio = universal_portfolio_analytics(min_var_port_m1, method = "AverageDrawdown"),
    invvol_portfolio = universal_portfolio_analytics(invvol_port_m1, method = "AverageDrawdown"),
    erc_portfolio = universal_portfolio_analytics(erc_port_m1, method = "AverageDrawdown"),
    maxdiv_portfolio = universal_portfolio_analytics(maxdiv_port_m1, method = "AverageDrawdown")
  ),
  maxDrawdown = tibble(
    naive_portfolio = universal_portfolio_analytics(naive_port_m1, method = "maxDrawdown"),
    min_var_portfolio = universal_portfolio_analytics(min_var_port_m1, method = "maxDrawdown"),
    invvol_portfolio = universal_portfolio_analytics(invvol_port_m1, method = "maxDrawdown"),
    erc_portfolio = universal_portfolio_analytics(erc_port_m1, method = "maxDrawdown"),
    maxdiv_portfolio = universal_portfolio_analytics(maxdiv_port_m1, method = "maxDrawdown")
  )
)
```

# Apendix

### Calculating Expected returns and Covarience matricies.

``` r
pacman::p_load(lubridate, RiskPortfolios, fitHeavyTail)
load("data/market_1.rda")

# selecting only one market
data <- market_1 %>% 
  filter(Universe == 1) %>% 
  select(date, Asset, Return) %>%
  spread(Asset, Return) %>% 
  arrange(date) %>% 
  filter(date<first(date) %m+% days(100)) # only first 100 days
rm(market_1); gc()

data_no_date <- data %>% select(-date) %>% data.matrix()

# Simple covariance matrix: Assuming Gaussian (when we know actually t)
cov <- RiskPortfolios::covEstimation(data_no_date)
# Ledoit Wolf shrinkage:
cov_lw <- RiskPortfolios::covEstimation(data_no_date, control = list(type = "lw"))
# Shrinkage using constant correlation matrix:
cov_const <- RiskPortfolios::covEstimation(data_no_date, control = list(type = "const"))
# FitHeavyTail method
HTT <- fitHeavyTail::fit_mvt(data_no_date)
mu_ht <- HTT$mu
cov_ht <- HTT$cov

cov %>% cov2cor() %>% ggcorrplot()
cov_lw %>% cov2cor() %>% ggcorrplot()
cov_const %>% cov2cor() %>% ggcorrplot() # this seems junk
cov_ht %>% cov2cor() %>% ggcorrplot()

Mu <- data %>%
  select(-date) %>% 
  data.matrix() %>% 
  RiskPortfolios::meanEstimation()
```

### Setting Constraints

### Calculating Some Portfolio Optimiser Weights

``` r
pacman::p_load(quadprog, tbl2xts, RiskPortfolios)

minvar(var = cov_ht, wmin = 0.0, wmax = 1)

optimalPortfolio(Sigma = cov_ht, 
                 control = list(type = "minvol", constraint = "lo"))

optimalPortfolio(Sigma = cov_ht, 
                 control = list(type = "invvol", constraint = "lo"))

optimalPortfolio(Sigma = cov_ht, 
                 control = list(type = "erc", constraint = "lo"))

optimalPortfolio(Sigma = cov_ht, 
                 control = list(type = "maxdiv", constraint = "lo"))
```

#### Nieve 1/N Portfilio Returns

#### Min Var Portfolio Returns

``` r
load("data/market_1.rda")

# Creating a single market dataset
data <- market_1 %>% 
  filter(Universe == 1) %>% 
  select(date, Asset, Return)  
rm(market_1)

# Creating a wide exante data set
data_exante <- data %>%
  spread(Asset, Return) %>% 
  arrange(date) %>% 
  filter(date<first(date) %m+% days(100))

# Calculating covarience matrix
cov <- data_exante %>% 
  select(-date) %>% 
  fit_mvt() %>% .$cov

# Calculating min var weights and making xts 
weights <- 
  data %>% 
  filter(date == first(date)) %>% 
  mutate(weight = minvar(cov, wmin = 0, wmax = 1)) %>% 
  select(date, Asset, weight) %>% 
  tbl_xts(cols_to_xts = "weight", spread_by = "Asset")

# Creating expost portfolio dataset
data_expost_xts <- data %>%
  spread(Asset, Return) %>% 
  arrange(date) %>% 
  filter(date>first(date) %m+% days(100)) %>% 
  tbl_xts()


rmsfuns::Safe_Return.portfolio(data_expost_xts, weights = weights)
```
