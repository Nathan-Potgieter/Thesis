# Claculating condition number

conditional_number <- function(market) {
    eigen_values <-
        market %>%
        spread(Asset, Return) %>%
        select(-date) %>%
        fitHeavyTail::fit_mvt() %>%
        .$cov %>% eigen() %>%  .$value

    cond_num <- max(eigen_values) / min(eigen_values)
    return(cond_num)
}

load("data/market_1.rda")
cond_num_m1 <- market_1 %>% map(conditional_number)
rm(market_1)

load("data/market_2.rda")
cond_num_m2 <- market_2 %>% map(conditional_number)
rm(market_2)

load("data/market_3.rda")
cond_num_m3 <- market_3 %>% map(conditional_number)
rm(market_3)

load("data/market_4.rda")
cond_num_m4 <- market_4 %>% map(conditional_number)
rm(market_4)

load("data/market_5.rda")
cond_num_m5 <- market_5 %>% map(conditional_number)
rm(market_5)

cond_num <- list(
    market_1 = cond_num_m1,
    market_2 = cond_num_m2,
    market_3 = cond_num_m3,
    market_4 = cond_num_m4,
    market_5 = cond_num_m5
)
save(cond_num, file = "data/cond_num.rda")
