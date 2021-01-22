table_risk_metrics <- function(perf_data) {
    risk_metrics <- tibble(
        Metric = c("Sharp", "SD", "Downside Deviation", "VaR"),
        EW = c(
            mean(perf_data$naive$Sharp[[1]]),
            mean(perf_data$naive$SD[[1]]),
            mean(perf_data$naive$DD[[1]]),
            mean(perf_data$naive$Var[[1]], na.rm = T)
        ) %>% round(5),
        MV = c(
            mean(perf_data$minvol$Sharp[[1]]),
            mean(perf_data$minvol$SD[[1]]),
            mean(perf_data$minvol$DD[[1]]),
            mean(perf_data$minvol$Var[[1]], na.rm = T)
        ) %>% round(5),
        IV = c(
            mean(perf_data$invvol$Sharp[[1]]),
            mean(perf_data$invvol$SD[[1]]),
            mean(perf_data$invvol$DD[[1]]),
            mean(perf_data$invvol$Var[[1]], na.rm = T)
        ) %>% round(5),
        ERC = c(
            mean(perf_data$erc$Sharp[[1]]),
            mean(perf_data$erc$SD[[1]]),
            mean(perf_data$erc$DD[[1]]),
            mean(perf_data$erc$Var[[1]], na.rm = T)
        ) %>% round(5),
        MD = c(
            mean(perf_data$maxdiv$Sharp[[1]]),
            mean(perf_data$maxdiv$SD[[1]]),
            mean(perf_data$maxdiv$DD[[1]]),
            mean(perf_data$maxdiv$Var[[1]], na.rm = T)
        ) %>% round(5)
    )
    return(risk_metrics)
}

table_entropy <- function(enc, enb) {
    tibble(Metric = c("Inv HHI", "Entropy")) %>%
        mutate(
            EW = c(enc$naive %>% reduce(mean),
                   enb$naive %>% reduce(mean)) %>% round(1),

            MV = c(enc$minvol %>% reduce(mean),
                   enb$minvol %>% reduce(mean)) %>% round(1),

            IV = c(enc$invvol %>% reduce(mean),
                   enb$invvol %>% reduce(mean)) %>% round(1),

            ERC = c(enc$erc %>% reduce(mean),
                    enb$erc %>% reduce(mean)) %>% round(1),

            MD = c(enc$maxdiv %>% reduce(mean),
                   enb$maxdiv %>% reduce(mean)) %>% round(1)
        )
}
