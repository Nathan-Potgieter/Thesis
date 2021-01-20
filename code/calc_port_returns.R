# --------------------------------------------------------------------
# This function creates a set of weights from a single market at a date
# --------------------------------------------------------------------
port_weight_calc <- function(data, method = "minvol", from_date, look_back = 200) {


    if (method != "naive") {
        cov <- data %>%
            filter(date < from_date & date >= from_date %m-% days(look_back)) %>%
            spread(Asset, Return) %>% select(-date) %>%
            fitHeavyTail::fit_mvt() %>% .$cov
    }

    # Calculating min var weights and making xts
    if (method == "naive") {
        weights <- data %>%
            filter(date == from_date) %>%
            mutate(weight = 1 / n()) %>%
            select(date, Asset, weight)

    } else
        if (method == "minvol") {
            weights <- data %>%
                filter(date == from_date) %>%
                mutate(weight = optimalPortfolio(
                    Sigma = cov,
                    control = list(
                        type = "minvol",
                        constraint = "user",
                        LB = rep(0, nrow(cov)),
                        UB = rep(0.1, nrow(cov))
                    )
                )) %>%
                select(-Return)

        } else
            if (method == "invvol") {
                weights <- data %>%
                    filter(date == from_date) %>%
                    mutate(weight = optimalPortfolio(
                        Sigma = cov,
                        control = list(
                            type = "invvol",
                            constraint = "user",
                            LB = rep(0, nrow(cov)),
                            UB = rep(0.1, nrow(cov))
                        )
                    )) %>%
                    select(-Return)

            } else
                if (method == "erc") {
                    weights <- data %>%
                        filter(date == from_date) %>%
                        mutate(weight = optimalPortfolio(
                            Sigma = cov,
                            control = list(
                                type = "erc",
                                constraint = "user",
                                LB = rep(0, nrow(cov)),
                                UB = rep(0.1, nrow(cov))
                            )
                        )) %>%
                        select(-Return)

                } else
                    if (method == "maxdiv") {
                        weights <- data %>%
                            filter(date == from_date) %>%
                            mutate(weight = optimalPortfolio(
                                Sigma = cov,
                                control = list(
                                    type = "maxdiv",
                                    constraint = "user",
                                    LB = rep(0, nrow(cov)),
                                    UB = rep(0.1, nrow(cov))
                                )
                            )) %>%
                            select(-Return)
                    }

    return(weights)
}

#eg: w <- port_weight_calc(data = dat[[1]], method = "minvol", from_date = as.Date("2021-04-26"), look_back = 100)

# --------------------------------------------------------------
# This function calculates portfolio returns for a single market
# --------------------------------------------------------------
port_returns <- function(data, weights, look_back) {

    returns_xts <- data %>%
        select(date, Asset, Return)%>%
        spread(Asset, Return) %>%
        filter(date>=first(date) %m+% days(look_back)) %>%
        tbl_xts()

    weights_xts <- weights %>%
        spread(Asset, weight) %>%
        tbl_xts()

    # Calculating portfolio returns
    rmsfuns::Safe_Return.portfolio(returns_xts, weights = weights_xts) %>%
        xts_tbl()

}

#eg: port_returns(data = dat[[1]], weights = w, look_back = 100)

# -----------------------------------------------------------------------------
# This function couples the first two functions in this chunk.
# Therefore, it creates portfolio returns with periodic re-balancing.
# ----------------------------------------------------------------------------

rebal_weights <- function(data, method, rebdates, look_back, progress = TRUE) {

    if (progress == TRUE) pb$tick()$print()

    weights <- rebdates %>%
        map_dfr(~port_weight_calc(data, method = method,
                                  from_date =  .x,
                                  look_back = look_back))
    return(weights)
}

rebal_port_returns <- function(data, method, rebdates, look_back) {

    pb$tick()$print()

    weights <- rebdates %>%
        map_dfr(~port_weight_calc(data, method = method,
                                  from_date =  .x,
                                  look_back = look_back))

    port_returns(data, weights = weights, look_back = look_back)
}

#eg. rebal_port_returns(dat[[1]], method = "minvol", rebdates, look_back = 100)

# ----------------------------------------------------------------------------
# This function maps rebal_port_returns over the Universe columns of the dataset
# Therefore, it creates portfolio returns with periodic re-balancing, for each universe
# ----------------------------------------------------------------------------
port_rets_universal <- function(data_universal, method, rebdates, look_back = 200) {
    # Tick to progress bar

    data_universal  %>% map(~rebal_port_returns(.x,
                                                method = method,
                                                rebdates = rebdates,
                                                look_back = look_back))

}

