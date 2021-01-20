# Effective_Number_of_constituits
enc <- function(wt) {
    hhi <- sum(wt$weight^2)
    1/hhi
}
ENC <- function(market_, method) {

    data <-
        rebal_weights(
            data = market_,
            method = method,
            rebdates = rebdates,
            look_back = 100,
            progress = FALSE
    ) %>% split(.$date)

    data %>% map(~enc(.x)) %>% reduce(mean)
}
