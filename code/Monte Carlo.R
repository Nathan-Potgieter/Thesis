# loading packages
pacman::p_load(dplyr, MCmarket)

# Correlation Mtrix
corr_1 <- diag(50)

# Creating vectors for portfolio mean and sd's
set.seed(8541683)
means <- rnorm(50, mean = 0.02, sd = 0.03)
sds <- rnorm(50, mean = 0.1, sd = 0.03)

N <- 10000
pb <- dplyr::progress_estimated(N)

#clearing memory and Viewing current limit
gc()
memory.size()
#memory.limit(size = 9999999999999)
memory.limit()

set.seed(9543712)

market_1 <-
    1:N %>%
    purrr::map_dfr(~sim_market(corr = corr_1,
                                             k = 300,
                                             mv_dist = "t",
                                             mv_df = 3,
                                             marginal_dist = "norm",
                                             marginal_dist_model = list(mu = means,
                                                                        sd = sds),
                                             ts_model = NULL),
                   .id = "Universe")

write.csv(market_1, file = "data/market_1.csv", row.names = FALSE)

market_1 <- read.csv("data/market.csv") %>% as_tibble()

# Saving dataframe
save(market_1, file = "data/market_1.rda")
rm(market_1)

