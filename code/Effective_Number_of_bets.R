# Effective_Number_of_bets <- function(R, Wt) {
#     sigma <- cov(R)
#     evec <- eigen(sigma, symmetric = TRUE)$vector #eigen vectors
#     eval <- eigen(sigma, symmetric = TRUE)$values #eigen values
#
#     e_min1 <- solve(evec) # Invert eigenvector (E^-1)
#     wtilde <- e_min1 %*% t(Wt)
#
#     v_n <- wtilde ^ 2 * eval
#     p_n <-
#         v_n / sum(v_n) # Proportion of variance attributed to each PC.
#     p_n <- ifelse(p_n < 0, 1e-5, p_n)
#     entropy <- exp(-sum(p_n * log(p_n)))
#     return(entropy)
# }

Effective_Number_of_bets <- function(R, Rp) {

    dates <- Rp %>% pull(date)
    dt <- R %>% filter(date %in% dates) %>%
        spread(Asset, Return) %>% select(-date)

    # Returns based: requires whitening
    z <- whitening::whiten(data.matrix(dt), T, "PCA")

    var_mod <- lm(Rp$portfolio.returns ~ z)

    v_n <- coef(var_mod)[-1] ^ 2
    p_n <- v_n / sum(v_n)

    entropy <- exp(-sum(p_n * log(p_n)))

    return(entropy)
}

# set.seed(8584)
# N = 100
# # Toy return matrix
# R <- tibble(a = rnorm(100, 0.02, 0.1),
#             b = a * rnorm(100, 1, 1)) %>% as.matrix()
#
# Wt <- tibble(a = 0.1, b = 0.9) %>% as.matrix()
#
# Rp <- 1:nrow(R) %>% map( ~ Wt %*% R[.x, ]) %>% unlist()
#
# # Port returns method
# z <- whitening::whiten(data.matrix(R), T, "PCA")
#
# var_mod <- lm(Rp ~ z)
#
# v_n <- coef(var_mod)[-1] ^ 2
# p_n <- v_n / sum(v_n)
#
# entropy <- exp(-sum(p_n * log(p_n)))
#
# # weight method
# sigma <- cov(R)
# evec <- eigen(sigma, symmetric = TRUE)$vector #eigen vectors
# eval <- eigen(sigma, symmetric = TRUE)$values #eigen values
#
# e_min1 <- solve(evec) # Invert eigenvector (E^-1)
# wtilde <- e_min1 %*% t(Wt)
#
# v_n_1 <- wtilde ^ 2 * eval
# p_n_1 <-
#     v_n / sum(v_n) # Proportion of variance attributed to each PC.
# p_n_1 <- ifelse(p_n < 0, 1e-5, p_n)
# entropy_1 <- exp(-sum(p_n * log(p_n)))
#
# all.equal(entropy, entropy_1)
