library(rstan)
library(ggfortify)
library()

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

file_beer_sales_1 <- read.csv("book-r-stan-bayesian-model-intro/book-data/2-4-1-beer-sales-1.csv")

sample_size <- nrow(file_beer_sales_1)

data_list <- list(sales = file_beer_sales_1$sales, N = sample_size)


mcmc_result <- stan(
  file = "2-4-1-beer-sales-1.stan",
  data = data_list,
  seed = 1,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  thin = 1
)

print(
  mcmc_reslut,
  probs = c(0.025, 0.5, 0.975)
)

traceplot(mcmc_reslut)
traceplot(mcmc_reslut, inc_warmup = T)

mcmc_sample <- rstan::extract(mcmc_reslut, permuted = FALSE)
class(mcmc_sample)

dim(mcmc_sample)
dimnames(mcmc_sample)

mcmc_sample[1, "chain:1", "mu"]

mu_mcmc_vec <- as.vector(mcmc_sample[,, "mu"])

median(mu_mcmc_sample)

mean(mu_mcmc_vec)

quantile(mu_mcmc_vec, probs = c(0.025, 0.975))

autoplot(ts(mcmc_sample[,,"mu"]),
         facets = F,
         ylab = "mu",
         main = "trace plot")

mu_df <- data.frame(
  mu_mcmc_sample = mu_mcmc_vec
)

ggplot(data = mu_df, mapping = aes(x = mu_mcmc_sample)) + geom_density(size = 1.5)










