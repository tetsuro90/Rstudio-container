library(rstan)
library(ggfortify)
library(bayesplot)

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
  mcmc_result,
  probs = c(0.025, 0.5, 0.975)
)

traceplot(mcmc_result)
traceplot(mcmc_result, inc_warmup = T)

mcmc_sample <- rstan::extract(mcmc_result, permuted = FALSE)
class(mcmc_sample)

dim(mcmc_sample)
dimnames(mcmc_sample)

mcmc_sample[1, "chain:1", "mu"]

mu_mcmc_vec <- as.vector(mcmc_sample[,, "mu"])

median(mu_mcmc_vec)

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


mcmc_hist(mcmc_sample, pars = c("mu", "sigma"))

mcmc_dens(mcmc_sample, pars = c("mu", "sigma"))

mcmc_trace(mcmc_sample, pars = c("mu", "sigma"))

mcmc_combo(mcmc_sample, pars = c("mu", "sigma"))

mcmc_intervals(
  mcmc_sample, pars = c("mu", "sigma"),
  prob = 0.8,
  prob_outer = 0.95
)

mcmc_areas(mcmc_sample, pars = c("mu", "sigma"),
           prob = 0.6,
           prob_outer = 0.99)

mcmc_acf_bar(mcmc_sample, pars = c("mu", "sigma"))


# 事後予測チェック
animal_num <- read.csv("book-r-stan-bayesian-model-intro/book-data/2-5-1-animal-num.csv")
head(animal_num)

sample_size <- nrow(animal_num)

data_list <- list(animal_num = animal_num$animal_num, N = sample_size)

mcmc_normal <- stan(
  file = "2-5-1-normal-dist.stan",
  data = data_list,
  seed = 1
)

mcmc_poisson <- stan(
  file = "2-5-2-poisson-dist.stan",
  data = data_list,
  seed = 1
)






























