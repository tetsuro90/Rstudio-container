library(rstan)
library(bayesplot)
library(ggplot2)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

file_beer_sales_2 <- read.csv("book-r-stan-bayesian-model-intro/book-data/3-2-1-beer-sales-2.csv")

sample_size <- nrow(file_beer_sales_2)

ggplot(file_beer_sales_2, aes(x = temperature, y = sales)) + 
  geom_point() +
  labs(title = "beer sales and temperature")

data_list <- list(
  N = sample_size,
  sales = file_beer_sales_2$sales,
  temperature = file_beer_sales_2$temperature
)

mcmc_result <- stan(
  file = "3-2-1-simple-lm.stan",
  data = data_list,
  seed = 1
)

print(mcmc_result, probs = c(0.025, 0.5 , 0.975))

mcmc_sample <- rstan::extract(mcmc_result, permuted = FALSE)

mcmc_combo(
  mcmc_sample,
  pars = c("Intercept", "beta", "sigma")
)



temperature_pred <- 11:30

data_list_pred <- list(
  N = sample_size,
  sales = file_beer_sales_2$sales,
  temperature = file_beer_sales_2$temperature,
  N_pred = length(temperature_pred),
  temperature_pred = temperature_pred
)

mcmc_result_pred <- stan(
  file = "3-3-1-simple-lm-pred.stan",
  data = data_list_pred,
  seed =1
)

mcmc_sample_pred <- rstan::extract(mcmc_result_pred, permuted = FALSE)

mcmc_intervals(
  mcmc_sample_pred,
  regex_pars = c("sales_pred."),
  prob = 0.8,
  prob_outer = 0.95
)

mcmc_intervals(
  mcmc_sample_pred,
  pars = c("mu_pred[1]", "sales_pred[1]"),
  prob = 0.8,
  prob_outer = 0.95
)

mcmc_areas(
  mcmc_sample_pred,
  pars = c("sales_pred[1]", "sales_pred[20]"),
  prob = 0.6,
  prob_outer = 0.99
)














