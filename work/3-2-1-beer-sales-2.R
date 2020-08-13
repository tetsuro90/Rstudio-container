library(rstan)
library(bayesplot)
library(ggplot2)
library(brms)

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

# ~の左に応答変数を、右に説明変数を
formula_lm <- formula(sales ~ temperature)

X <- model.matrix(formula_lm, file_beer_sales_2)

head(X, n=5)

N <- nrow(file_beer_sales_2)

K <- 2

Y <- file_beer_sales_2$sales

data_list_design <- list(N = N, K = K, Y = Y, X = X)


mcmc_result_design <- stan(
  file = "3-4-1-lm-design-matrix.stan",
  data = data_list_design,
  seed = 1
)

# brmsを用いた単回帰モデルを作る
simple_lm_brms <- brm(
  formula = sales ~ temperature,
  family = gaussian(link = "identity"),
  data = file_beer_sales_2,
  seed = 1
)

simple_lm_brms

plot(simple_lm_brms)

simple_lm_formula <- bf(sales ~ temperature)
gaussian()
binomial()
poisson()

simple_lm_brms_2 <- brm(
  formula = simple_lm_formula,
  family = gaussian(),
  data = file_beer_sales_2,
  seed = 1,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  thin = 1
)

simple_lm_brms_3 <- brm(
  formula = sales ~ temperature,
  family = gaussian(),
  data = file_beer_sales_2,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)

prior_summary(simple_lm_brms_3)

get_prior(simple_lm_brms_3)

stancode(simple_lm_brms_3)

stanplot(simple_lm_brms_3,
         type = "intervals",
         pars = "^b_",
         prob = 0.8,
         prob_outer = 0.95)

new_data <- data.frame(temperature = 20)

fitted(simple_lm_brms, new_data)

mcmc_sample <- as.mcmc(simple_lm_brms, combine_chains = TRUE)

mcmc_b_Intercept <- mcmc_sample[, "b_Intercept"]
mcmc_b_temperature <- mcmc_sample[, "b_temperature"]
mcmc_sigma <- mcmc_sample[, "sigma"]

saigen_fitted <- mcmc_b_Intercept + 20 * mcmc_b_temperature

plot(saigen_fitted)

set.seed(1)
saigen_predict <- do.call(
  rnorm,
  c(4000, list(mean = saigen_fitted, sd = mcmc_sigma))
)


eff <- marginal_effects(simple_lm_brms)
plot(eff, points = TRUE)

set.seed(1)

eff_pre <- marginal_effects(simple_lm_brms, method = "predict")
plot(eff_pre, points = TRUE)





























