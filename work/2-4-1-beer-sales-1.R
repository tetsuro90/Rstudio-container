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

y_rep_normal <- rstan::extract(mcmc_normal)$pred
y_rep_poisson <- rstan::extract(mcmc_poisson)$pred

dim(y_rep_normal)

ppc_hist(y = animal_num$animal_num,
         yrep = y_rep_normal[1:5, ])

ppc_hist(y = animal_num$animal_num,
         yrep = y_rep_poisson[1:5,])


file_beer_sales_ab <- read.csv("book-r-stan-bayesian-model-intro/book-data/2-6-1-beer-sales-ab.csv")

ggplot(data = file_beer_sales_ab, 
       mapping = aes(x = sales, y = ..density..,
                     color = beer_name, fill = beer_name)) + 
  geom_histogram(alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.5, size = 0)

sales_a <- file_beer_sales_ab$sales[1:100]
sales_b <- file_beer_sales_ab$sales[101:200]

data_list_ab <- list(
  sales_a = sales_a,
  sales_b = sales_b,
  N = 100
)


mcmc_result_6 <- stan(
  file = "2-6-5-difference-mean.stan",
  data = data_list_ab,
  seed = 1
)
















