#Problem 2
library(mosaicCalc)

Integrate((2/3)*exp((-1/3)*u) ~ u, domain(u=0:2)) + 
  Integrate(u*(1/3)*exp((-1/3)*u) ~ u, domain(u=2:Inf))

meanExp <- rep(0, 10000)
for(j in 1:10000){
  ob1 <- rexp(10000,1/3)
  for(i in 1:10000){
    if(ob1[i]<2){
      ob1[i] <-2
    }
  }
  meanExp[j]<-mean(ob1)
}
hist(meanExp)
t.test(meanExp)

#Problem 3
#head = 0, tail = 1
coin_flip <- function() {
  coin <- c("heads","tails")
  coin_sample_1 <- sample(coin, size = 4, replace = TRUE)
  coin_sample_2 <- sample(coin, size = 4, replace = TRUE)
  if(table(coin_sample_1)[[1]] == table(coin_sample_2)[[1]]){
    return(1)
  }
  else{
    return(0)
  }
}
simulation <- c()
for(i in 1:1000){
  simulation <- c(simulation, coin_flip())
}
prop.test(simulation)


#Problem 4
library(tidyverse)
set.seed(1976)
jobs_true <- 150
jobs_se <- 65  # in thousands of jobs
gen_samp <- function(true_mean, true_sd, 
                     num_months = 12, delta = 15, id = 1) {
  samp_year <- rep(true_mean, num_months) + 
    rnorm(num_months, mean = delta * (1:num_months), sd = true_sd)
  return(
    tibble(
      jobs_number = samp_year, 
      month = as.factor(1:num_months), 
      id = id
    )
  )
}

n_sims <- 3
params <- tibble(
  sd = c(0, rep(jobs_se, n_sims)), 
  id = c("Truth", paste("Sample", 1:n_sims))
)

df <- params %>%
  pmap_dfr(~gen_samp(true_mean = jobs_true, true_sd = ..1, id = ..2))

ggplot(data = df, aes(x = month, y = jobs_number)) + 
  geom_hline(yintercept = jobs_true, linetype = 2) + 
  geom_col() + 
  facet_wrap(~ id) + 
  ylab("Number of new jobs (in thousands)")

#Problem 6
n <- 100000
sim_meet <- tibble(
  sally = rnorm(n, mean = 30, sd = 10),
  joan = rnorm(n, mean = 30, sd = 10),
  result = ifelse(
    abs(sally - joan) <= 10, "They meet", "They do not"
  )
)
mosaic::tally(~ result, format = "percent", data = sim_meet)
mosaic::binom.test(~result, n, success = "They meet", data = sim_meet)

ggplot(data = sim_meet, aes(x = joan, y = sally, color = result)) + 
  geom_point(alpha = 0.3) + 
  geom_abline(intercept = 10, slope = 1) + 
  geom_abline(intercept = -10, slope = 1) + 
  scale_color_brewer(palette = "Set2")

#Problem 7
generate_model <- function() {
  n <- 250
  rmse <- 1
  x1 <- rep(c(0, 1), each = n / 2) # x1 resembles 0 0 0 ... 1 1 1
  x2 <- runif(n, min = 0, max = 5)
  beta0 <- -1
  beta1 <- 0.5
  beta2 <- 1.5
  y <- beta0 + beta1 * x1 + beta2 * x2 + rexp(n, rate = 1 / 2)
  return(lm(y~(x1+x2))$coefficients[[2]])
}
df <- c()
for(i in 1:1000){
  df <- c(df,generate_model())
}
df
hist(df)
qqnorm(df)
qqline(df)

#Problem 8
generate_model_8 <- function() {
  n <- 250
  rmse <- 1
  x1 <- rep(c(0, 1), each = n / 2) # x1 resembles 0 0 0 ... 1 1 1
  x2 <- runif(n, min = 0, max = 5)
  beta0 <- -1
  beta1 <- 0.5
  beta2 <- 1.5
  y <- beta0 + beta1 * x1 + beta2 * x2 + rnorm(n, mean = 0, sd = rmse + x2)
  return(lm(y~(x1+x2))$coefficients[[2]])
}

df <- c()
for(i in 1:1000){
  df <- c(df,generate_model_8())
}
hist(df)
qqnorm(df)
qqline(df)
