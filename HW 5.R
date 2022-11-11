#Problem 2
library(mosaicData)
library(tidyverse)
library(dplyr)
Gestation %>%
  pull(age) %>%
  t.test()


#Problem 5
library(mosaicData)
library(tidyverse)
library(gdata)
n <- nrow(Gestation)
orig_sample <- Gestation %>%
  slice_sample(n = n, replace = FALSE)

bootstrap <- 1:1000 %>%
  map_dfr(
    ~orig_sample %>%
      resample() 
  ) 
confint(lm(wt ~ age,data = bootstrap), level = 0.95)