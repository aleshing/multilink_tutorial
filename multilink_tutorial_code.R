#### Package installation ####
# install.packages(c("multilink", "tidyverse"))

#### Load packages ####
library(multilink)
library(tidyverse)

#### Load example data set ####
load("mixed_data_messy.RData")

#### Explore example data set ####
names(mixed_data_messy)

head(mixed_data_messy$mixed_data_file1)
head(mixed_data_messy$mixed_data_file2)
head(mixed_data_messy$mixed_data_file3)

#### Make file_sizes and duplicates ####
file_sizes <- c(nrow(mixed_data_messy$mixed_data_file1),
                nrow(mixed_data_messy$mixed_data_file2),
                nrow(mixed_data_messy$mixed_data_file3))
duplicates <- c(0, 1, 1)

#### Recode missing values ####
mixed_data_messy$mixed_data_file3 <- 
  mixed_data_messy$mixed_data_file3 %>%
  mutate(sex = ifelse(sex == "unknown", NA, sex))

#### Align fields ####
mixed_data_messy$mixed_data_file1 <- 
  mixed_data_messy$mixed_data_file1 %>%
  mutate(age = NA)

mixed_data_messy$mixed_data_file2 <- 
  mixed_data_messy$mixed_data_file2 %>%
  rename(gname = given_name)

colnames(mixed_data_messy$mixed_data_file3)

mixed_data_messy$mixed_data_file1 <- 
  mixed_data_messy$mixed_data_file1 %>%
  select(sex, gname, fname, phone, postcode, age, occup)

mixed_data_messy$mixed_data_file2 <- 
  mixed_data_messy$mixed_data_file2 %>%
  select(sex, gname, fname, phone, postcode, age, occup)

#### Stack files ####
records <- rbind(mixed_data_messy$mixed_data_file1,
                 mixed_data_messy$mixed_data_file2,
                 mixed_data_messy$mixed_data_file3)

#### Make types ####
types <- c("bi", "lv", "lv", "lv", "lv", "bi", "bi")

#### Create comparison data ####
breaks = list(sex = NA,  
              gname = c(0, 0.25, 0.5),  
              fname = c(0, 0.25, 0.5),
              phone = c(0, 0.25, 0.5), 
              postcode = c(0, 0.25, 0.5),  
              age = NA, 
              occup = NA)

comparison_list <- create_comparison_data(records = records,
                                          types = types,
                                          breaks = breaks,
                                          file_sizes = file_sizes,
                                          duplicates = duplicates)

head(comparison_list$comparisons)
head(comparison_list$record_pairs)

#### Reduce comparison data ####
pairs_to_keep <- (comparison_list$comparisons[, "gname_DL_3"] == FALSE) &
  (comparison_list$comparisons[, "fname_DL_3"] == FALSE)
cc <- 1

reduced_comparison_list <- 
  reduce_comparison_data(comparison_list = comparison_list,
                         pairs_to_keep = pairs_to_keep, 
                         cc = cc)

#### Specify hyperparameters ####
dup_upper_bound <- c(1, 10, 10)
dup_count_prior_family <- rep("Poisson", reduced_comparison_list$K)
dup_count_prior_pars <- rep(list(c(1)), reduced_comparison_list$K)

prior_list <- specify_prior(comparison_list = reduced_comparison_list,
                            mus = NA, 
                            nus = NA,
                            flat = 0, 
                            alphas = NA, 
                            dup_upper_bound = dup_upper_bound,
                            dup_count_prior_family = dup_count_prior_family,
                            dup_count_prior_pars = dup_count_prior_pars, 
                            n_prior_family = NA,
                            n_prior_pars = NA)

#### Explore the space of linkages ####
n_iter <- 2000
seed <- 44

results <- gibbs_sampler(comparison_list = reduced_comparison_list, 
                         prior_list = prior_list, 
                         n_iter = n_iter,
                         seed = seed)

#### Find best linkage ####
burn_in <- 1000
max_cc_size <- 50

full_estimate <- find_bayes_estimate(partitions = results$partitions, 
                                     burn_in = 1000,
                                     max_cc_size = max_cc_size)

full_estimate_relabel <- 
  relabel_bayes_estimate(reduced_comparison_list = reduced_comparison_list,
                         bayes_estimate = full_estimate)

records <- cbind(records,
                 full_estimate_id = full_estimate_relabel$link_id)
head(records)
head(records %>% arrange(full_estimate_id))
