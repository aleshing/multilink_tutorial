library(multilink)
library(tidyverse)

load("mixed_data_messy.RData")
names(mixed_data_messy)

head(mixed_data_messy$mixed_data_file1)
head(mixed_data_messy$mixed_data_file2)
head(mixed_data_messy$mixed_data_file3)

mixed_data_messy$mixed_data_file3 <- 
  mixed_data_messy$mixed_data_file3 %>%
  mutate(sex = ifelse(sex == "unknown", NA, sex))

mixed_data_messy$mixed_data_file1 <- 
  mixed_data_messy$mixed_data_file1 %>%
  mutate(age = NA)

mixed_data_messy$mixed_data_file2 <- 
  mixed_data_messy$mixed_data_file2 %>%
  rename(gname = given_name)

mixed_data_messy$mixed_data_file1 <- 
  mixed_data_messy$mixed_data_file1 %>%
  select(sex, gname, fname, phone, postcode, age, occup)

mixed_data_messy$mixed_data_file2 <- 
  mixed_data_messy$mixed_data_file2 %>%
  select(sex, gname, fname, phone, postcode, age, occup)

records <- rbind(mixed_data_messy$mixed_data_file1,
                 mixed_data_messy$mixed_data_file2,
                 mixed_data_messy$mixed_data_file3)

types <- c("bi", "lv", "lv", "lv", "lv", "bi", "bi")
breaks = list(sex = NA,  
              gname = c(0, 0.25, 0.5),  
              fname = c(0, 0.25, 0.5),
              phone = c(0, 0.25, 0.5), 
              postcode = c(0, 0.25, 0.5),  
              age = NA, 
              occup = NA)
file_sizes <- c(nrow(mixed_data_messy$mixed_data_file1),
                nrow(mixed_data_messy$mixed_data_file2),
                nrow(mixed_data_messy$mixed_data_file3))
duplicates <- c(0, 1, 1)

comparison_list <- create_comparison_data(records = records,
                                          types = types,
                                          breaks = breaks,
                                          file_sizes = file_sizes,
                                          duplicates = duplicates)

pairs_to_keep <- (comparison_list$comparisons[, "gname_DL_3"] != TRUE) &
  (comparison_list$comparisons[, "fname_DL_3"] != TRUE)
cc <- 1

reduced_comparison_list <- 
  reduce_comparison_data(comparison_list = comparison_list,
                         pairs_to_keep = pairs_to_keep, 
                         cc = cc)

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

n_iter <- 2000
seed <- 44

results <- gibbs_sampler(comparison_list = reduced_comparison_list, 
                         prior_list = prior_list, 
                         n_iter = n_iter,
                         seed = seed)

plot(1:n_iter, colSums(results$contingency_tables), type = "l",
     ylab = "Number of clusters", xlab = "Iteration")

burn_in <- 1000
L_A <- 0.1
max_cc_size <- 12
partial_estimate <- find_bayes_estimate(paritions = results$partitions, 
                                        burn_in = 1000,
                                        L_A = L_A, 
                                        max_cc_size = max_cc_size)

L_A <- Inf
max_cc_size <- 50
full_estimate <- find_bayes_estimate(paritions = results$partitions, 
                                     burn_in = 1000,
                                     L_A = L_A, 
                                     max_cc_size = max_cc_size)

full_estimate_relabel <- 
  relabel_bayes_estimate(reduced_comparison_list = reduced_comparison_list,
                         bayes_estimate = full_estimate)
partial_estimate_relabel <- 
  relabel_bayes_estimate(reduced_comparison_list = reduced_comparison_list,
                         bayes_estimate = partial_estimate)

records <- cbind(records,
                 full_estimate_id = full_estimate_relabel$link_id,
                 partial_estimate_id = partial_estimate_relabel$link_id)