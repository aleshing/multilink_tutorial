library(multilink)

#### Clean data and create stacked data.frame called records ####

# To be edited
# records <- 

#### Create comparison data ####

# To be edited
# types <- c()
# breaks <- list()
# file_sizes <- c()
# duplicates <- c()

comparison_list <- create_comparison_data(records = records,
                                          types = types,
                                          breaks = breaks,
                                          file_sizes = file_sizes,
                                          duplicates = duplicates)

#### Reduce comparison data ####

# To be edited
# pairs_to_keep <- 
# cc <- 

reduced_comparison_list <- 
  reduce_comparison_data(comparison_list = comparison_list,
                         pairs_to_keep = pairs_to_keep, 
                         cc = cc)

#### Specify prior ###

# To be edited
# dup_upper_bound <- c()

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

#### Run Gibbs sampler ####

n_iter <- 2000
seed <- 44

results <- gibbs_sampler(comparison_list = reduced_comparison_list, 
                         prior_list = prior_list, 
                         n_iter = n_iter,
                         seed = seed)

plot(1:n_iter, colSums(results$contingency_tables), type = "l",
     ylab = "Number of clusters", xlab = "Iteration")

#### Find point estimate ####

burn_in <- 1000
L_A <- Inf
max_cc_size <- 50

full_estimate <- find_bayes_estimate(paritions = results$partitions, 
                                     burn_in = burn_in,
                                     L_A = L_A, 
                                     max_cc_size = max_cc_size)

full_estimate_relabel <- 
  relabel_bayes_estimate(reduced_comparison_list = reduced_comparison_list,
                         bayes_estimate = full_estimate)

records <- cbind(records,
                 full_estimate_id = full_estimate_relabel$link_id)