################################################################################

# Big Five Network Analysis - Cross-Country Network Fitting (Test Data)

# Open Psychometrics

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse", 
              "psychonetrics", 
              "foreach",
              "doParallel",
              "readxl",
              "ggbeeswarm",
              "cowplot",
              "qgraph")

lapply(packages, library, character.only = TRUE)

# Load data --------------------------------------------------------------------

# Load precleaned data

op_ipip_ffm <- read_csv("data/openpsychometrics/bfi_open-psychometrics_clean.csv")

# Load comparison data

comparison_data <- 
  read_rds("output/bfi_model-comparison-data_open-psychometrics.rds")

# Procedural set up ------------------------------------------------------------

# Countries and pairs for comparison

countries <- unique(op_ipip_ffm$country)

country_pairs_1 <- combn(countries, 2) %>%
  t() %>% 
  as.data.frame()

colnames(country_pairs_1) <- c("country_1", "country_2")

country_pairs_2 <- country_pairs_1 %>% 
  relocate(country_2, .before = country_1) %>% 
  rename(
    country_1 = country_2,
    country_2 = country_1
  )

country_pairs <- bind_rows(country_pairs_1, country_pairs_2)

# Parallel computing set up

# IMPORTANT! Modify this for your system. Do not assume this default will work
# if you are reproducing the analyses. Running this code and not running a
# parallelized process will not be harmful, but you could have a suboptimal
# experience using this code without modifications tailored for your computing
# environment.

cores <- detectCores()

registerDoParallel(cl = (cores - 1)/2, cores = cores - 2)

# Cross-country confirmatory network modeling ----------------------------------

# This loop will fit a confirmatory network (i.e., the same network used for the
# network vs. factor comparisons) from each country to each other country,
# thereby testing how well each country's specific network model performs in the
# data for each other country. This process is computationally intensive and is
# set up for parallel computation using all available cores.

if (!file.exists("output/bfi_cross-country-test-data_open-psychometrics.rds")) {
  
  cross_country_data <- foreach(i = 1:nrow(country_pairs), 
                                .packages = packages,
                                .combine  = bind_rows) %dopar% {
                                  
    # Subset data
    
    country_1 <- country_pairs$country_1[i]
    country_2 <- country_pairs$country_2[i]
    
    ipip_subset <- read_csv(paste("data/openpsychometrics/test/bfi_test_op_", 
                                  tolower(country_2), 
                                  ".csv", 
                                  sep = ""))
    
    # Set up empty data
    
    fit_data <- data.frame(
      country_1     = country_1,
      country_2     = country_2,
      cfi_network   = NA,   
      tli_network   = NA,   
      rmsea_network = NA, 
      bic_network   = NA
    )
    
    # Retrieve omega matrix skeleton for Country 1
    
    omega_skeleton <- comparison_data$omega[comparison_data$country == country_pairs$country_1[i]]
    
    # Fit network from Country 1 to data from Country 2
    
    cross_country_network <- 
      varcov(data  = ipip_subset,
             type  = "ggm",
             omega = omega_skeleton[[1]]) %>% 
      runmodel()
    
    # Fit indices
    
    test_net_fit <- fit(cross_country_network)
    
    ## Extract indices
    
    fit_data$cfi_network    <- test_net_fit$Value[test_net_fit$Measure == "cfi"]
    fit_data$tli_network    <- test_net_fit$Value[test_net_fit$Measure == "tli"]
    fit_data$rmsea_network  <- test_net_fit$Value[test_net_fit$Measure == "rmsea"]
    fit_data$bic_network    <- test_net_fit$Value[test_net_fit$Measure == "bic"]
    
    # Store networks
    
    fit_data$omega_matrix <- list(getmatrix(cross_country_network,
                                            matrix = "omega"))
    
    fit_data
    
  }
  
  # Store data
  
  write_rds(cross_country_data, 
            "output/bfi_cross-country-test-data_open-psychometrics.rds")
  
  ## Store simplified data
  
  write_csv(cross_country_data %>% 
              select(-omega_matrix), 
            file = "output/bfi_cross-country-test-data_open-psychometrics.csv")
  
} else {
  
  cross_country_data <- 
    read_rds("output/bfi_cross-country-test-data_open-psychometrics.rds")
  
}

# Wrangle ----------------------------------------------------------------------

# Combine cross-country fit measures with within-country fit measures

ipip_network_fit <- ipip_comparison %>% 
  select(
    country_1 = country,
    country_2 = country,
    ends_with("network")
  )

cross_country_fit <- bind_rows(
  cross_country_data %>% 
    select(-omega_matrix), 
  ipip_network_fit
) %>% 
  arrange(by = country_2) %>% 
  arrange(by = country_1)

# Fit measure matrices

## Create matrices

matrix_cfi   <- matrix(cross_country_fit$cfi_network, 
                       nrow = 27,
                       dimnames = list(unique(cross_country_fit$country_1),
                                       unique(cross_country_fit$country_1)))

matrix_tli   <- matrix(cross_country_fit$tli_network, 
                       nrow = 27,
                       dimnames = list(unique(cross_country_fit$country_1),
                                       unique(cross_country_fit$country_1)))

matrix_rmsea <- matrix(cross_country_fit$rmsea_network, 
                       nrow = 27,
                       dimnames = list(unique(cross_country_fit$country_1),
                                       unique(cross_country_fit$country_1)))

matrix_bic   <- matrix(cross_country_fit$bic_network, 
                       nrow = 27,
                       dimnames = list(unique(cross_country_fit$country_1),
                                       unique(cross_country_fit$country_1)))

## Store matrices

matrix_cfi   <- as.data.frame(matrix_cfi) 
matrix_tli   <- as.data.frame(matrix_tli)  
matrix_rmsea <- as.data.frame(matrix_rmsea) 
matrix_bic   <- as.data.frame(matrix_bic) 

### Full

write.csv(matrix_cfi,   "output/ipip-neo_matrix-cfi-test.csv")
write.csv(matrix_tli,   "output/ipip-neo_matrix-tli-test.csv")
write.csv(matrix_rmsea, "output/ipip-neo_matrix-rmsea-test.csv")
write.csv(matrix_bic,   "output/ipip-neo_matrix-bic-test.csv")

### Readable (rounded to three digits)

write.csv(round(matrix_cfi, 3),   "output/ipip-neo_matrix-cfi-test-rounded.csv")
write.csv(round(matrix_tli, 3),   "output/ipip-neo_matrix-tli-test-rounded.csv")
write.csv(round(matrix_rmsea, 3), "output/ipip-neo_matrix-rmsea-test-rounded.csv")
