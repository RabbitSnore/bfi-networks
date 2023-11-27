################################################################################

# Big Five Network Analysis - Network Modeling vs. Factor Modeling

# Open Psychometrics

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse", 
              "qgraph", 
              "igraph", 
              "psychonetrics", 
              "lavaan",
              "foreach",
              "doParallel")

lapply(packages, library, character.only = TRUE)

# Load data --------------------------------------------------------------------

# Load precleaned data

op_ipip_ffm <- read_csv("data/openpsychometrics/bfi_open-psychometrics_clean.csv")

# Network models vs. Factor models ---------------------------------------------

# Create output directory, if needed

if (!dir.exists("output")) {
  
  dir.create("output")
  
}

## Five factor models

big_five <- 
'
e =~ EXT1 + EXT2 + EXT3 + EXT4 + EXT5 + EXT6 + EXT7 + EXT8 + EXT9 + EXT10
a =~ AGR1 + AGR2 + AGR3 + AGR4 + AGR5 + AGR6 + AGR7 + AGR8 + AGR9 + AGR10
c =~ CSN1 + CSN2 + CSN3 + CSN4 + CSN5 + CSN6 + CSN7 + CSN8 + CSN9 + CSN10
n =~ EST1 + EST2 + EST3 + EST4 + EST5 + EST6 + EST7 + EST8 + EST9 + EST10
o =~ OPN1 + OPN2 + OPN3 + OPN4 + OPN5 + OPN6 + OPN7 + OPN8 + OPN9 + OPN10

'

acquiescence <- 
'

e =~ EXT1 + EXT2 + EXT3 + EXT4 + EXT5 + EXT6 + EXT7 + EXT8 + EXT9 + EXT10
a =~ AGR1 + AGR2 + AGR3 + AGR4 + AGR5 + AGR6 + AGR7 + AGR8 + AGR9 + AGR10
c =~ CSN1 + CSN2 + CSN3 + CSN4 + CSN5 + CSN6 + CSN7 + CSN8 + CSN9 + CSN10
n =~ EST1 + EST2 + EST3 + EST4 + EST5 + EST6 + EST7 + EST8 + EST9 + EST10
o =~ OPN1 + OPN2 + OPN3 + OPN4 + OPN5 + OPN6 + OPN7 + OPN8 + OPN9 + OPN10

acq =~ 1*EXT1 + 1*EXT2 + 1*EXT3 + 1*EXT4 + 1*EXT5 + 1*EXT6 + 1*EXT7 + 1*EXT8 + 1*EXT9 + 1*EXT10 + 1*EST1 + 1*EST2 + 1*EST3 + 1*EST4 + 1*EST5 + 1*EST6 + 1*EST7 + 1*EST8 + 1*EST9 + 1*EST10 + 1*AGR1 + 1*AGR2 + 1*AGR3 + 1*AGR4 + 1*AGR5 + 1*AGR6 + 1*AGR7 + 1*AGR8 + 1*AGR9 + 1*AGR10 + 1*CSN1 + 1*CSN2 + 1*CSN3 + 1*CSN4 + 1*CSN5 + 1*CSN6 + 1*CSN7 + 1*CSN8 + 1*CSN9 + 1*CSN10 + 1*OPN1 + 1*OPN2 + 1*OPN3 + 1*OPN4 + 1*OPN5 + 1*OPN6 + 1*OPN7 + 1*OPN8 + 1*OPN9 + 1*OPN10

acq ~~ 0*e
acq ~~ 0*a
acq ~~ 0*c
acq ~~ 0*n
acq ~~ 0*o

'

# Data preparation -------------------------------------------------------------

# Procedural set up

train_test_ratio <- c(.50, .50)

countries <- table(op_ipip_ffm$country)

set.seed(1991)

seed_list <- round(runif(length(countries), 1000, 10000))

# Set up parallel computing

# IMPORTANT! Modify this for your system. Do not assume this default will work
# if you are reproducing the analyses. Running this code and not running a
# parallelized process will not be harmful, but you could have a suboptimal
# experience using this code without modifications tailored for your computing
# environment.

cores <- detectCores()

registerDoParallel(cl = (cores - 1)/2, cores = cores - 2)

# Modeling ---------------------------------------------------------------------

if (!file.exists("output/bfi_model-comparison-data_open-psychometrics.rds")) {
  
  comparison_data <- foreach(i = 1:length(countries), 
                             .packages = packages, 
                             .combine = bind_rows) %dopar% {
    
    country_current <- names(countries[i])
    
    # Select relevant cases
    
    model_data <- op_ipip_ffm %>% 
      filter(country == country_current) %>%
      select(-country) %>% 
      filter(complete.cases(.))
    
    # Split into training and test sets
    
    set.seed(seed_list[i])
    
    training_indices <- sample(1:countries[i], 
                               size = round(countries[i]*train_test_ratio[1]), 
                               replace = FALSE)
    
    training_data    <- model_data[ training_indices, ] 
    test_data        <- model_data[-training_indices, ] 
    
    n_train <- nrow(training_data)
    n_test  <- nrow(test_data)
    
    # Fit training model
    
    training_network <- EBICglasso(cov(training_data),
                                   n = nrow(training_data),
                                   nlambda = 1000,
                                   lambda.min.ratio = 0.01,
                                   returnAllResults = TRUE)
    
    ## Repeat training fit if sparsity may be violated
    
    lambda_index <- which(training_network$ebic == min(training_network$ebic))
    lambda_opt   <- training_network$lambda[lambda_index]
    
    if (lambda_opt == min(training_network$lambda)) {
      
      training_network <- EBICglasso(cov(training_data),
                                     n = nrow(training_data),
                                     nlambda = 10000, # Increase tested lambdas
                                     lambda.min.ratio = 0.1, # Increase ratio
                                     returnAllResults = TRUE)
      
      lambda_index <- which(training_network$ebic == min(training_network$ebic))
      lambda_opt   <- training_network$lambda[lambda_index]
      
      if (lambda_opt == min(training_network$lambda)) {
        
        training_network <- EBICglasso(cov(training_data),
                                       n = nrow(training_data),
                                       nlambda = 100000, # Increase tested lambdas
                                       threshold = TRUE, # Enforce threshold
                                       lambda.min.ratio = 0.1,
                                       returnAllResults = TRUE)
        
      }
      
    }
    
    ## Extract training model skeleton
    
    omega_skeleton <- training_network$optnet
    
    omega_skeleton[omega_skeleton != 0] <- 1
    
    # Fit test model
    
    test_network <- 
      varcov(data  = test_data,
             type  = "ggm",
             omega = omega_skeleton) %>% 
      runmodel()
    
    ## Calculate fit indices
    
    test_net_fit <- fit(test_network)
    
    # Fit five-factor models
    
    test_big_five     <- cfa(big_five,
                             data = test_data,
                             estimator = "ML")
    
    test_acquiescence <- cfa(acquiescence,
                             data = test_data,
                             estimator = "ML")
    
    ## Calculate fit indices
    
    ### Big Five
    
    if (test_big_five@optim$converged == TRUE) {
      
      test_big_five_fit <- fitmeasures(test_big_five, 
                                       fit.measures = c("cfi", "tli", "rmsea", "bic"))
      
    } else {
      
      test_big_five_fit <- c("cfi" = NA, "tli" = NA, "rmsea" = NA, "bic" = NA)
      
    }
    
    ### Acquiescence
    
    if (test_acquiescence@optim$converged == TRUE) {
      
      test_acq_fit    <- fitmeasures(test_acquiescence, 
                                     fit.measures = c("cfi", "tli", "rmsea", "bic"))
      
    } else {
      
      test_acq_fit <- c("cfi" = NA, "tli" = NA, "rmsea" = NA, "bic" = NA)
      
    }
    
    # Record fit indices
    
    cfi_network        <- test_net_fit$Value[test_net_fit$Measure == "cfi"]
    cfi_big_five       <- test_big_five_fit[names(test_big_five_fit) == "cfi"]
    cfi_acquiescence   <- test_acq_fit[names(test_acq_fit) == "cfi"]
    
    tli_network        <- test_net_fit$Value[test_net_fit$Measure == "tli"]
    tli_big_five       <- test_big_five_fit[names(test_big_five_fit) == "tli"]
    tli_acquiescence   <- test_acq_fit[names(test_acq_fit) == "tli"]
    
    rmsea_network      <- test_net_fit$Value[test_net_fit$Measure == "rmsea"]
    rmsea_big_five     <- test_big_five_fit[names(test_big_five_fit) == "rmsea"]
    rmsea_acquiescence <- test_acq_fit[names(test_acq_fit) == "rmsea"]
    
    bic_network        <- test_net_fit$Value[test_net_fit$Measure == "bic"]
    bic_big_five       <- test_big_five_fit[names(test_big_five_fit) == "bic"]
    bic_acquiescence   <- test_acq_fit[names(test_acq_fit) == "bic"]
    
    # Model fit data
    
    fit_data <- data.frame(
      country = country_current,
      n_train,
      n_test,
      cfi_network,       
      cfi_big_five,      
      cfi_acquiescence,  
      tli_network,       
      tli_big_five,      
      tli_acquiescence,  
      rmsea_network,     
      rmsea_big_five,    
      rmsea_acquiescence,
      bic_network,       
      bic_big_five,      
      bic_acquiescence  
    )
    
    fit_data$omega <- list(omega_skeleton)
    
    fit_data
    
  }
  
  write_rds(comparison_data, 
            "output/bfi_model-comparison-data_open-psychometrics.rds")
  
  write_csv(select(comparison_data, -omega), 
            "output/bfi_model-comparison-data_open-psychometrics.csv")
  
} else {
  
  comparison_data <- read_rds("output/bfi_model-comparison-data_open-psychometrics.rds")
  
}

# Extract and save training and test data --------------------------------------

if (!dir.exists("data/openpsychometrics/training/")) {
  
  dir.create("data/openpsychometrics/training/")
  
}

if (!dir.exists("data/openpsychometrics/test/")) {
  
  dir.create("data/openpsychometrics/test/")
  
}

for (i in 1:length(countries)) {
  
  country_current <- names(countries[i])
  
  # Select relevant cases
  
  model_data <- op_ipip_ffm %>% 
    filter(country == country_current) %>%
    select(-country) %>% 
    filter(complete.cases(.))
  
  # Split into training and test sets
  
  set.seed(seed_list[i])
  
  training_indices <- sample(1:countries[i], 
                             size = round(countries[i]*train_test_ratio[1]), 
                             replace = FALSE)
  
  training_data    <- model_data[ training_indices, ] 
  test_data        <- model_data[-training_indices, ]
  
  write_csv(training_data,
            paste("data/openpsychometrics/training/bfi_training_op_", 
                  tolower(country_current), 
                  ".csv", 
                  sep = "")
  )
  
  write_csv(test_data,
            paste("data/openpsychometrics/test/bfi_test_op_", 
                  tolower(country_current), 
                  ".csv", 
                  sep = "")
  )
  
}
