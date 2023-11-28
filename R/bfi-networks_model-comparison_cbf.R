################################################################################

# Big Five Network Analysis - Network Modeling vs. Factor Modeling

# Zhang et al (2019), https://doi.org/10.1371/journal.pone.0221621

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse", 
              "qgraph", 
              "igraph", 
              "psychonetrics", 
              "lavaan")

lapply(packages, library, character.only = TRUE)

# Load data --------------------------------------------------------------------

# Load precleaned data

cbf <- read_csv("data/cbf/bfi_cbf_clean.csv")

# Network models vs. Factor models ---------------------------------------------

# Procedural set up

train_test_ratio <- c(.50, .50)

# Create output directory, if needed

if (!dir.exists("output")) {
  
  dir.create("output")
  
}

## Five factor models

big_five <- 
  '
f1 =~ item1  + item2  + item3  + item4  + item5  + item6  + item7  + item8
f2 =~ item9  + item10 + item11 + item12 + item13 + item14 + item15 + item16
f3 =~ item17 + item18 + item19 + item20 + item21 + item22 + item23 + item24
f4 =~ item25 + item26 + item27 + item28 + item29 + item30 + item31 + item32
f5 =~ item33 + item34 + item35 + item36 + item37 + item38 + item39 + item40

'

acquiescence <- 
'

f1 =~ item1  + item2  + item3  + item4  + item5  + item6  + item7  + item8
f2 =~ item9  + item10 + item11 + item12 + item13 + item14 + item15 + item16
f3 =~ item17 + item18 + item19 + item20 + item21 + item22 + item23 + item24
f4 =~ item25 + item26 + item27 + item28 + item29 + item30 + item31 + item32
f5 =~ item33 + item34 + item35 + item36 + item37 + item38 + item39 + item40

acq =~ item1 + item2 + item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10 + item11 + item12 + item13 + item14 + item15 + item16 + item17 + item18 + item19 + item20 + item21 + item22 + item23 + item24 + item25 + item26 + item27 + item28 + item29 + item30 + item31 + item32 + item33 + item34 + item35 + item36 + item37 + item38 + item39 + item40

f1 ~~ 0*acq
f2 ~~ 0*acq
f3 ~~ 0*acq
f4 ~~ 0*acq
f5 ~~ 0*acq

'


# Data preparation -------------------------------------------------------------

model_data <- cbf %>% 
  filter(complete.cases(.))

# Modeling ---------------------------------------------------------------------

# Create data table

bfi_fit_data <- data.frame(
  model = c("big_five", "acquiescence", "network"),
  cfi   = rep(NA, 3),
  tli   = rep(NA, 3),
  rmsea = rep(NA, 3),
  bic   = rep(NA, 3)
)

set.seed(1975)

training_indices <- sample(1:nrow(model_data), 
                           size = round(nrow(model_data)*train_test_ratio[1]), 
                           replace = FALSE)

training_data    <- model_data[ training_indices, ] 
test_data        <- model_data[-training_indices, ] 

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

bfi_fit_data$omega <- list(omega_skeleton)

# Fit test model

test_network <- 
  varcov(data  = test_data,
         type  = "ggm",
         omega = omega_skeleton) %>% 
  runmodel()

## Calculate fit indices

test_net_fit <- fit(test_network)

# Fit five-factor models

test_big_five <- cfa(big_five,
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


bfi_fit_data$cfi[1]   <- test_big_five_fit[names(test_big_five_fit) == "cfi"]
bfi_fit_data$cfi[2]   <- test_acq_fit[names(test_acq_fit) == "cfi"]
bfi_fit_data$cfi[3]   <- test_net_fit$Value[test_net_fit$Measure == "cfi"]

bfi_fit_data$tli[1]   <- test_big_five_fit[names(test_big_five_fit) == "tli"]
bfi_fit_data$tli[2]   <- test_acq_fit[names(test_acq_fit) == "tli"]
bfi_fit_data$tli[3]   <- test_net_fit$Value[test_net_fit$Measure == "tli"]

bfi_fit_data$rmsea[1] <- test_big_five_fit[names(test_big_five_fit) == "rmsea"]
bfi_fit_data$rmsea[2] <- test_acq_fit[names(test_acq_fit) == "rmsea"]
bfi_fit_data$rmsea[3] <- test_net_fit$Value[test_net_fit$Measure == "rmsea"]

bfi_fit_data$bic[1]   <- test_big_five_fit[names(test_big_five_fit) == "bic"]
bfi_fit_data$bic[2]   <- test_acq_fit[names(test_acq_fit) == "bic"]
bfi_fit_data$bic[3]   <- test_net_fit$Value[test_net_fit$Measure == "bic"]

# Save goodness-of-fit data

write_rds(bfi_fit_data, 
          file = "output/bfi_model-comparison-data_cbf.rds")

write_csv(select(bfi_fit_data, -omega), 
          file = "output/bfi_model-comparison-data_cbf.csv")

