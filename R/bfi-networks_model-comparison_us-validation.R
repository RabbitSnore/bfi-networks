################################################################################

# Big Five Network Analysis - Network Modeling vs. Factor Modeling

# Zhang et al (2022), https://doi.org/10.1177/10731911211008245

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

bfi_us <- read_csv("data/chinese-adaptation/bfi_us-validation_clean.csv")

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
e =~ BFI1 + BFI6  + BFI11 + BFI16 + BFI21 + BFI26 + BFI31 + BFI36 + BFI41 + BFI46 + BFI51 + BFI56
a =~ BFI2 + BFI7  + BFI12 + BFI17 + BFI22 + BFI27 + BFI32 + BFI37 + BFI42 + BFI47 + BFI52 + BFI57
c =~ BFI3 + BFI8  + BFI13 + BFI18 + BFI23 + BFI28 + BFI33 + BFI38 + BFI43 + BFI48 + BFI53 + BFI58
n =~ BFI4 + BFI9  + BFI14 + BFI19 + BFI24 + BFI29 + BFI34 + BFI39 + BFI44 + BFI49 + BFI54 + BFI59
o =~ BFI5 + BFI10 + BFI15 + BFI20 + BFI25 + BFI30 + BFI35 + BFI40 + BFI45 + BFI50 + BFI55 + BFI60

'

bifactor <- 
  '

e =~ BFI1 + BFI6  + BFI11 + BFI16 + BFI21 + BFI26 + BFI31 + BFI36 + BFI41 + BFI46 + BFI51 + BFI56
a =~ BFI2 + BFI7  + BFI12 + BFI17 + BFI22 + BFI27 + BFI32 + BFI37 + BFI42 + BFI47 + BFI52 + BFI57
c =~ BFI3 + BFI8  + BFI13 + BFI18 + BFI23 + BFI28 + BFI33 + BFI38 + BFI43 + BFI48 + BFI53 + BFI58
n =~ BFI4 + BFI9  + BFI14 + BFI19 + BFI24 + BFI29 + BFI34 + BFI39 + BFI44 + BFI49 + BFI54 + BFI59
o =~ BFI5 + BFI10 + BFI15 + BFI20 + BFI25 + BFI30 + BFI35 + BFI40 + BFI45 + BFI50 + BFI55 + BFI60

e1 =~ BFI1  + BFI16 + BFI31 + BFI46
e2 =~ BFI6  + BFI21 + BFI36 + BFI51
e3 =~ BFI11 + BFI26 + BFI41 + BFI56

a1 =~ BFI2  + BFI17 + BFI32 + BFI47
a2 =~ BFI7  + BFI22 + BFI37 + BFI52
a3 =~ BFI12 + BFI27 + BFI42 + BFI57

c1 =~ BFI3  + BFI18 + BFI33 + BFI48
c2 =~ BFI8  + BFI23 + BFI38 + BFI53
c3 =~ BFI13 + BFI28 + BFI43 + BFI58

n1  =~ BFI4  + BFI19 + BFI34 + BFI49
n2  =~ BFI9  + BFI24 + BFI39 + BFI54
n3  =~ BFI14 + BFI29 + BFI44 + BFI59

o1  =~ BFI5  + BFI20 + BFI35 + BFI50
o2  =~ BFI10 + BFI25 + BFI40 + BFI55
o3  =~ BFI15 + BFI30 + BFI45 + BFI60

e1 ~~ 0*e1
e2 ~~ 0*e2
e3 ~~ 0*e3
a1 ~~ 0*a1
a2 ~~ 0*a2
a3 ~~ 0*a3
c1 ~~ 0*c1
c2 ~~ 0*c2
c3 ~~ 0*c3
n1 ~~ 0*n1
n2 ~~ 0*n2
n3 ~~ 0*n3
o1 ~~ 0*o1
o2 ~~ 0*o2
o3 ~~ 0*o3

e1 ~~ 0*e
e2 ~~ 0*e
e3 ~~ 0*e
a1 ~~ 0*e
a2 ~~ 0*e
a3 ~~ 0*e
c1 ~~ 0*e
c2 ~~ 0*e
c3 ~~ 0*e
n1 ~~ 0*e
n2 ~~ 0*e
n3 ~~ 0*e
o1 ~~ 0*e
o2 ~~ 0*e
o3 ~~ 0*e

e1 ~~ 0*a
e2 ~~ 0*a
e3 ~~ 0*a
a1 ~~ 0*a
a2 ~~ 0*a
a3 ~~ 0*a
c1 ~~ 0*a
c2 ~~ 0*a
c3 ~~ 0*a
n1 ~~ 0*a
n2 ~~ 0*a
n3 ~~ 0*a
o1 ~~ 0*a
o2 ~~ 0*a
o3 ~~ 0*a

e1 ~~ 0*c
e2 ~~ 0*c
e3 ~~ 0*c
a1 ~~ 0*c
a2 ~~ 0*c
a3 ~~ 0*c
c1 ~~ 0*c
c2 ~~ 0*c
c3 ~~ 0*c
n1 ~~ 0*c
n2 ~~ 0*c
n3 ~~ 0*c
o1 ~~ 0*c
o2 ~~ 0*c
o3 ~~ 0*c

e1 ~~ 0*n
e2 ~~ 0*n
e3 ~~ 0*n
a1 ~~ 0*n
a2 ~~ 0*n
a3 ~~ 0*n
c1 ~~ 0*n
c2 ~~ 0*n
c3 ~~ 0*n
n1 ~~ 0*n
n2 ~~ 0*n
n3 ~~ 0*n
o1 ~~ 0*n
o2 ~~ 0*n
o3 ~~ 0*n

e1 ~~ 0*o
e2 ~~ 0*o
e3 ~~ 0*o
a1 ~~ 0*o
a2 ~~ 0*o
a3 ~~ 0*o
c1 ~~ 0*o
c2 ~~ 0*o
c3 ~~ 0*o
n1 ~~ 0*o
n2 ~~ 0*o
n3 ~~ 0*o
o1 ~~ 0*o
o2 ~~ 0*o
o3 ~~ 0*o

'

higher_order <- 
  '
e =~ e1 + e2 + e3
a =~ a1 + a2 + a3 
c =~ c1 + c2 + c3
n =~ n1 + n2 + n3
o =~ o1 + o2 + o3

e1 =~ BFI1  + BFI16 + BFI31 + BFI46
e2 =~ BFI6  + BFI21 + BFI36 + BFI51
e3 =~ BFI11 + BFI26 + BFI41 + BFI56

a1 =~ BFI2  + BFI17 + BFI32 + BFI47
a2 =~ BFI7  + BFI22 + BFI37 + BFI52
a3 =~ BFI12 + BFI27 + BFI42 + BFI57

c1 =~ BFI3  + BFI18 + BFI33 + BFI48
c2 =~ BFI8  + BFI23 + BFI38 + BFI53
c3 =~ BFI13 + BFI28 + BFI43 + BFI58

n1  =~ BFI4  + BFI19 + BFI34 + BFI49
n2  =~ BFI9  + BFI24 + BFI39 + BFI54
n3  =~ BFI14 + BFI29 + BFI44 + BFI59

o1  =~ BFI5  + BFI20 + BFI35 + BFI50
o2  =~ BFI10 + BFI25 + BFI40 + BFI55
o3  =~ BFI15 + BFI30 + BFI45 + BFI60

'

acquiescence <- 
  '

e =~ 1*BFI1 + 1*BFI6  + 1*BFI11 + 1*BFI16 + 1*BFI21 + 1*BFI26 + 1*BFI31 + 1*BFI36 + 1*BFI41 + 1*BFI46 + 1*BFI51 + 1*BFI56
a =~ 1*BFI2 + 1*BFI7  + 1*BFI12 + 1*BFI17 + 1*BFI22 + 1*BFI27 + 1*BFI32 + 1*BFI37 + 1*BFI42 + 1*BFI47 + 1*BFI52 + 1*BFI57
c =~ 1*BFI3 + 1*BFI8  + 1*BFI13 + 1*BFI18 + 1*BFI23 + 1*BFI28 + 1*BFI33 + 1*BFI38 + 1*BFI43 + 1*BFI48 + 1*BFI53 + 1*BFI58
n =~ 1*BFI4 + 1*BFI9  + 1*BFI14 + 1*BFI19 + 1*BFI24 + 1*BFI29 + 1*BFI34 + 1*BFI39 + 1*BFI44 + 1*BFI49 + 1*BFI54 + 1*BFI59
o =~ 1*BFI5 + 1*BFI10 + 1*BFI15 + 1*BFI20 + 1*BFI25 + 1*BFI30 + 1*BFI35 + 1*BFI40 + 1*BFI45 + 1*BFI50 + 1*BFI55 + 1*BFI60

e1 =~ BFI1  + BFI16 + BFI31 + BFI46
e2 =~ BFI6  + BFI21 + BFI36 + BFI51
e3 =~ BFI11 + BFI26 + BFI41 + BFI56

a1 =~ BFI2  + BFI17 + BFI32 + BFI47
a2 =~ BFI7  + BFI22 + BFI37 + BFI52
a3 =~ BFI12 + BFI27 + BFI42 + BFI57

c1 =~ BFI3  + BFI18 + BFI33 + BFI48
c2 =~ BFI8  + BFI23 + BFI38 + BFI53
c3 =~ BFI13 + BFI28 + BFI43 + BFI58

n1  =~ BFI4  + BFI19 + BFI34 + BFI49
n2  =~ BFI9  + BFI24 + BFI39 + BFI54
n3  =~ BFI14 + BFI29 + BFI44 + BFI59

o1  =~ BFI5  + BFI20 + BFI35 + BFI50
o2  =~ BFI10 + BFI25 + BFI40 + BFI55
o3  =~ BFI15 + BFI30 + BFI45 + BFI60

e1 ~~ 0*e
e2 ~~ 0*e
e3 ~~ 0*e
a1 ~~ 0*e
a2 ~~ 0*e
a3 ~~ 0*e
c1 ~~ 0*e
c2 ~~ 0*e
c3 ~~ 0*e
n1 ~~ 0*e
n2 ~~ 0*e
n3 ~~ 0*e
o1 ~~ 0*e
o2 ~~ 0*e
o3 ~~ 0*e

e1 ~~ 0*a
e2 ~~ 0*a
e3 ~~ 0*a
a1 ~~ 0*a
a2 ~~ 0*a
a3 ~~ 0*a
c1 ~~ 0*a
c2 ~~ 0*a
c3 ~~ 0*a
n1 ~~ 0*a
n2 ~~ 0*a
n3 ~~ 0*a
o1 ~~ 0*a
o2 ~~ 0*a
o3 ~~ 0*a

e1 ~~ 0*c
e2 ~~ 0*c
e3 ~~ 0*c
a1 ~~ 0*c
a2 ~~ 0*c
a3 ~~ 0*c
c1 ~~ 0*c
c2 ~~ 0*c
c3 ~~ 0*c
n1 ~~ 0*c
n2 ~~ 0*c
n3 ~~ 0*c
o1 ~~ 0*c
o2 ~~ 0*c
o3 ~~ 0*c

e1 ~~ 0*n
e2 ~~ 0*n
e3 ~~ 0*n
a1 ~~ 0*n
a2 ~~ 0*n
a3 ~~ 0*n
c1 ~~ 0*n
c2 ~~ 0*n
c3 ~~ 0*n
n1 ~~ 0*n
n2 ~~ 0*n
n3 ~~ 0*n
o1 ~~ 0*n
o2 ~~ 0*n
o3 ~~ 0*n

e1 ~~ 0*o
e2 ~~ 0*o
e3 ~~ 0*o
a1 ~~ 0*o
a2 ~~ 0*o
a3 ~~ 0*o
c1 ~~ 0*o
c2 ~~ 0*o
c3 ~~ 0*o
n1 ~~ 0*o
n2 ~~ 0*o
n3 ~~ 0*o
o1 ~~ 0*o
o2 ~~ 0*o
o3 ~~ 0*o

'

# Data preparation -------------------------------------------------------------

model_data <- bfi_us %>% 
  filter(complete.cases(.))

# Modeling ---------------------------------------------------------------------

# Create data table

bfi_fit_data <- data.frame(
  model = c("big_five", "bifactor", "higher_order", "acquiescence", "network"),
  cfi   = rep(NA, 5),
  tli   = rep(NA, 5),
  rmsea = rep(NA, 5),
  bic   = rep(NA, 5)
)

set.seed(3435)

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

test_bifactor <- cfa(bifactor,
                     data = test_data,
                     estimator = "ML")

test_higher   <- cfa(higher_order,
                     data = test_data,
                     estimator  = "ML",
                     orthogonal.y = TRUE)

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

### Bifactor

if (test_bifactor@optim$converged == TRUE) {
  
  test_bifactor_fit <- fitmeasures(test_bifactor, 
                                   fit.measures = c("cfi", "tli", "rmsea", "bic"))
  
} else {
  
  test_bifactor_fit <- c("cfi" = NA, "tli" = NA, "rmsea" = NA, "bic" = NA)
  
}

### Higher Order

if (test_higher@optim$converged == TRUE) {
  
  test_higher_fit <- fitmeasures(test_higher, 
                                 fit.measures = c("cfi", "tli", "rmsea", "bic"))
  
} else {
  
  test_higher_fit <- c("cfi" = NA, "tli" = NA, "rmsea" = NA, "bic" = NA)
  
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
bfi_fit_data$cfi[2]   <- test_bifactor_fit[names(test_bifactor_fit) == "cfi"]
bfi_fit_data$cfi[3]   <- test_higher_fit[names(test_higher_fit) == "cfi"]
bfi_fit_data$cfi[4]   <- test_acq_fit[names(test_acq_fit) == "cfi"]
bfi_fit_data$cfi[5]   <- test_net_fit$Value[test_net_fit$Measure == "cfi"]

bfi_fit_data$tli[1]   <- test_big_five_fit[names(test_big_five_fit) == "tli"]
bfi_fit_data$tli[2]   <- test_bifactor_fit[names(test_bifactor_fit) == "tli"]
bfi_fit_data$tli[3]   <- test_higher_fit[names(test_higher_fit) == "tli"]
bfi_fit_data$tli[4]   <- test_acq_fit[names(test_acq_fit) == "tli"]
bfi_fit_data$tli[5]   <- test_net_fit$Value[test_net_fit$Measure == "tli"]

bfi_fit_data$rmsea[1] <- test_big_five_fit[names(test_big_five_fit) == "rmsea"]
bfi_fit_data$rmsea[2] <- test_bifactor_fit[names(test_bifactor_fit) == "rmsea"]
bfi_fit_data$rmsea[3] <- test_higher_fit[names(test_higher_fit) == "rmsea"]
bfi_fit_data$rmsea[4] <- test_acq_fit[names(test_acq_fit) == "rmsea"]
bfi_fit_data$rmsea[5] <- test_net_fit$Value[test_net_fit$Measure == "rmsea"]

bfi_fit_data$bic[1]   <- test_big_five_fit[names(test_big_five_fit) == "bic"]
bfi_fit_data$bic[2]   <- test_bifactor_fit[names(test_bifactor_fit) == "bic"]
bfi_fit_data$bic[3]   <- test_higher_fit[names(test_higher_fit) == "bic"]
bfi_fit_data$bic[4]   <- test_acq_fit[names(test_acq_fit) == "bic"]
bfi_fit_data$bic[5]   <- test_net_fit$Value[test_net_fit$Measure == "bic"]

# Save goodness-of-fit data

write_rds(bfi_fit_data, 
          file = "output/bfi_model-comparison-data_us-validation.rds")

write_csv(select(bfi_fit_data, -omega), 
          file = "output/bfi_model-comparison-data_us-validation.csv")

