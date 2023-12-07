################################################################################

# Big Five Network Analysis - Five Factor and Unconstrained ESEM Approach

# Gallardo-Pujol et al (2022), https://doi.org/10.1027/2698-1866/a000020

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse", 
              "qgraph", 
              "igraph", 
              "psychonetrics", 
              "lavaan",
              "foreach",
              "doParallel",
              "psych",
              "esem")

lapply(packages, library, character.only = TRUE)

# Load data --------------------------------------------------------------------

# Load precleaned data

bfi_spanish <- read_csv("data/spanish-adaptation/bfi_spanish-adaptation_clean.csv")

# Procedural set up ------------------------------------------------------------

train_test_ratio <- c(.50, .50)

# Create output directory, if needed

if (!dir.exists("output")) {
  
  dir.create("output")
  
}

if (!dir.exists("output/ffm-esem/")) {
  
  dir.create("output/ffm-esem/")
  
}

# Five factor model target loadings

big_five_target_loadings <- list(
  e = paste("BFI", seq(1, 60, 5), sep = ""),
  a = paste("BFI", seq(2, 60, 5), sep = ""),
  c = paste("BFI", seq(3, 60, 5), sep = ""),
  n = paste("BFI", seq(4, 60, 5), sep = ""),
  o = paste("BFI", seq(5, 60, 5), sep = "")
)

# Modeling ---------------------------------------------------------------------

model_data <- bfi_spanish %>% 
  filter(complete.cases(.))

set.seed(8989)

training_indices <- sample(1:nrow(model_data), 
                           size = round(nrow(model_data)*train_test_ratio[1]), 
                           replace = FALSE)

training_data    <- model_data[ training_indices, ] 
test_data        <- model_data[-training_indices, ] 

# Five factor ESEM

## Fit training model

ffm_target   <- make_target(training_data,
                            big_five_target_loadings)

training_efa <- esem_efa(data     = training_data,
                         fm       = "ml",
                         nfactors = 5,
                         rotate   = "TargetQ",
                         Target   = ffm_target)

esem_specs   <- esem_syntax(training_efa)

### Store ESEM model syntax

write_lines(esem_specs,
            paste("output/ffm-esem/bfi_spanish-adaptation_esem-syntax.txt",
                  sep = ""))

## Fit test model

esem_model <- cfa(esem_specs,
                  test_data,
                  std.lv  = TRUE,
                  estimator = "ML")

### Store standardized solution

if (esem_model@optim$converged == TRUE) {
  
  esem_std <- standardizedsolution(esem_model)
  
  write_rds(esem_std,
            paste("output/ffm-esem/bfi_spanish-adaptation_esem-std-solution.rds",
                  sep = ""))
  
}

## Calculate fit indices

### ESEM

if (esem_model@optim$converged == TRUE) {
  
  esem_fit <- fitmeasures(esem_model, 
                          fit.measures = c("cfi", "tli", "rmsea", "bic"))
  
} else {
  
  esem_fit <- c("cfi" = NA, "tli" = NA, "rmsea" = NA, "bic" = NA)
  
}

# Record fit indices

esem_data <- data.frame(
  cfi_esem      = esem_fit[names(esem_fit) == "cfi"],   
  tli_esem      = esem_fit[names(esem_fit) == "tli"],   
  rmsea_esem    = esem_fit[names(esem_fit) == "rmsea"], 
  bic_esem      = esem_fit[names(esem_fit) == "bic"]
)

write_csv(esem_data, "output/bfi_spanish-adaptation_esem-model-fit.csv")

# Unconstrained ESEM

# This approach allows the training data to determine the number of factors used
# in the ESEM in the test data. It is highly flexible but may substantially
# deviate from the Five Factor Model of personality.

## Fit training model

training_pa  <- fa.parallel(training_data, 
                            fm = "ml", 
                            fa = "fa")

training_efa <- esem_efa(data     = training_data,
                         fm       = "ml",
                         nfactors = training_pa$nfact,
                         rotate   = "geominT")

esem_specs   <- esem_syntax(training_efa)

## Store ESEM model syntax

write_lines(esem_specs,
            paste("output/ffm-esem/bfi_spanish-adaptation_uc-esem-syntax.txt",
                  sep = ""))

### Fit test model

esem_model <- cfa(esem_specs,
                  test_data,
                  std.lv  = TRUE,
                  estimator = "ML")

## Store standardized solution

if (esem_model@optim$converged == TRUE) {
  
  esem_std <- standardizedsolution(esem_model)
  
  write_rds(esem_std,
            paste("output/ffm-esem/bfi_spanish-adaptation_uc-esem-std-solution.rds",
                  sep = ""))
  
}

## Calculate fit indices

### Big Five

if (esem_model@optim$converged == TRUE) {
  
  esem_fit <- fitmeasures(esem_model, 
                          fit.measures = c("cfi", "tli", "rmsea", "bic"))
  
} else {
  
  esem_fit <- c("cfi" = NA, "tli" = NA, "rmsea" = NA, "bic" = NA)
  
}

## Record fit indices

esem_uc_data <- data.frame(
  cfi_ucesem    = esem_fit[names(esem_fit) == "cfi"],   
  tli_ucesem    = esem_fit[names(esem_fit) == "tli"],   
  rmsea_ucesem  = esem_fit[names(esem_fit) == "rmsea"], 
  bic_ucesem    = esem_fit[names(esem_fit) == "bic"],
  nfactors      = training_pa$nfact
)

write_csv(esem_uc_data, "output/bfi_spanish-adaptation_uc-esem-model-fit.csv")
