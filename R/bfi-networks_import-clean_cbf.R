################################################################################

# Big Five Network Analysis - Importation and Cleaning of Data

# Zhang et al (2019), https://doi.org/10.1371/journal.pone.0221621

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse",
              "readxl")

lapply(packages, library, character.only = TRUE)

# Download data ----------------------------------------------------------------

# This code will automatically download data from figshare

# If a data directory did not exist, it would be necessary to create it

if (!dir.exists("data")) {
  
  dir.create("data")
  
}

if (!dir.exists("data/cbf")) {
  
  dir.create("data/cbf")
  
}

# Download data files

if (!dir.exists("data/cbf/")) {
  
  options(timeout = max(1000, getOption("timeout")))
  
  download.file(
    url = "https://figshare.com/ndownloader/files/17438846", 
    destfile = "data/cbf/pone.0221621.s001.xls",
    method = "auto", 
    cacheOK = FALSE)
 
}

# Load and clean data ----------------------------------------------------------

cbf <- read_xls("data/cbf/pone.0221621.s001.xls")

cbf <- cbf %>% 
  select(
    starts_with("item")
  ) %>% 
  filter(complete.cases(.))

# Export cleaned data ----------------------------------------------------------

write_csv(cbf, "data/cbf/bfi_cbf_clean.csv")
