################################################################################

# Big Five Network Analysis - Importation and Cleaning of Data 

# Gallardo-Pujol et al (2022), https://doi.org/10.1027/2698-1866/a000020

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse", 
              "readxl", 
              "osfr")

lapply(packages, library, character.only = TRUE)

# Download data ----------------------------------------------------------------

# The following code automatically downloads Spanish BFI adaptation data for
# Gallardo-Pujol et al (https://osf.io/kp572/)

# If a data directory did not exist, it would be necessary to create it

if (!dir.exists("data")) {
  
  dir.create("data")
  
}

if (!dir.exists("data/spanish-adaptation")) {
  
  dir.create("data/spanish-adaptation/")
  
}

# Study 1

if (!file.exists("data/spanish-adaptation/BFI-2 Full data (study 1).csv")) {
  
  osf_retrieve_file("601bca78acd8db00aa332112") %>% 
    osf_download(
      path = "data/spanish-adaptation"
    )
  
}

# Study 2 (Wave 1)

if (!file.exists("data/spanish-adaptation/1st wave.csv")) {
  
  osf_retrieve_file("601bca63acd8db00aa3320ab") %>% 
    osf_download(
      path = "data/spanish-adaptation"
    )
  
}

# Study 3

if (!file.exists("data/spanish-adaptation/Study 3 data.csv")) {
  
  osf_retrieve_file("63e8030aa3fade01e2e7c556") %>% 
    osf_download(
      path = "data/spanish-adaptation"
    )
  
}

# Load and clean data ----------------------------------------------------------

# Study 1

bfi_s1 <- read_csv("data/spanish-adaptation/BFI-2 Full data (study 1).csv")

bfi_s1 <- bfi_s1 %>% 
  select(
    starts_with("BFI")
  )

# Study 2

bfi_s2 <- read_csv("data/spanish-adaptation/1st wave.csv")

bfi_s2 <- bfi_s2 %>% 
  slice(-1) %>% 
  type_convert()

bfi_s2 <- bfi_s2 %>% 
  select(
    starts_with("QID108")
  )

colnames(bfi_s2) <- paste("BFI", 1:60, sep = "")

# Study 3

bfi_s3 <- read_csv("data/spanish-adaptation/Study 3 data.csv")

bfi_s3 <- bfi_s3 %>% 
  select(
    starts_with("BFI")
  )

# Combine data

bfi_spanish <- bind_rows(bfi_s1, bfi_s2, bfi_s3)

# Export cleaned data ----------------------------------------------------------

write_csv(bfi_spanish, 
          "data/spanish-adaptation/bfi_spanish-adaptation_clean.csv")
