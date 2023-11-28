################################################################################

# Big Five Network Analysis - Importation and Cleaning of Data 

# Zhang et al (2022), https://doi.org/10.1177/10731911211008245

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse", 
              "haven", 
              "osfr")

lapply(packages, library, character.only = TRUE)

# Download data ----------------------------------------------------------------

# The data for these studies must be downloaded manually from
# https://osf.io/bjfdc/?view_only=5942590d234d458aba06eaff75167955. I assume
# each zip has been unpacked to data/chinese-adaptation/ in a folder named for
# the zip file.

# Load and clean data ----------------------------------------------------------

# Chinese non-clinical adult samples

bfi_cn_student <- read_sav("data/chinese-adaptation/1. College students/Raw data_Reverse coded.sav")

bfi_cn_student <- bfi_cn_student %>% 
  select(
    paste("BFI", 1:60, sep = "")
  )

bfi_cn_adult   <- read_sav("data/chinese-adaptation/2. Adult employee/BFI2_Employee_Raw.sav")

bfi_cn_adult <- bfi_cn_adult %>% 
  select(
    paste("BFI", 1:60, sep = "")
  )

## Combine data

bfi_cn <- bind_rows(bfi_cn_student, bfi_cn_adult)%>% 
  filter(complete.cases(.))

# US comparison samples from Soto and John (2017)

bfi_us_student <- read_sav("data/chinese-adaptation/1. College students/Student validation sample reverse coded.sav")

bfi_us_student <- bfi_us_student %>% 
  select(
    paste("BFI", 1:60, sep = "")
  )

bfi_us_adult   <- read_sav("data/chinese-adaptation/1. College students/Internet validation sample reverse coded.sav")

bfi_us_adult <- bfi_us_adult %>% 
  select(
    paste("BFI", 1:60, sep = "")
  )

## Combine data

bfi_us <- bind_rows(bfi_us_student, bfi_us_adult)%>% 
  filter(complete.cases(.))

# Export cleaned data ----------------------------------------------------------

write_csv(bfi_cn, 
          "data/chinese-adaptation/bfi_chinese-adaptation_clean.csv")

write_csv(bfi_us, 
          "data/chinese-adaptation/bfi_us-validation_clean.csv")
