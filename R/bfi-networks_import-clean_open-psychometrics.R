################################################################################

# Big Five Network Analysis - Importation and Cleaning of Data

# Open Psychometrics

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse")

lapply(packages, library, character.only = TRUE)

# Download data ----------------------------------------------------------------

# This code will automatically download data from openpsychometrics.org

# If a data directory did not exist, it would be necessary to create it

if (!dir.exists("data")) {
  
  dir.create("data")
  
}

if (!dir.exists("data/openpsychometrics")) {
  
  dir.create("data/openpsychometrics")
  
}

# Download and unpack zip files

if (!dir.exists("data/openpsychometrics/IPIP-FFM-data-8Nov2018")) {
  
  options(timeout = max(1000, getOption("timeout")))
  
  download.file(
    url = "https://openpsychometrics.org/_rawdata/IPIP-FFM-data-8Nov2018.zip", 
    destfile = "data/openpsychometrics/IPIP-FFM-data-8Nov2018.zip",
    method = "auto", 
    cacheOK = FALSE)
  
  unzip("data/openpsychometrics/IPIP-FFM-data-8Nov2018.zip", 
        exdir = "data/openpsychometrics")
  
  file.remove("data/openpsychometrics/IPIP-FFM-data-8Nov2018.zip")
  
}

# Load and clean data ----------------------------------------------------------

op_ipip_ffm <- read_tsv(
  "data/openpsychometrics/IPIP-FFM-data-8Nov2018/data-final.csv"
)

# Remove cases where more than one response was registered from the same IP

op_ipip_ffm <- op_ipip_ffm %>% 
  filter(IPC == 1)

# Select relevant variables

op_ipip_ffm <- op_ipip_ffm %>% 
  select(
    country, 
    starts_with("EXT"),
    starts_with("EST"),
    starts_with("AGR"),
    starts_with("CSN"),
    starts_with("OPN"),
    -ends_with("_E")
  )

# Remove incomplete cases

op_ipip_ffm[op_ipip_ffm == "NULL"] <- NA
op_ipip_ffm[op_ipip_ffm == "0"] <- NA

op_ipip_ffm <- op_ipip_ffm %>% 
  filter(complete.cases(.)) %>% 
  type_convert()

# Identify countries with 1500 or more observations

countries <- table(op_ipip_ffm$country)

countries <- countries[countries >= 1500]
countries <- countries[names(countries) != "NONE"]

op_ipip_ffm <- op_ipip_ffm %>% 
  filter(country %in% names(countries))

# Export cleaned data ----------------------------------------------------------

write_csv(op_ipip_ffm,
          "data/openpsychometrics/bfi_open-psychometrics_clean.csv")
