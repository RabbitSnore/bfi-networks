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

ipip_network_fit <- comparison_data %>% 
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
                       nrow = length(countries),
                       dimnames = list(unique(cross_country_fit$country_1),
                                       unique(cross_country_fit$country_1)))

matrix_tli   <- matrix(cross_country_fit$tli_network, 
                       nrow = length(countries),
                       dimnames = list(unique(cross_country_fit$country_1),
                                       unique(cross_country_fit$country_1)))

matrix_rmsea <- matrix(cross_country_fit$rmsea_network, 
                       nrow = length(countries),
                       dimnames = list(unique(cross_country_fit$country_1),
                                       unique(cross_country_fit$country_1)))

matrix_bic   <- matrix(cross_country_fit$bic_network, 
                       nrow = length(countries),
                       dimnames = list(unique(cross_country_fit$country_1),
                                       unique(cross_country_fit$country_1)))

## Store matrices

matrix_cfi   <- as.data.frame(matrix_cfi) 
matrix_tli   <- as.data.frame(matrix_tli)  
matrix_rmsea <- as.data.frame(matrix_rmsea) 
matrix_bic   <- as.data.frame(matrix_bic) 

### Full

write.csv(matrix_cfi,   "output/bfi_op_matrix-cfi-test.csv")
write.csv(matrix_tli,   "output/bfi_op_matrix-tli-test.csv")
write.csv(matrix_rmsea, "output/bfi_op_matrix-rmsea-test.csv")
write.csv(matrix_bic,   "output/bfi_op_matrix-bic-test.csv")

### Readable (rounded to three digits)

write.csv(round(matrix_cfi, 3),   "output/bfi_op_matrix-cfi-test-rounded.csv")
write.csv(round(matrix_tli, 3),   "output/bfi_op_matrix-tli-test-rounded.csv")
write.csv(round(matrix_rmsea, 3), "output/bfi_op_matrix-rmsea-test-rounded.csv")

# Visualization and description ------------------------------------------------

# Data for visualization

iso_countries <- read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")

colnames(iso_countries) <- str_replace_all(colnames(iso_countries), "-", "_")

## Country names

country_name_df <- data.frame(
  alpha_2  = countries
  ) %>% 
  left_join(select(iso_countries, name, alpha_2), by = "alpha_2") %>% 
  mutate(
    name = case_when(
      name == "United States of America" ~ "USA",
      name == "United Kingdom of Great Britain and Northern Ireland" ~ "UK",
      name == "United Arab Emirates" ~ "UAE",
      name == "Russian Federation" ~ "Russia",
      TRUE ~ name
    )
  ) %>% 
  arrange(alpha_2)

cross_country_fit <- cross_country_fit %>% 
  left_join(country_name_df, by = c("country_1" = "alpha_2")) %>% 
  mutate(
    country_1 = name
  ) %>% 
  select(-name) %>% 
  left_join(country_name_df, by = c("country_2" = "alpha_2")) %>% 
  mutate(
    country_2 = name
  ) %>% 
  select(-name)

## Identify whether fit measures correspond to origin country

cross_country_fit <- cross_country_fit %>% 
  mutate(
    model_source = case_when(
      country_1 == country_2 ~ 1,
      country_1 != country_2 ~ 0
    )
  )

## Cross country BIC comparison

cross_country_bic <- cross_country_fit %>%
  group_by(country_2) %>% 
  mutate(
    bic_scaled = as.numeric(scale(bic_network))
  ) %>% 
  ungroup()

bic_summary <- cross_country_bic %>% 
  group_by(country_2) %>% 
  summarise(
    mean_bic   = mean(bic_network, na.rm = TRUE),
    sd_bic     = sd(bic_network, na.rm = TRUE)
  ) %>% 
  ungroup()

## Long form BIC data for model comparison

esem_data <- read_rds("output/bfi_op_ffm-esem-model-fit.rds")

comparison_data <- comparison_data %>% 
  left_join(esem_data, by = "country")

test_data_bic_long <- comparison_data %>% 
  pivot_longer(
    cols = starts_with("bic"),
    names_to = "model",
    values_to = "bic"
  ) %>% 
  left_join(country_name_df, by = c("country" = "alpha_2")) %>% 
  mutate(
    country = name
  ) %>% 
  select(-name) %>% 
  left_join(
    select(bic_summary, country = country_2, mean_bic, sd_bic),
    by = "country"
  ) %>% 
  group_by(country) %>% 
  mutate(
    # Scaled for cross-country network comparison
    bic_scaled      = (bic - mean_bic) / sd_bic,
    # Scaled for network to factor model comparison
    bic_scaled_comp = as.numeric(scale(bic))
  ) %>% 
  ungroup()

## Identify best fitting models

cross_country_best <- cross_country_fit %>% 
  group_by(country_2) %>% 
  summarise(
    best_model_bic   = country_1[which(bic_network == min(bic_network))],
    best_model_rmsea = country_1[which(rmsea_network == min(rmsea_network))],
    best_model_cfi   = country_1[which(cfi_network == max(cfi_network))]
  ) %>% 
  rename(
    country = country_2
  )

## Bayes factors for each country's model vs. origin's model

cross_country_bf <- cross_country_fit %>% 
  left_join(
    select(
      filter(cross_country_fit,
             country_1 == country_2),
      country_2,
      bic_origin = bic_network
    ), 
    by = "country_2"
  ) %>% 
  mutate(
    bf_origin  = exp( (bic_network - bic_origin) / 2 ),
    bf_e_power = (bic_network - bic_origin) / 2
  ) %>% 
  filter(country_1 != country_2)

cross_country_bf_descriptives <- cross_country_bf %>% 
  group_by(country_2) %>% 
  summarise(
    origin_in_favor = sum(bf_e_power > 0),
    prop_origin     = sum(bf_e_power > 0)/n()
  )

### Best competitor vs. origin model

cross_country_bic_min <- cross_country_fit %>%
  filter(!(country_1 == country_2)) %>% 
  group_by(country_2) %>% 
  summarise(
    country_comp   = country_1[which(bic_network == min(bic_network))],
    bic_comparison = min(bic_network)
  )

best_competitor_bf <- cross_country_fit %>% 
  filter(country_1 == country_2) %>% 
  left_join(cross_country_bic_min, by = "country_2") %>% 
  mutate(
    bf_origin  = exp( (bic_comparison - bic_network) / 2 ),
    bf_e_power = (bic_comparison - bic_network) / 2
  ) %>% 
  select(country_2, country_comp, bf_origin, bf_e_power)

### Join data

cross_country_bf_descriptives <- cross_country_bf_descriptives %>% 
  left_join(best_competitor_bf, by = "country_2") %>% 
  select(country = country_2, everything())

write_csv(cross_country_bf_descriptives, 
          "output/bfi_op_origin-model-performance.csv")

# Swarm plots of fit statistics

## BIC

swarm_bic_cross_country <- 
  ggplot(cross_country_bic,
         aes(
           x     = bic_scaled,
           y     = country_2,
           color = as.factor(model_source),
           size  = as.factor(model_source),
         )) +
  geom_quasirandom(
    alpha = .50
  ) +
  geom_point(
    data = test_data_bic_long,
    aes(
      y = country
    ),
    color = "#B5446E",
    size = .75,
    alpha = .50
  ) +
  scale_color_manual(
    values = c("#355070", "#53131E"),
    labels = c("Other Countries", "Origin")
  ) +
  scale_size_discrete(
    range = c(.50, 1.5),
    labels = c("Other Countries", "Origin")
  ) +
  scale_y_discrete(
  ) +
  labs(
    y     = "Data Origin",
    x     = "BIC (standardized within country)",
    color = "Model Origin",
    size  = "Model Origin",
    subtitle = "Cross-Country Network Invariance"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom"
  )

swarm_bic_model_comparison <- 
  ggplot(test_data_bic_long,
         aes(
           x     = bic_scaled_comp,
           y     = country,
           color = as.factor(model)
         )) +
  geom_point(
    size = 1
  ) +
  scale_color_manual(
    labels = c("Acquiescence", "Big Five", "ESEM", "Network"),
    values = c("#37123C", "#FE7F2D", "#EFAAC4", "#5995ED", "#619B8A")
  ) +
  scale_y_discrete(
  ) +
  labs(
    color = "Model",
    y = "",
    x = "BIC (standardized within country)",
    subtitle = "Comparison of models"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom"
  )

## Combine plots

swarm_plots_bic <- plot_grid(swarm_bic_model_comparison, 
                             swarm_bic_cross_country, 
                             nrow = 1, rel_widths = c(1, 1))

## Save figure

save_plot("figures/bfi_op_bic_test-data_model-comparison-swarms.png", 
          swarm_plots_bic,
          base_width = 10.50, base_height = 7)
