################################################################################

# Big Five Network Analysis - Confirmatory Network Modeling (Full Data)

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse", 
              "qgraph", 
              "igraph", 
              "psychonetrics", 
              "lavaan",
              "foreach",
              "doParallel",
              "readxl",
              "lme4",
              "performance",
              "cowplot",
              "grid",
              "gridExtra",
              "png")

lapply(packages, library, character.only = TRUE)

# Load data --------------------------------------------------------------------

# Load precleaned data

op_ipip_ffm <- read_csv("data/openpsychometrics/bfi_open-psychometrics_clean.csv")

# Load network modeling output

## If reproducing from scratch, you will need to have run the network vs. factor
## model script to generate the comparison output

comparison_data <- read_rds("output/bfi_model-comparison-data_open-psychometrics.rds")

# Create item key

ffm_key <- data.frame(
  item  = colnames(op_ipip_ffm)[-1],
  trait = c(rep("E", 10),
            rep("N", 10),
            rep("A", 10),
            rep("C", 10),
            rep("O", 10))
)

# Procedural set up ------------------------------------------------------------

# Countries and pairs for comparison

countries <- comparison_data$country

# Parallel computing set up

# IMPORTANT! Modify this for your system. Do not assume this default will work
# if you are reproducing the analyses. Running this code and not running a
# parallelized process will not be harmful, but you could have a suboptimal
# experience using this code without modifications tailored for your computing
# environment.

cores <- detectCores()

registerDoParallel(cl = (cores - 1)/2, cores = cores - 2)

# Fit confirmatory models ------------------------------------------------------

# This loop will fit a confirmatory network (i.e., the same network used for the
# network vs. factor comparisons) for each country, using the full data set for
# that country (excluding incomplete cases), instead of the just the training or
# test sets. This process is computationally intensive and is set up for
# parallel computation using all available cores.

# Note that these are "confirmatory" in the sense that the model is
# prespecified, having been estimated from a training subset of the data. But
# they are not confirmatory in the sense of strong hypothesis testing.

if (!file.exists("output/bfi_op_confirmatory-networks.rds")) {
  
  confirmatory_networks <- foreach(i = 1:length(countries), 
                                   .packages = packages, 
                                   .combine = bind_rows) %dopar% {
                                     
    # Retrieve omega matrix skeleton
    
    omega_skeleton <- comparison_data$omega[comparison_data$country == countries[i]]
    
    # Subset data
    
    net_data <- op_ipip_ffm %>% 
      filter(country == countries[i]) %>% 
      select(-country) %>% 
      filter(complete.cases(.))
    
    # Country-level network
    
    country_conf_network <- 
      varcov(data  = net_data,
             type  = "ggm",
             omega = omega_skeleton[[1]]) %>% 
      runmodel()
    
    # Set up empty data
    
    fit_data <- data.frame(
      country       = countries[i],
      cfi_network   = NA,   
      tli_network   = NA,   
      rmsea_network = NA, 
      bic_network   = NA
    )
    
    # Fit indices
    
    test_net_fit <- fit(country_conf_network)
    
    ## Extract indices
    
    fit_data$cfi_network    <- test_net_fit$Value[test_net_fit$Measure == "cfi"]
    fit_data$tli_network    <- test_net_fit$Value[test_net_fit$Measure == "tli"]
    fit_data$rmsea_network  <- test_net_fit$Value[test_net_fit$Measure == "rmsea"]
    fit_data$bic_network    <- test_net_fit$Value[test_net_fit$Measure == "bic"]
    
    # Store networks
    
    fit_data$omega_matrix <- list(getmatrix(country_conf_network,
                                            matrix = "omega"))
    
    fit_data
    
  }
  
  ## Save estimated confirmatory networks
  
  write_rds(confirmatory_networks, "output/bfi_op_confirmatory-networks.rds")
  
} else {
  
  confirmatory_networks <- read_rds("output/bfi_op_confirmatory-networks.rds")
  
}

# Walktrap community identification

walktrap_communities <- foreach(i = 1:length(countries), .packages = packages) %do% {
  
  walktrap.community(
    as.igraph(qgraph(confirmatory_networks$omega_matrix[[i]])), 
    weights = abs(E(as.igraph(qgraph(confirmatory_networks$omega_matrix[[i]]))))
  )
  
}

## Add country names

names(walktrap_communities) <- countries

# Create network visualizations

if (!dir.exists("figures")) {
  
  dir.create("figures")
  
}

## Plot networks with walktrap communities indicated by color

network_graphs_walktrap <- foreach(i = 1:length(countries)) %do% {
  
  qgraph(confirmatory_networks$omega_matrix[[i]],
         layout    = "spring",
         color     = walktrap_communities[[i]]$membership,
         theme     = "colorblind",
         filename  = paste("figures/bfi_op_network-plot_", 
                           str_replace_all(tolower(countries[i]),
                                           " ",
                                           "_"),
                           "_walktrap",
                           sep = ""),
         filetype  = "png",
         height    = 5,
         width     = 5
  )
  
}

## Plot networks with Big Five traits indicated by color

network_graphs_bigfive <- foreach(i = 1:length(countries)) %do% {
  
  qgraph(confirmatory_networks$omega_matrix[[i]],
         layout    = "spring",
         groups    = as.factor(ffm_key$trait),
         theme     = "colorblind",
         filename  = paste("figures/bfi_op_network-plot_", 
                           str_replace_all(tolower(countries[i]),
                                           " ",
                                           "_"),
                           "_bigfive",
                           sep = ""),
         filetype  = "png",
         height    = 5,
         width     = 5,
         legend    = FALSE
  )
  
}

#  Centrality measures ---------------------------------------------------------

## Calculate centrality measures for each country's network

centrality_data <- foreach(i = 1:length(countries), .packages = packages, .combine = bind_rows) %do% {
  
  strength    <- centrality(confirmatory_networks$omega_matrix[[i]])$InDegree
  closeness   <- centrality(confirmatory_networks$omega_matrix[[i]])$Closeness
  betweenness <- centrality(confirmatory_networks$omega_matrix[[i]])$Betweenness
  
  data.frame(
    country     = countries[i],
    item        = 1:50,
    strength    = strength,
    closeness   = closeness,
    betweenness = betweenness
  )
  
}

centrality_data_long <- centrality_data %>% 
  pivot_longer(
    cols      = c("strength", "closeness", "betweenness"),
    names_to  = "measure",
    values_to = "centrality"
  )

## Visualization of centrality

centrality_plot <- 
  ggplot(centrality_data_long %>% 
           # Remove cases from networks that are not fully connected
           filter(centrality != 0),
         aes(
           x = centrality,
           y = item,
           group = country
         )) +
  facet_wrap(~ measure, 
             scales = "free_x") +
  geom_line(
    orientation = "y",
    alpha = .10
  ) +
  scale_y_continuous(
    breaks = 1:120
  ) +
  labs(
    x = "",
    y = "Item"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 3),
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)
  )

### Save centrality figure

save_plot("figures/bfi_op_centrality-plot.png", centrality_plot,
          base_height = 5, base_width = 7)

## Model centrality

### Unconditional linear mixed effects models

lmm_strength     <- lmer(strength 
                         ~ (1|country) 
                         + (1|item), 
                         data = centrality_data)

lmm_closeness    <- lmer(closeness 
                         ~ (1|country) 
                         + (1|item), 
                         # Remove cases from networks that are not fully connected
                         data = filter(centrality_data, closeness != 0))

lmm_betweenness  <- lmer(betweenness 
                         ~ (1|country) 
                         + (1|item), 
                         data = centrality_data)

### Intraclass correlation coefficients

# These can be interpreted as estimates of the variance in centrality explained
# by items and by country membership. These measures cannot give a nuanced
# assessment of the structures of the networks. Rather, the ICCs for country
# membership can be thought of as answering the question, "How much of the
# overall variation in network strength/closeness/betweenness can be attributed
# to the country being modeled?"

icc_strength    <- icc(lmm_strength, by_group = TRUE)
icc_closeness   <- icc(lmm_closeness, by_group = TRUE)
icc_betweenness <- icc(lmm_betweenness, by_group = TRUE)

# Create grids of network diagrams ---------------------------------------------

if (!dir.exists("figures/network-grids")) {
  
  dir.create("figures/network-grids")
  
}

# Selected countries for manuscript

countries_selected <- c("CA", "SG")
countries_full     <- c("Canada", "Singapore")

## Plot networks with walktrap communities indicated by color

network_graphs_walktrap_sel <- foreach(i = 1:length(countries_selected)) %do% {
  
  qgraph(confirmatory_networks$omega_matrix[[which(confirmatory_networks$country == countries_selected[i])]],
         layout    = "spring",
         color     = walktrap_communities[[which(names(walktrap_communities) == countries_selected[i])]]$membership,
         theme     = "colorblind",
         filename  = paste("figures/network-grids/bfi_op_network-plot_", 
                           str_replace_all(tolower(countries_selected[i]),
                                           " ",
                                           "_"),
                           "_walktrap_titled",
                           sep = ""),
         filetype  = "png",
         height    = 5,
         width     = 5,
         title     = paste(countries_full[i], "- Walktrap", sep = " ")
  )
  
}

## Plot networks with Big Five traits indicated by color

network_graphs_bigfive_sel <- foreach(i = 1:length(countries)) %do% {
  
  qgraph(confirmatory_networks$omega_matrix[[which(confirmatory_networks$country == countries_selected[i])]],
         layout    = "spring",
         groups    = as.factor(ffm_key$trait),
         theme     = "colorblind",
         filename  = paste("figures/network-grids/bfi_op_network-plot_", 
                           str_replace_all(tolower(countries_selected[i]),
                                           " ",
                                           "_"),
                           "_bigfive_titled",
                           sep = ""),
         filetype  = "png",
         height    = 5,
         width     = 5,
         legend    = FALSE,
         title     = paste(countries_full[i], "- Big Five", sep = " ")
  )
  
}

## Grid of select countries

titled_paths <- paste(
  "figures/network-grids/",
  dir("figures/network-grids/")[str_detect(dir("figures/network-grids/"), "titled")],
  sep = ""
)

for (i in 1:(length(countries_selected) * 2)) {
  
  assign(paste("network_titled_", i, sep = ""),
         readPNG(titled_paths[i]))
  
}

png("figures/network-grids/bfi_op_titled-network_grid.png", 
    height = 8, width = 8, units = "in", res = 1500)

grid.arrange(grobs = 
               map(paste("network_titled_", 1:length(titled_paths), sep = ""), 
                   function(x) { rasterGrob(get(x))}),
             nrow = 2)

dev.off()
