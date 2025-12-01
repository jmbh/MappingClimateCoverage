# jonashaslbeck@protonmail.com; Nov, 2025

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# Main results on percentages

# --------------------------------------------
# --------- Load Packages --------------------
# --------------------------------------------

# Database
library(DBI)
library(RMySQL)

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(jsonlite)

library(ggplot2)

library(plyr)
library(RColorBrewer)

source("Helpers.R") 
source("3_Plotting_Meta.R") # For labels, colors, item selection, etc


# --------------------------------------------
# --------- Version of AI Run ----------------
# --------------------------------------------

ai_handle <- "7.0.2-Mistral-Large-"


# --------------------------------------------
# --------- Load Data ------------------------
# --------------------------------------------

# ----- Select filter version -----
# filter <- "broadly"
filter <- "mainly"
# filter <- "noBM"

# ----- Load Data -----
data_bin_ss <- readRDS(file = paste0("Files/Df_binarized_Filtered_", filter, "_", ai_handle, ".RDS"))

dim(data_bin_ss)

# --------------------------------------------
# --------- Main 2x2 Figure [Across Years] ---
# --------------------------------------------

# Here we show the results for different ranges of years; in the paper we only show the results for the entire range of years

# Subset years
l_years <- list(2010:2014, 
                2015:2019,
                2020:2024, 
                2010:2024) # Full period
v_ynames <- c(" (2010-14)", 
              " (2015-19)", 
              " (2020-24)", 
              "")

for(t in 1:4) {
  
  # Subset Year-Range
  data_bin_ss_yr <- data_bin_ss[data_bin_ss$year %in% l_years[[t]], ]
  
  # Loop over four main paper topics
  sc <- 1.2
  pdf(paste0("Figures/", ai_handle, "/Fig_barplots_2x2_FullData_", filter, "_",  v_ynames[t], "_date=", figV, ".pdf"), width=10*sc, height=7.2*sc)
  
  par(mfrow=c(2,2))
  for(i in 1:4)   Plot1Barplot(data = data_bin_ss_yr, 
                               ind_qu = l_labels_P[[i]], 
                               labels_qu = l_indP_lab_lb[[i]], 
                               cols = cols,
                               outlets = outlets,
                               legend = c(0,1,0,0)[i], 
                               title = paste0(v_cats[i], v_ynames[t]), 
                               ymax=80, 
                               avBars = FALSE)
  
  dev.off()
  
} # end for: time period


# ---- Export Probabilities of this Figure for other Analysis ----
m_props_2x2 <- matrix(NA, 7, 28)
for(n in 1:7) {
  for(j in 1:4) {
    df_tmp <- data_bin_ss[data_bin_ss$newspaper %in% outlets[[n]], ]
    m_props_2x2[n, ((j-1)*7+1):(j*7)] <- colMeans(df_tmp[, l_labels_P[[j]]])
  }
}
saveRDS(m_props_2x2, paste0("Files/Datasum_prop_2x2_", filter, "_", ai_handle, ".RDS"))
m_props_2x2 <- as.data.frame(m_props_2x2)
colnames(m_props_2x2) <- unlist(l_labels_P)


# ---- Numbers for the Paper -----
v_newsp <- rep(NA, 7)
for(i in 1:7) {
  data_bin_ss_n <- data_bin_ss[data_bin_ss$newspaper == outlets[i], ]
  X <- unlist(data_bin_ss_n[, unlist(l_labels_P)])
  v_newsp[i] <- mean(X)
}
names(v_newsp) <- outlets
round(v_newsp, 3)

# Get averages across *newspapers*

# Averages across categories
round(mean(colMeans(data_bin_ss[, l_labels_P[[1]]])), 3) # causes
round(mean(colMeans(data_bin_ss[, l_labels_P[[2]]])), 3) # impacts
round(mean(colMeans(data_bin_ss[, l_labels_P[[3]]])), 3) # mitigation
round(mean(colMeans(data_bin_ss[, l_labels_P[[4]]])), 3) # adaptation
# SAME WITH OR
prop_OR <- rep(NA, 4)
names(prop_OR) <- v_cats
for(i in 1:4) prop_OR[i] <- mean(apply(data_bin_ss[, l_labels_P[[i]] ], 1, any))
round(prop_OR, 3)
# Averages across categories [WITHOUT FIRST TWP]
round(mean(colMeans(data_bin_ss[, l_labels_P[[1]][-1] ])), 3) # causes
round(mean(colMeans(data_bin_ss[, l_labels_P[[2]][-1] ])), 3) # impacts
round(mean(colMeans(data_bin_ss[, l_labels_P[[3]] ])), 3) # mitigation
round(mean(colMeans(data_bin_ss[, l_labels_P[[4]] ])), 3) # adaptation
# OR  [WITHOUT FIRST TWP]
mean(apply(data_bin_ss[, l_labels_P[[1]][-1] ], 1, any)) # causes
mean(apply(data_bin_ss[, l_labels_P[[2]][-1] ], 1, any)) # impacts
mean(apply(data_bin_ss[, l_labels_P[[3]] ], 1, any)) # mitigation
mean(apply(data_bin_ss[, l_labels_P[[4]] ], 1, any)) # adaptation


# General Questions
mean(data_bin_ss$general_causes)
mean(data_bin_ss$general_impacts)
mean(data_bin_ss$general_mitigation)
mean(data_bin_ss$general_adaptation)

### CAUSES
# Human Caused
round(mean(data_bin_ss$causes_human*100), 1)
round(mean(data_bin_ss$causes_human[data_bin_ss$newspaper=="zeit.de"]*100), 1)
round(mean(data_bin_ss$causes_human[data_bin_ss$newspaper=="bild.de"]*100), 1)
# Fossil Fuel
round(mean(data_bin_ss$causes_fossil_fuels*100), 1)
round(mean(data_bin_ss$causes_fossil_fuels[data_bin_ss$newspaper=="zeit.de"]*100), 1)
round(mean(data_bin_ss$causes_fossil_fuels[data_bin_ss$newspaper=="bild.de"]*100), 1)
# Agriculture
round(mean(data_bin_ss$causes_agriculture*100), 1)
# Economic System
round(mean(data_bin_ss$causes_economic_system*100), 1)
round(mean(data_bin_ss$causes_economic_system[data_bin_ss$newspaper=="taz.de"]*100), 1)
round(mean(data_bin_ss$causes_economic_system[data_bin_ss$newspaper=="welt.de"]*100), 1)
round(mean(data_bin_ss$causes_economic_system[data_bin_ss$newspaper=="bild.de"]*100), 1)
# Overconsumption
round(mean(data_bin_ss$causes_overconsumption*100), 1)
# Historic contribution
round(mean(data_bin_ss$causes_dev_countries_contrib*100), 1)
round(mean(data_bin_ss$causes_dev_countries_contrib[data_bin_ss$newspaper=="zeit.de"]*100), 1)
round(mean(data_bin_ss$causes_dev_countries_contrib[data_bin_ss$newspaper=="bild.de"]*100), 1)

### IMPACTS
# Advers impacts & extreme weather
round(mean(data_bin_ss$impact_adverse*100), 1)
round(mean(data_bin_ss$impact_extreme_weather*100), 1)
# biodiversity loss
round(mean(data_bin_ss$impact_biodiversity_loss*100), 1)
# Sea level rise
round(mean(data_bin_ss$impact_sea_level_rise*100), 1)
round(mean(data_bin_ss$impact_sea_level_rise[data_bin_ss$newspaper=="zeit.de"]*100), 1)
round(mean(data_bin_ss$impact_sea_level_rise[data_bin_ss$newspaper=="spiegel.de"]*100), 1)
round(mean(data_bin_ss$impact_sea_level_rise[data_bin_ss$newspaper=="welt.de"]*100), 1)
round(mean(data_bin_ss$impact_sea_level_rise[!(data_bin_ss$newspaper %in% c("welt.de", "zeit.de", "spiegel.de"))]*100), 1)

# Food and water security
round(mean(data_bin_ss$impact_food_water_security*100), 1)
round(mean(data_bin_ss$impact_food_water_security[data_bin_ss$newspaper=="zeit.de"]*100), 1)
round(mean(data_bin_ss$impact_food_water_security[data_bin_ss$newspaper=="welt.de"]*100), 1)
round(mean(data_bin_ss$impact_food_water_security[data_bin_ss$newspaper=="bild.de"]*100), 1)
# Migration
round(mean(data_bin_ss$impact_migration*100), 1)
round(mean(data_bin_ss$impact_migration[data_bin_ss$newspaper=="zeit.de"]*100), 1)
round(mean(data_bin_ss$impact_migration[data_bin_ss$newspaper=="welt.de"]*100), 1)
round(mean(data_bin_ss$impact_science_threat*100), 1)

### MITIGATION
# Reduction FF
round(mean(data_bin_ss$mitigation_reduction_fossil*100), 1)
round(mean(data_bin_ss$mitigation_reduction_fossil[data_bin_ss$newspaper=="taz.de"]*100), 1)
round(mean(data_bin_ss$mitigation_reduction_fossil[data_bin_ss$newspaper=="bild.de"]*100), 1)
# Renewable energy
round(mean(data_bin_ss$mitigation_renewable_energy*100), 1)
round(mean(data_bin_ss$mitigation_renewable_energy[data_bin_ss$newspaper=="taz.de"]*100), 1)
round(mean(data_bin_ss$mitigation_renewable_energy[data_bin_ss$newspaper=="bild.de"]*100), 1)
# Electrification
round(mean(data_bin_ss$mitigation_electrification*100), 1)
round(mean(data_bin_ss$mitigation_electrification[data_bin_ss$newspaper=="taz.de"]*100), 1)
round(mean(data_bin_ss$mitigation_electrification[data_bin_ss$newspaper=="bild.de"]*100), 1)

# Diets/Livestock
round(mean(data_bin_ss$mitigation_diets_livestock*100), 1)
round(mean(data_bin_ss$mitigation_diets_livestock[data_bin_ss$newspaper=="taz.de"]*100), 1)
round(mean(data_bin_ss$mitigation_diets_livestock[data_bin_ss$newspaper=="zeit.de"]*100), 1)
round(mean(data_bin_ss$mitigation_diets_livestock[data_bin_ss$newspaper=="welt.de"]*100), 1)
round(mean(data_bin_ss$mitigation_diets_livestock[data_bin_ss$newspaper=="bild.de"]*100), 1)

# Flying
round(mean(data_bin_ss$mitigation_reduce_flying*100), 1)
# Carbon Tax
round(mean(data_bin_ss$mitigation_net_zero*100), 1)
# Net Zero
round(mean(data_bin_ss$mitigation_carbon_tax*100), 1)

### ADAPTATION
# Infrastructure & Resiliance
round(mean(data_bin_ss$adaptation_infrastructure_resilience*100), 1)
# Urban Resilience
round(mean(data_bin_ss$adaptation_urban_climate_resilience*100), 1)
# Agriculture Resilience
round(mean(data_bin_ss$adaptation_agriculture_resilience*100), 1)
# Ecosystem Restoration
round(mean(data_bin_ss$adaptation_ecosystem_restoration*100), 1)
# Disaster preparedness
round(mean(data_bin_ss$adaptation_disaster_preparedness*100), 1)
# Human Migration
round(mean(data_bin_ss$adaptation_human_migration*100), 1)
# Countries
round(mean(data_bin_ss$adaptation_countries*100), 1)
round(mean(data_bin_ss$adaptation_countries[data_bin_ss$newspaper=="taz.de"]*100), 1)
round(mean(data_bin_ss$adaptation_countries[data_bin_ss$newspaper=="zeit.de"]*100), 1)
round(mean(data_bin_ss$adaptation_countries[data_bin_ss$newspaper=="welt.de"]*100), 1)
round(mean(data_bin_ss$adaptation_countries[data_bin_ss$newspaper=="bild.de"]*100), 1)







