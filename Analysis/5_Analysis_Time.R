# jonashaslbeck@protonmail.com; Dec, 2025

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# Analyses with Time Focus

# --------------------------------------------
# --------- Load Packages --------------------
# --------------------------------------------

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(jsonlite)

library(ggplot2)

library(plyr)
library(RColorBrewer)
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
filter <- "mainly"
# filter <- "broadly"
# filter <- "noBM"

data_bin_ss <- readRDS(file = paste0("Files/Df_binarized_Filtered_", filter, "_", ai_handle, ".RDS"))

dim(data_bin_ss)

# --------------------------------------------
# --------- Compute Proportions over Time ----
# --------------------------------------------

# ------ For each Year average across newspapers -----
a_year <- array(NA, dim=c(15, 4, 7)) # 15 years, four areas
for(i in 1:15) {
  data_bin_ss_ssy <- data_bin_ss[data_bin_ss$year == (2009+i), ]
  for(j in 1:4) {
    for(n in 1:7) {
      data_bin_ss_ssy_ssn <- data_bin_ss_ssy[data_bin_ss_ssy$newspaper == outlets[n], ]
      a_year[i, j, n] <- mean(as.matrix(data_bin_ss_ssy_ssn[, l_labels_P[[j]]])) 
    }
  }
}

# ------ For each Year average across newspapers | OR -----
a_year_OR <- array(NA, dim=c(15, 4, 7)) # 15 years, four areas
for(i in 1:15) {
  data_bin_ss_ssy <- data_bin_ss[data_bin_ss$year == (2009+i), ]
  for(j in 1:4) {
    for(n in 1:7) {
      data_bin_ss_ssy_ssn <- data_bin_ss_ssy[data_bin_ss_ssy$newspaper == outlets[n], ]
      a_year_OR[i, j, n] <- mean((rowSums(as.matrix(data_bin_ss_ssy_ssn[, l_labels_P[[j]]])) > 0))
    }
  }
}


# ------ For each Year average across newspapers -----
v_ind <- c("general_causes", 
           "general_impacts", 
           "general_mitigation", 
           "general_adaptation")

a_year_single <- array(NA, dim=c(15, 4, 7)) # 15 years, four areas
for(i in 1:15) {
  data_bin_ss_ssy <- data_bin_ss[data_bin_ss$year == (2009+i), ]
  for(j in 1:4) {
    for(n in 1:7) {
      data_bin_ss_ssy_ssn <- data_bin_ss_ssy[data_bin_ss_ssy$newspaper == outlets[n], ]
      a_year_single[i, j, n] <- mean(as.matrix(data_bin_ss_ssy_ssn[, v_ind[j]])) 
    }
  }
}


# --------------------------------------------
# --------- Prop Across time For Sel. Items --
# --------------------------------------------

# Sanity: check across years
v_ipcc_yr <- rep(NA, 15)
for(i in 1:15) {
  data_bin_ssy <- data_bin_ss[data_bin_ss$year == (2009+i), ]
  v_ipcc_yr[i] <- mean(data_bin_ssy$drivers_ipcc_report)
}

# Sanity: check across years
v_cop_yr <- rep(NA, 15)
for(i in 1:15) {
  data_bin_ssy <- data_bin_ss[data_bin_ss$year == (2009+i), ]
  v_cop_yr[i] <- mean(data_bin_ssy$drivers_cop_conference)
}

# --------------------------------------------
# --------- Figure: Time Agg Newspapers ------
# --------------------------------------------

library(RColorBrewer)
cols_t <- brewer.pal(4, "Set2") # 4 colors for topics

# Aggregate over newspapers
m_year1 <- apply(a_year, 1:2, mean)
m_year_single1 <- apply(a_year_single, 1:2, mean)
m_year_single1 <- apply(a_year_single, 1:2, mean)
a_year_OR1 <- apply(a_year_OR, 1:2, mean)


# ----- Plotting function single panel aggregate on X -----

PlotTS_agg <- function(show = "mean", 
                       legend=TRUE, 
                       legend.pos = "topright",
                       main="") {
  plot.new()
  ymax <- 100
  plot.window(xlim=c(1, 15), ylim=c(0, ymax))
  axis(1, at=1:15, labels=2010:2024, las=2)
  axis(2, las=2, at=seq(0, ymax, length=11), labels=paste0(seq(0, ymax, length=11), "%"))
  for(j in 1:4) {
    if(show == "mean") lines(m_year1[, j]*100, col=cols_t[j], lwd=2) # average within topic
    if(show == "SQ") lines(m_year_single1[, j]*100, col=cols_t[j], lwd=2) # take the single average question we have
    if(show == "OR") lines(a_year_OR1[, j]*100, col=cols_t[j], lwd=2) # logical OR
  }
  abline(h=seq(0, ymax, length=11), lty=3, col="grey")
  if(legend) legend(legend.pos, legend=c("Causes", "Impacts", "Mitigation", "Adaptation"), 
                    text.col=cols_t, bty="n", cex=1.25)
  # title(ylab="Percentage")
  title(main = paste0(main), font.main=1)
}


# --------------------------------------------
# --------- Make 2x2 Panel for Paper ---------
# --------------------------------------------
# Show: 
# - aggregate over newspapers, separate for topics, with average and OR
# - show COP and IPCC reports

sc <- 1
pdf(paste0("Figures/", ai_handle, "/Fig_TimePlots_2x2_", filter, "LegRight.pdf"), width=10*sc, height=8*sc)

par(mfrow=c(2,2), mar=c(4,4.5,2,2))
# Aggregate Plots
PlotTS_agg(show="mean", main = "Mean over Items")
PlotTS_agg(show="OR", legend=TRUE, main = "Logical OR over Items", legend.pos="topright")

## --- COP ---
# Compute year-averages
v_cop_yr <- rep(NA, 15)
for(i in 1:15) {
  data_bin_ssy <- data_bin_ss[data_bin_ss$year == (2009+i), ]
  v_cop_yr[i] <- mean(data_bin_ssy$drivers_cop_conference)
}
plot.new()
ymax <- 100
plot.window(xlim=c(1, 15), ylim=c(0, ymax))
axis(1, at=1:15, labels=2010:2024, las=2)
axis(2, las=2, at=seq(0, ymax, by=10), labels=paste0(seq(0, ymax, by=10), "%"))
abline(h=seq(0, ymax, length=9), lty=3, col="grey")
lines(v_cop_yr*100, lwd=2) # Yep, makes sense!
title(main="COP Conferences", font.main=1, line=1)

## --- IPCC ---
# Compute year-averages
v_cop_yr <- rep(NA, 15)
for(i in 1:15) {
  data_bin_ssy <- data_bin_ss[data_bin_ss$year == (2009+i), ]
  v_ipcc_yr[i] <- mean(data_bin_ssy$drivers_ipcc_report)
}
plot.new()
ymax <- 100
plot.window(xlim=c(1, 15), ylim=c(0, ymax))
axis(1, at=1:15, labels=2010:2024, las=2)
axis(2, las=2, at=seq(0, ymax, by=10), labels=paste0(seq(0, ymax, by=10), "%"))
abline(h=seq(0, ymax, length=9), lty=3, col="grey")
lines(v_ipcc_yr*100, lwd=2) # Yep, makes sense!
title(main="IPCC Reports", font.main=1)

dev.off()










