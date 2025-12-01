# jonashaslbeck@protonmail.com; Dec, 2025

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# Some additional analyses Analyses

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

library(brms)
library(marginaleffects)

library(xtable)


source("Helpers.R") 
source("3_Plotting_Meta.R") # For labels, colors, item selection, etc


# --------------------------------------------
# --------- Version of AI Run ----------------
# --------------------------------------------

ai_handle <- "7.0.2-Mistral-Large-"

# --------------------------------------------
# --------- Load Data ------------------------
# --------------------------------------------

# filter <- "broadly"
filter <- "mainly"
data_bin_ss <- readRDS(file = paste0("Files/Df_binarized_Filtered_", filter, "_", ai_handle, ".RDS"))
dim(data_bin_ss)


# --------------------------------------------
# --------- Argument about SEs ---------------
# --------------------------------------------

# Get minimum proporion
filter <- "mainly"
m_props_2x2 <- readRDS(paste0("Files/Datasum_prop_2x2_", filter, "_", ai_handle, ".RDS"))
min(m_props_2x2)
max(m_props_2x2)
N <- nrow(data_bin_ss)
N <- 5272 # N of newspaper with smallest sample size
p <- c(min(m_props_2x2), 0.5)
SE <- sqrt(p*(1-p) / N)
round(SE*100, 2)

# Actually compute minimum cound of "Yes" for each variable in each newspaper
m_counts <- matrix(NA, 28, 7)
for(i in 1:7) {
  data_bin_ss_n <- data_bin_ss[data_bin_ss$newspaper==outlets[i], ]
  m_counts[, i] <- colSums(data_bin_ss_n[, unlist(l_labels_P)])
}

# --------------------------------------------
# --------- Variance Decomposition -----------
# --------------------------------------------

# Subset to key questions
data_bin_ss_pr <- data_bin_ss[, c("url", "newspaper", "year", "wordcount", unlist(l_labels_P))]

# Turn into Long-Format
data_bin_ss_pr_long <- reshape2::melt(data_bin_ss_pr, id.vars=c("url", "newspaper", "year", "wordcount"), measure.vars=unlist(l_labels_P), variable.name="question", value.name="response")

# Add category
category <- rep(NA, nrow(data_bin_ss_pr_long))
category[data_bin_ss_pr_long$question %in% l_labels_P[[1]]] <- "Causes"
category[data_bin_ss_pr_long$question %in% l_labels_P[[2]]] <- "Impacts"
category[data_bin_ss_pr_long$question %in% l_labels_P[[3]]] <- "Mitigation"
category[data_bin_ss_pr_long$question %in% l_labels_P[[4]]] <- "Adaptation"


# ----- Anova -----
data_bin_ss_pr_long$year <- as.factor(data_bin_ss_pr_long$year)
lm_obj <- lm(response ~ newspaper + category + year, data=data_bin_ss_pr_long)
lm_obj
anov_obj <- anova(lm_obj)
anov_obj

# Note: I made the latex table in the paper with GPT5

# --------------------------------------------
# --------- Type of Articles vs. Newspaper ---
# --------------------------------------------

v_types <- c("Report", 
             "Interview", 
             "Opinion piece")

newspaper <- factor(data_bin_ss$newspaper, levels=outlets)

tb <- table(data_bin_ss$type_of_article, newspaper)
tbp <- prop.table(tb, 2)
rownames(tbp) <- v_types
round(tbp, 2)

library(xtable)

xtable(tbp, digits=2)

