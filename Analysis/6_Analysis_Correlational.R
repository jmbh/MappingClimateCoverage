# jonashaslbeck@protonmail.com; Dec, 2025

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# Correlational Analyses

# --------------------------------------------
# --------- Load Packages --------------------
# --------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(jsonlite)
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
data_bin_ss <- readRDS(file = paste0("Files/Df_binarized_Filtered_", filter, "_", ai_handle, ".RDS"))
dim(data_bin_ss)

# --------------------------------------------
# --------- Correlational Analysis -----------
# --------------------------------------------

# Subset key variables in 4 categories
data_cor <- data_bin_ss[, unlist(l_labels_P)]
n_cor <- ncol(data_cor)


# -------------------------------------
# ----- Spearman Correlation ----------
# -------------------------------------

# Compute
m_spearman <- cor(data_cor, method = "spearman")

m_spearman[upper.tri(m_spearman)] <- NA
diag(m_spearman) <- NA

# construct labels
lab_cor <- paste0(unlist(l_indP_lab), c(rep(" (C)", 7),
                                        rep(" (I)", 7),
                                        rep(" (M)", 7),
                                        rep(" (A)", 7)))
rownames(m_spearman) <- colnames(m_spearman) <- lab_cor


# Plotting PDF
sc <- 1
pdf(paste0("Figures/", ai_handle, "/Fig_Correlational_Spearman_", filter, "_", ai_handle, ".pdf"), width=10*sc, height=10*sc)
plot_acc(m_spearman, min=-.2, max=1, title="Correlational: Spearman")
dev.off()


# -------------------------------------------------------
# ----- Spearman Correlation: Make 4x4 version ----------
# -------------------------------------------------------

### Averaged across Newspapers
causes_OR <- rowSums(data_bin_ss[, l_labels_P[[1]]]) > 0
impacts_OR <- rowSums(data_bin_ss[, l_labels_P[[2]]]) > 0
mitigation_OR <- rowSums(data_bin_ss[, l_labels_P[[3]]]) > 0
adaptation_OR <- rowSums(data_bin_ss[, l_labels_P[[4]]]) > 0
m_OR <- cbind(causes_OR, impacts_OR, mitigation_OR, adaptation_OR)
corm <- cor(m_OR, method = "spearman")
corm[upper.tri(corm)] <- NA
diag(corm) <- NA
round(corm,2)

colnames(corm) <- rownames(corm) <- v_cats

# Plotting PDF
sc <- 0.5
pdf(paste0("Figures/", ai_handle, "/Fig_Correlational_OR_4x4_Spearman_", filter, "_", ai_handle, ".pdf"), width=10*sc, height=10*sc)
plot_acc(corm, min=-.3, max=1, title="Correlational: Spearman")
dev.off()


# -------------------------------------------------------
# ----- 4x4 OR: Conditional Probabilities ---------------
# -------------------------------------------------------

m_prob <- matrix(NA, 4, 4)
for(i in 1:4) for(j in 1:4) m_prob[i,j] <- mean(m_OR[, i][m_OR[, j] == TRUE])
colnames(m_prob) <- rownames(m_prob) <- v_cats
round(m_prob, 2)

# Plotting PDF
sc <- 0.5
pdf(paste0("Figures/", ai_handle, "/Fig_Correlational_OR_4x4_Conditional_", filter, "_", ai_handle, ".pdf"), width=10*sc, height=10*sc)
plot_acc(m_prob, min=0, max=1,
         title="Conditional Probabilities",
         leg_title="Cond. Prob.",
         symmetric=FALSE)
dev.off()


