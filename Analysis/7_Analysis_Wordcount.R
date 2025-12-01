# jonashaslbeck@protonmail.com; Dec, 2025

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# A few summaries on word counts, shown in the appendix

# --------------------------------------------
# --------- Load Packages --------------------
# --------------------------------------------

# Database
library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)

library(ggplot2)

library(plyr)
library(RColorBrewer)

library(xtable)
library(scales)


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
# --------- Overall Wordcount ----------------
# --------------------------------------------

mean(data_bin_ss$wordcount)
median(data_bin_ss$wordcount)


# --------------------------------------------
# --------- Word Count Figures ---------------
# --------------------------------------------

# ----- After Filter -----
pdf(paste0("Figures/", ai_handle, "/Fig_Wordcount_Violin_date=", figV, ".pdf"), width = 8, height = 4)

data_bin_ss_cop <- data_bin_ss

# Order newspapers, so they match colors
fac_ord <- factor(data_bin_ss_cop$newspaper, levels = outlets)
data_bin_ss_cop_ord <- data_bin_ss_cop[order(fac_ord), ]
data_bin_ss_cop_ord$newspaper <- factor(data_bin_ss_cop_ord$newspaper, levels = outlets)

# Creating the violin plot
ggplot(data_bin_ss_cop_ord, aes(x = newspaper, y = wordcount, fill = newspaper)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +  # Adds a box plot inside the violin plot
  scale_fill_manual(values = setNames(cols, unique(data_bin_ss_cop_ord$newspaper))) +  # Uses the color vector
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 4000, length = 9),  # Custom tick marks
                     limits = c(0, 4000)) +
  labs(title = "", x = "", y = "Word Count") +
  theme(legend.position = "none")  # Hides the legend

dev.off()


# ----- Make also table with summaries -----
tb_out <- aggregate(data_bin_ss_cop_ord$wordcount ~ data_bin_ss_cop_ord$newspaper, FUN = function(v) {
  c(median = median(v), sd = sd(v),
    q05 = quantile(v, 0.05), q95 = quantile(v, 0.95))
})

tb_out_r <- t(tb_out)[-1, ]
tb_out_r <- apply(tb_out_r, 1:2, function(x) round(as.numeric(x)))
colnames(tb_out_r) <- outlets
rownames(tb_out_r) <- c("Median", "SD", "5% Quantile", "95% Quantile")
tb_out_r

xtable(tb_out_r, digits=0)


