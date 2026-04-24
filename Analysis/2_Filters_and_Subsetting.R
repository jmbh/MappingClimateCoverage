# jonashaslbeck@protonmail.com

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# Applying the three data quality filter + different narrowness filters

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

source("Helpers.R")

# --------------------------------------------
# --------- Version of AI Run ----------------
# --------------------------------------------

ai_handle <- "7.0.2-Mistral-Large-"

# --------------------------------------------
# --------- Load Data ------------------------
# --------------------------------------------

data_bin <- readRDS(file = paste0("Files/Df_binarized_rdy_", ai_handle, ".RDS"))
dim(data_bin)

# --------------------------------------------
# --------- Analyze 4 Filter Variables -------
# --------------------------------------------

# ----- Proper Newsarticle -----
tb <- table(data_bin$proper_news_article)
round(prop.table(tb), 3)

# ----- Type of Article -----
tb <- table(data_bin$type_of_article)
round(prop.table(tb), 3)

# ----- Wordcount < 151 -----
tb <- table(data_bin$wordcount > 150)
round(prop.table(tb), 3)

# For revision: Sample 10 articles with wordcount < 151
data_bin_short_broadly <- data_bin[data_bin$wordcount < 151 & data_bin$mainly_climate_change == 1 & data_bin$proper_news_article == 1 , ]
data_bin_short_broadly$url[1] # Video
data_bin_short_broadly$url[2] # Article
data_bin_short_broadly$url[3] # Article
data_bin_short_broadly$url[4] # Article
data_bin_short_broadly$url[5] # Article (32 words) 
data_bin_short_broadly$url[6] # Article
data_bin_short_broadly$url[7] # Video
data_bin_short_broadly$url[8] # Article
data_bin_short_broadly$url[9] # Video
data_bin_short_broadly$url[10] # Article
# Many of the excluded articles are indeed articles; but they are *very* short

# ---------------------------
# ----- Content Filters -----
# ---------------------------

# ----- Filters -----
# Broadly CC
tb <- table(data_bin$broadly_climate_change)
prop.table(tb) |> round(3)

# Mainly CC
tb <- table(data_bin$mainly_climate_change)
prop.table(tb) |> round(4) 

# For Revision: Mainly-Broadly Subset-Superset Relation
tb <- table(data_bin$mainly_climate_change, data_bin$broadly_climate_change) # We see: subset relation is exact

# Percentage about climate change
tb <- table(data_bin$percentage_climate_change)
prop <- prop.table(tb) |> round(2) 
prop_ord <- prop[order(as.numeric(gsub("%", "", names(prop))))]

# ----- Broadly vs. Percentage -----
tb <- table(data_bin$broadly_climate_change, data_bin$percentage_climate_change)
prop <- prop.table(tb) |> round(2) 
tb.p <- prop[, order(as.numeric(gsub("%", "", colnames(prop))))]
tb.p

# ----- Mainly vs. Percentage -----
tb <- table(data_bin$mainly_climate_change, data_bin$percentage_climate_change)
prop <- prop.table(tb) |> round(2) 
tb.p <- prop[, order(as.numeric(gsub("%", "", colnames(prop))))]
tb.p
# Revision: Make barplot
pdf(paste0("Figures/", ai_handle, "/Fig_App_percentage_mainly_relation.pdf"), width = 8, height=5)
cols <- gray.colors(nrow(tb.p))
barplot(tb.p, col = cols, axes = FALSE)
axis(2, las = 2)
title(ylab = "Proportion", xlab="Percentage of Text on Climate Change")
legend("topright",
       legend = c("Not Mainly about Climate Change", "Mainly About Climate Change"),
       fill = cols,
       bty = "n")
dev.off()


# ------------------------------
# ----- Quali Filter Analysis --
# ------------------------------
# For Revision: Inspect and compare some articles in three groups:
# 1) Not broadly, not mainly
# 2) Broadlty, not mainly, 
# 3) Mainly (and broadly)
# to give a sense of the decision boundaries
# We sample after applying the other three data quality filters
data_bin_short_qualfil <- data_bin[data_bin$wordcount > 150 & data_bin$proper_news_article == 1 & data_bin$newspaper != "theguardian.com" & as.numeric(data_bin$type_of_article) %in% 0:2, ]

# --- Set 1: Not broadly, not mainly ---
data_bin_short_qualfil_nb_nm <- data_bin_short_qualfil[data_bin_short_qualfil$broadly_climate_change == 0 & data_bin_short_qualfil$mainly_climate_change == 0, ]
# Three random ones
set.seed(1)
data_bin_short_qualfil_nb_nm$url[sample(1:nrow(data_bin_short_qualfil_nb_nm), 3)]
# --- Set 2: Broadly, not mainly ---
data_bin_short_qualfil_b_nm <- data_bin_short_qualfil[data_bin_short_qualfil$broadly_climate_change == 1 & data_bin_short_qualfil$mainly_climate_change == 0, ]
# Three random ones
set.seed(1)
ind_samp <- sample(1:nrow(data_bin_short_qualfil_b_nm), 3)
data_bin_short_qualfil_b_nm$url[ind_samp]
data_bin_short_qualfil_b_nm$percentage_climate_change[ind_samp]
# --- Set 3: Mainly (implied Broadly) ---
data_bin_short_qualfil_b_m <- data_bin_short_qualfil[data_bin_short_qualfil$broadly_climate_change == 1 & data_bin_short_qualfil$mainly_climate_change == 1, ]
# Three random ones
set.seed(1)
ind_samp <- sample(1:nrow(data_bin_short_qualfil_b_m), 3)
data_bin_short_qualfil_b_m$url[ind_samp]
data_bin_short_qualfil_b_m$percentage_climate_change[ind_samp]




# --------------------------------------------
# --------- Subsetting -----------------------
# --------------------------------------------

# ----- Subset: ALL FOUR -----
data_bin_ss <- data_bin[data_bin$proper_news_article == 1 & data_bin$broadly_climate_change == 1 & data_bin$wordcount > 150 & data_bin$newspaper != "theguardian.com" & as.numeric(data_bin$type_of_article) %in% 0:2, ]

dim(data_bin_ss)

# How much data do we still have?
tb <- table(data_bin_ss$year, data_bin_ss$newspaper)
tb
colSums(tb)

sum(tb)
# Hoe much does the data drop?
nrow(data_bin)
nrow(data_bin_ss)
round(nrow(data_bin_ss)/nrow(data_bin), 3) # Left over data

# ----- Subset: ALL FOUR [with mainly] -----
data_bin_ss_mainly <- data_bin[data_bin$proper_news_article == 1 & data_bin$mainly_climate_change == 1 & data_bin$wordcount > 150 & data_bin$newspaper != "theguardian.com" & as.numeric(data_bin$type_of_article) %in% 0:2, ]

dim(data_bin_ss_mainly)

round(nrow(data_bin_ss_mainly)/nrow(data_bin), 3) # Left over data


# ----- Subset: ONLY THREE [without broadly/mainly] -----
data_bin_ss_no_BM <- data_bin[data_bin$proper_news_article == 1 & data_bin$wordcount > 150 & data_bin$newspaper != "theguardian.com" & as.numeric(data_bin$type_of_article) %in% 0:2, ]

dim(data_bin_ss_no_BM)

round(nrow(data_bin_ss_mainly)/nrow(data_bin), 3) # Left over data


# ----- Subset: ALL FOUR WITH Guardian-----
data_bin_ss_guard <- data_bin[data_bin$proper_news_article == 1 & data_bin$broadly_climate_change == 1 & data_bin$wordcount > 150 & as.numeric(data_bin$type_of_articl) %in% 0:2, ]

# How much data do we still have?
tb <- table(data_bin_ss_guard$year, data_bin_ss_guard$newspaper)
tb
sum(tb)

# ----- Subset: ONLY "Proper News article" -----
data_bin_partial_filter <- data_bin[data_bin$proper_news_article == 1 & data_bin$newspaper != "theguardian.com" & data_bin$wordcount > 150 & as.numeric(data_bin$type_of_article) %in% 0:2, ]


# --------------------------------------------
# --------- Save Filtered Data ---------------
# --------------------------------------------

saveRDS(data_bin_ss, file = paste0("Files/Df_binarized_Filtered_broadly_", ai_handle, ".RDS"))
saveRDS(data_bin_ss_mainly, file = paste0("Files/Df_binarized_Filtered_mainly_", ai_handle, ".RDS"))
saveRDS(data_bin_ss_no_BM, file = paste0("Files/Df_binarized_Filtered_NoBM_", ai_handle, ".RDS"))







