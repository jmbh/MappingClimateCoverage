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

# Percentage about climate change
tb <- table(data_bin$percentage_climate_change)
prop <- prop.table(tb) |> round(2) 
prop_ord <- prop[order(as.numeric(gsub("%", "", names(prop))))]

# ----- Broadly vs. Percentage -----
tb <- table(data_bin$broadly_climate_change, data_bin$percentage_climate_change)
prop <- prop.table(tb) |> round(2) 
tb.p <- prop[, order(as.numeric(gsub("%", "", colnames(prop))))]
tb.p

# ----- Broadly vs. Percentage -----
tb <- table(data_bin$mainly_climate_change, data_bin$percentage_climate_change)
prop <- prop.table(tb) |> round(2) 
tb.p <- prop[, order(as.numeric(gsub("%", "", colnames(prop))))]
tb.p

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







