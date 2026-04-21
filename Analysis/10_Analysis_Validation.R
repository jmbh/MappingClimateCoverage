# jonashaslbeck@protonmail.com; August 13th, 2025

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# We do some basic analysis on the AI responses we got from
# 100 x each newspaper


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

library(plyr)
library(RColorBrewer)

source("Helpers.R")
source("3_Plotting_Meta.R") # For labels, colors, item selection, etc


# --------------------------------------------
# --------- Load Packages --------------------
# --------------------------------------------

# Old
df_wide <- read.csv('Files/human_ai_validation_combined.csv')


# --------------------------------------------
# ------ Calculate Validation Metrics --------
# --------------------------------------------

# @Fabian: This needs to be adapted by you

dice <- function(x, y) {
  1 - proxy::dist(rbind(x, y), method = 'Dice')[1]
}

edice <- function(x1, x2) {
  p1 <- mean(x1, na.rm = TRUE)
  p2 <- mean(x2, na.rm = TRUE)
  2 * p1 * p2 / (p1 + p2)
}

selected_questions <- c(causes, impact, mitigation, adaptation)

# Loading responses of both AI and human consensus on validation Sample
df_wide <- read.csv('Files/human_ai_validation_combined.csv')

df_sum <- df_wide %>%
  dplyr::group_by(question) %>%
  dplyr::summarize(
    accuracy_pooled_gpt_updated = mean(Pooled_updated == gpt_bin, na.rm = TRUE),
    dice_pooled_gpt_updated = dice(Pooled_updated, gpt_bin),
    edice_pooled_gpt_updated = edice(Pooled_updated, gpt_bin)
  ) %>%
  dplyr::mutate(
    question = factor(question, levels = selected_questions)
  )

df_dice <- df_sum %>% 
  select(
    question, dice_pooled_gpt_updated,
    edice_pooled_gpt_updated, accuracy_pooled_gpt_updated
  ) %>% 
  mutate(
    dice_pooled_gpt_updated     = tidyr::replace_na(dice_pooled_gpt_updated, -0.5),
    edice_pooled_gpt_updated    = tidyr::replace_na(edice_pooled_gpt_updated, -0.5),
    accuracy_pooled_gpt_updated = tidyr::replace_na(accuracy_pooled_gpt_updated, -0.5)
  )

df_dice_long <- df_dice %>%
  pivot_longer(
    cols = -question,
    names_to = c("measure", "pair"),
    names_pattern = "(dice|edice|accuracy)_(.+)",
    values_to = "value"
  ) %>% 
  dplyr::mutate(
    category = sapply(strsplit(as.character(question), '_'), function(x) {
      x[1]
    })
  )

valdata <- df_dice_long %>% 
  filter(pair == 'pooled_gpt_updated') %>% 
  select(-pair, -category) %>% 
  pivot_wider(names_from = measure, values_from = value) %>% 
  left_join(
    df_wide %>% 
      dplyr::group_by(question) %>% 
      dplyr::summarize(
        baserate = mean(Pooled_updated, na.rm = TRUE),
        baserate_ai = mean(gpt_bin, na.rm = TRUE)
      ),
    by = 'question'
  )

# valdata <- read.csv(paste0(basedir, "validation_mistral_final.csv"))
# Eval(valdata)

# New
valdata <- read.csv('Files/valdata.csv')

head(valdata)

# Add expected accuracy
valdata$exp_accuracy <- valdata$baserate * valdata$baserate_ai + (1 - valdata$baserate) * (1 - valdata$baserate_ai)


# --------------------------------------------
# --------- Figure: Accuracy & Dice ----------
# --------------------------------------------

# Loop over four main paper topics
sc <- 1.2
pdf(paste0("Figures/Fig_Validation_Mistral_F3.pdf"), width=10*sc, height=8*sc)

par(mfrow=c(2,2))

for(i in 1:4) {
  
  # ----- Prep Data -----
  # Fix Fabian's annoying alphabetical order
  vec <- valdata[valdata$question %in% l_labels_P[[i]], 1]
  ord <- order(factor(vec, levels = l_labels_P[[i]]))
  acc_dice <- t(as.matrix(valdata[valdata$question %in% l_labels_P[[i]], c(4,2)]))[, ord]
  exp_dice <- t(as.matrix(valdata[valdata$question %in% l_labels_P[[i]], 3]))[, ord]
  exp_acc <- t(as.matrix(valdata[valdata$question %in% l_labels_P[[i]], 12]))[, ord]
  # CIs
  acc_CI <- t(as.matrix(valdata[valdata$question %in% l_labels_P[[i]], c(7:8)]))[, ord]
  dice_CI <- t(as.matrix(valdata[valdata$question %in% l_labels_P[[i]], c(5:6)]))[, ord]
  
  # --- Plotting ---
  v_names <- c("Causes", "Impacts", "Mitigation", "Adaptation")
  par(mar=c(10,4,3,1))
  barplot(acc_dice, beside=TRUE, las=2, ylim=c(0, 1),  # 0.65
          col=cols[1:2], xaxt = "n")
  abline(h=seq(0, 0.6, length=7), lty=3, col="grey")
  grid()
  title(ylab="Accuracy / DICE")
  title(main=v_names[i], font.main=1)
  bp <- barplot(acc_dice, beside=TRUE, las=2, ylim=c(0,0.65),
                col=cols[1:2], add=TRUE, xaxt = "n")
  # Plot expected dice
  points(bp[2, ], exp_dice, cex=2, pch=19)
  points(bp[2, ], exp_dice, cex=1.5, pch=19, col=cols[2])
  # Plot expected accuracy
  points(bp[1, ], exp_acc, cex=2, pch=19)
  points(bp[1, ], exp_acc, cex=1.5, pch=19, col=cols[1])
  axis(2, las=2)
  axis(1, at=colMeans(bp), labels=l_indP_lab[[i]], las=2, cex.axis=0.9)
  
  # Add Error bars
   for(j in 1:ncol(acc_dice)) {
     # Accuracy
     xs <- 0.1
     segments(bp[1, j], acc_CI[1, j], bp[1, j], acc_CI[2, j], col="black", lwd=2)
     segments(bp[1, j]-xs, acc_CI[1, j], bp[1, j]+xs, acc_CI[1, j], col="black", lwd=2)
     segments(bp[1, j]-xs, acc_CI[2, j], bp[1, j]+xs, acc_CI[2, j], col="black", lwd=2)
     # Dice
     segments(bp[2, j], dice_CI[1, j], bp[2, j], dice_CI[2, j], col="black", lwd=2)
     segments(bp[2, j]-xs, dice_CI[1, j], bp[2, j]+xs, dice_CI[1, j], col="black", lwd=2)
     segments(bp[2, j]-xs, dice_CI[2, j], bp[2, j]+xs, dice_CI[2, j], col="black", lwd=2)
     
     # segments(bp[2, j], acc_dice[2, j], bp[2, j], exp_dice[j], col=cols[2], lwd=2)
   }
  
  
  if(i==1) legend(0, 1.05, legend=c("Accuracy", "Dice Similarity"), text.col=cols[1:2], bty="n", cex=1.25)
  # title(main=v_cats[i], font.main=1, cex.main=1.5, line=0.5)
  
} #end for: cats

dev.off()


# --------------------------------------------
# --------- Figure: Baserates ----------------
# --------------------------------------------

# Loop over four main paper topics
sc <- 1.2
pdf(paste0("Figures/Fig_Baserate_Mistral_F3.pdf"), width=10*sc, height=8*sc)

par(mfrow=c(2,2))

for(i in 1:4) {
  
  # ----- Prep Data -----
  # Fix Fabian's annoying alphabetical order
  vec <- valdata[valdata$question %in% l_labels_P[[i]], 1]
  ord <- order(factor(vec, levels = l_labels_P[[i]]))
  baserates <- t(as.matrix(valdata[valdata$question %in% l_labels_P[[i]], c(5,6)]))[, ord]
  
  # --- Plotting ---
  par(mar=c(10,4,3,1))
  barplot(baserates, beside=TRUE, las=2, ylim=c(0, 1),  # 0.65
          col=cols[c(4, 3)], xaxt = "n")
  abline(h=seq(0, 0.6, length=7), lty=3, col="grey")
  grid()
  title(ylab="Proportion")
  title(main=v_names[i], font.main=1)
  bp <- barplot(baserates, beside=TRUE, las=2, ylim=c(0,0.65),
                col=cols[c(4, 3)], add=TRUE, xaxt = "n")
  # Plot in expected dice
  # points(bp[2, ], exp_dice, cex=2, pch=19)
  axis(2, las=2)
  axis(1, at=colMeans(bp), labels=l_indP_lab[[i]], las=2, cex.axis=0.9)
  
  if(i==1) legend("topright", legend=c("Baserate Human", "Baserate AI"), text.col=cols[c(4, 3)], bty="n", cex=1.25)
  # title(main=v_cats[i], font.main=1, cex.main=1.5, line=0.5)
  
} #end for: cats

dev.off()


# --------------------------------------------
# --------- Numeric Results for Paper --------
# --------------------------------------------

valdata_ss_disp <- valdata[valdata$question %in% unlist(l_labels_P), ]

# Mean
mean(valdata_ss_disp$accuracy)
# Min
valdata_ss_disp[which.min(valdata_ss_disp$accuracy), ]
# Max
valdata_ss_disp[which.max(valdata_ss_disp$accuracy), ]



