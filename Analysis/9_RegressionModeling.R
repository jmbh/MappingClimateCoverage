# jonashaslbeck@protonmail.com; Nov, 2025

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# Compute main result figure, controlling for article length and type of article

# Dependent:
# Independent:
# - questions (indicator)
# - topics (indicator, nesting)
# - newspaper
# - type of article
# - article length

# --------------------------------------------
# --------- Load Packages --------------------
# --------------------------------------------

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

library(plyr)
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
# --------- Prepare Dataset ------------------
# --------------------------------------------

# ----- Rework dataset -----
# We need variables as questions variable

# Subset to key questions
data_bin_ss_pr <- data_bin_ss[, c("url", "newspaper", "year", "wordcount", "type_of_article", unlist(l_labels_P))]

# Kick out wordcounts larger than 4k
hist(data_bin_ss_pr$wordcount) # Okay, need to get rid of the large ones
data_bin_ss_pr <- data_bin_ss_pr[data_bin_ss_pr$wordcount <= 4000, ]

nrow(data_bin_ss) - nrow(data_bin_ss_pr)
nrow(data_bin_ss_pr)

# Turn into Long-Format
data_bin_ss_pr_long <- reshape2::melt(data_bin_ss_pr, id.vars=c("url", "newspaper", "year", "wordcount", "type_of_article"), measure.vars=unlist(l_labels_P), variable.name="question", value.name="response")

# Compile clean dataframe
df <- data.frame(matrix(NA, nrow(data_bin_ss_pr_long), 6))
colnames(df) <- c("response", "question", "topic", "newspaper", "articletype", "wordcount")
df$response <- data_bin_ss_pr_long$response
df$question <- data_bin_ss_pr_long$question

# Add topic
group_lookup <- setNames(
  rep(1:4, sapply(l_labels_P, length)),   # group indices 1..4 repeated
  unlist(l_labels_P)                      # names = all question labels
)
question_group <- group_lookup[as.character(data_bin_ss_pr_long$question)]
question_group <- factor(
  question_group,
  levels = 1:4,
  labels = c("causes", "impacts", "mitigation", "adaptation")
)
df$topic <- question_group
df$newspaper <- factor(data_bin_ss_pr_long$newspaper, levels=outlets)
df$articletype <- factor(data_bin_ss_pr_long$type_of_article,
                         levels = c("0", "1", "2"),
                         labels = c("Report", "Interview", "Opinion Piece"))
df$wordcount <- data_bin_ss_pr_long$wordcount

head(df)
dim(df)
nrow(df)/28 # Sanity check: should be the sample size


# --------------------------------------------
# --------- Frequentist Fixed Effects Model --
# --------------------------------------------

# ---- Fit Model -----
# out <- glm(response ~ question*newspaper + articletype*newspaper + wordcount*newspaper + I(wordcount^2)*newspaper, family = "binomial", data=df)
# saveRDS(out, "Files/Model_glm.RDS")
out <- readRDS("Files/Model_glm.RDS")
sum_out <- summary(out)

# ---- Model Checking ----
df$predicted <- predict(out, type = "response")
prop_Qs_mod <- tapply(df$predicted, df$question, mean)
prop_Qs_emp <- tapply(df$response, df$question, mean)
plot(prop_Qs_mod, prop_Qs_emp)
abline(0,1) # Modeled perfectly, as expected

# ---- Some model checking ----
# Estimates
hist(sum_out$coefficients[, 1])
# SEs
hist(sum_out$coefficients[, 2]) # Not necessarilt a red flag; no massive outlies

# --- Reporting Reporting parameters ---
round(sum_out$coefficients, 3)
round(sum_out$coefficients[35:38, ], 4)


# ----- Compute Marginal Effects -----

# grid of combos at fixed covariates
nd <- expand.grid(
  question  = levels(df$question),
  newspaper = levels(df$newspaper)
)

v_types <- levels(df$articletype)
l_preds <- list()
for(i in 1:3) {
  # add the remaining model terms with correct types
  nd$articletype <- factor(v_types[i], levels = levels(df$articletype))
  nd$wordcount   <- mean(df$wordcount, na.rm = TRUE)
  # predicted probabilities (on response scale)
  preds <- predictions(out, newdata = nd, type = "response")
  pred_report_i <- data.frame(preds[, c("estimate", "question", "newspaper")])
  l_preds[[i]] <- pred_report_i
  print(i)
}

# Get Empirical Distribution of Article Types
tb <- prop.table(table(data_bin_ss_pr$type_of_article))

# Weighted sum, using empirical distribution of article types
av_est <- l_preds[[1]]$estimate*tb[1] + l_preds[[2]]$estimate*tb[2] + l_preds[[3]]$estimate*tb[3]
pred_report_av <- pred_report_i
pred_report_av$estimate <- av_est
head(pred_report_av)

# To wide format
pred_report_wide <- pred_report_av |>
  pivot_wider(
    id_cols    = newspaper,           # <- belangrijk
    names_from = question,
    values_from = estimate
  )

head(pred_report_wide)
dim(pred_report_wide)

# --------------------------------------------
# --------- Figure ---------------------------
# --------------------------------------------

# Loop over four main paper topics
sc <- 1.2
pdf(paste0("Figures/", ai_handle, "/Fig_barplots_Model_Implied_", filter, ".pdf"), 
    width=10*sc, height=7.2*sc)

par(mfrow=c(2,2))
title <- NULL
for(i in 1:4) {
  
  # --- Get Relevant proportions ---
  tab_i <- as.matrix(pred_report_wide[, l_labels_P[[i]]])
  
  # --- Plotting ---
  par(mar=c(4,4,1,1))
  ymax <- 0.85*100
  barplot(tab_i*100, beside=TRUE, las=2, ylim=c(0, ymax), 
          col=cols[row(tab_i)], xaxt = "n", axes=FALSE)
  abline(h=seq(0, 0.8, length=13)*100, lty=3, col="grey")
  bp <- barplot(tab_i*100, beside=TRUE, las=2, ylim=c(0,ymax),
                col=cols[row(tab_i)], add=TRUE, xaxt = "n", axes=FALSE)

  # axis(1, at=colMeans(bp), labels=l_indP_lab[[i]], las=2, cex.axis=0.9)
  axis(1, at=colMeans(bp), labels=FALSE, las=1, cex.axis=0.75)
  text(
    x = colMeans(bp),
    y = - 7,
    labels = l_indP_lab_lb[[i]],
    xpd = NA,
    srt = 0,          
    adj = 0.5,
    cex = 0.90
  )
  
  axis(2, las=2, at=seq(0, ymax, by=10), labels=paste0(seq(0, ymax, by=10), "%"))
  
  if(i==4) legend("topright", legend=outlets, text.col=cols, bty="n", cex=1.25)
  if(is.null(title)) {
    title(main=v_cats[i], font.main=1, cex.main=1.5, line=-1) 
  } else {
    title(title, font.main=1, cex.main=1.5, line=-1)
  }
  
} # end loop
dev.off()


# --------------------------------------------
# --------- Variance Explained / Question ----
# --------------------------------------------

# Original probabilities
m_props_2x2 <- readRDS(paste0("Files/Datasum_prop_2x2_", filter, "_", ai_handle, ".RDS"))
dim(m_props_2x2)
# Model Implied
tab_i <- pred_report_wide[, -1]
dim(tab_i)

# Make output data frame
m_expl_var <- as.data.frame(matrix(NA, 28, 5))
lab_wCat <- paste0(unlist(l_indP_lab), c(rep(" (C)", 7),
                                         rep(" (I)", 7),
                                         rep(" (M)", 7),
                                         rep(" (A)", 7)))
rownames(m_expl_var) <- lab_wCat
# rownames(m_expl_var) <- lab_wCat
colnames(m_expl_var) <- c("Prop(E)", "Prop(M)", "Var(E)", "Var(M)", "1 - Var(M)/Var(E)")
for(i in 1:28) {
  # Base Rate: Empirical
  m_expl_var$`Prop(E)`[i] <- mean(m_props_2x2[, i])
  # Base Rate: Model-implied
  m_expl_var$`Prop(M)`[i] <- mean(unlist(tab_i[, i]))
  # Variance: Empirical
  m_expl_var$`Var(E)`[i] <- var(m_props_2x2[, i])
  # Variance: Model-implied
  m_expl_var$`Var(M)`[i] <- var(tab_i[, i])
  # Ratio
  m_expl_var$`Var(M)/Var(E)`[i] <- 1 - (var(tab_i[, i]) / var(m_props_2x2[, i]))
}
# Round
m_expl_var <- round(m_expl_var, 4)
m_expl_var

# Some analysis of negative R2
# plot(m_expl_var$`Base Rate`, m_expl_var$`%Variance Explained Away`)

# ----- Export with Xtable -----
xtable(m_expl_var, digit=c(1,4,4,4,4,4))



x <- unlist(tab_i[, 16]) # Model
y <- m_props_2x2[, 16]
barplot(y)

cbind(round(x,2), round(y, 2))

sd(x) - sd(y)




