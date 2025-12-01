# jonashaslbeck@protonmail.com; Nov 2025

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# Here we parse the JSON, do some checks, and binarize the data
# The output is the dataframe that is being used for analysis

# --------------------------------------------
# --------- Load Packages --------------------
# --------------------------------------------

# Database
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(jsonlite)
library(dplyr)

library(plyr)
library(RColorBrewer)

source("Helpers.R")
source("3_Plotting_Meta.R")


# --------------------------------------------
# --------- Version of AI Run ----------------
# --------------------------------------------
ai_handle <- "7.0.2-Mistral-Large-"


# --------------------------------------------
# --------- Load Data ------------------------
# --------------------------------------------

data_cmpl <- readRDS(paste0("Files/RawData_With_WordCount", ai_handle, ".RDS"))
dim(data_cmpl)


# --------------------------------------------
# --------- Processing JSON ------------------
# --------------------------------------------

N <- nrow(data_cmpl)
l_df <- list()

for(i in 1:N) {
  
  # Parse JSON into a list
  parsed <- fromJSON(data_cmpl$response[i])
  
  # Keep only the first element for each key
  first_only <- lapply(parsed, function(x) {
    if (length(x) > 1) x[[1]] else x
  })
  
  # Convert to a one-row dataframe
  l_df[[i]] <- as.data.frame(first_only, stringsAsFactors = FALSE)
  l_df[[i]]$url <- data_cmpl$url[i]
  l_df[[i]]$content <- data_cmpl$content[i]
  l_df[[i]]$newspaper <- data_cmpl$newspaper[i]
  l_df[[i]]$publication_date <- data_cmpl$publication_date[i]
  l_df[[i]]$year <- data_cmpl$year[i]
  l_df[[i]]$wordcount <- data_cmpl$wordcount[i]
  
  print(paste0(i, "/", N))
  
} # end for


# --------------------------------------------
# --------- Analyze JSON Failures ------------
# --------------------------------------------

if(ai_handle == "7.0.2-Mistral-Large-") n_columns <- 90
v_ncol <- unlist(lapply(l_df, ncol))
table(v_ncol) 
1-mean(v_ncol==n_columns)
# Only for a single article the parsing failed
# And this case we exclude
if(sum(v_ncol != n_columns) > 0) {
  l_df_cl <- l_df[-which(v_ncol != n_columns)] 
} else {
  l_df_cl <- l_df
}

# --------------------------------------------
# --------- Combine into Dataframe -----------
# --------------------------------------------

data_pr <- do.call(rbind, l_df_cl)
dim(data_pr)


# --------------------------------------------
# --------- Check Year Variable --------------
# --------------------------------------------

table(is.na(data_pr$year)) # no problem!
table(data_pr$year) # no problem !


# --------------------------------------------
# --------- Rename Newspapers ----------------
# --------------------------------------------

data_pr$newspaper[data_pr$newspaper == "zeit"] <- "zeit.de"
data_pr$newspaper[data_pr$newspaper == "spiegel-online"] <- "spiegel.de"


# --------------------------------------------
# --------- Exclude Tagesschau Articles ------
# --------------------------------------------

outlets = c("taz.de", "zeit.de", "spiegel.de", "sueddeutsche.de", "faz.net", "welt.de", "bild.de", "theguardian.com")
data_pr <- data_pr[data_pr$newspaper %in% outlets, ]
# Because: Some Tagesschau article in the sample, which is unwanted


# --------------------------------------------
# --------- Recode 4-Responses ---------------
# --------------------------------------------

source("3_Plotting_Meta.R")

# ----- Get proportion of the four response options -----
tb <- table(as.character(unlist(data_pr[, unlist(l_labels_P)]))) # 28 core questions
round(prop.table(tb), 5)
tbn <- prop.table(tb)[-5]
names(tbn) <- c("None", "Weakly", "Strongly", "Mentioned")
round(tbn , 4)

# ----- Recode responses before binarization -----
if(ai_handle=="7.0.2-Mistral-Large-") {
  data_pr_cop <- data_pr_cop_i <- data_pr[, -(1:8)]
  data_pr_cop[data_pr_cop_i == "0"] <- 1
  data_pr_cop[data_pr_cop_i == "1"] <- 2
  data_pr_cop[data_pr_cop_i == "2"] <- 3
  data_pr_cop[data_pr_cop_i == "3"] <- 4
  data_pr[, -(1:8)] <- data_pr_cop
}


# --------------------------------------------
# --------- Recode Binary-Responses ----------
# --------------------------------------------

data_pr[data_pr == "no"] <- 0
data_pr[data_pr == "yes"] <- 1


# --------------------------------------------
# --------- Binarize -------------------------
# --------------------------------------------

if(ai_handle == "7.0.2-Mistral-Large-") not <- c(1:8, 83:90) # For Final F3 Mistral Model

source("3_Plotting_Meta.R")
data <- data_pr
data[, -not] <- ifelse(data[, -not] > 1, 1, 0)


# --------------------------------------------
# --------- Save -----------------------------
# --------------------------------------------

saveRDS(data, paste0("Files/Df_binarized_rdy_", ai_handle, ".RDS"))
dim(data)

