# jonashaslbeck@protonmail.com; Nov 2025

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# Helpers for the media analysis project


# -----------------------------------------
# ----- Plotting Cor Matrix ---------------
# -----------------------------------------


# Plotting function, works both with symmetric and not
plot_acc <- function(X, digits = 2, min, max, title, 
                     leg_title = "Correlation", 
                     symmetric = TRUE) {
  df <- as.data.frame(as.table(X))
  colnames(df) <- c("Var1", "Var2", "Accuracy")
  
  # keep only lower triangular if symmetric = TRUE
  if (symmetric) {
    df <- df[as.numeric(df$Var1) >= as.numeric(df$Var2), ]
  }
  
  ggplot(df, aes(x = Var1, y = Var2, fill = Accuracy)) +
    geom_tile() +
    geom_text(aes(label = ifelse(is.na(Accuracy), "", 
                                 round(Accuracy, digits))),
              color = "white", size = 2) +
    scale_fill_gradient(
      low = "blue", high = "red",
      limits = c(min, 1),
      na.value = "white"
    ) +
    coord_equal() +
    theme_minimal() +
    labs(
      title = title,
      x = "",
      y = "",
      fill = leg_title
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}


# --------------------------------------------
# --------- Summary Validation Results -------
# --------------------------------------------

Eval <- function(data, new=TRUE, decimals=2) {
  
  # Subset
  if(new) data <- data[data$question %in% unlist(l_indP_lab), ]
  
  out <- c(
    round(mean(data$accuracy), decimals),
    round(mean(data$dice), decimals),
    round(mean(data$dice/data$edice), decimals),
    round(mean(data$baserate), decimals),
    round(mean(data$baserate_ai), decimals),
    round(data$baserate_ai[data$question=="causes_human"], decimals)
  )
  names(out) <- c("accuracy", "dice", "dice/edice", "baserate_hum", "baserate_ai", "baserate_hc")
  
  return(out)
  
} # eof

# --------------------------------------------
# --------- Plot one Panel of Barplot Figure -
# --------------------------------------------

Plot1Barplot <- function(data, 
                         ind_qu, 
                         labels_qu, 
                         cols, 
                         outlets,
                         legend=FALSE, 
                         title=NULL, 
                         ymax=65, 
                         grid=TRUE, 
                         avBars=FALSE) {
  
  # --- Compute proportions ---
  n_col_i <- length(ind_qu)
  tab_i <- matrix(NA, nrow=length(outlets), ncol=n_col_i)
  rownames(tab_i) <- outlets
  colnames(tab_i) <- l_labels_P[[i]]
  n_outlets <- length(outlets)
  for(j in 1:n_outlets) {
    df_wide_j <- data[data$newspaper == outlets[j], ]
    tab_i[j, ] <- colMeans(df_wide_j[, ind_qu]) # / colMeans(df_wide_j_ds[, l_indP_lab[[i]]])
  }
  
  # --- Plotting ---
  par(mar=c(4,4,1,1))
  barplot(tab_i*100, beside=TRUE, las=2, ylim=c(0, ymax),  # 0.65
          col=cols[row(tab_i)], xaxt = "n", axes=F)
  if(grid) {
    abline(h=seq(0, ymax, length=9), lty=3, col="grey")
  }
  
  # Add average bars
  if(avBars) {
    av <- colMeans(tab_i)*100
    for(a in 1:7) {
      segments(bp[1,a]-0.5, av[a], bp[1,a]+6.5, av[a], col="black", lwd=4)
      segments(bp[1,a]-0.5, av[a], bp[1,a]+6.5, av[a], col="grey", lwd=2.4)
    }
  } # if: AV bar
  
  
  bp <- barplot(tab_i*100, beside=TRUE, las=2, ylim=c(0, ymax),
                col=cols[row(tab_i)], add=TRUE, xaxt = "n", axes=F)
  axis(2, las=2, at=seq(0, ymax, by=10), labels=paste0(seq(0, ymax, by=10), "%"))
  
  
  # ## Version Double Line
  # par(mgp = c(3, 1.5, 0)) 
  # axis(1, at=colMeans(bp), labels=labels_qu, las=1, cex.axis=0.75)
  # par(mgp = c(3, 1, 0)) 
  
  ## Version Doube line Force
  axis(1, at=colMeans(bp), labels=FALSE, las=1, cex.axis=0.75)
  text(
    x = colMeans(bp),
    y = - 7,
    labels = labels_qu,
    xpd = NA,
    srt = 0,           # rotate if needed
    adj = 0.5,
    cex = 0.90
  )
  
  # ## Version 45 degree Single Line
  # axis(1, at=colMeans(bp), labels=FALSE, las=1, cex.axis=0.75)
  # text(
  #   x = colMeans(bp), y = par("usr")[3]-2,
  #   labels = labels_qu,
  #   srt = 45, adj = 1, xpd = NA, cex = 0.75
  # )
  
  
  if(legend) legend("topright", legend=outlets, text.col=cols, bty="n", cex=1.25)
  if(is.null(title)) {
    title(main=v_cats[i], font.main=1, cex.main=1.5, line=-1.2) 
  } else {
    title(title, font.main=1, cex.main=1.5, line=-1.5)
  }
  
} # EoF


# --------------------------------------------
# --------- Parse JSON -----------------------
# --------------------------------------------

fix_json <- function(json_text) {
  # Parse as a list
  parsed <- fromJSON(json_text)
  
  # Detect "bad" structure: explanation keys that are long sentences, values repeating short answer
  keys <- names(parsed)
  new_list <- list()
  skip_next <- FALSE
  
  for (i in seq_along(keys)) {
    if (skip_next) {
      skip_next <- FALSE
      next
    }
    
    key <- keys[i]
    value <- parsed[[i]]
    
    # If this is a "good" style entry: array length >= 2, just keep it
    if (is.vector(value) && length(value) >= 2) {
      new_list[[key]] <- value
      next
    }
    
    # If "bad" style: next key is an explanation and has same value
    if (i < length(keys)) {
      next_key <- keys[i + 1]
      next_value <- parsed[[i + 1]]
      
      # Check if next key looks like an explanation (longer than 20 chars and contains spaces)
      if (nchar(next_key) > 20 && grepl(" ", next_key) &&
          identical(as.character(value), as.character(next_value))) {
        new_list[[key]] <- c(value, next_key)
        skip_next <- TRUE
      } else {
        # Just keep as single element (no explanation found)
        new_list[[key]] <- c(value, NA)
      }
    } else {
      # Last key: keep as is
      new_list[[key]] <- c(value, NA)
    }
  }
  
  return(new_list)
}



# --------------------------------------------
# --------- Compute Jaccard for 2 vectors ----
# --------------------------------------------

jaccard_index <- function(vec1, vec2) {
  # Ensure both vectors are binary (0/1)
  if (!all(vec1 %in% c(0, 1)) | !all(vec2 %in% c(0, 1))) {
    stop("Both vectors must be binary (0 or 1).")
  }
  
  # Compute intersection and union
  intersection <- sum(vec1 & vec2)
  union <- sum(vec1 | vec2)
  
  # Handle case where both vectors are all zeros
  if (union == 0) {
    return(NA)
  }
  
  # Compute Jaccard index
  jaccard <- intersection / union
  return(jaccard)
}


# --------------------------------------------
# --------- Little JSON fix ------------------
# --------------------------------------------

LittleJSONfix <- function(x) {
  
  new_string <- gsub("```", "", x)
  new_string <- gsub("json", "", new_string)
  new_string
}

# --------------------------------------------
# --------- Line Break For Labels ------------
# --------------------------------------------

# Function to split labels into two lines if needed
split_labels <- function(labels) {
  sapply(labels, function(lbl) {
    words <- unlist(strsplit(lbl, " "))
    if (length(words) > 1) {
      paste(words, collapse = "\n")  # Insert a line break
    } else {
      lbl  # Keep as is
    }
  })
}



# --------------------------------------------
# --------- Stuff Adapted from Fabian --------
# --------------------------------------------

map_yesno <- function(answer_vec) {
  x <- tolower(answer_vec)
  x[x == 'no'] <- 0
  x[x == 'yes'] <- 1
  
  # Get the other questions, remove % sign of the percentage climate change question
  x[x != 'yes' & x != 'no'] <- str_sub(x[x != 'yes' & x != 'no'], 0, 1)
  as.numeric(x)
}

get_newspaper <- function(url) {
  newspapers <- c(
    'taz.de', 'sueddeutsche.de', 'spiegel.de', 'zeit.de', 'news.google',
    'faz.net', 'welt.de', 'bild.de', 'tagesschau.de', 'theguardian.com'
  )
  
  newspapers[str_detect(url, newspapers)]
}

# get_wide_df2 <- function(df) {
#   
#   df_wide <- c()
#   question_names <- names(parse_json(df$response[1]))
#   
#   for (i in seq(nrow(df))) {
#     url <- df$url[i]
#     newspaper <- df$newspaper[i]
#     
#     response <- tryCatch(parse_json(df$response[i]),
#                          error = function(e) {
#                            print(e)
#                            NULL
#                          }
#     )
#     
#     if (!is.null(response)) {
#       
#       response <- map_yesno(sapply(response, `[`, 1))
#       
#       
#       df_wide <- rbind(
#         df_wide,
#         c(url, newspaper, response)
#       )
#     }
#   }
#   
#   colnames(df_wide) <- c('url', 'newspaper', question_names)
#   data.frame(df_wide) %>% 
#     mutate(across(-c(url, newspaper), as.numeric))
# } # eoF


get_wide_df <- function(df) {
  
  df_wide <- c()
  question_names <- names(parse_json(df$response[1]))
  nr_questions <- length(question_names)
  
  for (i in seq(nrow(df))) {
    url <- df$url[i]
    newspaper <- get_newspaper(url)
    
    response <- tryCatch(parse_json(df$response[i]),
                         error = function(e) {
                           print(e)
                           NULL
                         }
    )
    
    if (!is.null(response)) {
      
      response <- map_yesno(sapply(response, `[`, 1))
      df_wide <- rbind(df_wide,
                       c(newspaper, response[seq(nr_questions)], url))
    } # end if: isnull
    
    # print(i)
  } # end for
  
  colnames(df_wide) <- c('url', 'newspaper', question_names)
  df_final <- data.frame(df_wide) %>% 
    mutate(across(-c(url, newspaper), as.numeric))
  
  return(df_final)
}




# ------------------------------------------
# -------- Plot Labels ---------------------
# ------------------------------------------

PlotLabel <- function(text, srt=0, cex=1.5,
                      xpos=0.5, ypos=0.5) {
  
  par(mar=rep(0, 4))
  
  plot.new()
  plot.window(xlim=c(0, 1), ylim=c(0,1))
  text(x=xpos, y=ypos, labels=text, srt=srt, cex=cex, adj=0.4)
  
}

