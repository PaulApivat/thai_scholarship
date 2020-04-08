# R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"

# load previously saved data
load(file = "thaischolarship.RData")

# libraries
library(tidyverse)
library(tibble)
library(treemap)
library(ggridges)

# loading data
df <- read.csv("/Users/paulapivat/Desktop/data_go_th/thai_scholarship.csv")

######------- Data Cleaning ------######
df2 <- df

# change column names
colnames(df2)[1:16] <- c("id", "countries", "KING", "CSC", "MFA", "MOST", "MOPH", "HEC", "ODOS", "IPST", "granted", "purpose_study", "purpose_training", "purpose_other", "granted_plus_purpose", "percentage")

# add new column ‘purpose_total’ to separate “purpose” from “scholarships granted” (doesn’t make sense to combine them)
# use library(tibble)
df2 <- add_column(df2, purpose_total = NA, .after = "purpose_other")

# change columns from factor to numeric
df3 <- df2
df3[,3:16] <- sapply(df3[,3:16],as.numeric)

# now add purpose_study, purpose_training and purpose_other to equal “purpose_total” (remove NA)
df3$purpose_total <- rowSums(df3[,c("purpose_study", "purpose_training", "purpose_other")], na.rm = TRUE)

# add rows from KING - IPST to equal “granted” column (remove NA)
df3$granted <- rowSums(df3[,c("KING", "CSC", "MFA", "MOST", "MOPH", "HEC", "ODOS", "IPST")], na.rm = TRUE)

# add rows “granted” + “purpose_total” to “granted_plus_total”
df3$granted_plus_purpose <- rowSums(df3[,c("granted", "purpose_total")], na.rm = TRUE)

# save back to df2
df2 <- df3