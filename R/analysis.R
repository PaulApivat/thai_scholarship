# R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"

# load previously saved data
load(file = "thaischolarship.RData")

# libraries
library(tidyverse)
library(tibble)
library(treemap)
library(ggridges)
library(RColorBrewer)

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

### Tidy data ###
# note: we would not need ‘granted’, ‘granted_plus_purpose’ or ‘percentage’ for tidy data - 
# it could be easily calculated (create new data frame df2a, but do not select unneeded columns)

# select subset of data frame to separate “scholarships” from “purpose”

# scholarship
df2 %>%
    select(id, countries, KING, CSC, MFA, MOST, MOPH, HEC, ODOS, IPST) -> df2a

# purpose
df2 %>%
    select(id, countries, purpose_study, purpose_training, purpose_other) -> df2b

# to create two new columns - scholarships and grants
df2a <- df2a %>%
    gather(`KING`, `CSC`, `MFA`, `MOST`, `MOPH`, `HEC`, `ODOS`, `IPST`, key = "scholarship_provider", value = "number_recipient")

df2b <- df2b %>%
    gather(`purpose_study`, `purpose_training`, `purpose_other`, key = "purpose", value = "number_people")

# join (create new data frame to separate from two previous one)
library(tidyverse)
df2c <- left_join(df2a, df2b)

#### TreeMap ####
library(treemap)

tree <- treemap(df2c, 
	index = c("scholarship_provider", "countries"), 
	vSize = "number_recipient", 	
	type = "index", 
	fontsize.labels = c(18,12), 
	fontcolor.labels = c("black", "white"), 
	bg.labels = c("transparent"), 
	inflate.labels = FALSE, 
	border.col = c("black", "white"), 
	border.lwds = c(7,2), 
	palette = "Set2"
)

### Barchart
## Number of Scholarships granted by Type of Scholar (filled with ‘purpose’)



