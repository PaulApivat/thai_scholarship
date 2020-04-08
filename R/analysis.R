# R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"

# load previously saved data
load(file = "thaischolarship.RData")

# libraries
library(tidyverse)
library(tibble)
library(treemap)
library(ggridges)
library(RColorBrewer)
library(ggthemes)
library(countrycode)

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

# change scholarship_provider from character to factor
df2c_factor <- df2c
df2c_factor$scholarship_provider <- as.factor(df2c_factor$scholarship_provider)

# re-order factor
df2c_factor$scholarship_provider <- factor(df2c_factor$scholarship_provider, levels = c("MOST", "ODOS", "CSC", "IPST", "HEC", "KING", "MOPH", "MFA"))

# plot - bar_purpose
library(ggthemes)

bar_purpose <- ggplot(data = df2c_factor, mapping = aes(x = scholarship_provider, y = number_recipient, fill = purpose)) 
+ geom_bar(stat = "identity") 
+ scale_fill_manual(values = c("#A51931", "#F4F5F8", "#2D2A4A"), name = "The Purpose", guide = guide_legend(reverse = TRUE), labels = c("Other", "Study", "Training")) 
+ theme_economist() 
+ labs(x = "Scholarship Provider", y = "Number of Scholarship Recipient", fill = "Purpose", title = "Number of Scholarships granted by Type") 
+ theme(legend.position = "bottom")

# create region / continent
df2c_factor[,"region"] <- NA
colnames(df2c_factor)[7] <- "continent"


# use countrycode package to derive continent from country name
library(countrycode)

df2c_factor$continent <- countrycode(sourcevar = df2c_factor[,"countries"], origin = "country.name", destination = "continent")

# Tried to change the spelling of Netherland(s)
df2c_factor$countries[df2c_factor$countries == "NETHERLAND"] <- "NETHERLANDS"
# but first had to change from factor to character
df2c_factor$countries <- as.character(df2c_factor$countries)
# then back as factor
df2c_factor$countries <- as.factor(df2c_factor$countries)
# Then re-run countrycode package (correctly classifying Netherlands as Europe)
df2c_factor$continent <- countrycode(sourcevar = df2c_factor[,"countries"], origin = "country.name", destination = "continent")

# plot - bar_country
bar_country <- ggplot(data = df2c_factor, mapping = aes(x = scholarship_provider, y = number_recipient, fill = continent)) 
+ geom_bar(stat = "identity") 
+ coord_flip() 
+ labs(x = "Scholarship Provider", y = "Number of Scholarship Recipients", fill = "Continent", title = "Number of Scholarships by Continent") 
+ scale_fill_manual(values = c("#ca0020", "#f4a582", "#ffffbf", "#92c5de", "#0571b0")) 
+ theme_economist() 
+ theme(legend.position = "bottom")

### Circular Bar Plot (basic)
## Note: Need to delete every 2nd and 3rd row from df2c_factor 
## to create circular bar plot with only "unique observations"
## NOT multiplied by three because 'purpose' included (e.g., see df2c_factor id)
## IMPORTANT NOTE: ylim() is importatn for circular bar plot because scale is off if not applied correctly


# every 3rd row
df2c_factor %>%
    filter(row_number() %% 3 != 1) -> df2d

# delete every EVEN row (every 2nd row)
df2d %>%
    filter(row_number() %% 2 != 0) -> df2d

# plot
circle_bar <- ggplot(data = df2d, aes(x=countries, y=number_recipient, fill=scholarship_provider)) 
+ geom_bar(stat = "identity", fill = alpha("blue", 0.3)) 
+ ylim(-100, 1100) 
+ theme_minimal() 
+ coord_polar(start = 0)



