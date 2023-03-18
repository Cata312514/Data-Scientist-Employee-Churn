#############################################
# APRIORI ALGORITM
## Step 2. Exploring and Preparing the data ----
# Load the grocery data into sparse matrix
library(arules)
library(Amelia)
library(ggplot2)
library(tidyverse)
library(plyr)
library(cluster)
library(lattice)
library(graphics)
library(grid)
library(gridExtra)

################################################################################
####################################
# PHASE 3: Model Planning

# Extract data
setwd("C:/Users/ataca/OneDrive/MSCS Classes/Spring 2021/DS515 Data Science/TeamProject/")

aug_train <- read.csv('aug_train3.csv', header = FALSE, na.strings=c("", "NA"))
str(aug_train)
head(aug_train)

# EDA
# Convert df to transactions
# most freq items: STEM, REMAIN, No_enrollment, Has relevant exp, Male, Other
library(arules)
train_tran <-read.transactions('aug_train3.csv', sep =",", rm.duplicates = TRUE) # , na.rm=TRUE)
summary(train_tran)

# Convert dataframe to transaction
# train_tran <-as(aug_train, "transactions")
# summary(train_tran)

# add transaction ID
#tid <- transactionInfo(train_trans)[["transactionID"]] 

# Look at the first 5 transactions
inspect(train_tran[1:5])

library(RColorBrewer)

# Examine the frequency of items
itemFrequency(train_tran[, 1:3])

# Plot the frequency of items
itemFrequencyPlot(train_tran, support = 0.2, col =brewer.pal(8, 'Pastel2'))

# Show top 20 frequency items
# STEM, Remain, no_enrollment, Has Rel. exp,, Male, Grad, Pvt Ltd,
# and 1 yr gap between current and last job 
# are the top 8 most Frequent items.
itemFrequencyPlot(train_tran, topN= 10, col =brewer.pal(20, 'Pastel2')) 

## Step 3: Training a model on the data ----
library(arules)

# default settings result in zero rules learned
ap_rules <- apriori(train_tran)
ap_rules # 465 rules
# set better support and confidence levels to learn more rules

# Support: 40% 
# Confidence: 80%
# 23 Rules output
train_rules <-apriori(train_tran, parameter = list(support = 
                        0.40, confidence = 0.80, minlen = 2, maxlen = 10))
train_rules

# Select top 10 rules from train_rules having the highest support
top10Rules <- head(train_rules, n = 10, by = "support")

# Look at the top10Rules
inspect(top10Rules)

# Plot the rules using htmlwidget engine
library(arulesViz)

plot(top10Rules, method ="graph", engine = "htmlwidget")

## Step 4" Evaluating the model performance ----
# Summary of grocery association rules
summary(train_rules) # 8 rules with 2 items, 15 rules with 3 items

# Look at the first five rules
inspect(train_rules[1:5])
inspect(train_rules)

## Step 5: Improving model performance ----

# Sorting grocery rules by lift
inspect(sort(train_rules, by = "lift")[1:10])

# Factors that is correlated to "Remain" at 40% Support
subsettrules <- subset(train_rules, items %ain% c("Remain"))
inspect(subsettrules) #8 rules with (male, pvt ltd, HRE, STEM, no Enr, Grad)

plot(subsettrules, method ="graph", engine = "htmlwidget")

subsettrules <- subset(train_rules, items %ain% c("Has relevent experience", "Remain"))
inspect(subsettrules) # 6 rules with Male, STEM, no_enrollment factors 

subsettrules <- subset(train_rules, items %ain% c("Male", "Remain"))
inspect(subsettrules) # 2 rules

subsettrules <- subset(train_rules, items %ain% c("STEM", "Remain"))
inspect(subsettrules) # 2 rules

subsettrules <- subset(train_rules, items %ain% c("no_enrollment", "Remain"))
inspect(subsettrules) # 4 rules

subsettrules <- subset(train_rules, items %ain% c("Has relevent experience", "Male", "Remain"))
inspect(subsettrules) # 1 rule

subsettrules <- subset(train_rules, items %ain% c("Has relevent experience", "STEM", "Remain"))
inspect(subsettrules) # 2 rules

subsettrules <- subset(train_rules, items %ain% c("Has relevent experience", "no_enrollment", "Remain"))
inspect(subsettrules) # 3 rules

#####


# Writing the rules to a CSV files
write(train_rules, file = "train_rules40.csv", sep =",", quote = TRUE, row.names = FALSE)

# Converting the rule set to a data frame
train_rules40_df <-as(train_rules, "data.frame")
str(train_rules40_df)

head(train_rules40_df)

################ Support at 35%
# Step 3: Training a model on the data ----
library(arules)
# 44 Rules output
train_rules <-apriori(train_tran, parameter = list(support = 
                        0.35, confidence = 0.80, minlen = 2, maxlen = 10))
train_rules

library(arulesViz)
# Select top 10 rules from train_rules having the highest support
top10Rules <- head(train_rules, n = 10, by = "support")

# Look at the top10Rules
inspect(top10Rules)

# Plot the rules using htmlwidget engine
plot(top10Rules, method ="graph", engine = "htmlwidget")

## Step 4" Evaluating the model performance ----
# Summary of grocery association rules
summary(train_rules) # 8 rules with 2 items, 27 rules with 3 items, 9 rules with 4 items
# other support statistics are not close to the min value

# Look at the first five rules
inspect(train_rules[1:5])

## Step 5: Improving model performance ----

# Sorting grocery rules by lift
inspect(sort(train_rules, by = "lift")[1:10])

# Subset with 1 item 
subsettrules <- subset(train_rules, items %ain% c("Remain"))
inspect(subsettrules) # 18 rules with (Pvt Ltd, HRE, Grad, STEM, MALE, no Enrol, = factors that correlates to Remain

# Finding subsets of rules containing multiple items
subsettrules <- subset(train_rules, items %ain% c("Has relevent experience", "Remain"))
inspect(subsettrules) # 15 rules (Grad, Male, no Enrol, STEM)

subsettrules <- subset(train_rules, items %ain% c("Has relevent experience", "Remain", "no_enrollment"))
inspect(subsettrules) # 9 rules (with STEM, Male)

subsettrules <- subset(train_rules, items %ain% c("Has relevent experience", "Remain", "STEM"))
inspect(subsettrules) # 5 rules (with no enroll)

subsettrules <- subset(train_rules, items %ain% c("Has relevent experience", "Remain", "Male"))
inspect(subsettrules) # 4 rules (with no enroll)

subsettrules <- subset(train_rules, items %ain% c("Remain", "Pvt Ltd"))   # "Has relevent experience",
inspect(subsettrules) # 3 rules

subsettrules <- subset(train_rules, items %ain% c("Graduate", "Pvt Ltd"))   # "Has relevent experience",
inspect(subsettrules) # 3 rules

subsettrules <- subset(train_rules, items %ain% c("Has relevent experience", "Remain", "Graduate"))
inspect(subsettrules) # 1 rule
###
subsettrules <- subset(train_rules, items %ain% c("Churn"))
inspect(subsettrules) # no result
####
subsettrules <- subset(train_rules, items %ain% c("Has relevent experience"))
inspect(subsettrules) # 36 rules

subsettrules <- subset(train_rules, items %ain% c("Graduate"))
inspect(subsettrules) # 8 rules

subsettrules <- subset(train_rules, items %ain% c("Male"))
inspect(subsettrules) # 13 rules


# Save to CSV file
write(train_rules, file = "train_rules35.csv", sep =",", quote = TRUE, row.names = FALSE)

# Converting the rule set to a data frame
train_rules35_df <-as(train_rules, "data.frame")
str(train_rules35_df)
head(train_rules35_df)


################ Support at 30%
# Step 3: Training a model on the data ----
library(arules)
# 73 Rules output
train_rules <-apriori(train_tran, parameter = list(support = 
                        0.30, confidence = 0.80, minlen = 2, maxlen = 10))
train_rules

# Plot the rules using htmlwidget engine
library(arulesViz)
# Select top 10 rules from train_rules having the highest support
top10Rules <- head(train_rules, n = 10, by = "support")

# Look at the top10Rules
inspect(top10Rules)

# Plot the rules using htmlwidget engine
plot(top10Rules, method ="graph", engine = "htmlwidget")
## Step 4" Evaluating the model performance ----
# Summary of grocery association rules
summary(train_rules) # 8 rules with 2 items, 37 rules with 3 items, 28 rules with 4 items
# other support statistics are close to the min value

# Look at the first five rules
inspect(train_rules[1:5])

## Step 5: Improving model performance ----

# Sorting grocery rules by lift
inspect(sort(train_rules, by = "lift"))
# at Pvt Ltd company, DS has relevant experience, STEM, Male, Grad, no enrollment
# Subset with 1 item 
subsettrules <- subset(train_rules, items %ain% c("Remain"))
inspect(subsettrules) # 18 rules with (Pvt Ltd, MALE, HRE, Grad, STEM, no Enrol, = factors that correlates to Remain

# Finding subsets of rules containing multiple items
subsettrules <- subset(train_rules, items %ain% c("Has relevent experience", "Remain"))
inspect(subsettrules) # 25 rules (Grad, Male, no Enrol, STEM)

subsettrules <- subset(train_rules, items %ain% c("Has relevent experience", "Remain", "no_enrollment"))
inspect(subsettrules) # 12 rules

subsettrules <- subset(train_rules, items %ain% c("Has relevent experience", "Remain", "STEM"))
inspect(subsettrules) # 12 rules (with no enroll)

subsettrules <- subset(train_rules, items %ain% c("Has relevent experience", "Remain", "Pvt Ltd"))
inspect(subsettrules) # 8 rules

subsettrules <- subset(train_rules, items %ain% c("Has relevent experience", "Remain", "Male"))
inspect(subsettrules) # 6 rules (with no enroll)

subsettrules <- subset(train_rules, items %ain% c("Has relevent experience", "Remain", "Graduate"))
inspect(subsettrules) # 3 rule
###
subsettrules <- subset(train_rules, items %ain% c("Churn"))
inspect(subsettrules) # no result
####
subsettrules <- subset(train_rules, items %ain% c("Has relevent experience"))
inspect(subsettrules) # 55 rules

subsettrules <- subset(train_rules, items %ain% c("Graduate"))
inspect(subsettrules) # 15 rules

subsettrules <- subset(train_rules, items %ain% c("Male"))
inspect(subsettrules) # 22 rules


# Save to CSV file
write(train_rules, file = "train_rules30.csv", sep =",", quote = TRUE, row.names = FALSE)

# Converting the rule set to a data frame
train_rules35_df <-as(train_rules, "data.frame")
str(train_rules35_df)
head(train_rules35_df)


#### NOT INCLUDED
## 115 Rules output
train_rules <-apriori(train_tran, parameter = list(support = 
                        0.25, confidence = 0.80, minlen = 2, maxlen = 10))
train_rules

