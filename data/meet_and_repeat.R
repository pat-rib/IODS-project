#Patricia Carvalho Ribeiro

# 01.12.20

#Exercise 6
library(dplyr)
library(tidyr)
library(ggplot2)

# 1 

# Load the datasets (BPRS and RATS) into R:
BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = TRUE, sep = '\t')

# Look at the column names, the structure, and summaries of the datasets
names(BPRS)
names(RATS)
str(BPRS)
# The data-frame has 40 observations and 11 variables. The dataset is taken from Davis (2002). The subjects were 40 male randomly assigned to one of two treatment groups. The subjects received a rate on the brief psychiatric rating scale (BPRS) measured before treatment began (week 0) and then at weekly intervals for eight weeks. 

str(RATS)
# the dataset has 16 obs and 13 variables. The data is from a nutrition study conducted in three groups of rats (Crowder and Hand, 1990) puted on different diets, where each animal’s body weight (grams) was recorded repeatedly (approximately weekly) over a 9-week period. 
summary(BPRS)
# the variables 'treatment' and 'subject' are not aligned. The variables related to weeks are aligned. 
summary(RATS)
# same observation about the variables: weeks are aligned. ID and Group are not.

# 2

# Convert the categorical variables of both data sets to factors. (1 point)
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)
RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)

# 3

# Convert the data sets to long form. Add a week variable to BPRS and a Time variable to RATS
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject) %>% mutate(week = as.integer(substr(weeks,5,5)))
RATSL <- RATS %>% gather(key = WD, value = Weight, -ID, -Group) %>% mutate(Time = as.integer(substr(WD, 3, 4))) 
glimpse(BPRSL)
glimpse(RATSL)

# 4

# Compare the long dataset version with their wide form versions.
# Check the variable names, view the data contents and structures, and create some brief summaries of the variables. 
names(BPRSL)
names(RATSL)
str(BPRSL)
str(RATSL)
summary(BPRSL)
summary(RATSL)
# The wide form version have more variables and less observations. In the long version, the same kind of variables are concentrated in one variable, reducing the amount of variables, while the observations are computed individually and independently.

# save the modified datasets
write.csv(BPRSL, "BPRSL_data.csv", row.names = FALSE)
write.csv(RATSL, "RATSL_data.csv", row.names = FALSE)


