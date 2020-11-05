# Patricia Ribeiro
# 4.11.20
# task 2

# Access the dplyr library
library(dplyr)

# read the data into memory
learning2014 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS2-data.txt", sep="\t", header=TRUE)

# get a summary of the structure of learning2014
str(learning2014)

# retrieve the dimension of learnin2014
dim(learning2014)


# list of columns
variableNames <- c("Gender","Age","Attitude", "Deep", "Stra", "Surf", "Points")

# select the columns from learning2014
selectedData <- select(learning2014, one_of(variableNames))

# divide each number in the column vector by the number of questions used to 
# build the corresponding combination variables in the original dataset

selectedData$Attitude <- selectedData$Attitude / 10
selectedData$Deep <- selectedData$Deep / 12
selectedData$Stra <- selectedData$Stra / 8
selectedData$Surf <- selectedData$Surf / 12


# select rows where points is greater than zero
selectedData <- filter(selectedData, Points > 0)

# save dataset
write.table(selectedData, "learning2014.csv")

#read dataset
newData <- read.table("learning2014.csv") 

# demonstrating that I can read the data again
str(newData)

# readind the data again
head(newData)

#Analysis

# read the data into memory
learning2014 <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/learning2014.txt", sep=",", header=TRUE)

# explore the structure of the data
str(learning2014)

# explore the dimension of the data
dim(learning2014)

# description of the dataset: it is a dataset that has 166 objects and 7 variables (gender, age, attitude, deep, stra, surf, and points).

# show a graphical overview of the data drawing a scatter plot matrix of the variables in learning2014
plot(learning2014)

# show summaries of the variables
summary(learning2014)

# Access the gglot2 library
library(ggplot2)

# choose 3 variables where points is the target and fit a linear model
my_model <- lm(points ~ attitude + stra + surf, data = learning2014)


# show summaries of the model
summary(my_model)
# the multiple R squared of the model shows that the explanatory variablesstra and surf are not related to the target variable points because their p-values are too high.
# since p-value for stra and surf are too high, I removed them from the model
my_model2 <- lm(points ~ attitude, data = learning2014)

# show summaries of the model again
summary(my_model2)

# produce the following diagnostic plots: QQ-plot and Residual versus Leverage
plot(my_model2, c(1, 2, 5))
# the main assumption of the model is that the modeling errors are normally distributed with equal variance.
# the variance

 

















