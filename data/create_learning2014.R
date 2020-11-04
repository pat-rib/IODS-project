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
selectedDataMeans <- rowMeans(selectedData)
selectedDataNorm <-  selectedData / selectedDataMeans

# select rows where points is greater than zero
selectedData <- filter(selectedData, Points > 0)

# save dataset
write.csv(selectedData, "learning2014.csv")

#read dataset
newData <- read.csv("learning2014.csv") 

# demonstrating that I can read the data again
str(newData)

# readind the data again
head(newData)
