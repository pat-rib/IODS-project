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
