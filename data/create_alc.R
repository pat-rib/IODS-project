#Patricia Carvalho Ribeiro

#10.11.20

#Exercise 3

# read student-mat.csv and student-por.csv from data folder
math <- read.table("student-mat.csv", sep = ";" , header=TRUE)
por <- read.table("student-por.csv", sep = ";" , header=TRUE)

# explore the structure and dimensions of the data
str(math)
str(por)
dim(math)
dim(por)

# access the dplyr library
library(dplyr)

# common columns to use as identifiers
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

# join the two datasets by the selected identifiers
math_por <- inner_join(math, por, by = join_by)

# create a new data frame with only the joined columns
alc <- select(math_por, one_of(join_by))

# explore the structure and dimensions of the joined data
str(alc)
dim(alc)

# columns that were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

# print out the columns not used for joining
notjoined_columns


# combine the 'duplicated' answers in the joined data
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  # if that first column  vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# define a new column alc_use by combining weekday and weekend alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)
    
# define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)
    
# access the tidyverse libraries tidyr, dplyr, ggplot2
library(tidyr); library(dplyr); library(ggplot2)

# glimpse at the alc data
glimpse(alc) 
  
# save the joined modified data
write.csv(alc, "alc_data.csv")

