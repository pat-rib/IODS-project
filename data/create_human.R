#Patricia Carvalho Ribeiro

#19.11.20

#Exercise 4

library(dplyr)
library(ggplot2)
library(MASS)

# read the “Human development” and “Gender inequality” datas into R.
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)  
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

# explore the structure and dimensions of the data
str(hd)
str(gii)
dim(hd)
dim(gii)
summary(hd)
summary(gii)

# Rename hd variables
names(hd)[names(hd) == "Human.Development.Index..HDI."] <- "HDI"
names(hd)[names(hd) == "Life.Expectancy.at.Birth"] <- "LE.Birth"
names(hd)[names(hd) == "Expected.Years.of.Education"] <- "EY.Edu"
names(hd)[names(hd) == "Mean.Years.of.Education"] <- "MY.Edu"
names(hd)[names(hd) == "Gross.National.Income..GNI..per.Capita"] <- "GNI.PC"
names(hd)[names(hd) == "GNI.per.Capita.Rank.Minus.HDI.Rank"] <- "GNI.Rank"

# Rename gii variables
names(gii)[names(gii) == "Gender.Inequality.Index..GII."] <- "GII"
names(gii)[names(gii) == "Maternal.Mortality.Ratio"] <- "Mat.MR"
names(gii)[names(gii) == "Adolescent.Birth.Rate"] <- "Ado.BR"
names(gii)[names(gii) == "Percent.Representation.in.Parliament"] <- "Per.Rep"
names(gii)[names(gii) == "Population.with.Secondary.Education..Female."] <- "Edu.Sec.F"
names(gii)[names(gii) == "Population.with.Secondary.Education..Male."] <- "Edu.Sec.M"
names(gii)[names(gii) == "Labour.Force.Participation.Rate..Female."] <- "Labour.F"
names(gii)[names(gii) == "Labour.Force.Participation.Rate..Male."] <- "Labour.M"

# Create new variable for ratio of Female and Male populations with secondary education in each country
gii <- mutate(gii, eduRatio = Edu.Sec.F / Edu.Sec.M)

# Create new variable for ratio of labour force participation of females and males in each country
gii <- mutate(gii, labRatio = Labour.F / Labour.M)

# Join the datasets for countries that appear in both datasets
human <- inner_join( hd, gii, by = "Country" )
str(human)
write.table(human, "data/human.csv", row.names = FALSE)

# Data wrangling - part 2

read.table("data/human.csv")
str(human)
dim(human)
# description: data is composed by 195 objects and 9 variables about the Human Development Index (HDI) - a summary measure of average achievement of human development.
# The data combines several indicators from most countries in the world, such as life expectancy at birth, expected years of schooling, maternal mortality ratio, percentage of female representatives in parliament, proportion of females and males with at least secondary education, proportion of females and males in the labour force, among others.

# 1 - Mutate the data: transform the Gross National Income (GNI) variable to numeric
library(stringr)
# str_replace(human$GNI.PC, pattern=",", replace ="") %>% as.numeric
human$GNI.PC <- str_replace(human$GNI.PC, pattern=",", replace ="") %>% as.numeric
#2 - Exclude unneeded variables. 
keep <- c("Country", "Edu.Sec.F", "Labour.F", "LE.Birth", "EY.Edu", "GNI.PC", "Mat.MR", "Ado.BR", "Per.Rep")

# 3 Remove all rows with missing values
# select the 'keep' columns
human <- dplyr::select(human, one_of(keep))

# print out a completeness indicator of the 'human' data
complete.cases(human)

# print out the data along with a completeness indicator as the last column
# data.frame(human[-1], comp = complete.cases(human))

# filter out all rows with NA values
human_complete <- filter(human, complete.cases(human))

#4 Remove the observations which relate to regions instead of countries
last <- nrow(human_complete) - 7
human_complete <- human_complete[1:last, ]


#5 - Define the row names of the data by the country names and remove the country name column from the data. The data should now have 155 observations and 8 variables. 
# Save the human data in your data folder including the row names. You can overwrite your old ‘human’ data.
rownames(human_complete) <- human_complete$Country
human_complete <- dplyr::select(human_complete, -Country)
write.csv(human_complete, "human_data.csv")
dim(human_complete)
