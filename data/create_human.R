#Patricia Carvalho Ribeiro

#19.11.20

#Exercise 4

library(dplyr)

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

write.table(human, "data/human.csv", row.names = FALSE)

