#name: Maija Absetz
#email: maija.absetz@helsinki.fi
#8.3.2017
#Final assifgnment with human data. Data will be used for classification and clustering

#This is strongly based on ch4 modifications. New adjustments will be marked with "!!!".#!!! Rest of the required data wrangling is done in the web page: https://macabset.github.io/IODS-final/#
#New wranglings will be: 1. New categorical variable eduexp.
# 2. Scaled dataset
# 3. train set and test set.
# I believe the structure is more logical if that much data wrangling is done at the web page in stead of here.


#Setting the needed packages and working directory to my computer:

library(Matrix)
library(ggplot2)
library(dplyr)


getwd()

#Read files Human development and gender inequality to R. 

hd2 <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)

gii2 <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

str(hd2)
dim(hd2)
str(gii2)
dim(gii2)

#We have 2 datasets that we eventually want to combine, but let's first see what we have.
#Human development focuses what are the factors that determine how well our country ranks when looking at human development. It has 8 variables and 195 observations. Variables are both numeric and character.
#Gender inequality tries to grasp the inequality between men and women in achievements. It focuses on health, empowerement and work markets. This dataset contains 10 variables and also 195 observations( since it's based on the same resaerch??). This dataset has also both numeric and character variables.

summary(hd2)
summary(gii2)

#Let's give our variables shorter names, so they will be easier to use further.
#!!! New, clearer names compared to previous exercise.

colnames(hd2)
colnames(hd2)[1] <-"HDI.rank"
colnames(hd2)[3] <-"HDI"
colnames(hd2)[4] <- "Life.exp"
colnames(hd2)[5] <- "Edu.exp"
colnames(hd2)[6] <- "Edu.y.mean"
colnames(hd2)[7] <- "GNI"
colnames(hd2)[8] <- "GNI-HDI.rank"
colnames(hd2)


colnames(gii2)
colnames(gii2)[1] <- "GII.rank"
colnames(gii2)[3] <- "GII"
colnames(gii2)[4] <- "Mot.mor"
colnames(gii2)[5] <- "Adol.birth"
colnames(gii2)[6] <- "Parl.F"
colnames(gii2)[7] <- "Edu2F"
colnames(gii2)[8] <- "Edu2M"
colnames(gii2)[9] <- "LabF"
colnames(gii2)[10] <- "LabM"
colnames(gii2)

#Mutate: gii
#2 new variables

gii2 <- mutate(gii2, Edu2FM = Edu2F / Edu2M)
gii2 <- mutate(gii2, LabFM = LabF/LabM)
colnames(gii2)

#Now we have 3 new variables. 1) Edu2FM is the ratio between the amount of females in 2 level education to men
#2) LabFM is the ratio of women to men in labour

join_by <- c("Country")


hdi_gii2 <- inner_join(hd2, gii2, by= join_by, suffix= c(".hd2", ".gii2"))
colnames(hdi_gii2)


#Saving project to data file


write.csv(hdi_gii2, file = "human2.csv", row.names = FALSE)
human2 <- read.csv("human2.csv", sep=",", header= T)

#And final check that everything works.
str(human2)

#This is continuum for last week's data wrangling. We are continuing with the same data.
#1. mutate data GNI to numeric

human2 <- mutate(human2, GNI = as.numeric(human2$GNI))
str(human2)
colnames(human2)


#2. Keep columns: Country, Edu2FM, Edu.exp, Life.exp, GNI, Mot.mor, Adol.birth, Parl.F 
#!!!: and 2 new variables Edu.y.mean, HDI.

keep_columns <- c("Country", "Edu2FM","LabFM","HDI", "Edu.exp", "Life.exp", "GNI", "Mot.mor", "Adol.birth", "Parl.F", "Edu.y.mean")
human2 <- select(human2, one_of(keep_columns))
str(human2)

#3.Remove rows with missing values
#create column with missing values and then filter leaving NA's out.
complete.cases(human2)
data.frame(human2[-1], comp = complete.cases(human2))
human2 <- filter(human2, complete.cases(human2))
complete.cases(human2)
str(human2)

#Let's clean the column country and filter regions out
#The last 7 rows with column country are infact regions instead of countries.
last <- nrow(human2) - 7
last
# I'll choose everything until the last 7 observations, the 155's being Nigeria.
human2 <- human2[1:155, ]
str(human2)

#4. add countries as rownames and remove country as column

rownames(human2) <- human2$Country
#Remove country as column
human2 <- dplyr::select(human2, -Country)
str(human2)
head(human2)

#Now we have wanted 155 observations and 10 variables, with countries as rownames.
#override the old data:

write.csv(human2, file = "human2.csv", row.names = TRUE)
human2 <- read.csv("human2.csv", sep=",", header= T, row.names = 1)
str(human2)
summary(human2)
complete.cases(human)
head(human)

