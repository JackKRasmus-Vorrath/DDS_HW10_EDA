#The following EDA summarizes and visualizes factors of Click-Through-Rate data

#Q1: Data Import

fileLocation <- "http://stat.columbia.edu/~rachel/datasets/nyt1.csv"
data1 <- read.csv(url(fileLocation))
head(data1)


#Q2: Age Grouping

data1$Age_Group <- cut(data1$Age, c(-Inf, 18, 24, 34, 44, 54, 64, Inf))
levels(data1$Age_Group) <- c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
head(data1)


#Q3&4: Subset & Click-Through-Rate (CTR)

d1 <-subset(data1, Impressions > 0)
d1$CTR <- d1$Clicks/d1$Impressions
head(d1)


#Q5: Impressions & CTR by Age_Group

library(ggplot2)
ggplot(d1, aes(x = Impressions, fill = Age_Group)) + geom_histogram(binwidth = 1)
ggplot(subset(d1, CTR > 0), aes(x = CTR, fill = Age_Group)) + geom_histogram(binwidth = .025)


#Q6: CTR Grouping

d1$CTR_Group <- cut(d1$CTR, c(-Inf, .2, .4, .6, .8, Inf))
levels(d1$CTR_Group) <- c("<.2", ".2-.4", ".4-.6", ".6-.8", ">.8")
head(d1)


#Q7: Summary Totals

##Male = 1, Female = 0
g <- sum(d1$Gender)
g
sum(d1$Impressions)
sum(d1$Clicks)
s <- sum(d1$Signed_In)
s


#Q8: Means & Percents

mean(d1$Age)
mean(d1$Impressions)
mean(d1$Clicks)
mean(d1$CTR)

##Percent Male
l_gender <- length(d1$Gender)
pct_male <- g/l_gender
pct_male

##Percent Signed_In
l_signedin <- length(d1$Signed_In)
pct_signedin <- s/l_signedin
pct_signedin


#Q9: Means & Percents by Age_Group

##Means by Age_Group
library(doBy)
summary1 <- summaryBy(Impressions + Clicks + CTR ~ Age_Group, data = d1, FUN = mean)
summary1

##Percents by Age_Group
percent <- function(x){percent = sum(x)/length(x)}
summary2 <- summaryBy(Gender + Signed_In ~ Age_Group, data = d1, FUN = percent)
summary2


#Q10: Counts of CTR_Group & Age_Group

library(plyr)
ctr_age <- count(d1, c('CTR_Group', 'Age_Group'))
ctr_age


#Q11&12: CTR_Group by Gender

##All CTR_Groups
ggplot(d1, aes(x = CTR_Group, fill = factor(Gender)))+geom_bar()

##Excluding Lowest CTR_Group
ggplot(subset(d1, CTR_Group != "<.2"), 
       aes(x = CTR_Group, fill = factor(Gender)))+ geom_bar()

d2 <- subset(d1, CTR_Group != "<.2")
d3 <- subset(d2, CTR_Group != ".2-.4")
d4 <- subset(d3, CTR_Group != ".4-.6")

##Mid-to-High CTR_Groups by Gender
ggplot(d3, aes(x = CTR_Group, fill = factor(Gender)))+geom_bar()
ggplot(d4, aes(x = CTR_Group, fill = factor(Gender)))+geom_bar()

##Male-to-Female Ratio by CTR_Group
library(plyr)
ctr_gender <- count(d1, c('CTR_Group', 'Gender'))
m2f_CTR <- c(ctr_gender[2, 3]/ctr_gender[1, 3], ctr_gender[4, 3]/ctr_gender[3, 3], 
             ctr_gender[6, 3]/ctr_gender[5, 3], ctr_gender[8, 3]/ctr_gender[7, 3], 
             ctr_gender[10, 3]/ctr_gender[9, 3])
names(m2f_CTR) <- c("<.2", ".2-.4", ".4-.6", ".6-.8", ">.8")
m2f_CTR


#Consistently more Females than Males click through, especially in the higher CTR categories.