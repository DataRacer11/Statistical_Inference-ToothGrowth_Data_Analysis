#Statistical Inference - Data Analysis Part 2
#The Effect of Vitamin C on Tooth Growth in Guinea Pigs
#Author Name: DataRacer11
#9-26-2015
#References:
#- McNeil, D. R. (1977) Interactive Data Analysis. New York: Wiley.
#- Crampton, E. W. (1947) The growth of the odontoblast of the incisor teeth as a criterion of vitamin C #intake of the guinea pig. The Journal of Nutrition 33(5): 491-504. #http://jn.nutrition.org/content/33/5/491.full.pdf
#- C. I. Bliss (1952) The Statistics of Bioassay. Academic Press.
#- R Tutorials - William B. King, Ph.D, Coastal Carolina University,
#http://ww2.coastal.edu/kingw/statistics/R-tutorials/factorial.html

#This report has been developed to respond to the following questions:
#1. Was exploratory data analysis performed with at least a single plot or table highlighting basic features of the data?
#2. Were relevant confidence intervals and/or tests performed on the data?
#3. Were results of the tests and/or intervals interpreted in the context of the problem correctly?
#4. Were assumptions described as needed to make conclusions?

#Load the ToothGrowth data and perform some basic exploratory data analyses:
library(datasets)

#Obtain a basic summary of the data. A snapshot of the data.frame:
str(ToothGrowth)
summary(ToothGrowth)
# Data Reshaping and Group Manipulation
# Investigate the data table further
dt<-data.frame(ToothGrowth)
#Set the names of the data columns so review the data so that it is easier to understand, review and reproduce.
#Add 'Dosage' and set the join with the new column names
                                                                          #Exploratory Data Analysis
head(dt)
summary(dt)

# Correlating averages
cor(ToothGrowth$dose, ToothGrowth$len)
                                                                              #The data can be split into three dosages groups and the dosages values can then be computed as well as the group averages.
l <-split(ToothGrowth$len, ToothGrowth$dose)
group_means <- c(mean(l[[1]]), mean(l[[2]]), mean(l[[3]]))
cor(c(0.5, 1, 2), group_means)

#Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose.
#There are several ways that we can plot for data visualization such as this boxplot.
boxplot(len ~ supp * dose, data=ToothGrowth, ylab="Tooth Length", main="Boxplots of Tooth Growth Data")
                                                                              #Calculate the length and mean by supplement and dose. Create a scatterplot to evaluate the doses
avgs<- aggregate(len~., dt, mean)
                                                                              #plot the scatter points and the averages
library(ggplot2)
g <- ggplot(ToothGrowth, aes(x=dose, y=len))
g <- g + geom_point(aes(group=supp, colour=supp, size=2, alpha=0.6))
g <- g + geom_line(data=avgs, aes(group=supp, colour=supp))
print(g)
                                                                              ## To test this hypothesis, create subsets of the data for conducting several T.Tests to better understand the confidence interval.

#T-Test at .5 mg
t.test(len ~ supp, ToothGrowth[ToothGrowth$dose == .5, ])

#T-Test at 1 mg
t.test(len ~ supp, ToothGrowth[ToothGrowth$dose == 1, ])

#T-Test at 2 mg
t.test(len ~ supp, ToothGrowth[ToothGrowth$dose == 2, ])
###Summary and Assumptions:
#- The confidence intervals are assumed to not be paired, comparing two different supplement types from individual guinea pig did not take place.
##- The dataset contains six measurements for ten guinea pigs each, three for each dose for each of the #two supplement types. As it is not the same ten guniea pigs for each of the six measurements, we #cannot effectively analyze the differences of the three different dose measurements (for each #supplement type).
#- We cannot assume equal variances within each group. As we only have n=10 observations, T-tests #were performed with ??=0.05
#- The samples are independent.
##- The distribution approximately is normal.
                                                                              ###Conclusions:
#- As Vitamin C dose size alone increases, the tooth length increases as well
#- We have analyzed the influence of both the dosage and the supplement type of Vitamin C on the #length of the guinea pigs' teeth.
#- We have found that the dosage very likely has an impact on the length of the teeth, whereas the #supplement type has no significant impact.
#- The supplement type of Orange Juice affects tooth length greater then Ascorbic Acid, or Vitamin C #with a 0.5 and 1.0 dose size.
#- When the dose size reached 2.0 milligrams, the effect between Orange Juice and Ascorbic Acid is very #similar.
