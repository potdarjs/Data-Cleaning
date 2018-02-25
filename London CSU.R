#####-----------------LCSU----------------------


# call required libraries
library(readxl)
library(psych)
library(dplyr)
library(outliers)
library(mice)
library(ggplot2)
library(xlsx)
# Import Data Set
Data <- read_excel("E:/Data Analytics with RET/My R Tutorials/Material by Dr Vinod/Assignment 2/Data.xlsx", 
                   col_types = c("text", "text", "date", 
                                 "text", "text", "date", "date", "text", 
                                 "text", "text", "text"))
# Understand Data Set
View(Data)
dim(Data)
str(Data)
sapply(Data, function(x) sum(is.na(x)))

# Modify the rate column which to numeric type while removing text from it
Data$`Weekly rate` <- as.numeric(gsub("[^0-9\\.]","",Data$`Weekly rate`))

# Modify the text to correct case
Data$`Provision type` <- gsub("h", "H",Data$`Provision type`)
Data$`Provision type` <- gsub("c", "C",Data$`Provision type`)
Data$`Provision type` <- gsub("p", "P",Data$`Provision type`)
Data$`Provision type`[ Data$`Provision type` == "missing" ] <- NA

Data$`Care group` <- toupper(Data$`Care group`)
Data$`Care group`[Data$`Care group` == "MISSING"] <- NA

Data$Gender <- toupper(Data$Gender)
Data$Gender[Data$Gender == "MISSING"] <- NA
Data$Gender[Data$Gender == "MR"] <- "MALE"
Data$Gender[Data$Gender == "MALEALE"] <- NA

# Change variable types
dat <- mutate(Data, CCG = as.factor(CCG),
              `Care group`= as.factor(`Care group`),
              `Provision type` = as.factor(`Provision type`),
              `Discharge reason` = as.factor(`Discharge reason`),
              `Gender` = as.factor(`Gender`))
dat$Remark <- NULL
str(dat)
View(dat)


install.packages("VIM")
library(VIM)
mice_plot <- aggr(dat, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(dat), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))

# understand the distribution and outliers of continuos varibale 
boxplot(dat$DOB)
boxplot(dat$`Provision start date`)
boxplot(dat$`Provision end date`)
# There are outliers

# Lets remove the outliers

# calculate age and treatment days
# treatment days are calculated inorder to get help to calculate the missing age values
dat$age <- as.numeric(dat$`Provision start date`-dat$DOB)
dat$age <- dat$age/365
dat$days <- as.numeric(dat$`Provision end date` - dat$`Provision start date`)
str(dat)
names(dat)


library(outliers)

data_outlier <- rm.outlier(data, fill = TRUE)

dat$WeeklyRate1 <- data_outlier$`Weekly rate` 
dat$days1 <- data_outlier$days
dat$age1 <- data_outlier$age

str(dat)
dim(dat)
names(dat)
sapply(dat, function(x) sum(is.na(x)))

# ignoring the values which are negative as age cannot be negative
# and select the required columns only

dat <- dat%>%filter(age1>0)%>%select(`CCG`,`ID`,`Care group`,`Provision type`,`Discharge reason`, Gender, age1,WeeklyRate1,days1)
str(dat)
# using library mice ---- all the varibale types should be numeric or factor
set.seed(1)
# impute the missing values
imputed = mice(dat, method='cart', m=5)
imputed <- complete(imputed)
# check for missings in the imputed dataset
#modelfit <- with(imputed, lm(age1~`Care group`+`Provision type`+WeeklyRate1+days1))
#summary(pool(modelfit))

sapply(imputed, function(x) sum(is.na(x)))
dim(imputed)
View(imputed)
describe(imputed$age1)
describe(imputed$WeeklyRate1)


par(mfrow=c(1,2))
boxplot(imputed$age1)
boxplot(imputed$WeeklyRate1)

l<-quantile(imputed$age1, .25) - 1.5*IQR(imputed$age1) # lower limit
u<-quantile(imputed$age1, .75) + 1.5*IQR(imputed$age1) # upper limit

final <- imputed%>%filter(age1>l&age1<u)%>%select(CCG,ID,`Care group`,
                                                  `Provision type`,`Discharge reason`,Gender,
                                                  age1,WeeklyRate1,days1)

ll<-quantile(imputed$WeeklyRate1, .25) - 1.5*IQR(imputed$WeeklyRate1)
uu<-quantile(imputed$WeeklyRate1, .75) + 1.5*IQR(imputed$WeeklyRate1)

final <- imputed%>%filter(WeeklyRate1>ll&WeeklyRate1<uu)%>%select(CCG,ID,`Care group`,
                                                  `Provision type`,`Discharge reason`,Gender,
                                                  age1,WeeklyRate1,days1)

dim(final)
boxplot(final$age1)
boxplot(final$WeeklyRate1)
setwd("E:/Data Analytics with RET/My R Tutorials/
          Material by Dr Vinod/Assignment 2")
write.xlsx(final, "LCSUfinal.xlsx")

# a)	Split between provision types (home vs Placement) across CCGs

types <- table(final$`Provision type`, final$CCG)
types
par(mfrow=c(1,1))
barplot(types, main = "Split bet Provision Types across CCGs",
        xlab = "CCGs", ylab = "Counts", col = c("Blue", "Red"),
        legend = rownames(types), las = 3)

# b)	Variations in care groups across CCGs
caregroups <- table(final$`Care group`, final$CCG)
caregroups

barplot(caregroups, main = "Variation in Care Group",
        xlab = "CCGs", ylab = "Counts", 
        col = c("Blue", "Red", "Green","Yellow","Brown","Cyan"),
        legend = rownames(caregroups), las = 3)

# Statistics for Age
# OVerall
View(describe(final$age1))

# age per Care Group
agepercaregroup <- final%>%group_by(`Care group`)%>%
  select(`Care group`, age1)%>%
  summarise(meanage = mean(age1), minage = min(age1), maxage = max(age1), medianage = median(age1))
View(agepercaregroup)

# age per Provision Type
ageperprovisiontype <- final%>%group_by(`Provision type`)%>%
  select(`Provision type`, age1)%>%
  summarise(meanage = mean(age1), minage = min(age1), maxage = max(age1), medianage = median(age1))
View(ageperprovisiontype)

# age per CCG

ageperCCG <- final%>%group_by(CCG)%>%
  select(CCG, age1)%>%
  summarise(meanage = mean(age1), minage = min(age1), maxage = max(age1), medianage = median(age1))
View(ageperCCG)
# Produce box plots to compare age at admission across CCGs for 
# each care group

ggplot(final, aes(x=CCG, y=age1, fill = `Care group`))+ geom_boxplot()

# Produce box plots to compare weekly rate across CCGs for 
# each care group

ggplot(final, aes(x=CCG, y=WeeklyRate1, fill = `Care group`))+ geom_boxplot()


# Plot histograms of age at admission and weekly rate and comment on the 
# distributions of these variables
par(mfrow=c(1,2))
hist(final$age1, col = "Blue")
# data is negatively skewed

hist(final$WeeklyRate1, col = "Red")
# data is positively skewed

# Determine whether In the FMH care group, the age at admission is
# higher in CCG W than in CCG X

FMH1 <- final%>%filter(`Care group` == "FUNCTIONAL MENTAL HEALTH" )
FMH <- FMH1%>%filter(CCG == "CCG_W" | CCG == "CCG_X")
dim(FMH)
ggplot(FMH, aes(x=CCG, y=age1, fill = CCG))+ geom_boxplot()

# Yes age at admission is higher in CCG W than CCG X


# 8.	Determine whether the weekly rate of LD is higher 
# in CCG W than in CCG X

LD1 <- final%>%filter(`Care group` == "LEARNING DISABILITY" )
LD <- LD1%>%filter(CCG == "CCG_W" | CCG == "CCG_X")
dim(LD)
ggplot(LD, aes(x=CCG, y=WeeklyRate1, fill = CCG))+ geom_boxplot()

# weekly rate of LD is NOT higher in CCG W than in CCG X

# 9.	Are females more (or less) likely than males to be 
# cared for at home?

gen1 <- final %>% filter(`Provision type` == "Home Care")
gen <- table(gen1$Gender, gen1$`Provision type`)

barplot(gen, beside = T, col = c("Yellow","Brown"),
        legend = rownames(gen))
# Yes. Females are more likely than males to be cared for at home

# 10.	Is age at admission correlated with weekly rate

par(mfrow=c(1,1))
scatter.smooth(final$age1, final$WeeklyRate1, col = "Yellow")

cor(final$age1 , final$WeeklyRate1)
# No. age at admission is not correlated with weekly rate

# Summary ---------
# Maximum cases are of "Palliative Care Group" with average age of 76 years 
# having maximum death cases