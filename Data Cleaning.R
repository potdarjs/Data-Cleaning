

#-----------Data Cleaning--------------------

# Character Cleaning

  
x <- c("Home placement", "home placement", "home Placement", "missing",
       "Home Placement.s")
x


x <- gsub("h", "H", x)     # Replaces 'h' by 'H'
x <- gsub("p", "P", x)     # Replaces 'p' by 'P'
x <- gsub("\\..*", "", x)  # Replaces characters after '.' (dot) by nothing
x <- gsub("\\.", "", x)    # Replaces '.' (dot) by nothing
x[x == "missing"] <- NA    # Replaces word missing by NA  
x

#  package stringr can also be used 

# Direct input in the form YYYY-MM-DD or YYYY/MM/DD

date1 <- as.Date('1915-6-16')
date1


date2 <- as.Date('1990/02/17')
date2

#From list input OR Character input

dts = c("2005-10-21 18:47:22","2005-12-24 16:39:58",
        "2005-10-28 07:30:05 PDT")
date3 <- as.POSIXlt(dts)
date3


dts = c("2005-10-21 18:47:22","2005-12-24 16:39:58",
        "2005-10-28 07:30:05 PDT")
date4 <- as.POSIXct(dts)
date4

#From number input

dts = c(1127056501,1104295502,1129233601,1113547501,
        1119826801,1132519502,1125298801,1113289201)
date5 = dts
class(date5) = c('POSIXt','POSIXct')
date5

#See the Class of all the dates created..

class(date1)
class(date2)
class(date3)
class(date4)
class(date5)


date3 <- as.Date(date3)
date4 <- as.Date(date4)
date5 <- as.Date(date5)




as.numeric(date1)
as.numeric(date2)
as.numeric(date3)
as.numeric(date4)
as.numeric(date5)

#x is continuous variable but has many other class elements too

x <- c(rep(c(2,3.5,4),5),NA,"4 ",53.,NA,rep(c(1,3,2),5),"3..", "2//", 2,3,4)
x
class(x)



x <- gsub("[^0.00-9.00^]","", x)   # will clear all the elements other than numerics
x <- gsub("..",".",x, fixed = T)   # will replace double dots with single dots
x


x <- as.numeric(x)  # will convert the characters to numeric
x
class(x)

#load data

data("iris")


# Seed missing values

library(missForest)
iris.mis <- prodNA(iris, noNA = 0.1)

# check the missing values graphically

library(VIM)
mice_plot <- aggr(iris.mis, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(iris.mis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))



library(mice)
md.pattern(iris.mis)


#using argImpute

library(Hmisc)
impute_arg <- aregImpute(~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width +
                           Species, data = iris.mis, n.impute = 5)
impute_arg

#impute missing values, using all parameters as default values

library(missForest)
iris.imp <- missForest(iris.mis)

#check imputed values

iris.imp$ximp

#check imputation error

iris.imp$OOBerror

#comparing actual data accuracy

iris.err <- mixError(iris.imp$ximp, iris.mis, iris)
iris.err



library(mice)
imputed_Data <- mice(iris.mis, m=5, maxit = 50, method = 'cart', seed = 500)



summary(imputed_Data)


x <- c(1:10, 20, 30)
boxplot.stats(x, coef = 1)$out



inputData <- read.csv("E:/Data Analytics with RET/Assignment 3 Data Cleaning/ozone.csv")
head(inputData)


mod <- lm(ozone_reading ~ ., data=ozone) 

cooksd <- cooks.distance(mod) 




plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") # plot cook's distance 
abline(h = 4*mean(cooksd, na.rm=T), col="red") # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, 
     labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red") # add labels 


influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))]) # influential row numbers

head(ozone[influential, ]) # influential observations


setwd("E:\\Data Analytics with RET\\Assignment 3 Data Cleaning")
library(deducorrect)
marx <- read.csv("marx.csv", stringsAsFactors = FALSE)
R <- correctionRules("conversions.txt")
cor <- correctWithRules(R, marx)
cor$corrected


E <- editmatrix(expression(
  staff+cleaning+housing == total,
  staff >= 0,
  housing >= 0,
  cleaning >= 0))

dat <- data.frame(
  staff = c(100,100,100),
  housing = c(NA,50,NA),
  cleaning = c(NA,NA,NA),
  total = c(100, 180, NA)
)
dat
cor <- deduImpute(E, dat)
cor$corrected















