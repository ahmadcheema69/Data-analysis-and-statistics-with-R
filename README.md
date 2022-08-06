# Data-analysis-and-statistics-with-R

Appendix
#load of data
data<- read.csv ("bike_buyers.csv")
data
#dimension of data
dim(data)
#Find nulls
sapply (data, function(x) sum (is.na(x)))
#non null columns
new_df <- subset(data, select = -c (ID, Commute. Distance))
#find datatype
sapply(new_df, typeof)
#find datatye in structural view
library(dplyr)
glimpse(new_df)
#Boxplot
boxplot(Income~ Purchased.Bike , data = new_df, xlab = "Buy Bikes",ylab = "Income", main = "Buy Bike according to 
income")
#Boxplot to show the range of ages having cars 
boxplot(Age~ Cars , data = new_df, xlab = " CARS",ylab = "Age", main = "Cars according to age")

#Statistical Analysis
#summary of data
summary(new_df)
#Find mode for character data type columns
# Create the function.
getmode <- function(v) {
 unique_var <- unique(v)
 unique_var[which.max(tabulate(match(v, unique_var)))]
}
#mode of observing that the people purchased bikes more or not
getmode(new_df$Purchased.Bike)
getmode(new_df$Gender)
#Mostly bike buyers are male
getmode(new_df$Marital.Status)
#mostly are married
getmode(new_df$Cars)
#dnorm()
dnom<-dnorm(new_df$Cars)
dnom
plot(new_df$Cars,dnom)
#for having children
dn_child<-dnorm(new_df$Children)
dn_child
plot(new_df$Children,dn_child)
#pnorm()
pnorm_cars <- pnorm(new_df$Cars, mean = 2.5, sd = 2)
pnorm_cars
plot(new_df$Cars,pnorm_cars)
#qnorm()
qnor_cars <- qnorm(dnom, mean = 2, sd = 1)
qnor_cars

plot(dnom,qnor_cars)
#it will produace Nans
#binormal distributiion
#dbinom()
dbi=dbinom(new_df$Cars, 100, 0.5)
dbi
plot(new_df$Cars,dbi)
dbi2=dbinom(new_df$Children, 100, 0.3)
dbi2
plot(new_df$Children,dbi2)
#Poisson Regression
poisson_=glm(formula = Cars ~ Gender+Marital.Status, data = new_df,
 family = poisson)
summary(poisson_)
#chisqr
chisq.test(new_df$Purchased.Bike,new_df$Income)
#ANCOVA Analysis
result <- aov(Cars ~ Gender*Marital.Status,data = new_df)
summary(result)
#p value in both cases is less than 0.05. But the interaction between these two variables is not significant as the pvalue is more than 0.05.
x=new_df$Purchased.Bike
t.test(x-9,alternative="two.sided",conf.level=0.95) 
#There is strong evidence to reject the null hypothesis. There is overwhelming evidence against the null hypothesis.
#linear Regression
#label encoding
#install.packages("superml")
library(superml)
lbl = LabelEncoder$new()
new_df$Purchased.Bike = lbl$fit_transform(new_df$Purchased.Bike)
#0=no,1=yes

x=new_df$Children
y=new_df$Purchased.Bike
relation <- lm(y~x)
a <- data.frame(x = 2)
result <- predict(relation,a)
result
#for Income Column
z=new_df$Income
relation <- lm(y~z)
a <- data.frame(z = 500000)
result <- predict(relation,a)
result
#gives high accuracy upto 78%
#Multivariab;e linear regressionn
model <- lm(Purchased.Bike~Cars+Children+Income, data = new_df)
print(model)
a <- coef(model)[1]
print(a)
xcars <- coef(model)[2]
xchildren <- coef(model)[3]
xincome <- coef(model)[4]
# predict having a car, 2 children and 30000 income
Y = a+xcars*1+xchildren*2+xincome*30000
Y
boxplot(Cars~ Marital.Status , data = new_df, xlab = " Marital Status",ylab = "HAVING cARS", main = "Cars according 
to marital status")
