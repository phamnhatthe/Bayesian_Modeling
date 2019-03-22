# Creating a simple linear regression model for ramus bone data
# Reading in raw data

setwd("C:/hw/upwork data and project/")
ramus<-read.table("ramusdata.txt",header=F,sep=",")
str(ramus) # Checking integrity of data
# Cleaning data and naming variables
ramus<-ramus[,1:4]
colnames(ramus)<-c("y1","y2","y3","y4")
matrix1<-matrix(1,nrow=20,ncol=1)
# Assumption: The exact age of the children does not matter. What matters is each measurement
# was taken one year apart so we can assume that the ages for the 4 measurements were 1,2,3, and 4
# Creating age matrix
l<-list()
for(i in 1:4){
  l[[i]]<-matrix(i,nrow=20,ncol=1)
}

len<-as.vector(c(ramus$y1,ramus$y2,ramus$y3,ramus$y4))
age<-as.vector(c(l[[1]],l[[2]],l[[3]],l[[4]]))
ramusdata<-data.frame(len,age) # Modified data consist of a column with lengths and a column
# with ages 1-4
# -------End of data cleaning-------
# Data exploration and running diagnostics
plot(len~age) # the predictor variable is in count. Any attempt to fit a straight line to predict
# the length of ramus bone based on age will be poor since the spread of data points within each
# age group is high

attach(ramusdata)
mod<-lm(len~age) # Fitting a linear model. Parameters estimated here are the intercept and age.
res<-summary(mod)

plot(len~age)
abline(res$coefficients[1,1],res$coefficients[2,1])
# Fitting regression line through the data points. It is obvious that the regression line
# poorly represents the data points.

# Running simple linear regression diagnostics

cor(ramus)
# The lengths of the ramus bone are highly time correlated. This means data points collected
# are not independent from each other. This means we failed to meet the assumption that the residuals
# are independent from each other.
plot(acf(res$residuals)) # The acf plot somewhat supports our conclusion that the residuals are
# not independent from each other since even after lag 15 the correlation is still about -.1 or .1

# Reshuffling data to reflect higher correlation in the acf plot
ramusmatrix<-as.matrix(ramus)
t_matrix<-t(ramus)
ramusmatrix2<-matrix(t_matrix,nrow=80,ncol=1)
agematrix<-matrix(c(1,2,3,4),nrow=20,ncol=4,byrow=T)
t_matrix2<-t(agematrix)
agematrix2<-matrix(t_matrix2,nrow=80,ncol=1)
mod2<-lm(ramusmatrix2~agematrix2)
res2<-summary(mod2)
plot(acf(res2$residuals)) # The acf plot now shows even more correlation between residuals. 
# This highly supports our conclusion that the residuals are not independent from each other.
# This means a regression model might be inappropriate for our analysis. This warrants a future
# investigation into the violation of the simple linear regression model assumption that the residuals
# are independent from each other.

par(mfrow=c(2,2))
plot(mod)
# The Residuals vs Fitted appears as if the residuals are sufficiently randomized, not showing 
# any visible pattern meaning that the corrected model has been fitted. The Scale-Location plot
# appears normal, meaning that the residual variance is homoscedastic.The Normal Q-Q plot shows us
# that the residuals might not follow a normal distribution since the left tail of the distribution
# of residuals is thick while the right tail is thin. A thick tail can invalidate inferential tools
# such as p-values and confidence intervals. Since the regression assumption that residuals are
# normally distributed is violated, prediction intervals might not be appropriate to use on the data.

# Finding outliers and high leverage points
leverage<-hatvalues(mod)
stdres<-rstandard(mod)
leverage[which(leverage>=2*mean(leverage))] # There is no high leverage point
stdres[which(stdres>=2|stdres<=(-2))] # There is no outlier

cooksdistance<-cooks.distance(mod) 
par(mfrow=c(1,1))
plot(cooksdistance) # The plot of Cook's Distance did not show any significant jump among data points

# The fact that there is no high leverage point or outlier and the plot of Cook's Distance did not
# show any significant jump among data points means that there is no concern to be addressed.
#------End of data exploration and running diagnostics------

# Analysis of the regression model
res # printing the result of the linear model
# R-squared value is .1443, which means this model explains approximately 14.43% of variation
# in the data. This is relatively poor given that both parameters estimated are statistically
# significant.
# The parameter age is statistically significant, meaning that there is a linear relationship
# between the length of ramus bone and age, which is logically sound and expected.

# 95% confidence intervals for the parameter intercept
res$coefficients[1,1]+c(-1,1)*qt(.975,nrow(ramusdata)-2)*res$coefficients[1,2]
# We are 95% confident that children 1 year younger than the youngest age collected in the data
# have ramus bone lengths averaging between 46.34 and 49.14 units
# 95% confidence intervals for the parameter age
res$coefficients[2,1]+c(-1,1)*qt(.975,nrow(ramusdata)-2)*res$coefficients[2,2]
# We are 95% confident that with 1 year increase in age, a child's ramus bone will grow between
# 0.42 and 1.45 units on average.

# data extrapolation
# Using the regression model, predict the length of ramus bones from children 1 and 2 years older
# than the oldest age recorded in the data.
predict(mod,newdata=data.frame(age=c(5,6)),interval="prediction",level=.95)
# We are 95% confident that children 1 year older than the oldest age recorded in the data
# have an average ramus bone length between 47.10 and 57.72 units.
# We are 95% confident that children 2 years older than the oldest age recorded in the data
# have an average ramus bone length between 47.88 and 58.80 units.

# Creating prediction and confidence interval for the data on a plot
dat1<-predict(mod,newdata=data.frame(age),interval="prediction",level=.95)
dat2<-predict(mod,newdata=data.frame(age),interval="confidence",level=.95)
# Ordering dat1 and dat2. Need to do this step in order to plot the confidence interval and
# prediction interval
#dat1new<-dat1[order(dat1[,1]),]
#dat2new<-dat2[order(dat2[,1]),]
plot(len~age,ylim=c(40,68))
abline(res$coefficients[1,1],res$coefficients[2,1])
lines(age,dat1[,2],type="l",col="blue")
lines(age,dat1[,3],type="l",col="blue")
lines(age,dat2[,2],type="l",col="red")
lines(age,dat2[,3],type="l",col="red")
legend("topright",legend=c("regression line","confidence int.","prediction int."),col=c("black","red","blue"),lwd=2)

# The 95% confidence interval means that we are 95% confident the regression line can be only
# be anywhere within the red lines. This means the margin of error for regression line lies between the red
# lines. The 95% prediction interval tells us that 95% of all data points will fall between the
# blue lines.
