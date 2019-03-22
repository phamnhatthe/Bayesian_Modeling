# Setting working directory and reading in the raw data
setwd("C:/hw/upwork data and project/")
dat1<-readLines("fourmeasures.dat")
# Data cleaning.
# Issues encountered: Data was entered in 1 single line. Needed to separate the data into 
# multiple lines. Also, there was a lot of space character in place of new line character.
# Line 20 is missing a comma character at the end, making this line different from all the 
# other lines. In addition, the columns do not have names.
# Cleaning steps
dat2<-gsub(" ","",dat1) # cleaning out all surplus space character
library(stringr)
str_count(dat2[20],pattern="") # count number of positions within this string
string1<-dat2[20]
dat2[20]<-paste(string1,",",sep="") # Adding comma character at the end of line 20
writeLines(dat2,"ramusdata.txt")
data<-read.table('ramusdata.txt',header=F,sep=",") 
data<-data[,1:4] # Cleaned data
str(data) # checking integrity of data
colnames(data)<-c("year1","year2","year3","year4") # naming columns
str(data) 
# -----End of data cleaning-------

# Data exploration
cor(data) # the data in the columns are heavily correlated. We have time correlated data.

library(rjags)
library(R2jags)
library(coda)

# Building a Bayesian compound symmetric hierarchical model, a mixed model
# Notes regarding this model:
# There are 20 individuals represented in the data, thus we see there are 20 rows in the data
# as well as in certain loops in this model. This model utilizes a multivariate normal distribution
# with 4 means and a 4x4 variance, covariance matrix.
# A quick note regarding JAGS distributions: the multivariate normal distribution syntax in JAGS
# uses an omega matrix, not a sigma matrix. The normal distribution utilizes precision instead of variance.
# Besides creating a beta0 and beta1 for the whole population, the model also creates beta0 and beta1
# for each individual represented in the data set.
mod<-"model{
for (i in 1:20){
  dat[i,1:4] ~ dmnorm(mu[i,1:4], omega[1:4,1:4])
  for (j in 1:4){
    mu[i,j] <- b0[id[i]] + b1[id[i]]*agez[i,j]
  }
}
for (i in 1:20){
  b0[i] ~ dnorm(m0, .001)
  b1[i] ~ dnorm(m1, .01)
}
m0 ~ dnorm(48, .01)
m1 ~ dnorm(0, .01)
s2b ~ dgamma(2, .5)
s2e ~ dgamma(3, .5)
for (i in 1:4){
  for (j in 1:4){
    sigma[i,j] <- ifelse(i != j, s2b, s2b + s2e)
  }
}
omega[1:4,1:4] <- inverse(sigma[1:4,1:4])
}"
writeLines(mod,"model1.txt")
# Defining variables in the model
# Creating id for each participant. There are 20 participants so each id number will be repeated
# 4 times to cover all 80 observations.
i <- matrix(1:20,nrow=20,ncol=1)
id <- matrix(i,nrow=80,ncol=1)
id <- as.numeric(id)
dat <- data
agez <- matrix(c(0,1,2,3),nrow=20,ncol=4,byrow=T)
data.jags <- c("id","dat","agez")
parms <- c("b0","b1","m0","m1","s2b","s2e")
reg.sim2<-jags(data=data.jags,inits=NULL,parameters.to.save=parms,model.file="model1.txt",n.iter=36000,n.burnin=800,n.thin=3)
reg.sim2

sims2<-as.mcmc(reg.sim2)
chains<-as.matrix(sims)
sims<-as.mcmc(chains)
raftery.diag(sims) # Higher dependence factor is worse. Watch out for dependence factor greater than 5
effectiveSize(sims)
autocorr.diag(sims) # autocorrelation within each parameter
autocorr(sims) # autocorrelation between parameters
HPDinterval(sims)
