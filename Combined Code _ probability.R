install.packages("Lock5Data")
installed.packages("bbmle")
library(Lock5Data)
library(ggplot2)
library(stats4)
library(Ecdat)
library(bootstrap)
str(CommuteStLouis)
summary(CommuteStLouis)

min(CommuteStLouis$City)

# Looking at the structure of the data allows us to see 500 observations 
# with 5 variables. In our case we are looking at commute times so we will be finding the 
# mean of "Time" in the CommuteStLouis dataset.

str(CommuteStLouis)

#Doing this gives us the point estimate from the time sample. 

Time_mean <- mean(CommuteStLouis$Time)


#ecdf for time
time.ecdf = ecdf(CommuteStLouis$Time)
plot(time.ecdf)
CommuteStLouis$Time
time.ecdf(10)-time.ecdf(5)

#ecdf for distance
dis.ecdf = ecdf(CommuteStLouis$Distance)
plot(dis.ecdf)
CommuteStLouis$Distance
dis.ecdf(15) - dis.ecdf(14)



nt = length(CommuteStLouis$Time)
nd = length(CommuteStLouis$Distance)

mint = min(CommuteStLouis$Time)
maxt = max(CommuteStLouis$Time)


#confidence band for ecdf of time

plot(time.ecdf)
alpha = 0.05

eps = sqrt(log(2/alpha)/(2*n))

grid = seq(1,150, length.out = 10000)
lines(grid,pmin(time.ecdf(grid)+eps,1))
lines(grid,pmax(time.ecdf(grid)-eps,0))


#confidence band for ecdf of distance


mind = min(CommuteStLouis$Distance)
maxd = max(CommuteStLouis$Distance)

plot(dis.ecdf)
alpha = 0.05

eps = sqrt(log(2/alpha)/(2*n))

grid = seq(0,80, length.out = 10000)
lines(grid,pmin(dis.ecdf(grid)+eps,1))
lines(grid,pmax(dis.ecdf(grid)-eps,0))

# I wanted to see the distribution of mean time to get to work so 
# we need to find the SE. In order to do this I created a matrix with 1000 rows. 

B = 1000
n = nrow(CommuteAtlanta) 
boot.x = matrix(sample(CommuteStLouis$Time, size = B * n, replace = TRUE), B, n)

# In this instance, sample() was used to sample the values in Commute$StLouis. The replace = TRUE 
# function fills all the B multipled by n elements in the matrix created. 


# Using the apply() function learned in Data Wrangling,
# we use this function to apply() on three arguments. 

boot.s <- apply(boot.x,1,mean)

# Next I found the standard deviation and plotted a density chart and histogram.

sd <- sd(boot.s)
sd

ggplot(data.frame(meanTime=boot.s),aes(x=meanTime)) + 
  geom_histogram(binwidth=0.25,aes(y=..density..)) +
  geom_density(color="blue")


# We first looked at the mean of the CommuteStlouis in the boot package. 
# The indices function is used to compute the statistics from the data whee
# the first argument is the data and the second argument being the indices of the observations
# of the bootstrap sample. 

library(boot) 
data(CommuteStLouis) 
my.mean = function(x, indices) { 
  return( mean( x[indices] ) ) 
} 
time.list = boot(CommuteStLouis$Time, my.mean, 10000)

#The object "time.list" is a list with many elements, however in our 
# instance it is created to use the boot.ci function. This function allows you to 
#calculte bootstrap confidence intervals 

boot.ci(time.list) 


#estimate the probability that the time taken by a random person to travel to work is less than 20 mins f(20)

p.hat = time.ecdf(20)
nt = length(CommuteStLouis$Time)
se.p.hat = sqrt(p.hat*(1-p.hat)/nt)

#confidence interval for f(20)

CIt = c(p.hat - 2*se.p.hat, p.hat + 2*se.p.hat)
CIt
#estimate the probability that the distance travelled by a random person to work is less than 10 miles f(10)

p.hatd = dis.ecdf(10)
nd = length(CommuteStLouis$Distance)

se.p.hatd = sqrt(p.hatd*(1-p.hatd)/nd)

#confidence interval for f(10)

CId = c(p.hatd - 2*se.p.hatd, p.hatd + 2*se.p.hatd)
CId


#extracting the data for different genders
males = subset(CommuteStLouis, Sex == "M")
females = subset(CommuteStLouis, Sex == "F")



#mean travel time for males and females
hist(males$Time)
hist(females$Time)
medianm = median(males$Time)
meanm = mean(males$Time)
meanf = mean(females$Time)


#mean distance travelled by males and females
hist(males$Distance)
hist(females$Distance)
meanmd = mean(males$Distance)
meanfd = mean(females$Distance)




#hypothesis at alpha = 0.05, both genders travel the same average distance for work
#Ho = meanmd = meanfd
#Ha = meanmd != meanfd


varm = var(males$Distance)
varf = var(females$Distance)

lenm = length(males$Distance)
lenf = length(females$Distance)

SE = sqrt((varm/lenm) + (varf/lenf))

tstat = (meanmd - meanfd)/SE

#couldnt figure out how to find out the pvalue for above t stat so used t.test function

#ttest for hypothesis testing
t.test(males$Distance, females$Distance, conf.level = .95)
#t = 0.7368
#p-value = 0.4616
#since the pvalue is significantly gtrater than the alpha = 0.05 we cannot reject the null hypothesis, which means that both males and females travel equal distance on average for work 



#density plots
d= density(males$Distance)
hist(males$Distance, probability = TRUE)
lines(d)
hist(females$Distance, probability = TRUE)
lines(density(females$Distance))

#mle of the means of the distance travelled by males and females.
#mle mean distance of males
mu_hat = mean(males$Distance)

#mle mean distance for females
mu_hatf = mean(females$Distance)
