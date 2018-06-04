library(UsingR)
library(prob)
library(sampling)
library(stringi)
library(stringr)
library(repr) 
options(repr.plot.height=6)

#Preparing the data . Import the data set into R. . Document the steps for the import process 
#and any preprocessing had to be done prior to or after the import. Any R code used in the 
#process should be included. 
#https://vincentarelbundock.github.io/Rdatasets/datasets.html ais.csv
# getting directory to place data
getwd()
#Reading File
x1 <- read.csv(file="ais.csv", header=TRUE, sep=",")
#displaying data
x1
# Taking the relevant columns from the dataset and creating new dataframe
aus.athletes <- data.frame(x1$wcc, x1$bmi, x1$ht,
                           x1$wt, x1$sex, x1$sport)
# Changing headings of data frame
col_headings <- c('WBC','BMI','HT','WT','sex','sport')
names(aus.athletes) <- col_headings
#Do the analysis as in Module3 for at least one categorical variable and at 
#least one numerical variable. Show appropriate plots for your data. 
#Getting number of participants by gender and sport
aus.athletes
x <- table(aus.athletes$sex, aus.athletes$sport)
dimnames(x) <- list(Gender = rownames(x), Sport = colnames(x)) 
x
# get number by gender
margin.table(x,1)
# get number by sport
margin.table(x,2)
# sum of columns and rows
addmargins(x)
options(digits = 2)
# fraction of sport accross row by sex each row == 1
prop.table(x,1)
#fraction of sex by sport each column == 1
prop.table(x,2)
#barplot of Australian Atheletes by sport and sex
barplot(x,col = c("red","blue"), beside = TRUE, main = "Australian Atheletes",
        ylim = c(0,25))
legend("topright", 
       legend = c("Female", "Male"), 
       fill = c("red", "blue"))
# barplot Australian Atheletes by Gender
barplot(t(x),col = rainbowPalette(n=10), ylim = c(0,30), beside = TRUE,
        main = "Australian Atheletes", xlab = "Gender")
legend("top", 
       legend = unique(aus.athletes$sport), 
       fill = rainbowPalette(n=10))
#Stem and leaf plot The numbers range from 149-209. Most values fall between 170 and 194, and 
#it's easy to see that the mode is 180. #The distribution appears slightly left skewed.
height <- aus.athletes$HT
height
stem(height)
# get the five number summary
f = fivenum(height)
f
# check for ouliers
whiskers <- c(f[2] - 1.5*(f[4] - f[2]), f[4] + 1.5*(f[4] - f[2])) 
whiskers # < 156 > 204 outliers are 149 149 and 209
height[height < whiskers[1]] 
height[height > whiskers[2]]
# display boxplot of 5 number summary of atheletes Height
boxplot(height, main = "Australian Atheletes Height 5 Number Summary", 
        horizontal = TRUE, xaxt = "n", las=2, col = c("blue"))
axis(side = 1, at = fivenum(height), labels = TRUE)

# Pick one variable with numerical data and examine the distribution of the data. 
WBC <- aus.athletes$WBC # White Blood Count of atheletes
WBC
#Calculate mean of WBC
mu <- mean(WBC)
mu
n <- length(WBC)  # number of values

# Calculate Standard Deviation
std <- sd(WBC)
std
sigma <-sqrt((std ^ 2) * ((n - 1) / n))
sigma

# calculate pdf for plot of WBC
pdf <- dnorm(WBC, mean = mu, sd = sigma)
# pdf plot of WBC a normal distribution that is skewed Right
plot(WBC, pdf, col = "red", main = " White Blood Count of Atheletes")
legend("topright", 
       legend = c("Mean", round(mu, digits = 1) , "SD", round(sigma, digits = 1 )))
# CDF probability atheletes WBC within mean of 7.1 is 0.5 or 50%
pnorm(mu, mean = mu, sd = sigma)
#
#The cumulative probability of a WBC within less than the 3rd standard deviation, 
#mu - 3*sigma = 1.7 is  0.0013
mu - 3*sigma
pnorm(mu - 3*sigma, mean = mu, sd = sigma)

# calculate cumulative distribution function for WBC
cdf <- pnorm(WBC, mean = mu, sd = sigma)
cdf
# Plot CDF of WBC
plot(WBC, cdf, col = "red", main = " White Blood Count of Atheletes")
legend("topright", 
       legend = c("Mean", round(mu, digits = 1) , "SD", round(sigma, digits = 1 )))
# the quantile function does the reverse of pnorm, you give it the probability and it returns
#the number for that probability. in this case the mean
qnorm(0.5, mean = mu, sd = sigma)
# the reverse of mu - 3*sigma from above
qnorm(0.0013, mean = mu, sd = sigma)
# random generated values with mu and sigma from WBC plotted on graph
x <- rnorm(1000, mean = mu, sd = sigma)
x <- round(x)
x
plot(table(x), type = "h", col = "violet", ylab = "Frequency 1000 Random Values",
     xlab = "White Blood Counts")
legend("topright", 
       legend = c("Mean", round(mu, digits = 1) , "SD", round(sigma, digits = 1 )))


# Draw various random samples of the WBC data and show the applicability of the 
# Central Limit Theorem for this variable. 

set.seed(100)
# random 1000 samples from mean and standard deviation of WBC
x <- rnorm(1000, mean = 7.1, sd = 1.8)
# Histogram of this distribution with the normal curve shows right skewness
hist(x, prob = TRUE, breaks = 15)
curve(dnorm(x, mean = 7.1, sd = 1.8), 
      add = TRUE, col = "red")

options(digits=2)
mean(x)
sd(x)

# 10,000 random samples drawn with sample size of 5
samples <- 10000
sample.size <- 5

xbar <- numeric(samples)

for (i in 1: samples) {
  xbar[i] <- mean(rnorm(sample.size, 
                        mean = 7.1, sd = 1.8))
}
# histogram of 10000 random samples with sample size 5
hist(xbar, prob = TRUE, breaks = 15)


mean(xbar)
sd(xbar)

par(mfrow = c(2,2))

for (size in c(10, 20, 30, 40)) {
  for (i in 1:samples) {
    xbar[i] <- mean(rnorm(size, 
                          mean = 7.1, sd = 1.8))
  }
  
  hist(xbar, prob = TRUE,  main = paste("Sample Size =", size), breaks = 10)
  
  cat("Sample Size = ", size, " Mean = ", mean(xbar),
      " SD = ", sd(xbar), "\n")
}

par(mfrow = c(1,1))


# The larger the sample size the more normal the distribution gets
# 10 and 20 a little right skewed 30 and 40 closer to Normal distribution.
# proving Central Limit Theorem 

#Show how various sampling methods can be used on your data. 

#sample drawn using simple random sampling without replacement.
#Showing the frequencies for each Sport. Showing the percentages of these 
#with respect to the entire dataset.
aus.athletes

sample.size <- 15

set.seed(123)
#### srswor ####
s <- srswor(sample.size, nrow(aus.athletes))

s[s != 0]

rows <- (1:nrow(aus.athletes))[s!=0]
rows <- rep(rows, s[s != 0])
rows

sample.1 <- aus.athletes[rows, ]
sample.1
par(mfrow = c(2,2))

table(sample.1$sport)
x <- table(sample.1$sport)
plot(x, ylab = "Frequency")
y = table(sample.1$sport)

p <- y/sum(y) # get percent for 15 sample size
p
x <- table(aus.athletes$sport)
p1 <- x/sum(x) # get percent for whole dataset
options(digits = 1)
p1
# 7of 10 sports sampled
length(unique(sample.1$sport)) 
length(table(aus.athletes$sport))

#15 samples drawn using systematic sampling. frequencies for each  
#sport. 

set.seed(123)
#

N <- nrow(aus.athletes)
n <- 15

k <- ceiling(N / n)
k

r <- sample(k, 1)
r

# select every kth item
s <- seq(r, by = k, length = n)
# put sample selected in sample.3
sample.3 <- aus.athletes[s, ]
sample.3
table(sample.3$sport)
x <- table(sample.3$sport)
plot(x, ylab = "Frequency")
#9 out of 10 sports were sampled
length(unique(sample.3$sport))
length(table(aus.athletes$sport))

#c) Calculating the inclusion probabilities using the WBC variable.  
#showing the sample drawn using systematic sampling. Showing the frequencies 
#for each sport. 

set.seed(123)

sample.size = 15

pik <- inclusionprobabilities(
  aus.athletes$WBC, sample.size)

length(pik)
sum(pik)
#selecting samples
s <- UPsystematic(pik)
#putting samples in sampl.4
sample.4 <- aus.athletes[s != 0, ]
sample.4
# Display Freq of sample.4$sport
table(sample.4$sport)
x <- table(sample.4$sport)
plot(x, ylab = "Frequency")
y = table(sample.4$sport)

p5 <- y/sum(y) # get percent for 15 sample size
p5
x <- table(aus.athletes$sport)
p6 <- x/sum(x) # get percent for whole dataset
options(digits = 1)
p6
# 8 out of 10 sports sampled
length(unique(sample.4$sport))
length(table(aus.athletes$sport))

#ordering data using the sport variable. Drawing a stratified sample using 
#proportional sizes based on the sport variable. showing frequencies for each 
#sport. 

set.seed(123)
sample.size <- 70
order.index <- order(aus.athletes$sport)
data <- aus.athletes[order.index, ]

st <- strata(data, stratanames = c("sport"),
             size = rep(1, sample.size) , 
             method = "srswor")

sample.5 <- getdata(data, st)

table(sample.5$sport)
x <- table(sample.5$sport)
plot(x, ylab = "Frequency")

# proportion
freq <- table(aus.athletes$sport)
p7 <- freq/sum(freq)  # percent sport from aus.atheletes dataset
p7

sizes <- round(70 * freq / sum(freq))
p8 <- sizes/sum(sizes) # percent from ordered stratified proportion
p8

st <- strata(data, stratanames = c("sport"),
             size = sizes, method = "srswor")
sample <- getdata(data, st)
par(mfrow = c(1,1))
#e) Comparing the means of WBC variable for these four samples with the entire data.
options(digits = 2)
mean(aus.athletes$WBC) # mean WBC entire data set
mean(sample.1$WBC) 
sample.1$WBC
mean(sample.3$WBC)
sample.3$WBC
mean(sample.4$WBC)
sample.4$WBC
mean(sample.5$WBC) 
sample.5$WBC
# The 1st 3 means are pretty close, 7.1 is mean for dataset sample.1 = 7.2 , 
#sample.3 = 7.3, the last 2 means are both .6 from dataset mean of 7.1 
#sample.4 = 7.7, and sample.5 = 6.5.


#For confidence levels of 80 and 90, show the confidence intervals of the mean 
#of the numeric variable for various samples and compare against the population mean
# Using BMI of atheletes for this task

mu <-mean(aus.athletes$BMI)
mu
sigma <-sd(aus.athletes$BMI)
sigma

set.seed(150)

samples <- 5000
sample.size <- 50
# taking 5000 samples mean  and sd 
x1 <- rnorm(samples, mean = mu, sd =  sigma)

xbar <- numeric(samples)

for (i in 1: samples) {
  xbar[i] <- mean(rnorm(sample.size, 
                        mean = mu, sd =  sigma))
}

x <- xbar
# plotting confidence at any percent default 95.44 
plot.confidence <- function (conf = 95.44) {
  alpha <- 1 - conf/100
  z <- qnorm(1 - alpha/2)
  print(z, digits=4)
  #plotting bellcurve line
  
  f1 <- curve(dnorm(x), from=-3, to=3, 
              xaxt="n", yaxt="n",
              xlab = "z")
  
  title(paste("Confidence =", conf, "%"))
  
  axis(side=1, at=c(-z, 0, z), las=0,
       labels=formatC(c(-z, 0, z), digits=3))
  
  # filling under the curve 
  
  polygon(f1$x, f1$y, col="lightblue")
  
  x.1 <- seq(-3, -z, by = 0.05)
  y.1 <- dnorm(x.1)
  x.1 <- c(x.1, -z, -z)
  y.1 <- c(y.1, dnorm(-z), dnorm(-3))
  
  # filling in alpha under the curve
  
  polygon(x.1, y.1, col="white")
  
  x.2 <- seq(3, z, by = -0.05)
  y.2 <- dnorm(x.2)
  
  x.2 <- c(x.2, z, z)
  y.2 <- c(y.2, dnorm(z), dnorm(3))
  
  polygon(x.2, y.2, col="white")
  
  # lines(c(0,0), c(dnorm(-3), dnorm(0)), lty=2)
  
  text(0, 0.2, 1-alpha)
  text(-2.6, 0.2, alpha/2)
  text(2.6, 0.2, alpha/2)
  
  return (z)
}
# plotting confidence of 90% and 80%
par(mfrow = c(1,2))
plot.confidence(90)
plot.confidence(80)
par(mfrow = c(1,1))

# Drawing 100 samples from the above data. Plot the confidence intervals for
# these 100 samples. Determine how many samples do not have the population
# mean within their confidence intervals. Repeat for 80% and 90%


par(mfrow = c(1,2))
set.seed(150)

pop.mean <- mu

pop.sd <- sigma


x <- rnorm(50000, mean = pop.mean, sd = pop.sd)
x <- as.integer(x)

sample.size <- 5

sd.sample.means <- pop.sd/sqrt(sample.size)
sd.sample.means

sample.data <- sample(x, size=sample.size)
sample.data

xbar <- mean(sample.data)
xbar

cat("90% Conf Interval = ",
    xbar - 1.64*sd.sample.means, "-", 
    xbar + 1.64*sd.sample.means, "\n")

samples <- 40

xbar2 <- numeric(samples)
# print sample means and confidence levels
for (i in 1: samples) {
  sample.data.1 <- sample(x, size=sample.size)
  xbar2[i] <- mean(sample.data.1)
  str <- sprintf("%2d: xbar = %.2f, CI = %.2f - %.2f",
                 i, xbar2[i], xbar2[i] - 1.64*sd.sample.means,
                 xbar2[i] + 1.64*sd.sample.means)
  cat(str,"\n")
}
# Above print to screen 100 samples with confidence intervals
xbar2
# number outside the range
sum(abs(xbar2-pop.mean) > 1.64*sd.sample.means)

# Plot the intervals

matplot(rbind(xbar2 - 1.64*sd.sample.means, xbar2 + 1.64*sd.sample.means),
        rbind(1:samples, 1:samples), xlab = "50,000 random sample with sample size 5"
        ,type="l", lty=1)
abline(v = pop.mean)

# Do same thing for 80% confidence 
cat("80% Conf Interval = ",
    xbar - 1.28*sd.sample.means, "-", 
    xbar + 1.28*sd.sample.means, "\n")

samples <- 40

xbar2 <- numeric(samples)

for (i in 1: samples) {
  sample.data.1 <- sample(x, size=sample.size)
  xbar2[i] <- mean(sample.data.1)
  str <- sprintf("%2d: xbar = %.2f, CI = %.2f - %.2f",
                 i, xbar2[i], xbar2[i] - 1.28*sd.sample.means,
                 xbar2[i] + 1.28*sd.sample.means)
  cat(str,"\n")
}
# Above print to screen 100 samples with confidence intervals
xbar2
# number outside the range
sum(abs(xbar2-pop.mean) > 1.28*sd.sample.means)

# Plot the intervals

matplot(rbind(xbar2 - 1.28*sd.sample.means, xbar2 + 1.28*sd.sample.means),
        rbind(1:samples, 1:samples), xlab = "50,000 random sample with sample size 5",  type="l", lty=1)
abline(v = pop.mean)
par(mfrow = c(1,1))

