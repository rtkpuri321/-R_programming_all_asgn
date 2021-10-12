#ASSIGNMENT1
v1=c()
a<-3
c<-10
m<-101
num<-750
x0<-13
total=c()

for(i in 1:750)
{
  v1<-append(v1,0)
  total<-append(total,0)
}

getmode<- function(v1) {
  uniqv<- unique(v1)
  uniqv[which.max(tabulate(match(v1, uniqv)))]
}

for(i in 1:6)
{
  
  cat(sprintf("Subject %d \n", i))
  for(j in 1:num)
  {
    x0=(a*x0 + c)%%m
    v1[j]=x0
  }
  
  
  print(v1)
  cat(sprintf("%f is mean \n", mean(v1,750)))
  
  
  cat(sprintf("%f is median \n", median(v1)))
  
  
  
  cat(sprintf("%f is mode \n", getmode(v1)))
  
  num=toString(i)
  res=paste("Subject ",num)
  res1=paste(res,'.png')
  jpeg(file=res1)
  
  hist(v1,main=res,xlab = "Marks",col = "yellow",border = "blue")
  dev.off()
  
  total=total+v1[]
  
  x0=v1[750]
}

cat(sprintf("total of 6 Courses: \n"))
print(total)

cat(sprintf("%f is mean \n", mean(total,750)))


cat(sprintf("%f is median \n", median(total)))



cat(sprintf("%f is mode \n", getmode(total)))

jpeg(file="Total.png")

hist(total,main="Total of 6 Subject",xlab = "Marks",col = "yellow",border = "blue")
dev.off()

#ASSIGNMENT2
library(Hmisc)

data <- read.csv("Sacramentorealestatetransactions.csv")

print(data)


data[is.na(data)] <- 0

print("Mean: ");

#Mean of all columns

colMeans(data[sapply(data, is.numeric)]) 

print("Median: ");

#Median of all columns

apply(data[,c(1:ncol(data))], 2, median) 


#Standard deviation

print("Standard Deviation:");

apply(data[,c(2:ncol(data))], 2, sd)


#Plotting histogram for all values

jpeg(file="Allvalues.jpg")
hist.data.frame(data)
dev.off()

#Outliers

print("Outliers")

outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  for(i in x)
  {
    if (i>upper_limit | i<lower_limit)
      print(i)
  }
  
}

print_outliers<- function(data, cols = names(data)) {
  for (col in cols) {
    cat(sprintf("%s: \n", col))
    outliers(data[,col])
  }
}

print_outliers(data, colnames(data))
#colnames(data)

#OULIERS
x=summary(data$zip)
x
for(i in data$zip)
{
  if(i>x[5] | i<x[1])
    print(i)
}

#Assignment-3
library(moments)
library(Metrics)

mode <- function(v)
{
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
subjects = c("Sub1", "Sub2", "Sub3", "Sub4", "Sub5", "Sub6")
m = matrix(nrow = 6, ncol = 750)
q = matrix(nrow = 1, ncol = 750)
#sum = runif(750, min = 0, max = 0)
#sum = rnorm(750)
sum = round(sample(0, size=750, replace=TRUE), 0)
for(i in 1:6)
{
  print(paste("Subject :",subjects[i]))
  #a = round(runif(1, max = 50), 0)
  #c = round(runif(1, max = 50), 0)
  #a = round(rnorm(1, mean=50), 0)
  #c = round(rnorm(1, mean=50), 0)
  #a = round(sample(1:100, size=1, replace=TRUE), 0)
  #c = round(sample(1:100, size=1, replace=TRUE), 0)
  for(j in 1:750)
  {
    m[i,j] = round(sample(1:100, size=1, replace=TRUE), 0)
    q[1,j] = round(sample(1:100, size=1, replace=TRUE), 0)
    sum[j] = sum[j] + m[i,j]
  }
  print(paste("Mean : ", round(mean(m[i,]), 2)))
  print(paste("Median : ", median(m[i,])))
  print(paste("Mode : ", mode(m[i,])))
  print(paste("Standard Deviation : ",round(sd(m[i,]), 2)))
  print(paste("Range : ", range(m[i,])))
  print(paste("Mean Deviation : ", mad(m[i,], center=mean(m[i,]))))
  print(paste("Variance : ", var(m[i,])))
  print(paste("RMSE : ", rmse(m[i,], q[1,])))
  print(paste("Skewness : ", skewness(m[i, ])))
  print(paste("Kurtosis : ", kurtosis(m[i, ])))
  plot(density(m[i, ]), main = paste("Gaussian Plot : ", subjects[i]))
  writeLines("\n")
}

#ASSIGNMENT 4
#QUES1
#A. 0.747565
n=floor(runif(1, 2,100))
n
prod=1
i=0
while(i<n)
{
  prod=prod*((365-i)/365)
  i=i+1
}
#prod
prob=1-prod
cat("Probabaility of two people having same birthday for ",n," persons: ",prob)

n=1
prob=0
while(prob<0.5)
{
  prod=1
  i=0
  while(i<n)
  {
    prod=prod*((365-i)/365)
    i=i+1
  }
  prob=1-prod
  n=n+1
}
cat("Smallest Value of n for which probability is greater than 0.5: ",n-1,"\n")

#C
n=floor(runif(1, 2 , 365))
prob<-vector(length=n)
s<-1
for(i in 0:n-1){
  s=(s*(365-i))/365
  pro=1-s
  prob[i]=pro
}

sameday=sum(prob)/n
print(cat("Probability on same day for ",n," persons: ",sameday))
plot(prob)

#ques2
n=300
x=0
arr=c()
amt=0
for(i in 1:n)
{
  k=sample(c(0,1),size=10, replace = TRUE, c(0.4,0.6))
  print(k)
  x=sum(k)
  amt=(x*x)-(7*x)
  arr=append(arr,amt)
}

hist(arr)
summary(arr)

if(sum(arr)>0)
{
  cat(sprintf("good"))
} else {
  cat(sprintf("bad"))
}

#ASSIGNMENT-5
#ques1
sum=0

for(i in 1:1000)
{
  
  s=rbinom(n=250,size=1,prob=0.8)
  
  v=table(s)
  
  b=rle(s)
  b
  a=sum(rle(s)$length==16)
  sum=sum+(a/250)
}

print(v)
cat("Average probability for 1000 rounds:", sum/1000)

#QUES2
s = rbinom(n=8,size=1, prob=0.5)
#print(rle(s)$value)
#print(rle(s)$length)
rle(s)
a = sum(rle(s)$length >= 2)
cat("Probability in sample size 8:",1 - a/8)

s2 = rbinom(n=250,size=1, prob=0.5)
#print(rle(s2)$value)
#print(rle(s2)$length)
a2 = sum(rle(s2)$length >= 2)
cat("Probability in sample size 250:",1 - a2/250)

#ASSIGNMENT-6
#ques1
fun<- function(b,a)
{
  x=sample(b)
  print(x)
  if(identical(x,b))
    return(1)
  else{
    return(0)
  }
  
}

a<-c("dog","cat","rat","lion","fox","tiger")
b=sort(a)
n=100
s<-replicate(n,fun(b,a))
s
print(sum(s)/n)
plot(s)

#QUES2
fun<- function(b,a)
{
  x=sample(b)
  if(identical(x,b))
    return(1)
  else{
    return(0)
  }
  
}
a<-c("dog","dog","dog","horse","horse","horse")
b=sort(a)
n=100
s<-replicate(n,fun(b,a))
print(sum(s)/n)
plot(s)


#ASSIGNMENT 7
#Ques1
heights<-rnorm(1000, 1.70, 0.1)
d<-(density(heights))
plot(d, main = 'Density')
polygon(d, border = 'purple')

heights1<-rnorm(10000, 1.70, 0.1)
d1<-(density(heights1))
lines(d1, main = 'Density')
polygon(d1, border = 'red')

c(qnorm(0.05, 1.70, 0.1), qnorm(0.95, 1.70, 0.1))
quantiles<-seq(0, 1, by = .05)
qvalues<-qnorm(quantiles)
plot(qvalues,
     type = 'l',
     xaxt = 'n',
     xlab = 'Probability Density',
     ylab = 'Z scores')

pvalues<-pnorm(quantiles)
plot(pvalues,
     xaxt = 'l',
     main = 'cdf of the standard normal',
     xlab = 'Quantiles',
     ylab = 'Probability Density')
#ques2
a<-read.csv("auto.csv")
res1 <- cor.test(a[,3], a[, 4], method = "pearson")
print(res1)

data1<-a[, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12)]
correlation_matrix <- cor(data1)

df_new <- data.frame(data1)

#install.packages('corrgram')
library(corrgram)
corrgram(df_new, order = TRUE,
         lower.panel = panel.shade,
         upper.panel = panel.pie,
         text.panel = panel.txt,
         main = "correlegram")

data2<-a[, c(3, 4, 5, 6)]
df2 <- data.frame(data2)

corrgram(df2, order = TRUE,
         lower.panel = panel.shade,
         upper.panel = panel.pts,
         text.panel = panel.txt,
         main = "correlegram")

#ASSIGNMENT8
dataset<-read.csv('regressionDataSet.csv')
#install.packages('caTools')
# install.packages('Metrics')
library(caTools)
library(Metrics)
split = sample.split(dataset$RMSD, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
regressor = lm(formula = RMSD ~ .,data = training_set)
y_pred = predict(regressor, newdata = test_set)
#correlation between predicted value and actual value on testing data
res1 <- cor.test(y_pred, test_set$RMSD, method = "pearson")
print(res1)
#ACCURACY
rmse(dataset$RMSD,y_pred)
plot(y_pred ~ test_set$RMSD, data = test_set)
abline(lm(y_pred ~ test_set$RMSD))

#QUES2 Assignment-8
x <- rpois(100, 50)
y <- rpois(100, 100)
z <- rpois(100, 150)

model<- lm(z ~ x + y)
summary(model)
plot(z, x + y)

model1 <-lm(y ~ x)
summary(model1)

model2 <-lm(y ~ poly(x, 2, raw = TRUE))
summary(model2)

model3 <-lm(log(y) ~ x)
summary(model3)


