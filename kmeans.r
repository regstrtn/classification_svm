Sys.setenv(https_proxy="http://10.3.100.207:8080")
Sys.setenv(http_proxy="http://10.3.100.207:8080")
setwd("/Users/mohammadluqman/Downloads/sem1/clab1/a9/")
set.seed(7)
library(ggplot2)
library(dplyr)
library(e1071)
library(caret)
library(doMC)
library(stats)

set.seed(7)

power <- read.csv("~/Downloads/sem1/clab1/a9/power.csv", sep=";")
power = power[,-c(1,2)]
power = data.frame(lapply(power, function(x) as.numeric(as.character(x))))
acc = rep(0, times = 20)
withinss = rep(0, times = 20)
time = rep(0, times = 20)

for (i in 1:ncol(power)) {
  power[is.na(power[,i]),i] = 0
}

for (i in 5:10) {
  clusters = i
  p = Sys.time()
  k = kmeans(power, i)
  if(k$ifault == 4) k = kmeans(power, i, algorithm = "MacQueen")
  acc[i] = k$betweenss/k$totss
  withinss[i] = k$tot.withinss
  time[i] = Sys.time() - p
}




