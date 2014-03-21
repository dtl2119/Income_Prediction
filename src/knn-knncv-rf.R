# SETUP

# Notes: Some of this takes a maaaad long time

# require packages
require(stringr)
require(useful)
require(cluster)
require(ggplot2)
require(class)
require(randomForest)

# set random seed
set.seed(1014)

# set working directory
setwd("~/Documents/School/Year Four/Second Semester/2 - STAT 4249 - Applied Data Science/Homework/Homework 3")

# original data
rawdata = read.csv("hw3data.csv", header = FALSE)

# remove spaces
# make into data.frame
Data = data.frame(apply(rawdata, 2, function(x) gsub(" ", "", x)))

# clean up
names(Data) = c("age", "workclass", "fnlwgt", "education", "education.num", "marital.status", "occupation", 
                "relationship", "race", "sex", "capital.gain", "capital.loss", "hours.per.week", 
                "native.country", "income")

# change classes
Data$age = as.integer(Data$age)
Data$fnlwgt = as.integer(Data$fnlwgt)
Data$education.num = as.integer(Data$education.num)
Data$capital.gain = as.integer(Data$capital.gain)
Data$capital.loss = as.integer(Data$capital.loss)
Data$hours.per.week = as.integer(Data$hours.per.week)

# make into binary outcome
Data$income = str_replace_all(Data$income, "<=50K", "0")
Data$income = str_replace_all(Data$income, ">50K", "1")
Data$income = as.integer(Data$income)

# values
half = ceiling(dim(Data)[1]/2)
end = dim(Data)[1]

# split data
train = Data[1:half,]
test = Data[(half+1):end,]

# make formula
f = income ~ age + workclass + fnlwgt + education + education.num + marital.status + 
  occupation + relationship + race + sex + capital.gain + capital.loss +
  hours.per.week + native.country - 1

# CREATE FULL MODEL DATA

# data.frames
train.frame = data.frame(build.x(f, data = train, contrast = FALSE))
test.frame = data.frame(build.x(f, data = test, contrast = FALSE))
Data.frame = data.frame(build.x(f, data = Data, contrast = FALSE))
response.train.frame = data.frame(build.y(f, data = train))
response.test.frame = data.frame(build.y(f, data = test))
response.Data.frame = data.frame(build.y(f, data = Data))

# matrices
train.matrix = build.x(f, data = train, contrast = FALSE)
test.matrix = build.x(f, data = test, contrast = FALSE)
Data.matrix = build.x(f, data = Data, contrast = FALSE)
response.train.matrix = build.y(f, data = train)
response.test.matrix = build.y(f, data = test)
response.Data.matrix = build.y(f, data = Data)

# KMEANS CLUSTERING

# find optimal k
bestk = FitKMeans(x = train.frame, max.clusters = 20, seed = 1014)
PlotHartigan(bestk)

# cluster, k = 11 (best k from hartigan)
means = kmeans(train.frame, centers = 11, iter.max = 50, nstart = 1, algorithm = "Hartigan-Wong")

# take mean of income for each cluster
### make this more elegant
average = c(rep(0, 11))
for(i in 1:11)
{
  average[i] = sum(train$income[means$cluster == i])/length(train$income[means$cluster == i])
}

# cluster, k = 2 (try to make kmeans do the classifying for me)
means = kmeans(train.frame, centers = 2, iter.max = 50, nstart = 1, algorithm = "Hartigan-Wong")

# take mean of income for each cluster
### make this more elegant
average = c(rep(0, 2))
for(i in 1:2)
{
  average[i] = sum(train$income[means$cluster == i])/length(train$income[means$cluster == i])
}

# gap statistic (finds optimal k)
##### does not converge, BOTH take forever
#gapstat = clusGap(train.frame, K.max = 2, FUNcluster = pam, B = 3)
#system.time(pam(train.frame, k = 2, cluster.only = TRUE))

# K NEAREST NEIGHBORS

# must factor response matrix (not data frame)

# k = 1
nearest.1 = knn(train.frame, test.frame, cl = factor(response.train.matrix), k = 1)
nearest.1.rate = dim(test.frame[response.test.matrix != nearest.1,])[1]/dim(test.frame)[1]
# k = 2
nearest.2 = knn(train.frame, test.frame, cl = factor(response.train.matrix), k = 2)
nearest.2.rate = dim(test.frame[response.test.matrix != nearest.2,])[1]/dim(test.frame)[1]
# k = 3
nearest.3 = knn(train.frame, test.frame, cl = factor(response.train.matrix), k = 3)
nearest.3.rate = dim(test.frame[response.test.matrix != nearest.3,])[1]/dim(test.frame)[1]
# k = 5
nearest.5 = knn(train.frame, test.frame, cl = factor(response.train.matrix), k = 5)
nearest.5.rate = dim(test.frame[response.test.matrix != nearest.5,])[1]/dim(test.frame)[1]
# k = 10
nearest.10 = knn(train.frame, test.frame, cl = factor(response.train.matrix), k = 10)
nearest.10.rate = dim(test.frame[response.test.matrix != nearest.10,])[1]/dim(test.frame)[1]
# k = 15
nearest.15 = knn(train.frame, test.frame, cl = factor(response.train.matrix), k = 15)
nearest.15.rate = dim(test.frame[response.test.matrix != nearest.15,])[1]/dim(test.frame)[1]
# k = 20
nearest.20 = knn(train.frame, test.frame, cl = factor(response.train.matrix), k = 20)
nearest.20.rate = dim(test.frame[response.test.matrix != nearest.20,])[1]/dim(test.frame)[1]

# K NEAREST NEIGHBORS - CROSS VALIDATION

# try using all data?  not sure how this method actually classifies anything

# k = 1
nearest.cv.1 = knn.cv(Data.frame , cl = factor(response.Data.matrix), k = 1, l = 1)
nearest.cv.1.rate = dim(Data.frame [response.Data.matrix != nearest.cv.1,])[1]/dim(Data.frame )[1]
# k = 2
nearest.cv.2 = knn.cv(Data.frame , cl = factor(response.Data.matrix), k = 2, l = 1)
nearest.cv.2.rate = dim(Data.frame [response.Data.matrix != nearest.cv.2,])[1]/dim(Data.frame )[1]
# k = 3
nearest.cv.3 = knn.cv(Data.frame , cl = factor(response.Data.matrix), k = 3, l = 1)
nearest.cv.3.rate = dim(Data.frame [response.Data.matrix != nearest.cv.3,])[1]/dim(Data.frame )[1]
# k = 5
nearest.cv.5 = knn.cv(Data.frame , cl = factor(response.Data.matrix), k = 5, l = 1)
nearest.cv.5.rate = dim(Data.frame [response.Data.matrix != nearest.cv.5,])[1]/dim(Data.frame )[1]
# k = 10
nearest.cv.10 = knn.cv(Data.frame , cl = factor(response.Data.matrix), k = 10, l = 1)
nearest.cv.10.rate = dim(Data.frame [response.Data.matrix != nearest.cv.10,])[1]/dim(Data.frame )[1]
# k = 15
nearest.cv.15 = knn.cv(Data.frame , cl = factor(response.Data.matrix), k = 15, l = 1)
nearest.cv.15.rate = dim(Data.frame [response.Data.matrix != nearest.cv.15,])[1]/dim(Data.frame )[1]
# k = 20
nearest.cv.20 = knn.cv(Data.frame , cl = factor(response.Data.matrix), k = 20, l = 1)
nearest.cv.20.rate = dim(Data.frame [response.Data.matrix != nearest.cv.20,])[1]/dim(Data.frame )[1]

# RANDOM FOREST

# regression
forest.r = randomForest(x = train.matrix, y = response.train.matrix)
forest.r.pred = predict(forest.r, test.matrix, type = "response")
forest.r.rate = dim(test.frame[response.test.matrix != forest.r.pred,])[1]/dim(test.frame)[1]

# classifier
forest.c = randomForest(x = train.matrix, y = as.factor(response.train.matrix))
forest.c.pred = predict(forest.c, test.matrix, type = "response")
forest.c.rate = dim(test.frame[response.test.matrix != forest.c.pred,])[1]/dim(test.frame)[1]

