# Use RCurl package to increase timeout time (data file is large)
require(RCurl)
curlSetOpt(timeout = 200)




# Parse Data
original <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",header=FALSE)
colnames(original) <- c("age", "workclass","fnlwgt","education","education.num", "marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours.per.week","native.country", "income")

# keep an original copy
dat<-original



# Change income vector to binary value (1 if income > 50K, else -1)
dat$income<-as.character(dat$income)
dat$income<-gsub('\\s+', '', dat$income)
dat$income[dat$income == ">50K"] = 1  # 1 if >50K income
dat$income[dat$income == "<=50K"] = -1  # -1 if <=50K income
dat$income<-as.numeric(dat$income)


#dat$native.country<-gsub("-",".",dat$native.country)

n<-nrow(dat)


# Divide data (1st half train, 2nd half test)
train<-dat[1:ceiling(n/2),]
test<-dat[(ceiling(n/2)+1):n,]







# numeric: age, fnlwgt, education.num, capital.gain, capital.loss, hours.per.week
# factor: workclass, education, marital-status, occupation, relationship, race, sex
# omit: fnlwgt, education, and country

# Define formula
theFormula <- income ~ workclass+ education  + marital.status + occupation + relationship + race + sex + native.country - 1

# Create input matrix
require(useful)
trainXinputs<-build.x(theFormula, data=train, contrasts=F)
trainX<-as.matrix(data.frame(train$age, train$fnlwgt, train$education.num, train$capital.gain, train$capital.loss, train$hours.per.week, trainXinputs))



require(glmnet)
##### RIDGE (alpha = 0)
set.seed(9999)
trainCVRidge<-cv.glmnet(trainX, train$income, family="binomial", alpha=0)


# CV curve for glmnet on train data
plot(trainCVRidge)
title("Cross-Validation Error vs. Lambda - RIDGE", line = 3)

# Coefficient profile
plot(trainCVRidge$glmnet.fit, xvar="lambda", xlab="ln(lambda)")
abline(v=log(c(trainCVRidge$lambda.min, trainCVRidge$lambda.1se)))
title("Coefficient Profile - RIDGE", line = 3)

##### Fit to test data (and train)

# create test input matrix
testXinputs<-build.x(theFormula,data=test,contrasts=F)
testX<-as.matrix(data.frame(test$age,  test$education.num, test$capital.gain, test$capital.loss, test$hours.per.week, testXinputs))

# Test Preidiction
test.predict<-predict(trainCVRidge, testX, trainCVRidge$lambda.min)
test.predict[test.predict>0]<-1
test.predict[test.predict<=0]<-0

# Train Prediction
train.predict<-predict(trainCVRidge, trainX, trainCVRidge$lambda.min)
train.predict[train.predict>0]<-1
train.predict[train.predict<=0]<-0

# RUN CONTINGENCY
# Test data
test.result.table<-as.matrix(xtabs(~test.predict + test$income))


# CALCULATE PREDICTION ERROR (incorrect/total)
# test data
(test.result.table[2,1] + test.result.table[1,2])/sum(test.result.table) * 100
# 14.78501% error


#Coefficient Value Plot
require(ggplot2)
theCoef<-as.matrix(coef(trainCVRidge, s="lambda.min"))
coefDF<-data.frame(Value=theCoef, Coefficient=rownames(theCoef))
ridge.coefDF<-coefDF[nonzeroCoef(coef(trainCVRidge, s="lambda.min")),]
ggplot(coefDF, aes(x=X1, y=reorder(Coefficient,X1))) +  geom_vline(xintercept=0, color="grey", linetype=2) + geom_point(color="green") + labs(x="Value", y="Coefficient", title = "Variable Influence - RIDGE")






yugo<-train[train$native.country=="Yugoslavia",]

