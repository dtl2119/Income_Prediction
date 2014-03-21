#########   Make Sure the matricies train, theFormula, and test are defined from the ridge.R script   #########




##### LASSO (alpha = 1)
require(glmnet)

# Create input matrix
trainXinputs<-build.x(theFormula, data=train, contrasts=F)
trainX<-as.matrix(data.frame(train$age,  train$education.num, train$capital.gain, train$capital.loss, train$hours.per.week, trainXinputs))


set.seed(9999)
trainCVLasso<-cv.glmnet(trainX, train$income, family="binomial", alpha=1)




# CV curve for glmnet on train data
plot(trainCVLasso)
title("Cross-Validation Error vs. Lambda - LASSO", line = 3)


# Coefficient profile
plot(trainCVLasso$glmnet.fit, xvar="lambda", xlab="ln(lambda)")
abline(v=log(c(trainCVLasso$lambda.min, trainCVLasso$lambda.1se)))
title("Coefficient Profile - LASSO", line = 3)

##### Fit to test data

# create test input matrix
testXinputs<-build.x(theFormula,data=test,contrasts=F)
testX<-as.matrix(data.frame(test$age,  test$education.num, test$capital.gain, test$capital.loss, test$hours.per.week, testXinputs))

test.predict<-predict(trainCVLasso, testX, trainCVLasso$lambda.min)
test.predict[test.predict>0]<-1
test.predict[test.predict<=0]<-0


# Run contingency
result.table<-as.matrix(xtabs(~test.predict + test$income))



# incorrect/total
(result.table[2,1] + result.table[1,2])/sum(result.table) * 100
# 14.82187% error


#Coefficient Plot
require(ggplot2)
theCoef<-as.matrix(coef(trainCVLasso, s="lambda.min"))
coefDF<-data.frame(Value=theCoef, Coefficient=rownames(theCoef))
lasso.coefDF<-coefDF[nonzeroCoef(coef(trainCVLasso, s="lambda.min")),]
ggplot(coefDF, aes(x=X1, y=reorder(Coefficient,X1))) +  geom_vline(xintercept=0, color="grey", linetype=2) + geom_point(color="green") + labs(x="Value", y="Coefficient", title = "Variable Influence - LASSO")


