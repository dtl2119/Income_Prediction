### Applied Data Science - HW3 - Team 9
### SVM

# - data in "full.train" and "full.test"
# - data alread split into test and train sets

##############################################
# Coding response variables for SVM
n.train = dim(train)[1]
income = rep(-1, n.train)
income[train$V15 == ">50K"] = 1

n.test = dim(test)[1]
income.t = rep(-1, n.test)
income.t[test$V15 == ">50K"] = 1


##############################################
# Training best.svm()
new.svm = best.svm(full.train, y = income,
	tunecontrol = tune.control(sampling = "cross"))
new.pred = sign(predict(new.svm, full.test))

### Misclassification Rates
# Test
print(1 - sum(as.numeric(new.pred == income.t)) / n.test)
# Train
print(1 - sum(as.numeric(sign(new.svm$dec) == income)) / n.test)






