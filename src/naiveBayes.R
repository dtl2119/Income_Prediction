require(e1071)

train<-adult[1:16281,]
test<-adult[16282:32561,]
classifierNB<-naiveBayes(train[,-15], train[,15])
resNB<-table(predict(classifierNB, test[,-15]), test[,15], dnn=list('predicted','actual'))
misclNB<-(resNB[1,2]+resNB[2,1])/sum(resNB)

