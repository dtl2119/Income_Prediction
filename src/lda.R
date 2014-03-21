require(MASS)
xnam <- paste0("x", 1:99)
trainLDA<-cbind(income,as.data.frame(full.train))

names(trainLDA)<-c("y",xnam)
xfmla<-paste0("x", c(1:18,20:99)) #we get rid of the x19 variable because it is constant over the training set
fmla <- as.formula(paste("y ~ ", paste(xfmla, collapse= "+")))

classifierLDA<-lda(fmla,data=trainLDA)

testLDA<-as.data.frame(full.test[,-19])
names(testLDA)<-xfmla
pred<-predict(classifierLDA,testLDA)
res<-as.numeric(pred$class)-1
tbl<-table(abs(res-income.t))
misclLDA<-tbl[2]/sum(tbl)





#We sphere the data
newX<-full.train[,-19] %*% classifier$scaling
newXtest<-full.test[,-19] %*% classifier$scaling
newTestLDA<-as.data.frame(newXtest)
newRes<-as.numeric(newPred$class)-1
newTbl<-table(abs(newRes-income.t))
newMiscl<-newTbl[2]/sum(newTbl)

dataplot<-as.data.frame(cbind(newX,income))
c<-ggplot(dataplot,aes(LD1,fill=as.factor(income)))
c<-c+geom_bar(binwidth = 0.2,alpha=0.5,position='identity')
c<-c+geom_vline(xintercept=0.45)
c<-c+labs(title='Combined histograms of the data with the LDA boundary (train data)',
          x='Coordinate obtained after data sphering',
          y='Count') 
c+guides(fill=FALSE)

dataplot.t<-as.data.frame(cbind(newXtest,income.t))
c<-ggplot(dataplot.t,aes(LD1,fill=as.factor(income.t)))
c<-c+geom_bar(binwidth = 0.2,alpha=0.5,position='identity')
c<-c+geom_vline(xintercept=0.45)
c<-c+labs(title='Combined histograms of the data with the LDA boundary (test data)',
          x='Coordinate obtained after data sphering',
          y='Count') 
c+guides(fill=FALSE)


