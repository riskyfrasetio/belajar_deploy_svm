library(e1071)
load("data_training.RData")
model.svm=svm(kredit.bermasalah~.,data=data.step3.olah1,kernel="linear",cost=100,scale=TRUE, type="C-classification")
