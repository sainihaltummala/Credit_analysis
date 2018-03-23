#1a
library(class)
library(tree)
library(ROCR)

credit<-read.csv('Credit_Dataset.csv')

#1b
credit$PROFITABLE<-as.factor(ifelse(credit$PROFIT>=0,1,0))

#1c
credit$CHK_ACCT<-as.factor(credit$CHK_ACCT)
credit$SAV_ACCT<-as.factor(credit$SAV_ACCT)
credit$HISTORY<-as.factor(credit$HISTORY)
credit$TYPE<-as.factor(credit$TYPE)
credit$JOB<-as.factor(credit$JOB)

#1d
set.seed(11217)
valid_inst = sample(nrow(credit), 0.30*nrow(credit))
credit_test <- credit[valid_inst,]
credit_rest <- credit[-valid_inst,]

#1e
valid_inst<-sample(nrow(credit_rest),0.25*nrow(credit_rest))
credit_validationationation<-credit_rest[valid_inst,]
credit_train<-credit_rest[-valid_inst,]

#1f


#2a
attach(credit_train)

logmodel <- glm(PROFITABLE~AGE+DURATION+RENT+TELEPHONE+FOREIGN+CHK_ACCT+SAV_ACCT+HISTORY+JOB+TYPE,family = "binomial")
summary(logmodel)

logpred <- predict(logmodel,newdata=credit_validationationation, type = "response")
logpred1<-predict(logmodel,newdata = credit_train, type="response")
pred <- prediction(logpred,credit_validationationation$PROFITABLE)
pred1<- prediction(logpred1,credit_train$PROFITABLE)

tpr.perf = performance(pred, measure = "tpr")
tnr.perf = performance(pred, measure = "tnr")
acc.perf = performance(pred, measure = "acc")

plot(tpr.perf,ylim=c(0,1), col = 'red')
plot(tnr.perf, add=T, col = 'black')
plot(acc.perf,add=T, col = 'yellow')

best = which.max(slot(acc.perf,"y.values")[[1]])
max.acc = slot(acc.perf,"y.values")[[1]][best]
max.cutoff = slot(acc.perf,"x.values")[[1]][best]
print(c(accuracy= max.acc, cutoff = max.cutoff))
print(max.cutoff)
print(max.acc)
#2b
roc.valid = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.valid,col = 'red')
abline(a=0,b=1,lty=3)
roc.train<-performance(pred1, measure = "tpr", x.measure = "fpr")
plot(roc.train,col = 'black', add=T)


#3

credit_tree = tree(PROFITABLE~AGE+DURATION+RENT+TELEPHONE+FOREIGN+CHK_ACCT+SAV_ACCT+HISTORY+JOB+TYPE,credit_train)
summary(credit_tree)
plot(credit_tree)
text(credit_tree,pretty=1)

credit_pruned2 = prune.tree(credit_tree,best=2)
summary(credit_pruned2)
plot(credit_pruned2)
text(credit_pruned2,pretty=1)

credit_pruned5 = prune.tree(credit_tree,best=5)
summary(credit_pruned5)
plot(credit_pruned5)
text(credit_pruned5,pretty=1)

credit_pruned10 = prune.tree(credit_tree,best=10)
summary(credit_pruned10)
plot(credit_pruned10)
text(credit_pruned10,pretty=1)

credit_pruned15 = prune.tree(credit_tree,best=15)
summary(credit_pruned15)
plot(credit_pruned15)
text(credit_pruned15,pretty=1)

#3a

##TRAINING
training_account1 = sum(ifelse(credit_train$PROFITABLE==1,1,0))/nrow(credit_train)
training_account1

prediction_tree2 <- predict(credit_pruned2, newdata=credit_train)
class_train2 = ifelse(prediction_tree2[,2]>0.5,1,0)
training_account2 = sum(ifelse(credit_train$PROFITABLE==class_train2,1,0))/nrow(credit_train)
training_account2

prediction_tree5 <- predict(credit_pruned5, newdata=credit_train)
class_train5 = ifelse(prediction_tree5[,2]>0.5,1,0)
training_account5 = sum(ifelse(credit_train$PROFITABLE==class_train5,1,0))/nrow(credit_train)
training_account5

prediction_tree10 <- predict(credit_pruned10, newdata=credit_train)
class_train10 = ifelse(prediction_tree10[,2]>0.5,1,0)
training_account10 = sum(ifelse(credit_train$PROFITABLE==class_train10,1,0))/nrow(credit_train)
training_account10

prediction_tree15 <- predict(credit_pruned15, newdata=credit_train)
class_train15 = ifelse(prediction_tree15[,2]>0.5,1,0)
training_account15 = sum(ifelse(credit_train$PROFITABLE==class_train15,1,0))/nrow(credit_train)
training_account15

##VALIDATION
validation_tree1 = sum(ifelse(credit_validationation$PROFITABLE==1,1,0))/nrow(credit_validationation)
validation_tree1


prediction_tree2 <- predict(credit_pruned2, newdata=credit_validationation)
class_valid2 = ifelse(prediction_tree2[,2]>0.5,1,0)
validation_tree2 = sum(ifelse(credit_validationation$PROFITABLE==class_valid2,1,0))/nrow(credit_validationation)
validation_tree2

prediction_tree5 <- predict(credit_pruned5, newdata=credit_validationation)
class_valid5 = ifelse(prediction_tree5[,2]>0.5,1,0)
validation_tree5 = sum(ifelse(credit_validationation$PROFITABLE==class_valid5,1,0))/nrow(credit_validationation)
show(validation_tree5)

prediction_tree10 <- predict(credit_pruned10, newdata=credit_validationation)
class_valid10 = ifelse(prediction_tree10[,2]>0.5,1,0)
validation_tree10 = sum(ifelse(credit_validationation$PROFITABLE==class_valid10,1,0))/nrow(credit_validationation)
show(validation_tree10)

prediction_tree15 <- predict(credit_pruned15, newdata=credit_validationation)
class_valid15 = ifelse(prediction_tree15[,2]>0.5,1,0)
validation_tree15 = sum(ifelse(credit_validationation$PROFITABLE==class_valid15,1,0))/nrow(credit_validationation)
show(validation_tree15)

tree.size = c(1,2,5,10,15)
tree_train_accuracy = c(training_account1,training_account2,training_account5,training_account10,training_account15)
tree_valid_accuracy = c(validation_tree1,validation_tree2,validation_tree5,validation_tree10,validation_tree15)

plot(tree.size,tree_train_accuracy,col="green",type="b")
plot(tree.size,tree_valid_accuracy,col="orange",type="b")

table(credit_validation$PROFITABLE,class_valid10)

plot(credit_tree)

plot(credit_pruned10)
text(credit_pruned10)
##4
colnames(credit)

train_X = credit_train[,c(2:4,6,7,10,12,17,18,20)]
valid_X = credit_validation[,c(2:4,6,7,10,12,17,18,20)]
test_X = credit_test[,c(2:4,6,7,10,12,17,18,20)]

train_profitable = credit_train$PROFITABLE
valid_profitable = credit_validation$PROFITABLE
test_profitable = credit_test$PROFITABLE

knn_pred_x1 = knn(train_X,train_X,train_profitable,k=1)
knn_pred_x3 = knn(train_X,train_X,train_profitable,k=3)
knn_pred_x5 = knn(train_X,train_X,train_profitable,k=5)
knn_pred_x10 = knn(train_X,train_X,train_profitable,k=10)
knn_pred_x25 = knn(train_X,train_X,train_profitable,k=25)

knn_pred_1 = knn(train_X,valid_X,train_profitable,k=1)
knn_pred_3 = knn(train_X,valid_X,train_profitable,k=3)
knn_pred_5 = knn(train_X,valid_X,train_profitable,k=5)
knn_pred_10 = knn(train_X,valid_X,train_profitable,k=10)
knn_pred_25 = knn(train_X,valid_X,train_profitable,k=25)


##TRAINING
knn_credit_x1 = sum(ifelse(knn_pred_x1==train_profitable,1,0))/nrow(train_X)
knn_credit_x1
knn_credit_x3 = sum(ifelse(knn_pred_x3==train_profitable,1,0))/nrow(train_X)
knn_credit_x3
knn_credit_x5 = sum(ifelse(knn_pred_x5==train_profitable,1,0))/nrow(train_X)
knn_credit_x5
knn_credit_x10 = sum(ifelse(knn_pred_x10==train_profitable,1,0))/nrow(train_X)
knn_credit_x10
knn_credit_x25 = sum(ifelse(knn_pred_x25==train_profitable,1,0))/nrow(train_X)
knn_credit_x25

##VALIDATION
knn_credit_1 = sum(ifelse(knn_pred_1==valid_profitable,1,0))/nrow(valid_X)
knn_credit_1
knn_credit_3 = sum(ifelse(knn_pred_3==valid_profitable,1,0))/nrow(valid_X)
knn_credit_3
knn_credit_5 = sum(ifelse(knn_pred_5==valid_profitable,1,0))/nrow(valid_X)
knn_credit_5
knn_credit_10 = sum(ifelse(knn_pred_10==valid_profitable,1,0))/nrow(valid_X)
knn_credit_10
knn_credit_25 = sum(ifelse(knn_pred_25==valid_profitable,1,0))/nrow(valid_X)
knn_credit_25


kval=c(1,3,5,10,25)
knn_train_acc = c(knn_credit_x1,knn_credit_x3,knn_credit_x5,knn_credit_x10,knn_credit_x25)
knn_valid_acc = c(knn_credit_1,knn_credit_3,knn_credit_5,knn_credit_10,knn_credit_25)

plot(kval,knn_train_acc,col="black",type="b")
plot(kval,knn_valid_acc,col="orange",type="b")

##5

#LOGISTIC
log_test_preds = predict(logmodel, newdata = credit_test, type="response")
test_log = ifelse(log_test_preds>0.5,1,0)
log_acc_test = sum(ifelse(credit_test$PROFITABLE==test_log,1,0))/nrow(credit_test)
log_acc_test

#TREE
test_tree_10 <- predict(credit_pruned10, newdata=credit_test)
test_class_10 = ifelse(test_tree_10[,2]>0.5,1,0)
tree_credit_10 = sum(ifelse(credit_test$PROFITABLE==test_class_10,1,0))/nrow(credit_test)
tree_credit_10

#KNN
knn_last_test = knn(train_X,test_X,train_profitable,k=3)
knn_result = sum(ifelse(knn_last_test==test_profitable,1,0))/nrow(test_X)
knn_result
