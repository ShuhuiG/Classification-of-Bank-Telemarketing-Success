bank <- read.csv2("C:/Users/guoshuhui/Desktop/OA/bank-additional/bank-additional-full.csv", header=TRUE, sep=";")

# check the class of variables
sapply(bank, class)

# convert the change the factor values to numeric
bank$age <- as.numeric(bank$age)
bank$duration <- as.numeric(bank$duration)
bank$campaign <- as.numeric(bank$campaign)
bank$pdays <- as.numeric(bank$pdays)
bank$previous <- as.numeric(bank$previous)
bank$emp.var.rate <- as.numeric(as.character(bank$emp.var.rate))
bank$cons.price.idx <- as.numeric(as.character(bank$cons.price.idx))
bank$cons.conf.idx <- as.numeric(as.character(bank$cons.conf.idx))
bank$euribor3m <- as.numeric(as.character(bank$euribor3m))
bank$nr.employed <- round(as.numeric(as.character(bank$nr.employed)))

# observe data
summary(bank)

# split training and testing data
library(caret)
#set random seed for the results to be reproducible
set.seed(123) 
# sample 75% of observations as the training set
trainX <- createDataPartition(bank$y, p=0.75, list=FALSE) 
train <- bank[trainX,]
# the rest 25% as the test set
test <- bank[-trainX,]

lr.fit <- glm(y~.,data=train[,-11], family=binomial) #take away 11th column
chisq.test(train$loan,train$housing)
chisq.test(train$loan,train$age)
chisq.test(train$loan,train$job)

## logistic with lasso penalty
library(glmnet)
y_train <- train[,21]
y_test <- test[,21]
X_train <- train[,-21]
X_test <- test[,-21]
Xdummy_train <- model.matrix(~ .-1, X_train)
Xdummy_test <- model.matrix(~ .-1, X_test)
y_train <- as.numeric(as.character(factor(y_train, levels = c("no", "yes"), labels = c("0", "1"))))
y_test <- as.numeric(as.character(factor(y_test, levels = c("no", "yes"), labels = c("0", "1"))))
m1.cv <- cv.glmnet(Xdummy_train[,-44], as.factor(y_train), family="binomial", alpha = 1, nfolds=10)
#lambda_use <- m1.cv$lambda.min
model <- m1.cv
coef(model)

# specify type="response" to get the estimated probabilities
prob1 <- predict(model, Xdummy_train[,-44], type = "response")
prob <- predict(model, Xdummy_test[,-44], type = "response")

# store the predicted labels using 0.5 as a threshold
library(dplyr)
train = train %>%
  mutate(predy = as.factor(ifelse(prob1 < 0.5, "no", "yes")))
# confusion matrix
table(pred = train$predy, true = train$y)

test = test %>%
  mutate(predy = as.factor(ifelse(prob < 0.5, "no", "yes")))
# confusion matrix
table(pred = test$predy, true = test$y)

#ROC curve because low true positive rate
library(ROCR)
pred = prediction(prob1, train$y)
#TPR on the y axis and FPR on the x axis
perf = performance(pred, measure="tpr", x.measure="fpr")
plot(perf, col=2, lwd=3, main="ROC curve")
abline(0,1)
auc <- performance(pred, "auc")@y.values
auc

# change the threshold
# FPR
fpr = performance(pred, "fpr")@y.values[[1]]
cutoff = performance(pred, "fpr")@x.values[[1]]
# FNR
fnr = performance(pred,"fnr")@y.values[[1]]

#plot the FPR and FNR versus threshold values
matplot(cutoff, cbind(fpr,fnr), type="l",lwd=2, xlab="Threshold",ylab="Error Rate")
legend(0.4, 1, legend=c("False Positive Rate","False Negative Rate"),
       col=c(1,2), lty=c(1,2))

# calculate the euclidean distance between (fpr,fnr) and (0,0)
rate = as.data.frame(cbind(Cutoff=cutoff, FPR=fpr, FNR=fnr))
rate$distance = sqrt((rate[,2])^2+(rate[,3])^2)

# select the probability threshold with the smallest euclidean distance
index = which.min(rate$distance)
best = rate$Cutoff[index]
best
abline(v=best, col=3, lty=3, lwd=3)

#calculate subscription probabilities again with threshold value
new.train = train %>%
  mutate(predy = as.factor(ifelse(prob1 <= best, "no", "yes")))
# confusion matrix
table(pred = new.train$predy, true = train$y)

new.test = test %>%
  mutate(predy = as.factor(ifelse(prob <= best, "no", "yes")))
table(pred = new.test$predy, true = test$y)


## classification tree
rm(list=ls())
bank <- read.csv2("C:/Users/guoshuhui/Desktop/OA/bank-additional/bank-additional-full.csv", header=TRUE, sep=";")
bank <- bank[,-11]
# convert the change the factor values to numeric
bank$age <- as.numeric(bank$age)
# bank$duration <- as.numeric(bank$duration)
bank$campaign <- as.numeric(bank$campaign)
bank$pdays <- as.numeric(bank$pdays)
bank$previous <- as.numeric(bank$previous)
bank$emp.var.rate <- as.numeric(as.character(bank$emp.var.rate))
bank$cons.price.idx <- as.numeric(as.character(bank$cons.price.idx))
bank$cons.conf.idx <- as.numeric(as.character(bank$cons.conf.idx))
bank$euribor3m <- as.numeric(as.character(bank$euribor3m))
bank$nr.employed <- round(as.numeric(as.character(bank$nr.employed)))
# split training and testing data
library(caret)
#set random seed for the results to be reproducible
set.seed(123) 
# sample 75% of observations as the training set
trainX <- createDataPartition(bank$y, p=0.75, list=FALSE) 
train <- bank[trainX,]
# the rest 25% as the test set
test <- bank[-trainX,]

library(rpart)
library(rpart.plot)
library(rattle)
bank.rpart <- rpart(y ~ ., data = train,method="class", control = rpart.control(xval = 10, cp = 0.001))

#fancyRpartPlot(bank.rpart)
#predictions <- predict(bank.rpart, test, type = "class")
# confusion matrix
#table <- table(pred = predictions, true = test$y)
#accuracy <- (table[1,1]+table[2,2])/(table[1,1]+table[1,2]+table[2,1]+table[2,2])
#accuracy

printcp(bank.rpart)
plotcp(bank.rpart)
bank.rpart$cptable
bank.prune <- prune(bank.rpart, cp = bank.rpart$cptable[3,1])
fancyRpartPlot(bank.prune)

predictions <- predict(bank.prune, test, type = "class")
# confusion matrix
table <- table(pred = predictions, true = test$y)
accuracy <- (table[1,1]+table[2,2])/(table[1,1]+table[1,2]+table[2,1]+table[2,2])
accuracy

# draw ROC curve and find auc
prob1 <- predict(bank.prune, train, type = "class")
prob1 <- as.numeric(as.character(factor(prob1, levels = c("no", "yes"), labels = c("0", "1"))))
library(ROCR)
pred = prediction(prob1, train$y)
#TPR on the y axis and FPR on the x axis
perf = performance(pred, measure="tpr", x.measure="fpr")
plot(perf, col=2, lwd=3, main="ROC curve")
abline(0,1)
auc <- performance(pred, "auc")@y.values
auc


## boosting

rm(list=ls())
bank <- read.csv2("C:/Users/guoshuhui/Desktop/OA/bank-additional/bank-additional-full.csv", header=TRUE, sep=";")
bank <- bank[,-11]
# convert the change the factor values to numeric
bank$age <- as.numeric(bank$age)
# bank$duration <- as.numeric(bank$duration)
bank$campaign <- as.numeric(bank$campaign)
bank$pdays <- as.numeric(bank$pdays)
bank$previous <- as.numeric(bank$previous)
bank$emp.var.rate <- as.numeric(as.character(bank$emp.var.rate))
bank$cons.price.idx <- as.numeric(as.character(bank$cons.price.idx))
bank$cons.conf.idx <- as.numeric(as.character(bank$cons.conf.idx))
bank$euribor3m <- as.numeric(as.character(bank$euribor3m))
bank$nr.employed <- round(as.numeric(as.character(bank$nr.employed)))
# split training and testing data
library(caret)
#set random seed for the results to be reproducible
set.seed(123) 
# sample 75% of observations as the training set
trainX <- createDataPartition(bank$y, p=0.75, list=FALSE) 
train <- bank[trainX,]
# the rest 25% as the test set
test <- bank[-trainX,]
ytrain <- as.numeric(as.character(factor(train$y, levels = c("no", "yes"), labels = c("0", "1"))))
Xtrain <- train[,-20]
ytest <- as.numeric(as.character(factor(test$y, levels = c("no", "yes"), labels = c("0", "1"))))
Xtest <- test[,-20]

bank_new <- as.data.frame(cbind(Xtrain, ytrain))
library(gbm)
gbm.fit = gbm(ytrain~., data = bank_new, 
              distribution="bernoulli", n.trees=1000, shrinkage=0.01, bag.fraction=0.8, cv.folds = 10)
usetree = gbm.perf(gbm.fit, plot.it = FALSE, method="cv")
pre1 <- predict(gbm.fit, Xtrain, n.trees = usetree)
pre <- predict(gbm.fit, Xtest, n.trees = usetree)

#gbm.fit2 = gbm(ytrain~., data = bank_new, 
#              distribution="bernoulli", n.trees=2000, shrinkage=0.1, bag.fraction=0.8, cv.folds = 10)
#usetree2 = gbm.perf(gbm.fit2, plot.it = TRUE, method="cv")
#usetree <- usetree2

prob1 = 1/(1+exp(-2*pre1))
prob = 1/(1+exp(-2*pre))
library(dplyr)
train = train %>%
  mutate(predy = as.factor(ifelse(prob1 < 0.5, "no", "yes")))
# confusion matrix
a = table(pred = train$predy, true = train$y)
a1 <- a[1,1]
a2 <- a[1,2]
a3 <- a[2,1]
a4 <- a[2,2]
atotal <- (a1+a4)/30891
atrueno <- a1/(a1+a3)
atrueyes <- a4/(a2+a4)

test = test %>%
  mutate(predy = as.factor(ifelse(prob < 0.5, "no", "yes")))
# confusion matrix
b = table(pred = test$predy, true = test$y)
b1 <- b[1,1]
b2 <- b[1,2]
b3 <- b[2,1]
b4 <- b[2,2]
btotal <- (b1+b4)/10297
btrueno <- b1/(b1+b3)
btrueyes <- b4/(b2+b4)

#ROC curve because low true positive rate
library(ROCR)
pred = prediction(prob1, train$y)
#TPR on the y axis and FPR on the x axis
perf = performance(pred, measure="tpr", x.measure="fpr")
plot(perf, col=2, lwd=3, main="ROC curve")
abline(0,1)
auc <- performance(pred, "auc")@y.values
auc

# change the threshold
# FPR
fpr = performance(pred, "fpr")@y.values[[1]]
cutoff = performance(pred, "fpr")@x.values[[1]]
# FNR
fnr = performance(pred,"fnr")@y.values[[1]]

#plot the FPR and FNR versus threshold values
matplot(cutoff, cbind(fpr,fnr), type="l",lwd=2, xlab="Threshold",ylab="Error Rate")
legend("right", 0.4, legend=c("False Positive Rate","False Negative Rate"),
       col=c(1,2), lty=c(1,2))

# calculate the euclidean distance between (fpr,fnr) and (0,0)
rate = as.data.frame(cbind(Cutoff=cutoff, FPR=fpr, FNR=fnr))
rate$distance = sqrt((rate[,2])^2+(rate[,3])^2)

# select the probability threshold with the smallest euclidean distance
index = which.min(rate$distance)
best = rate$Cutoff[index]
best
abline(v=best, col=3, lty=3, lwd=3)

#calculate subscription probabilities again with threshold value
new.train = train %>%
  mutate(predy = as.factor(ifelse(prob1 <= best, "no", "yes")))
# confusion matrix
c = table(pred = new.train$predy, true = train$y)
c1 <- c[1,1]
c2 <- c[1,2]
c3 <- c[2,1]
c4 <- c[2,2]
ctotal <- (c1+c4)/30891
ctrueno <- c1/(c1+c3)
ctrueyes <- c4/(c2+c4)

new.test = test %>%
  mutate(predy = as.factor(ifelse(prob <= best, "no", "yes")))
d = table(pred = new.test$predy, true = test$y)
d1 <- d[1,1]
d2 <- d[1,2]
d3 <- d[2,1]
d4 <- d[2,2]
dtotal <- (d1+d4)/10297
dtrueno <- d1/(d1+d3)
dtrueyes <- d4/(d2+d4)


