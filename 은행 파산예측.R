setwd('C:/Users/junkyu/Desktop/DataProject')

credicard = read.csv('creditcard.csv')
str(credicard)
sum(credicard$Class)
summary(credicard)
# 처음 모든 변수를 일반화 회귀모형에 돌려보았음. 그리고 step 함수를 이용하여 후진제거법으로 AIC를 낮추는 방향으로 진행.



important_var = c('V4' ,'V5', 'V8', 'V10', 'V13', 'V14', 'V20', 'V21', 'V22', 'V27', 'V28', 'Class')
important_frame = data.frame(credicard[,'V4'])
for( i in 2:length(important_var)) {
  important_frame[,important_var[i]] = credicard[,important_var[i]]
}


####

library(caret)

set.seed(123456789)

idx = sample(1:nrow(important_frame), nrow(important_frame)*0.7, replace=F)


train = important_frame[idx,]
test = important_frame[-idx,]

res.lg = glm(Class ~ ., data=train, family = binomial)
summary(res.lg)


pred.lg = predict(res.lg, test[,-12], type='response')
pred.lg.frame = as.data.frame(pred.lg)

head(pred.lg.frame)
pred.lg.frame$Class = ifelse(pred.lg.frame$pred.lg <= 0.5, pred.lg.frame$Class <- 0, pred.lg.frame$Class <- 1)
confusionMatrix(data=as.factor(pred.lg.frame$Class), reference=as.factor(test[,12]), positive='1')

library(randomForest)
res.rf = randomForest(Class~., data = train, ntree=50, mtry=sqrt(12), important=T)

varImpPlot(res.rf)
res.rf$importance

pred.rf = predict(res.rf, test[,-12], type='class')

pred.rf.frame = as.data.frame(pred.rf)
pred.rf.frame$Class = ifelse(pred.rf.frame$pred.rf <= 0.5, pred.rf.frame$Class <- 0, pred.rf.frame$Class <- 1)

confusionMatrix(data = as.factor(pred.rf.frame$Class), reference = as.factor(test[,12]), positive='1')

library(e1071)
# tune.svm(Class~., data=train, gamma=10^(-6:-1), cost=10^(1:2))