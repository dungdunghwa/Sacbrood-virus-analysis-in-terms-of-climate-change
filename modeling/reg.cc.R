install.packages('caret')
install.packages('glmnet') 
install.packages('pscl')
###########package installed######################
library(dplyr)
library(tidyverse)
library(caret)
library(glmnet)
library(pscl)
################library call############################
set.seed(1999)
#################
#ccnam
#################
exp1<-read.csv("C:/Users/localadmin/Desktop/data/modeling data/exp/ccnam_x.csv")
res1<-read.csv("C:/Users/localadmin/Desktop/data/modeling data/res/ccnam_y.csv")

#x변수 min-max scaling, 음수도 예외는 없다. 온도 변수까지 모조리 minmax 정규화를 한다.
colnames(res1)=c('noname', 'infected')
exp1=exp1[-1] #의미없는 index 삭제
res1=res1[-1]
res1=ifelse(res1>0, 1, 0) #숫자 형식으로 되어있는걸 1과 0으로 바꾼다. 
range(exp1$rain_lasting) #rain lasting hour가 0은 아니다.

#split the data into training and test set
bees=cbind(res1, exp1)
training.samples<-createDataPartition(bees[,'infected'], p=0.8, list=FALSE)
train.data<-bees[training.samples,]
test.data<-bees[-training.samples,]

x=model.matrix(infected~acid+co_2+ch_4+n_20+utray+dust+avg_temp+low_temp+high_temp+rain_lasting+max_rain_hour+day_rain+avg_wind+avg_hum+min_hum+tot_hour_sun+avg_bottom_temp, data=train.data)[,-1]
y=as.matrix(train.data[, 1])
#Find the best lambda using cross-validation
cv.lasso<-cv.glmnet(x, y, alpha=1, family='binomial',standardize=TRUE, type.measure = 'deviance')
cv.ridge<-cv.glmnet(x, y, alpha=0, family='binomial',standardize=TRUE, type.measure = 'deviance')
plot(cv.lasso)
#fit the model on the training data
lasso.ccnam1<-glmnet(x, y, alpha=1, family='binomial', lambda=cv.lasso$lambda.min,standardize=TRUE, type.measure = 'deviance') #alpha=1 means lasso fit
#display regression coefficients
coef(lasso.ccnam1)
x.test<-model.matrix(infected~acid+co_2+ch_4+n_20+utray+dust+avg_temp+low_temp+high_temp+rain_lasting+max_rain_hour+day_rain+avg_wind+avg_hum+min_hum+tot_hour_sun+avg_bottom_temp, test.data)[,-1]
prob<-predict(lasso.ccnam1, newx=x.test, type='response')
predicted<-ifelse(prob>0.1, 1, 0)
#model accuracy
obs<-test.data$infected
mean(predicted==obs)
confusionMatrix(as.factor(predicted),as.factor(obs)) #sensitivity와 specificity 값이 뒤바껴 나옴.

#alasso
best_ridge_coef <- as.numeric(coef(cv.ridge, s=cv.ridge$lambda.min))[-1]
cv.alasso<-cv.glmnet(x, y, alpha=1, family='binomial', penalty.factor = 1 / abs(best_ridge_coef),standardize=TRUE, type.measure = 'deviance')
alasso.ccnam1<-glmnet(x, y, alpha=1, family='binomial', penalty.factor= 1/abs(best_ridge_coef), lambda=cv.alasso$lambda.min,standardize=TRUE, type.measure = 'deviance')
prob.alasso<-predict(alasso.ccnam1, newx=x.test, type='response')
predicted.alasso<-ifelse(prob.alasso>0.1, 1, 0)
#model accuracy
obs<-test.data$infected
mean(predicted.alasso==obs)
confusionMatrix(as.factor(predicted.alasso), as.factor(obs)) #sensitivity와 specificity 값이 뒤바껴 나옴.


#일반 glm모델
full.model<-glm(infected ~., data=train.data, family=binomial)
#make predictions
prob.full<-predict(full.model, test.data[-1], type='response')
predicted.full<-ifelse(prob.full>0.1, 1, 0)
mean(predicted.full==obs)
#0을 0으로 예측하는건 쉬우나 1을 0으로 예측하는게 어려움.
confusionMatrix(as.factor(predicted.full), as.factor(obs)) 


############
#ccbuk
############
exp2<-read.csv("C:/Users/localadmin/Desktop/data/modeling data/exp/ccbuk_x.csv")
res2<-read.csv("C:/Users/localadmin/Desktop/data/modeling data/res/ccbuk_y.csv")

#x변수 min-max scaling, 음수도 예외는 없다. 온도 변수까지 모조리 minmax 정규화를 한다.
colnames(res2)=c('noname', 'infected')
exp2=exp2[-1] #의미없는 index 삭제
res2=res2[-1]
res2=ifelse(res2>0, 1, 0) #숫자 형식으로 되어있는걸 1과 0으로 바꾼다. 
range(exp2$rain_lasting)

#split the data into training and test set
bees=cbind(res2, exp2)
training.samples<-createDataPartition(bees[,'infected'], p=0.8, list=FALSE)
train.data<-bees[training.samples,]
test.data<-bees[-training.samples,]

x=model.matrix(infected~acid+co_2+ch_4+n_20+utray+dust+avg_temp+low_temp+high_temp+rain_lasting+max_rain_hour+day_rain+avg_wind+avg_hum+min_hum+tot_hour_sun+avg_bottom_temp, data=train.data)[,-1]
y=as.matrix(train.data[, 1])
#Find the best lambda using cross-validation
cv.lasso<-cv.glmnet(x, y, alpha=1, family='binomial',standardize=TRUE, type.measure = 'deviance')
cv.ridge<-cv.glmnet(x, y, alpha=0, family='binomial',standardize=TRUE, type.measure = 'deviance')
plot(cv.lasso)
#fit the model on the training data
lasso.ccbuk<-glmnet(x, y, alpha=1, family='binomial', lambda=cv.lasso$lambda.min,standardize=TRUE, type.measure = 'deviance') #alpha=1 means lasso fit
#display regression coefficients
coef(lasso.ccbuk)
x.test<-model.matrix(infected~acid+co_2+ch_4+n_20+utray+dust+avg_temp+low_temp+high_temp+rain_lasting+max_rain_hour+day_rain+avg_wind+avg_hum+min_hum+tot_hour_sun+avg_bottom_temp, test.data)[,-1]
prob<-predict(lasso.ccbuk, newx=x.test, type='response')
predicted<-ifelse(prob>0.1, 1, 0)
#model accuracy
obs<-test.data$infected
mean(predicted==obs)
confusionMatrix(as.factor(predicted),as.factor(obs)) 

#alasso
best_ridge_coef <- as.numeric(coef(cv.ridge, s=cv.ridge$lambda.min))[-1]
cv.alasso<-cv.glmnet(x, y, alpha=1, family='binomial', penalty.factor = 1 / abs(best_ridge_coef),standardize=TRUE, type.measure = 'deviance')
alasso.ccbuk<-glmnet(x, y, alpha=1, family='binomial', penalty.factor= 1/abs(best_ridge_coef), lambda=cv.alasso$lambda.min,standardize=TRUE, type.measure = 'deviance')
prob.alasso<-predict(alasso.ccbuk, newx=x.test, type='response')
predicted.alasso<-ifelse(prob.alasso>0.1, 1, 0)
#model accuracy
obs<-test.data$infected
mean(predicted.alasso==obs)
confusionMatrix(as.factor(predicted.alasso), as.factor(obs))

#일반 glm모델
full.model<-glm(infected ~., data=train.data, family=binomial)
#make predictions
prob.full<-predict(full.model, test.data[-1], type='response')
predicted.full<-ifelse(prob.full>0.1, 1, 0)
mean(predicted.full==obs)
confusionMatrix(as.factor(predicted.full), as.factor(obs)) 
