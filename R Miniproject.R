#Install Packages
install.packages('ggpubr')
library('ggpubr')
install.packages('ggcorrplot')
library('ggcorrplot')
install.packages('mice')
library('mice')
install.packages('VIM')
library('VIM')
install.packages('caret')
library('caret')
install.packages('rpart')
library('rpart')
install.packages('rpart.plot')
library('rpart.plot')
install.packages('MLmetrics')
library('MLmetrics')
install.packages('ROCR')
library('RCOR')
install.packages('pROC')
library('pROC')
install.packages('class')
library('class')
install.packages('randomForest')
library('randomForest')
install.packages('xgboost')
library('xgboost')
install.packages('gbm')
library('gbm')
install.packages('funModeling')
library('funModeling')
install.packages('DataExplorer')
library('DataExplorer')
install.packages('InformationValue')
library('InformationValue')
install.packages('magrittr')
library('magrittr')
install.packages('dplyr')
library('dplyr')
library('GGally')
install.packages('roxygen2')
install.packages('devtools')
installed.packages('digest')
library(roxygen2)
library(devtools)
library(digest)



#Load dataset
data <- read.csv(file             = "C:\\Users\\jaypc\\Downloads\\diabetes.csv",
                 header           = TRUE,
                 stringsAsFactors = FALSE)




#Summary of dataset
summary(data)



#Data cleaning(Input missing values)
data <- data %>% mutate(Insulin = ifelse(data$Insulin == "0", NA, Insulin))
data <- data %>% mutate(BMI= ifelse(data$BMI == "0", NA, BMI))
data <- data %>% mutate(BloodPressure = ifelse(data$BloodPressure == "0", NA, BloodPressure))
data <- data %>% mutate(SkinThickness = ifelse(data$SkinThickness == "0", NA, SkinThickness))
data <- data %>% mutate(Glucose = ifelse(data$Glucose == "0", NA, Glucose))



#Summary after cleaning
summary(data)




#Sort according to # of missing values.
aggr <- VIM::aggr(data,
                  numbers   = TRUE,
                  prop      = c(TRUE, TRUE),
                  sortVars  = TRUE,            
                  sortCombs = TRUE,
                  only.miss = TRUE,
                  labels    = names(data),
                  cex.axis  = .7,
                  gap       = 2,
                  col       = c("navyblue", "red"),
                  ylab      = c("Histogram of Missings", "Pattern"))



#Replacing NA values by using Predictive Mean Matching Method
micedia <- mice::mice(data,
                      seed  = 1234,
                      m     = 5,      #the number of imputed datsets
                      maxit = 50,
                      methd = 'pmm')  #the imputation method (Predictive mean matching)
micedia





#Checking that the imputed values are properly replaced
xyplot(micedia, 
       Insulin ~ Glucose+BloodPressure+SkinThickness+BMI,
       pch = 18,
       cex = 1,
       col = c("red", "navyblue"))




#Check the number of zero, NA values
#q_zeros: quantity of zeros (p_zeros: in percentage)
#q_na : quantity of NA (p_na: in percentage)
#type : factor or numeric
#unique : quantity of unique values
data <- complete(micedia, 1)
data$Outcome <- as.factor(data$Outcome)
levels(data$Outcome) <- c("No", "Yes")
funModeling::df_status(data)

DataExplorer::profile_missing(data)




ggpairs(data, aes(colour=Outcome, alpha = 0.8), lower = list(combo = wrap("facethist", binwidth = 1)))




#Boxplot
par(mfrow = c(2, 4), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0)) # 2 rows 4 columns 
with(data, {
  plot(Outcome, Pregnancies, main = "Pregnancies and Outcome")
  plot(Outcome, Age, main = "Age and Outcome")
  plot(Outcome, Glucose, main = "Glucose and Outcome")
  plot(Outcome, BMI, main = "BMI and Outcome")
  plot(Outcome, SkinThickness, main = "SkinThickness and Outcome")
  plot(Outcome, BloodPressure, main = "BloodPressure and Outcome")
  plot(Outcome, DiabetesPedigreeFunction, main = "DPF and Outcome")
  plot(Outcome, Insulin, main = "Insulin and Outcome")
  mtext("Relationship of Variables and Outcomes", outer = T)
})




#Distribution
b1 <- qplot(Age, data = data, geom = "density", col = Outcome)
b2 <- qplot(BMI, data = data, geom = "density", col = Outcome)
b3 <- qplot(Pregnancies, data = data, geom = "density", col = Outcome)
b4 <- qplot(Glucose, data = data, geom = "density", col = Outcome)
b5 <- qplot(Insulin, data = data, geom = "density", col = Outcome)
b6 <- qplot(BloodPressure, data = data, geom = "density", col = Outcome)
b7 <- qplot(SkinThickness, data = data, geom = "density", col = Outcome)
b8 <- qplot(DiabetesPedigreeFunction, data = data, geom = "density", col = Outcome)
ggpubr::ggarrange(b1, b2, b3, b4, b5, b6, b7, b8, nrow = 4, ncol = 2, heights = c(3, 3, 3))




#Correlation Matrix
corr <- data[ , -9]
corr <- round(cor(corr), 3) 
corr
# library(ggcorrplot)
ggcorrplot::ggcorrplot(corr, method        = "square", # default
                       type          = "lower",
                       show.legend   = TRUE,     # default
                       legend.title  = "Correlation",
                       show.diag     = TRUE,
                       hc.order      = TRUE,
                       outline.color = "white",
                       ggtheme       = ggplot2::theme_minimal, # default
                       colors        = c("blue", "white", "red"),
                       lab           = TRUE, # Adding Correlation Coefficient
                       lab_size      = 4,
                       sig.level     = 0.05)





#Data sepeartion(Checking the ratio of the target variable before spliting the dataset)
data.scale <- data.frame(scale(data[, -9]))
data.scale$Outcome <- data$Outcome
summary(data.scale)
table(data.scale$Outcome)





#Splitting the scaled data set into train and test set (7:3) for KNN & check the proportions of train and test set
set.seed(123)
index <- caret::createDataPartition(y    = data.scale$Outcome,
                                    p    = 0.7,
                                    list = FALSE)

train <- data.scale[index, ]
test <- data.scale[-index, ]

# Check
train$Outcome %>% table() %>% prop.table()
test$Outcome %>% table() %>% prop.table()




#Splitting the entire data set into train and test set (7:3) for other models & checking the proportions of train and test set
set.seed(123)
index1 <- caret::createDataPartition(y    = data$Outcome,
                                     p    = 0.7,
                                     list = FALSE)

train1 <- data[index, ]
test1 <- data[-index, ]

# Check
train1$Outcome %>% table() %>% prop.table()
test1$Outcome %>% table() %>% prop.table()




#KNN
#To find a best tuning parameter k for KNN, make grid range from 2 to 20 by 1.

grid1 <- expand.grid(.k = seq(from = 2, to = 20, by = 1))
control <- caret::trainControl(method = "cv")
set.seed(123)
knn.train <- caret::train(Outcome ~., 
                          data = train,
                          method = "knn",
                          trControl = control,
                          tuneGrid = grid1)
knn.train

#Best k = 18
#Accuracy = 0.779
#Kappa = 0.480



#Apply this model to test set
knn.test <- class::knn(train = train[, -9],
                       test  = test[, -9],
                       cl    = train[, 9],
                       k     = 18)
knn.real <- test$Outcome
caret::confusionMatrix(data      = knn.test,
                       reference = knn.real,
                       positive  = "Yes")

#Accuracy : 0.744
#Sensitivity : 0.513
#Specificity : 0.867
#Kappa : 0.401

#Both Accuracy & Kappa of test set decrease when comparing with Accuracy & Kappa of train set (77.9% -> 74.4%, 48% -> 40.1%)



knn.predobj <- ROCR::prediction(predictions = as.numeric(x = knn.test),
                                labels      = as.numeric(x = knn.real))
knn.perform <- ROCR::performance(prediction.obj = knn.predobj,
                                 measure        = 'tpr',
                                 x.measure      = 'fpr')
plot(x = knn.perform, main = 'ROC curve')
MLmetrics::F1_Score(y_pred   = knn.test,
                    y_true   = knn.real,
                    positive = "Yes"); pROC::auc(response = as.numeric(x = knn.real),
                                                 predictor = as.numeric(x = knn.test))
#F1 Score : 0.582
#AUC : 0.690





#Linear SVM

svm.real <- test1$Outcome
svm.real




set.seed(123)
linear.tune <- e1071::tune.svm(Outcome ~.,
                               data   = train1,
                               kernel = 'linear',
                               cost   = c(0.001, 0.01, 0.1, 1, 5, 10))
summary(linear.tune)

#Best parameters (cost) : 0.1
#Best error rate : 0.219




#Using test data set to predict
best.linear <- linear.tune$best.model
tune.test <- predict(best.linear, newdata = test1)
caret::confusionMatrix(data      = tune.test,
                       reference = svm.real,
                       positive  = 'Yes')
#Accuracy : 0.765
#Sensitivity : 0.575
#Specificity : 0.867
#Kappa : 0.460




MLmetrics::F1_Score(y_pred   = tune.test,
                    y_true   = svm.real,
                    positive = "Yes") ;pROC::auc(predictor = as.numeric(x = tune.test),
                                                 response  = as.numeric(x = svm.real))

#F1_Score : 0.630
#AUC : 0.721





#RandomForest
#Grid serach for finding best parameters (Because the tuning process may take a long time in large datasets, it is recommended that you tune the data into smaller samples)
#ntree = 100, 300, 500, 700, 1000
#mtry = 3, 4, 5, 6, 7


rf.grid <- expand.grid(ntree = c(100, 300, 500, 700, 1000),
                       mtry = c(3,4,5,6,7))
rf.tuned <- data.frame()
for(i in 1:nrow(rf.grid)){
  set.seed(123)
  fit <- randomForest(Outcome ~.,
                      data = train1,
                      xtest = test1[, -9],
                      ytest = test1[, 9],
                      ntree = rf.grid[i, 'ntree'],
                      mtry  = rf.grid[i, 'mtry'],
                      importance = TRUE,
                      do.trace   = 100,
                      keep.forest = TRUE)
  mcSum <- sum(fit$predicted != train1$Outcome)
  mcRate <- mcSum / nrow(train1)
  df <- data.frame(index = i, misClassRate = mcRate)
  rf.tuned <- rbind(rf.tuned, df)
  cat('\n')
}
plot(x = rf.tuned, xlab = '', ylab = 'MisClassification Rate')
abline(h = min(rf.tuned$misClassRate), col = 'red', lty = 2)
min <- which(rf.tuned$misClassRate == min(rf.tuned$misClassRate))
bestPara <- rf.grid[min, ]
bestPara

#Best parameter : ntree = 300, mtry = 3



set.seed(123)
rf.train <- randomForest::randomForest(Outcome ~.,
                                       data        = train1,
                                       xtest       = test1[,-9],
                                       ytest       = test1[, 9],
                                       ntree       = bestPara$ntree,
                                       mtry        = bestPara$mtry,
                                       importance  = TRUE,
                                       do.trace    = 100,
                                       keep.forest = TRUE)
rf.train

#train OOB : 0.242
#test OOB : 0.239



plot(rf.train)


plot(rf.train$err.rate[, 1], type = 'l')



#Use test data set to predict
rf.test <- rf.train$test$predicted
rf.real <- test1$Outcome
caret::confusionMatrix(data = rf.real,
                       reference = rf.test,
                       positive = "Yes")

#Accuracy : 0.761
#Sensitivity : 0.687
#Specificity : 0.791
#Kappa : 0.452





MLmetrics::F1_Score(y_pred   = rf.real,
                    y_true   = rf.test,
                    positive = "Yes") ;pROC::auc(predictor = as.numeric(x = rf.real),
                                                 response  = as.numeric(x = rf.test))
#F1_Score : 0.626
#AUC : 0.739



#Variable Importance
#Mean is used because variables from many trees evaluate aspects that contribute to improved accuracy and Gini impurity.
#MeanDecreaseAccuracy
#MeanDecreaseGini
rf.train %>% treesize(terminal = TRUE) %>% hist(main = 'Number of Terminal Nodes')

randomForest::varImpPlot(rf.train, main = "Variable Importance Plot"); randomForest::importance(rf.train)
#MeanDecreaseAccuracy : Glucose > Age > BMI
#MeanDecreaseGini : Glucose > BMI > Age










## Model	          Train	   Test	   Difference

## KNN	           0.779	   0.744	  0.035

## Linear SVM      0.781	   0.765	  0.016

## Randomforest	   0.758	   0.761	 -0.003





## Model	      F1 Score	  AUC

## KNN	         0.582	  0.690

## Linear SVM  	0.630	  0.721

## Randomforest	0.626	  0.739

