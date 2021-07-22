---
title: "Prediction of Activity Execution of Unilateral Dumbell Biceps Curl"
author: "Sasikala"
date: "20/07/2021"
output: 
  html_document: 
    keep_md: yes
---

## INTRODUCTION
This project aims to predict the Activity of how well it is executed with a young health participants of the unilateral dumbell biceps curl.The activity is classified into five different classes as a performance execution.Classe A represents outcome is exact as per specification,B represents throwing the elbows to the front ,class C represents lifting the dumbbell only halfway ,class D represents  lowering the dumbbell only halfway and class E represents throwing the hips to the front.To predict the variable Decision tree and Random Forest algorithm are used and for predicting the coursera quiz as well.

## LOADING DATA SETS 

```r
Trainset <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"))
Testquiz <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"))
dim(Trainset) #Training data csv 
```

```
## [1] 19622   160
```

```r
dim(Testquiz) # Test data for prediction 
```

```
## [1]  20 160
```
## DATA CLEANING
The data sets in csv file has many NA columns,blank columns and other not needed columns which is not useful for implementing machine learning algorithm.So I have reduced columns from 160 to 53. 

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data_Trainset <- Trainset %>% 
  select(where(~!any(is.na(.)))) # Removing NA value columns
Traindata <- data_Trainset[!sapply(data_Trainset, function(x) any(x == ""))]# Removing Blank
Traindata1 <- select(Traindata,-c(1:7)) #Removing just names, etc columns
dim(Traindata1)
```

```
## [1] 19622    53
```

```r
names(Traindata1)
```

```
##  [1] "roll_belt"            "pitch_belt"           "yaw_belt"            
##  [4] "total_accel_belt"     "gyros_belt_x"         "gyros_belt_y"        
##  [7] "gyros_belt_z"         "accel_belt_x"         "accel_belt_y"        
## [10] "accel_belt_z"         "magnet_belt_x"        "magnet_belt_y"       
## [13] "magnet_belt_z"        "roll_arm"             "pitch_arm"           
## [16] "yaw_arm"              "total_accel_arm"      "gyros_arm_x"         
## [19] "gyros_arm_y"          "gyros_arm_z"          "accel_arm_x"         
## [22] "accel_arm_y"          "accel_arm_z"          "magnet_arm_x"        
## [25] "magnet_arm_y"         "magnet_arm_z"         "roll_dumbbell"       
## [28] "pitch_dumbbell"       "yaw_dumbbell"         "total_accel_dumbbell"
## [31] "gyros_dumbbell_x"     "gyros_dumbbell_y"     "gyros_dumbbell_z"    
## [34] "accel_dumbbell_x"     "accel_dumbbell_y"     "accel_dumbbell_z"    
## [37] "magnet_dumbbell_x"    "magnet_dumbbell_y"    "magnet_dumbbell_z"   
## [40] "roll_forearm"         "pitch_forearm"        "yaw_forearm"         
## [43] "total_accel_forearm"  "gyros_forearm_x"      "gyros_forearm_y"     
## [46] "gyros_forearm_z"      "accel_forearm_x"      "accel_forearm_y"     
## [49] "accel_forearm_z"      "magnet_forearm_x"     "magnet_forearm_y"    
## [52] "magnet_forearm_z"     "classe"
```

```r
library(dplyr)
data_Testquiz <- Testquiz %>% 
  select(where(~!any(is.na(.)))) # Removing NA value columns
quizdata <- data_Testquiz[!sapply(data_Testquiz, function(x) any(x == ""))]# Removing Blank
quizdata1 <- select(quizdata,-c(1:7))#Removing just names, etc columns
dim(quizdata1)
```

```
## [1] 20 53
```
Now the data has been cleaned and it is ready for partitioning,model building and performance prediction.

## DATA SPLITTING

To check the performance,we have to train the data.The training set data is the whole data set now,so we have segregate the data into training and testing samples from the Traindata1.70% for training and 30% for testing.


```r
set.seed(1234)# For reproducibility
ind <- sample(2,nrow(Traindata1),replace=TRUE,prob = c(0.7,0.3))
train <- Traindata1[ind==1,] #Samples for Training
test <- Traindata1[ind==2,]  #Samples for Testing
```

## 1 IMPLEMENTING DECISION TREE
The Decision Tree is the recursive partition algorithm,so the function rpart() is to be used and to plot rpart.plot() is used.


```r
library(rpart)
```

```
## Warning: package 'rpart' was built under R version 4.0.5
```

```r
library(rpart.plot)
```

```
## Warning: package 'rpart.plot' was built under R version 4.0.5
```

```r
set.seed(123)
Tree <- rpart(classe~.,data=train)
rpart.plot(Tree,type=3)
```

![](Exercise_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### Model performance of Decision Tree

 To check the performance,accuracy the confusion matrix is used. because it gives and validates sum of true positives and true negatives divided by the sum of(TP+TN+FP+FN).After implementing decision tree the accuracy of the model is 75.02%.Let,s implement other models and compare
 

```r
library(caret)
```

```
## Warning: package 'caret' was built under R version 4.0.5
```

```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 4.0.4
```

```r
set.seed(122)

p <- predict(Tree,newdata=test,type="class")

p1 <- confusionMatrix(p,as.factor(test$classe))
p1
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1474  187   13   33   15
##          B   58  697   63   82   99
##          C   37  103  834  159  125
##          D   59   70   54  623   60
##          E   22   69   62   97  777
## 
## Overall Statistics
##                                           
##                Accuracy : 0.7502          
##                  95% CI : (0.7389, 0.7612)
##     No Information Rate : 0.281           
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.684           
##                                           
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.8933   0.6190   0.8129   0.6268   0.7221
## Specificity            0.9413   0.9364   0.9125   0.9502   0.9479
## Pos Pred Value         0.8560   0.6977   0.6630   0.7194   0.7566
## Neg Pred Value         0.9576   0.9120   0.9584   0.9259   0.9383
## Prevalence             0.2810   0.1918   0.1747   0.1693   0.1832
## Detection Rate         0.2510   0.1187   0.1420   0.1061   0.1323
## Detection Prevalence   0.2933   0.1701   0.2142   0.1475   0.1749
## Balanced Accuracy      0.9173   0.7777   0.8627   0.7885   0.8350
```
## 2 DECISION TREE WITH CROSS VALIDATION
With CV i have given 10 parts and it repeats 5 times.The rpart has hyperparameter as CP which is shown in the figure as at 0.03 the model shows the accuracy.so i have tune the model in that way as tune length =3.But model has shown the accuracy of 49.65% which is not said to be good.Let's try with another model

```r
set.seed(135)
library(caret)
crossVal <- trainControl(method="repeatedcv",number=10,repeats=5)
treeCV <- train(classe~.,data=train,method="rpart",trControl=crossVal,tuneLength=3)
treeCV
```

```
## CART 
## 
## 13750 samples
##    52 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 5 times) 
## Summary of sample sizes: 12375, 12375, 12374, 12375, 12376, 12375, ... 
## Resampling results across tuning parameters:
## 
##   cp          Accuracy   Kappa     
##   0.03391039  0.5078852  0.36038723
##   0.05902919  0.4111510  0.19903557
##   0.11598778  0.3285843  0.06538319
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was cp = 0.03391039.
```

```r
plot(treeCV)
```

![](Exercise_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

### Model performance with Decision Tree Cross validation


```r
p2<- predict(treeCV,newdata = test,type = "raw")
p3 <- confusionMatrix(p2,as.factor(test$classe))
p3
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1508  454  451  447  172
##          B   25  395   40  167  157
##          C  116  277  535  380  268
##          D    0    0    0    0    0
##          E    1    0    0    0  479
## 
## Overall Statistics
##                                           
##                Accuracy : 0.4968          
##                  95% CI : (0.4839, 0.5096)
##     No Information Rate : 0.281           
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.3443          
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9139  0.35080  0.52144   0.0000  0.44517
## Specificity            0.6390  0.91804  0.78518   1.0000  0.99979
## Pos Pred Value         0.4974  0.50383  0.33947      NaN  0.99792
## Neg Pred Value         0.9500  0.85633  0.88571   0.8307  0.88928
## Prevalence             0.2810  0.19176  0.17473   0.1693  0.18324
## Detection Rate         0.2568  0.06727  0.09111   0.0000  0.08157
## Detection Prevalence   0.5163  0.13351  0.26839   0.0000  0.08174
## Balanced Accuracy      0.7765  0.63442  0.65331   0.5000  0.72248
```
## 3 IMPLEMENTING BAGGING(BOOTSTRAP AGGREGATING)
For implementing bagging we have to use the method "treebag" and with cross validation.The roll_belt variable is the most important variable for prediction followed by yaw_belt and so on.The model performance is comparitively better and it has an accuracy of 98.7%


```r
set.seed(1351)
library(caret)
crossVal <- trainControl(method="repeatedcv",number=10,repeats=5)
bag <- train(classe~.,data=train,method="treebag",trControl=crossVal,importance=TRUE)
plot(varImp(bag),top=10)
```

![](Exercise_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### Model performance of Bagging


```r
p4<- predict(bag,newdata = test,type = "raw")
p5<- confusionMatrix(p4,as.factor(test$classe))
p5
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1647   15    1    2    0
##          B    1 1105   12    1    4
##          C    1    4 1006   16    0
##          D    0    1    6  974    4
##          E    1    1    1    1 1068
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9877          
##                  95% CI : (0.9846, 0.9904)
##     No Information Rate : 0.281           
##     P-Value [Acc > NIR] : < 2e-16         
##                                           
##                   Kappa : 0.9845          
##                                           
##  Mcnemar's Test P-Value : 0.00156         
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9982   0.9813   0.9805   0.9799   0.9926
## Specificity            0.9957   0.9962   0.9957   0.9977   0.9992
## Pos Pred Value         0.9892   0.9840   0.9796   0.9888   0.9963
## Neg Pred Value         0.9993   0.9956   0.9959   0.9959   0.9983
## Prevalence             0.2810   0.1918   0.1747   0.1693   0.1832
## Detection Rate         0.2805   0.1882   0.1713   0.1659   0.1819
## Detection Prevalence   0.2835   0.1912   0.1749   0.1677   0.1826
## Balanced Accuracy      0.9970   0.9888   0.9881   0.9888   0.9959
```
## 4 IMPLEMENTING RANDOM FOREST METHOD
By calling randomforest(),the model is implemented with crossvalidation with 3 parts.

```r
set.seed(12345)
library(randomForest)
```

```
## Warning: package 'randomForest' was built under R version 4.0.5
```

```
## randomForest 4.6-14
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```r
forestCV <- trainControl(method="cv", number=3, verboseIter=FALSE)
randomf <- train(classe ~ ., data=train, method="rf",
                          trControl=forestCV,importance=TRUE)

print(randomf$finalModel)
```

```
## 
## Call:
##  randomForest(x = x, y = y, mtry = min(param$mtry, ncol(x)), importance = TRUE) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 27
## 
##         OOB estimate of  error rate: 0.63%
## Confusion matrix:
##      A    B    C    D    E  class.error
## A 3928    1    1    0    0 0.0005089059
## B   17 2649    5    0    0 0.0082366155
## C    0   14 2373    9    0 0.0095993322
## D    0    0   25 2194    3 0.0126012601
## E    0    0    5    6 2520 0.0043461083
```

### OUT OF BAG Misclassification(Rf with CV)

 The out of bag is the misclasification error rate which the random forest has done.Here,it is 0.63% ,that is 1-OOB is accuracy.So,the OOB is very small here and it is said to be a good fit.Since,it has less OOB and high accuracy which is 99.13% we can use this model for our quizdata.
 
### Model performance of Random forest

```r
library(caret)
p6 <- predict(randomf,test)
p7 <- confusionMatrix(p6,as.factor(test$classe))
p7
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1649    9    0    0    0
##          B    0 1112    4    1    1
##          C    0    5 1017   16    0
##          D    0    0    5  976    6
##          E    1    0    0    1 1069
## 
## Overall Statistics
##                                          
##                Accuracy : 0.9917         
##                  95% CI : (0.989, 0.9938)
##     No Information Rate : 0.281          
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : 0.9895         
##                                          
##  Mcnemar's Test P-Value : NA             
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9994   0.9876   0.9912   0.9819   0.9935
## Specificity            0.9979   0.9987   0.9957   0.9977   0.9996
## Pos Pred Value         0.9946   0.9946   0.9798   0.9889   0.9981
## Neg Pred Value         0.9998   0.9971   0.9981   0.9963   0.9985
## Prevalence             0.2810   0.1918   0.1747   0.1693   0.1832
## Detection Rate         0.2808   0.1894   0.1732   0.1662   0.1821
## Detection Prevalence   0.2824   0.1904   0.1768   0.1681   0.1824
## Balanced Accuracy      0.9986   0.9932   0.9934   0.9898   0.9965
```

## 6 PREDICTION FOR TEST QUIZ DATA
 By comparing all the models,the model with random Forest gives better accuracy as 99.13% when compared to all other methods.So, i have implemented Random Forest to predict the testquiz data.


```r
p8 <- predict(randomf,quizdata1)
p8
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```
