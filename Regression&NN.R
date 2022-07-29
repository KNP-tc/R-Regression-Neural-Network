install.packages("mlbench")
install.packages(c("nnet","NeuralNetTools"))
library(nnet)
library(NeuralNetTools)
library(mlbench)
data("BreastCancer")
data("BostonHousing")

##Preview Data
head(BreastCancer)
head(BostonHousing)

##1.Linear Reg
##1.1 Split
n1 <- nrow(BostonHousing)
id1 <- sample(1:n1,size = n1*0.8)
train_data_bh <- BostonHousing[id1,]
test_data_bh <- BostonHousing[-id1,]
##1.2 Linear Reg Model
model1 <- lm(medv ~ .,data = train_data_bh)
##1.3 Test & Evaluate
p_train_bh <- predict(model1, newdata = train_data_bh)
rmse_train_bh <- sqrt(mean((train_data_bh$medv - p_train_bh)**2))
p_test_bh <- predict(model1, newdata = test_data_bh)
rmse_test_bh <- sqrt(mean((test_data_bh$medv - p_test_bh)**2))

paste("RMSE Train:",rmse_train_bh)
paste("RMSE Test:",rmse_test_bh)

##BreastCancer data prep:
BreastCancer <- BreastCancer %>%
  mutate(Class_label = ifelse(BreastCancer$Class == "benign",0,1))
glimpse(BreastCancer)
##2. Logistic Regression
##2.1 Split
n2 <- nrow(BreastCancer)
id2 <- sample(1:n2,size = n2*0.8)
train_data_bc2 <- BreastCancer[id2,]
test_data_bc2 <- BreastCancer[-id2,]
##2.2 Logistic Reg Model using Cell.shape
model2 <- glm(Class_label ~ Cell.shape,data = train_data_bc2,family = "binomial")
##2.3 Test & Evaluate
train_data_bc2$prob <- predict(model2, newdata = train_data_bc2, type = "response")
train_data_bc2$pred <- ifelse(train_data_bc2$prob >= 0.5,1,0)

test_data_bc2$prob <- predict(model2, newdata = test_data_bc2, type = "response")
test_data_bc2$pred <- ifelse(test_data_bc2$prob >= 0.5,1,0)

metrix_2 <- table(test_data_bc2$pred,test_data_bc2$Class_label,dnn=c("Predicted","Actual"))
Acc_2 <- (metrix_2[1,1]+metrix_2[2,2])/sum(metrix_2)
Prec_2 <- (metrix_2[2,2]/(metrix_2[2,1]+metrix_2[2,2]))
Recall_2 <- (metrix_2[2,2]/(metrix_2[1,2]+metrix_2[2,2]))
F1_2 <- 2*Prec_2*Recall_2/(Prec_2+Recall_2)
cat("Result:","\n","Accuracy:",Acc_2,"\n","Precision:",Prec_2,"\n","Recall:",Recall_2,"\n","F1:",F1_2)

##3. Neural Network
##3.1 Split
glimpse(BreastCancer)
n3 <- nrow(BreastCancer)
id3 <- sample(1:n3,size = n3*0.8)
##3.1.1 Removing Id & Class_label
train_data_bc3 <- BreastCancer[id3,-1]
test_data_bc3 <- BreastCancer[-id3,-1]
##3.2.1 Model Training
nn_model <- nnet(Class ~ . -Class_label,data = train_data_bc3,size = 3)
summary(nn_model)
##3.2.2 Plot Network
plotnet(nn_model)
##3.3 Test & Evaluate
test_data_bc3$pred <- predict(nn_model, newdata = test_data_bc3, type = "class")
test_data_bc3$pred <- ifelse(test_data_bc3$pred == "malignant",1,0 )

metrix_3 <- table(test_data_bc3$pred,test_data_bc3$Class_label,dnn=c("Predicted","Actual"))
Acc_3 <- (metrix_3[1,1]+metrix_3[2,2])/sum(metrix_3)
Prec_3 <- (metrix_3[2,2]/(metrix_3[2,1]+metrix_3[2,2]))
Recall_3 <- (metrix_3[2,2]/(metrix_3[1,2]+metrix_3[2,2]))
F1_3 <- 2*Prec_3*Recall_3/(Prec_3+Recall_3)
cat("Result:","\n","Accuracy:",Acc_3,"\n","Precision:",Prec_3,"\n","Recall:",Recall_3,"\n","F1:",F1_3)
