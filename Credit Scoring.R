library(dplyr)
library(naniar)
library(kableExtra)
library(ggplot2)
library(gridExtra)
library(plyr)
library(pROC)
library(tidyr)
library(dummies)
library(randomForest)


kredyt <- read.csv("C:/Users/Esterka/Desktop/kredyt_indie.csv", sep = ";", dec = ".")
kredyt[kredyt==""] <- NA

#statystyki opisowe
statystyki <- summary(kredyt) %>% kable(escape=F,digits=0, align = "c") %>% 
  kable_styling(full_width = F,position="center",bootstrap_options = c("striped", "hover", "condensed", "responsive"))%>%
  column_spec(1:5, width = "5cm")

kredyt <- kredyt[,-1]
#zamiana czynnika 3+ na 3
kredyt$Dependents <- revalue(kredyt$Dependents, c("3+"="3"))


naniar::vis_miss(kredyt[,c(1:3,5,8:10)], cluster = T)

#outliers
w <- ggplot(kredyt) +
  guides(fill=FALSE, color=FALSE) +
  xlab(NULL) + ylab(NULL) +
  theme(plot.title = element_text(hjust = 0.5))

w1 <- w + geom_boxplot(aes(y=as.numeric(Dependents)),fill="#66CC99") + 
  ggtitle("Liczba osób na utrzymaniu")

w2 <- w + geom_boxplot(aes(y=ApplicantIncome), fill="#56B4E9") +
  ggtitle("Dochód wnioskodawcy")

w3 <- w + geom_boxplot(aes(y=CoapplicantIncome), fill="#FF9999") + 
  ggtitle("Dochód poręczyciela")

w4 <- w + geom_boxplot(aes(y=LoanAmount),fill="#CC6666") + 
  ggtitle("Kwota kredytu") 

w5 <- w + geom_boxplot(aes(y=Loan_Amount_Term),fill="#E69F00") + 
  ggtitle("Okres kredytu") 

grid.arrange(w1, w2, w3, w4, w5, ncol = 2, nrow = 3)


kredyt[which(kredyt$CoapplicantIncome>=20000),]
kredyt[which(kredyt$ApplicantIncome>=40000),]
kredyt[which(kredyt$LoanAmount>=550),]

kredyt <- kredyt[-which(kredyt$ApplicantIncome>=30000),]
kredyt <- kredyt[-which(kredyt$CoapplicantIncome>=20000),]
kredyt <- kredyt[-which(kredyt$LoanAmount>=550),]

#filling in missing values with knn
kredyt <- VIM::kNN(kredyt, k = 6)
kredyt <- subset(kredyt, select = Gender:Loan_Status)

kredyt$Loan_Status <- ifelse(kredyt$Loan_Status=="Y",0,1)

#division to training and test sets
set.seed(123)
division <- sample(nrow(kredyt), 0.75*nrow(kredyt), replace = F)
train <- kredyt[division,]
test <- kredyt[-division,]

#logistic regression
set.seed(13)
logistic_regression <- glm(Loan_Status~., family = "binomial", data = train)
summary(logistic_regression)
regr_log_final <- step(logistic_regression, direction = "backward", trace=FALSE, k=3 ) 
summary(regr_log_final)
prob <- predict(regr_log_final, test, type = "response")
pred_rl <- ifelse(prob>0.3,1,0)
confM <- caret::confusionMatrix(as.factor(pred_rl), as.factor(test$Loan_Status))
print(confM$table)
Accurancy <- confM$overall[1]
Sensitivity <- confM$byClass[1]
Specificity <- confM$byClass[2]
print(Accurancy)
print(Sensitivity)
print(Specificity)
ROC <- roc(test$Loan_Status,as.numeric(pred_rl))
pROC::auc(ROC)
plot(ROC,main="Logistic regression")

#random Forest
set.seed(13)
oob = c()
for(i in 1:120){
  classifier <-  randomForest(as.factor(Loan_Status)~.,data = train,
                              ntree = i)
  oob[i] <- classifier$err.rate[nrow(classifier$err.rate), "OOB"]
}
opt_i <- which.min(oob)
opt_i

set.seed(13)
random_Forest <- randomForest::randomForest(as.factor(Loan_Status)~.,data = train, ntree = 78, keep.forest=TRUE,na.action=na.roughfix)
pred_rF <- predict(random_Forest, test, type = "response")
confM <- caret::confusionMatrix(as.factor(pred_rF), as.factor(test$Loan_Status))
print(confM$table)
Accurancy <- confM$overall[1]
Sensitivity <- confM$byClass[1]
Specificity <- confM$byClass[2]
print(Accurancy)
print(Sensitivity)
print(Specificity)
ROC <- roc(test$Loan_Status,as.numeric(pred_rF))
pROC::auc(ROC)
plot(ROC,main="Random Forest")


#bagging
set.seed(13)
bagging <- ipred::bagging(Loan_Status~., data=train, nbag =100)
pred_bag <- predict(bagging, test, type = "class")
pred_bag <- ifelse(pred_bag>0.3, 1, 0)
confM <- caret::confusionMatrix(as.factor(pred_bag), as.factor(test$Loan_Status))
print(confM$table)
Accurancy <- confM$overall[1]
Sensitivity <- confM$byClass[1]
Specificity <- confM$byClass[2]
print(Accurancy)
print(Sensitivity)
print(Specificity)
ROC <- roc(test$Loan_Status,as.numeric(pred_bag))
pROC::auc(ROC)
plot(ROC,main="Bagging")

#Boosting
set.seed(13)
boosting <- gbm::gbm(Loan_Status~.,data=train, n.trees = 100)
pred_bo <- predict(boosting, newdata = test, n.trees = 100, type="response")
pred_bo <- ifelse(pred_bo>0.3, 1, 0)
confM <- caret::confusionMatrix(as.factor(pred_bo), as.factor(test$Loan_Status))
print(confM$table)
Accurancy <- confM$overall[1]
Sensitivity <- confM$byClass[1]
Specificity <- confM$byClass[2]
print(Accurancy)
print(Sensitivity)
print(Specificity)
ROC <- roc(test$Loan_Status,as.numeric(pred_bo))
pROC::auc(ROC)
plot(ROC,main="Boosting")

#preparing variables to knn
kredyt2 <- kredyt
kredyt2$Gender <- ifelse(kredyt2$Gender=="Female",1,0)
kredyt2$Married <- ifelse(kredyt2$Married=="Yes",1,0)
kredyt2$Education <- ifelse(kredyt2$Education=="Graduate",1,0)
kredyt2$Self_Employed <- ifelse(kredyt2$Self_Employed=="Yes",1,0)


kredyt2 <- cbind(kredyt2, dummy(kredyt2$Dependents, sep = "_"),dummy(kredyt2$Property_Area, sep = "_"))
kredyt2 <- kredyt2[,-c(3,11,16,19)]
names(kredyt2)[11:15] <- c("Dependents_0","Dependents_1","Dependents_2","Prop_Area_Rural", "Prop_Area_Semiurban")

#knn
#trening and test sets
set.seed(13)
division2 <- sample(nrow(kredyt2), 0.75*nrow(kredyt2), replace = F)
train2 <- kredyt2[division2,]
test2 <- kredyt2[-division2,]

acc = sen = spec = c()
for(k in 1:30){
  match.knn <- class::knn(train = train2[,-10], test = test2[,-10], cl= train2[,10],k = k,prob=TRUE)
  #confusion matrix
  confM <- table(match.knn, test2[,10])
  acc[k] = (confM[2,2]+confM[1,1])/sum(confM)
}
#measures charts
accurancy <- data.frame(acc, k=1:30)
accurancy <- gather(accurancy, "acc", "value", -k)

ggplot(accurancy, aes(x=k))+
  geom_line(aes(y=value, colour=acc), size=1.5)+
  scale_x_continuous(breaks = seq(1,60,2)) +
  labs(x="k", y="ACC") +
  ggtitle("ACC w zalezno?ci od przyjetej wartosci k") +
  theme(plot.title = element_text(hjust = 0.5, size=13, face="bold")) +
  labs(colour = "Rodzaj zbioru:") 

set.seed(13)
#knn for k=7
knn <- class::knn(train = train2[,-10], test = test2[,-10], cl= train2$Loan_Status,k = 7,prob=TRUE)
confM <- caret::confusionMatrix(as.factor(knn), as.factor(test2$Loan_Status))
print(confM$table)
Accurancy <- confM$overall[1]
Sensitivity <- confM$byClass[1]
Specificity <- confM$byClass[2]
print(Accurancy)
print(Sensitivity)
print(Specificity)
ROC <- roc(test2$Loan_Status,as.numeric(knn))
pROC::auc(ROC)
plot(ROC,main="Knn")

analiza_dyskryminacyjna <- MASS::qda(Loan_Status~., train2)
prob <- predict(analiza_dyskryminacyjna, test2, type = "response")
confM <- caret::confusionMatrix(as.factor(prob$class), as.factor(test$Loan_Status))
print(confM$table)
Accurancy <- confM$overall[1]
Sensitivity <- confM$byClass[1]
Specificity <- confM$byClass[2]
print(Accurancy)
print(Sensitivity)
print(Specificity)
ROC <- roc(test$Loan_Status,as.numeric(prob$class))
pROC::auc(ROC)
plot(ROC,main="Analiza dyskryminacyjna")

nb.train <- naive_bayes(as.factor(SRed)~.,data=train)
pred.train <- predict(nb.train, train)
confusionMatrix(pred.train, as.factor(y.train))

ROC <- roc(train$SRed, as.numeric(pred.train))
plot(ROC)
auc(ROC)
