lcdata <- read_csv('~/Downloads/lcDataSample 2.csv') 
  lcdata <- lcDataSample
dim(lcdata)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

#Q3. a. Look at the data attributes. 
attributes(lcdata)$spec

#b). 
lcdata$last_pymnt_d
lcdata$issue_d
head(lcdata) %>% str()

#we are only interested in Fully paid and charged off
lcdata <- lcdata %>% filter(loan_status == "Fully Paid" | loan_status == "Charged Off" )

#method 1 to calculate return on loans
simple_ret <- lcdata %>% group_by(grade) %>% summarise((total_pymnt - funded_amnt)*100/(3*funded_amnt))
lcdata$annRet <- ((lcdata$total_pymnt -lcdata$funded_amnt)/lcdata$funded_amnt)*(1/3)*100
lcdata %>% group_by(grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), avgInterest= mean(int_rate), stdInterest=sd(int_rate), avgLoanAMt=mean(loan_amnt), avgPmnt=mean(total_pymnt), avgRet=mean(annRet), stdRet=sd(annRet), minRet=min(annRet), maxRet=max(annRet))

library(lubridate)
head(lcdata[, c("last_pymnt_d", "issue_d")])
str(lcdata$last_pymnt_d)
str(lcdata$issue_d)

lcdata$last_pymnt_d <- paste(lcdata$last_pymnt_d, "-01", sep = "")
lcdata$last_pymnt_d<-parse_date_time(lcdata$last_pymnt_d,  "myd")

lcdata$actualTerm <- ifelse(lcdata$loan_status=="Fully Paid", as.duration(lcdata$issue_d  %--% lcdata$last_pymnt_d)/dyears(1), 3)
lcdata$actualReturn <- ifelse(lcdata$actualTerm>0, ((lcdata$total_pymnt -lcdata$funded_amnt)/lcdata$funded_amnt)*(1/lcdata$actualTerm)*100, 0)

lcdata %>% select(loan_status, int_rate, funded_amnt, total_pymnt, annRet, actualTerm, actualReturn) %>%  head()
lcdata$emp_length <- factor(lcdata$emp_length, levels=c("n/a", "< 1 year","1 year","2 years", "3 years" ,  "4 years",   "5 years",   "6 years",   "7 years" ,  "8 years", "9 years", "10+ years" ))


#Another method to calculate - 
#a <- 1 + ((1+lcdata$int_rate) ^ lcdata$actualterm)
#b <- 1 - (1+lcdata$int_rate)
#c<- (1+lcdata$int_rate) ^ (36 - lcdata$actualterm)
#t <- lcdata$total_pymnt / lcdata$actualterm
#actual <- lcdata %>% group_by(loan_status) %>% summarise(({(t* (a/b)) * c} - lcdata$funded_amnt) / 3)

#Q3 c.Visualisation of various attributes
# we create a new column default rate
lcdata <- lcdata %>% group_by(grade) %>% mutate(defaultRate=sum(loan_status=='Charged Off')/n())

ggplot(lcdata, aes(x=int_rate, y=defaultRate)) + geom_point(aes(col = grade, size = )) + geom_smooth(method="loess", se=F)
cor(lcdata$int_rate, lcdata$defaultRate)

ggplot(lcdata, 
       aes(x = grade, 
           fill = defaultRate)) + 
  geom_bar(position = "stack")


ggplot(lcdata, aes(x= delinq_2yrs, y= defaultRate)) + geom_smooth(method="gam", se=F ) 

ggplot(lcdata, aes(x= dti)) +geom_boxplot(aes(fill= grade))

ggplot(lcdata, aes( x = total_pymnt)) + geom_boxplot(aes(fill=grade))
ggplot(lcdata, aes( x = loan_amnt)) + geom_boxplot(aes(fill=loan_status))
ggplot(lcdata, aes( x = total_pymnt)) + geom_boxplot(aes(fill=grade))
ggplot(lcdata, aes(x= dti)) +geom_boxplot(aes(fill= grade))

#Q3 d The values of loan_status are Charged off, Current, Fully Paid, In Grace Period, Late(16-30 days), Late(31-120 days)
table(lcdata$loan_status)

# The below code is used to restrict attention to "fully paid" and "charged off" loan. 
lcdata %>% filter(loan_status== c("Charged Off" , "Fully Paid")) %>% group_by(loan_status) %>% view()

#Proportion of default in charged off vs fully paid loans 
total <- lcdata %>% filter(loan_status== c("Charged Off" , "Fully Paid")) %>% group_by(grade) %>% count() 

chargedoff <- lcdata %>% group_by(grade) %>% filter(loan_status=="Charged Off") %>% count()
chargedoff

ProportionOfDefault <- sum(chargedoff$n)/sum(total$n) 
ProportionOfDefault

lcdata %>% group_by(grade) %>% summarise(charged_off = sum(loan_status == "Charged Off") , defaultrate = (charged_off/n())*100) 
default_by_sub <- lcdata %>% group_by(sub_grade) %>% summarise(charged_off = sum(loan_status == "Charged Off") , nloans=n(), defaultrate = (charged_off/n())*100) 
default_by_sub
#Proportion of defaults can be calculate using the above code, and it comes out to be 0.280, which means that 28.06% of individuals default on their loans.  ```

# Q3. Number of loans in each grade
lcdata %>% group_by(grade) %>% tally()

# do loan amount, interest rate vary by grade
lcdata %>% group_by(grade) %>% summarise(sum(loan_amnt), mean(int_rate))

# do loan amount, interest rate vary by subgrade
lcdata %>% group_by(sub_grade) %>% summarise(sum(loan_amnt), mean(int_rate))

# calculate average, standard deviation, min and max interest rate by grade and sub-grade
lcdata %>% group_by(grade) %>% summarise(nLoans=n(), avgInterest= mean(int_rate), stdInterest=sd(int_rate), minret=min(int_rate) , maxret = max(int_rate))
subgrade <- lcdata %>% group_by(sub_grade) %>% summarise(nLoans=n(), avgInterest= mean(int_rate), stdInterest=sd(int_rate), minret=min(int_rate) , maxret = max(int_rate))
subgrade

# Q3. d.iv. for fully paid back loans, how does time to full payoff vary
lcdata %>% filter(loan_status == "Fully Paid") %>% group_by(grade) %>% summarise(stddev = sd(lcdata$actualTerm))
lcdata %>% filter(loan_status == "Fully Paid")  %>% ggplot(aes(x = grade , y=actualTerm)) + geom_boxplot(fill= "pink")

lcdata<- lcdata %>% mutate_if(is.numeric,  ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))
dim(lcdata)
lcdata <- lcdata %>% mutate_if(is.character, as.factor)

#(d)(iv) What is 'recoveries'
lcdata %>% group_by(loan_status) %>%summarise(avgRec=mean(recoveries))
#average number of recoveries for charged off loans are 926.

lcdata %>% group_by(loan_status) %>%summarise(avgRec=mean(recoveries), avgPmnt=mean(total_pymnt), mean(total_rec_prncp), mean(total_rec_int), mean(total_rec_late_fee))

#(v)
lcdata$annRet <- ((lcdata$total_pymnt -lcdata$funded_amnt)/lcdata$funded_amnt)*(1/3)*100
lcdata$annRet
lcdata %>% group_by(grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), avgInterest= mean(int_rate), stdInterest=sd(int_rate), avgLoanAMt=mean(loan_amnt), avgPmnt=mean(total_pymnt), avgRet=mean(annRet), stdRet=sd(annRet), minRet=min(annRet), maxRet=max(annRet))



lcdata %>% select(loan_status, int_rate, funded_amnt,  annRet) %>% filter(annRet < 0) %>% head()
#For Charged off loans
lcdata %>% filter( loan_status == "Charged Off") %>% group_by(grade) %>% summarise(nLoans=n(), avgInterest= mean(int_rate),avgRet=mean(annRet))

#For fully paid loans
lcdata %>% filter( loan_status == "Fully Paid") %>% group_by(grade) %>% summarise(nLoans=n(), avgInterest= mean(int_rate),avgRet=mean(annRet))

#(vi) What are people borrowing money for (purpose)? 
lcdata %>% group_by(purpose) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), defaultRate=defaults/nLoans, avgIntRate=mean(int_rate),  avgLoanAmt=mean(loan_amnt),  avgActRet = mean(actualReturn), avgActTerm=mean(actualTerm))
table(lcdata$purpose, lcdata$grade) 

#The total number of loans taken for each purpose can be grouped as follows.
lcdata%>%group_by(purpose) %>% tally()

#Vii 
table(lcdata$purpose, lcdata$emp_length)
table(lcdata$grade, lcdata$emp_length)
ggplot(lcdata, aes( x = total_pymnt)) + geom_boxplot(aes(fill=loan_status))

#viii 
lcdata$propSatisBankcardAccts <- ifelse(lcdata$num_bc_tl>0, lcdata$num_bc_sats/lcdata$num_bc_tl, 0)
ggplot(lcdata, aes(x=propSatisBankcardAccts, y=defaultRate))  + geom_smooth(method="loess", se=F)

cor (lcdata$propSatisBankcardAccts , lcdata$defaultRate )

ggplot(lcdata, 
       aes(x = lcdata$purpose, 
           y = lcdata$loan_amnt)) + 
  geom_bar(stat = "summary" ,fun = "sum")


#e & f) Finding and dealing with missing values

#Finding the number of NAs in various columns 
na_count <-sapply(lcdata, function(y) sum(length(which(is.na(y)))))
table(na_count)
data.frame(na_count)
names(lcdata)[colSums(is.na(lcdata)) > 70000]
lcdata <- lcdata %>% select(-all_of(names(lcdata)[colSums(is.na(lcdata)) > 70000]))
# Replacing NA or missing values with 0

lcdata[is.na(lcdata$mths_since_last_major_derog) , "mths_since_last_major_derog"] <- 0
lcdata[is.na(lcdata$mths_since_recent_revol_delinq ) , "mths_since_recent_revol_delinq"] <- 0
lcdata[is.na(lcdata$mths_since_last_delinq ) , "mths_since_recent_revol_delinq"] <- 0


# Replacing values of NAs with the median group wise for an important column with 46 missing values
lcdata %>% group_by( grade) %>% summarize(median(revol_util , na.rm = TRUE))
lcdata[is.na(lcdata$revol_util) & lcdata$grade == "A", "revol_util"] <- 41.5
lcdata[is.na(lcdata$revol_util) & lcdata$grade == "B", "revol_util"] <- 55.2
lcdata[is.na(lcdata$revol_util) & lcdata$grade == "C", "revol_util"] <- 60.1
lcdata[is.na(lcdata$revol_util) & lcdata$grade == "D", "revol_util"] <- 61.9
lcdata[is.na(lcdata$revol_util) & lcdata$grade == "E", "revol_util"] <- 61.4
lcdata[is.na(lcdata$revol_util) & lcdata$grade == "F", "revol_util"] <- 61.7
lcdata[is.na(lcdata$revol_util) & lcdata$grade == "G", "revol_util"] <- 64.8


dim(lcdata)

# Removing outliers for important variables 
# Found outlier for annual income 
summary(lcdata)
#All those having max value way above median are suspicious eg. annual income
g<- ggplot(lcdata, aes(lcdata$loan_amnt, lcdata$annual_inc))
 g + geom_jitter(width = .5, size=1)


which(lcdata$annual_inc >= 7000000 )
lcdata <- lcdata [-c(7201, 42780,82338),  ]

# we now try to find the outlier using boxplot for revol_util

boxplot(lcdata$revol_util)$out
out_ru <- boxplot(lcdata$revol_util)$out
out_i <- which(lcdata$revol_util %in% out_ru)
lcdata <- lcdata [-out_i ,  ]

#Q5. univariate testing using AUC ROC

library(pROC)
lcdata$loan_status <- as.factor(lcdata$loan_status)
auc(response=lcdata$loan_status, lcdata$loan_amnt)
auc(response=lcdata$loan_status, lcdata$dti)

lcdata$home_ownership1 <- as.numeric(as.factor(lcdata$home_ownership))
auc(response=lcdata$loan_status, lcdata$home_ownership1)
lcdata <- lcdata %>% mutate_if(is.character, as.factor)
lcdata$loan_status <- as.factor(lcdata$loan_status)
lcdata <- data.frame(lcdata)
install.packages("plyr")
library(plyr)
library(pROC)
aucsNum <- sapply(lcdata %>% select_if(is.numeric), auc , response=lcdata$loan_status)
aucAll <- sapply((lcdata %>% mutate_if(is.factor, as.numeric) %>% select_if(is.numeric)), auc, response=lcdata$loan_status)

data.frame (aucAll)
library(broom)
tidy(aucAll) %>% arrange(desc(aucAll)) %>% view()    




# Q 6, 7, 8.Developing Models 

# Step 1: Removal of variables and insignificant values which are significant for our model developing 
# Also, remove variables which can cause leakage in our model 

varsToRemove = c('funded_amnt_inv', 'term', 'emp_title', 'pymnt_plan', 
                 'earliest_cr_line', 'title', 'zip_code', 'addr_state', 'out_prncp', 
                 'out_prncp_inv', 'total_pymnt_inv', 'total_rec_prncp', 'total_rec_int', 
                 'total_rec_late_fee', 'recoveries', 'collection_recovery_fee', 'last_credit_pull_d', 
                 'policy_code', 'disbursement_method', 'debt_settlement_flag', 
                 'application_type', 'last_pymnt_d' , 'last_pymnt_amnt',  'issue_d', 'installment', 'funded_amnt', 'propSatisBankcardAccts')

lcdata <- lcdata %>% select(-all_of(varsToRemove))  
lcdata <- lcdata %>% select(-c('id' )) 
dim(lcdata)


# Variables of starting with 'hardships' and 'settlement' are not important for our model 

lcdata <- lcdata %>% select(-starts_with("hardship"))
lcdata <- lcdata %>% select(-starts_with("settlement"))
dim(lcdata)



install.packages('caTools')
library(caTools)



split = sample.split(lcdata$loan_status , SplitRatio = 0.7)
train = subset(lcdata, split == TRUE)
test = subset(lcdata, split == FALSE)

library(caret)
library(ROCR)
library(rpart)

#the below variables can cause data leakage so not considering them in preparing our model
varsOmit <- c('actualTerm', 'actualReturn', 'annRet' , 'total_pymnt' ) 

lcdata$loan_status <- factor(lcdata$loan_status, levels = c("Fully Paid", "Charged Off"))
# Omit newly created variables

model <- rpart(loan_status ~., data=train %>%  select(-all_of(varsOmit)), method="class", parms = list(split = "information"), control = rpart.control(cp=0.0001, minsplit = 50))
printcp(model)
model$variable.importance
# We find that a few leakage varibales  have been given a lot of importance and thus we remove them 


#Convert employment length into factor 
lcdata$emp_length <- as.factor(lcdata$emp_length)

# cp is complexity parameter which helps you to tell how much to prune. So using cp table, you find the value of cp which gives the min
# xerror 
mincp_i <- which.min(model$cptable[, 'xerror'])

#To find optimum cp value, you find xerror +xstd error 
optError <- model$cptable[mincp_i, "xerror"] + model$cptable[mincp_i, "xstd"]

optCP_i <- which.min(abs( model$cptable[,"xerror"] - optError))
optCP <- model$cptable[optCP_i, "CP"]
model_p2 <- prune(model, cp = optCP)
# Evaluating and comparing Performances with different distribution

pred <- predict(model_p2, train, type='class')
pred_test <- predict(model_p2, test, type='class')
table(pred, train$loan_status)

# optimum CP is given for balanced data and therefore we get a 0 charged off predictions
# So we will use CP as 0.0001

model_p2 <- prune(model, cp = 0.0001)
pred <- predict(model_p2, train, type='class')
pred_test <- predict(model_p2, test, type='class')
table(pred, train$loan_status)
mean(pred == train$loan_status)
table(pred_test, test$loan_status)
mean(pred_test ==test$loan_status)

#confusion matrix 
library (caret)

# Here the positive is considered as 'Charged off', it can be changed by specifying the value in positive parameter)
# Model with default 'prior' value 
pred <- factor(pred, levels= c("Charged Off" , "Fully Paid"))
train$loan_status <- factor(train$loan_status, levels= c("Charged Off", "Fully Paid"))
confusionMatrix(pred, train$loan_status)
# Here the positive is considered as 'Charged off', it can be changed by specifying the value in positive parameter)
pred_test <- factor(pred_test, levels= c("Charged Off" , "Fully Paid"))
test$loan_status <- factor(test$loan_status, levels= c("Charged Off", "Fully Paid"))
confusionMatrix(pred_test, test$loan_status)

#-------------------------------------------Next Model--------------------------------

# Run a model with more balanced distribution by using 'prior' parameter which defines the distribution among the classes. 
# By default prior is taken from the dataset 
model2 <- rpart(loan_status ~., data=train, 
                method="class", parms = list(split = "information", prior=c(0.5, 0.5)), 
                control = rpart.control(cp=0.0, minsplit = 20, minbucket = 10, maxdepth = 20,  xval=10) )

mincp_i2 <- which.min(model2$cptable[, 'xerror'])
optError2 <- model2$cptable[mincp_i2, "xerror"] + model2$cptable[mincp_i2, "xstd"]
optCP_i2 <- which.min(abs( model2$cptable[,"xerror"] - optError2))
optCP2 <- model2$cptable[optCP_i2, "CP"]
model_p22 <- prune(model2, cp = optCP2)

#Model with balanced distribution
pred2 <- predict(model_p22, train, type ='class')
pred2 <- factor(pred2, levels= c("Charged Off" , "Fully Paid"))
table(pred = pred2, true=train$loan_status)
#we find that model overfits training data

pred_test2 <- predict(model_p22, test ,type ='class')
pred_test2 <- factor(pred_test2, levels= c("Charged Off" , "Fully Paid"))
table(pred = pred_test2, true=test$loan_status)

#Variation in classification threshold value (default = 0.5)
CTHRESH = 0.1
predProbTrain = predict(model_p2, train, type= 'prob')
result <- ifelse(predProbTrain[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
table (result, train$loan_status)

CTHRESH = 0.3
predProbTrain2 = predict(model_p2, train, type= 'prob')
result2 <- ifelse(predProbTrain2[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
table (result2, train$loan_status)

CTHRESH = 0.7
predProbTrain3 = predict(model_p2, train, type= 'prob')
result3 <- ifelse(predProbTrain3[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
table (result3, train$loan_status)

CTHRESH = 0.9
predProbTrain4 = predict(model_p2, train, type= 'prob')
result4 <- ifelse(predProbTrain4[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
table (result4, train$loan_status)

# The best result out of these trails is given by a classification threshold value of 0.3 but it doesn't mean it is the optimal value 
# of threshold for this case

#Calculate ROC and AUC curve

library(ROCR)
score=predict(model_p2,test, type="prob")[,"Charged Off"]
pred_curve=prediction(score, test$loan_status, label.ordering = c("Fully Paid", "Charged Off"))

#label.ordering here specifies the 'negative', 'positive' class labels   

#ROC curve
aucPerf <-performance(pred_curve, "tpr", "fpr")
plot(aucPerf)
abline(a=0, b= 1)

#AUC value
aucPerf=performance(pred_curve, "auc")
aucPerf@y.values


#Lift curve
liftPerf <-performance(pred_curve, "lift", "rpp")
plot(liftPerf)

# Evaluation for model2
score2=predict(model_p22,train, type="prob")[,"Charged Off"]
pred_curve2=prediction(score2, train$loan_status, label.ordering = c("Fully Paid", "Charged Off"))

score2=predict(model_p22,test, type="prob")[,"Charged Off"]
pred_curve2=prediction(score2, test$loan_status, label.ordering = c("Fully Paid", "Charged Off"))

#ROC curve
aucPerf2 <-performance(pred_curve2, "tpr", "fpr")
plot(aucPerf2, col = "blue", lwd= 1.5)
abline(a=0, b= 1)

#AUC value
aucPerf2=performance(pred_curve2, "auc")
aucPerf2@y.values

#Lift curve
liftPerf <-performance(pred_curve2, "lift", "rpp")
plot(liftPerf, col = "red" , lwd= 1.5)
#--------------------------------------------------Next Model building-----------------------

#Using C50 library
#We build a C50 tree model called it as C5_DT1 using the code
library(C50)
c5_DT1 <- c5_DT1 <- C5.0(loan_status ~., data=train, control=C5.0Control(minCases=30))

#considering a more balanced data for building the tree
caseWeights <- ifelse(train$loan_status=="Charged Off", 6, 1)
c5_DT1 <- C5.0(loan_status ~., data=train %>%  select(-all_of(varsOmit)), weights = caseWeights, control=C5.0Control(minCases=30))
summary(c5_DT1)

predTrn <- predict(c5_DT1, train, type='prob')
head(predTrn)
CTHRESH=0.5
table(pred = predTrn[,'Fully Paid' ] > CTHRESH, true=train$loan_status)
# we can see the model slightly overfits train data

score=predict(c5_DT1,train, type="prob")[,"Charged Off"]
pred=prediction(score, train$loan_status, label.ordering = c("Fully Paid", "Charged Off"))
aucPerf <-performance(pred, "tpr", "fpr")
plot(aucPerf)
abline(a=0, b= 1)


#AUC value
aucPerf=performance(pred, "auc")
sprintf("AUC: %f", aucPerf@y.values)

#Now we test on test data set
predTst <- predict(c5_DT1, test, type='prob')
table(pred = predTst[,'Fully Paid' ] > CTHRESH, true=test$loan_status)


score=predict(c5_DT1,test, type="prob")[,"Charged Off"]
pred=prediction(score, test$loan_status, label.ordering = c("Fully Paid", "Charged Off"))
aucPerf <-performance(pred, "tpr", "fpr")
plot(aucPerf)
abline(a=0, b= 1)


#AUC value
aucPerf=performance(pred, "auc")
sprintf("AUC: %f", aucPerf@y.values)

#analyzing this AUC value we find that it didnt perform well on the test set


# Random Forest model 
library(ranger) #Building the model
#rfModel <- ranger( loan_status ~ . , data=train %>% select(-all_of(varsOmit)) , num.trees = 200, importance='permutation', probability = TRUE)
# we find that random forest build using this method has overfitted

#rfModel3 <- ranger(loan_status ~., data=train %>%  select(-all_of(varsOmit)), num.trees =300, class.weights = c(6, 1) , importance='permutation', probability = TRUE)

#We will use the model2
rfModel2 <- ranger(loan_status ~., data=train %>%  select(-all_of(varsOmit)),
                   num.trees =800, probability = TRUE, min.node.size = 70, max.depth = 15 , importance = 'permutation' )

#we find that using rfModel2 we were able to over come overfitting 

#viewing important attributes
view(importance(rfModel2))

#predicting on training set first
ypredtrn <- predict(rfModel2,train )
head(ypredtrn$predictions)
#we find two columns with prediction probability of Charged off and Fully paid 

table(predictions = ypredtrn3$predictions[, "Fully Paid"] > 0.6 , actuals =train$loan_status)

#Building the ROC curve
pred = prediction(ypredtrn$predictions[, "Fully Paid"], train$loan_status)  


aucPerftrain <-performance(pred, "tpr", "fpr")
plot(aucPerftrain , col = "red" , lwd = 1.5)
abline(a=0, b= 1)


sprintf("AUC: %f", performance(pred, "auc")@y.values)



#we now test the model on test set
ypred <- predict(rfModel2 ,test )
head(ypred$predictions)
ypred$num.independent.variables

table(predictions = ypred$predictions[, "Fully Paid"] > 0.7 , actuals = test$loan_status)

pred1 = prediction(ypred$predictions[, "Fully Paid"], test$loan_status)

#Generating the AUC value
aucPerftest1 <-performance(pred1, "tpr", "fpr")
plot(aucPerftest1 , col = 'red')
abline(a=0, b= 1)

sprintf("AUC: %f", performance(pred1, "auc")@y.values)
# we find that it performed the best out of the three models 


liftPerf <-performance(pred1, "lift", "rpp")
plot(liftPerf  )

#Preparing Lift

lcdata %>% group_by(loan_status) %>% summarise(avgInterest= mean(int_rate),  
                                               avgRet=mean(annRet),  avgActualTerm=mean(actualTerm) )


#scoresFP <- ypred$predictions[ , "Fully Paid"]
#head(scoresFP)

ypred_scores <- predict(rfModel2 ,test )$predictions[, "Fully Paid"]
perfrmnc <- cbind(data.frame(ypred_scores) , status = test$loan_status)

#sorting the order in the descending order
perfrmnc <- perfrmnc[order(-ypred_scores),]
view(perfrmnc)

#from here we get which loans would be fully paid along with there probabilities
Profit <- 15.48
COST <-  -36
perfrmnc$profit <- ifelse(perfrmnc$status == "Fully Paid" , Profit , COST)
perfrmnc$cumProfit <- cumsum ( perfrmnc$profit )

view(perfrmnc)
which.max(perfrmnc$cumProfit)
perfrmnc$cumProfit[which.max(perfrmnc$cumProfit)]


#Returns performance by ntiles
fnDecileReturnsPerformance <- function( scores, dat) {
  decRetPerf <- data.frame(scores)
  decRetPerf <- cbind(decRetPerf, status=dat$loan_status, grade=dat$grade, actRet=dat$actualReturn, actTerm = dat$actualTerm)
  decRetPerf <- decRetPerf %>% mutate(decile = ntile(-scores, 25))
  decRetPerf %>% group_by(decile) %>% summarise (
    count=n(), numDefaults=sum(status=="Charged Off"), avgActRet=mean(actRet), minRet=min(actRet), maxRet=max(actRet),
    avgTer=mean(actTerm), totA=sum(grade=="A"), totB=sum(grade=="B" ), totC=sum(grade=="C"), totD=sum(grade=="D"),
    totE=sum(grade=="E"), totF=sum(grade=="F") )
  
  
  
}
view(fnDecileReturnsPerformance(predict(rfModel2, test)$predictions[,"Fully Paid"], dat=test))








































