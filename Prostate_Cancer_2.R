library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(ggcorrplot)
library(tidyr)
library(e1071)
library(factoextra)
library(ggfortify)
library(tidyverse)
library(readr)

#Read Data
Data <- read_csv("training_data_modified.csv")
#View(Data)
Dataset<-Data
Dataset<-Dataset[,-c(1,2)]

#Check for Missing Values
sapply(Dataset,function(x) sum(is.na(x)))

#Removing Missing value records form clinical data
Dataset<-Dataset[which(complete.cases(Dataset$gleason_score)),]  #320
Dataset<-Dataset[which(complete.cases(Dataset$first_degree_history)),]  #1550
Dataset<-Dataset[which(complete.cases(Dataset$psa_diagnosis)),]  #1174
Dataset<-Dataset[which(complete.cases(Dataset$tumor_1_year)),]  #427
Dataset<-Dataset[which(complete.cases(Dataset$psa_1_year)),]  #691
Dataset<-Dataset[which(complete.cases(Dataset$tumor_diagnosis)),]  #175
Dataset<-Dataset[which(complete.cases(Dataset$tumor_6_months)),]  #123
Dataset<-Dataset[which(complete.cases(Dataset$symptoms)),]  #282

#Removing Symptoms variables as it will be treated separately
Symptoms<-Dataset$symptoms
Dataset$symptoms<-NULL

#Converting variables datatype
Dataset_fac<-data.frame(lapply(Dataset[ ,c(1:5,7,10:14,21:31)], as.factor))
Dataset_num<-data.frame(sapply(Dataset[ ,-c(1:5,7,10:14,21:31)], as.numeric))
Dataset<-cbind(Dataset_fac, Dataset_num)
str(Dataset)

# plot(Dataset$race)
# hist(Dataset$age)
# hist(Dataset$height)
# hist(Dataset$weight)

library(mice)
#Now lets use Mice package to impute the missing values for demographic variables. 
#Default method is PMM.
Dataset_I = mice(Dataset, m = 5, maxit = 3)
Dataset_I<-complete(Dataset_I,1)
Dataset_II<-Dataset_I
#Dataset_I<-Dataset_II
# plot(Dataset_I$race)
# hist(Dataset_I$age)
# hist(Dataset_I$height)
# hist(Dataset_I$weight)
#Distribution before and after imputation remains the same

#Converting multi-valued symbols variable to 16 Binary variables
symp_symbols <- c("U03","U06","S07","U01","U02","S10","O11","U05","S04","P02","P01","O01","O09","O08","O10","P03")
sympdataframe <- data.frame(matrix(ncol=16,nrow=0))
colnames(sympdataframe) <- symp_symbols
for(i in 1:dim(Dataset_I)[1]){
  a <- as.list(strsplit(as.character(Symptoms[i]),",")[[1]])
  compare <- NULL
  for(j in 1:length(a)){
    compare <- rbind(compare,symp_symbols == a[j])
  }
  sympdataframe <- rbind(sympdataframe,apply(compare,2,sum))
} 
colnames(sympdataframe) <- symp_symbols
Symptms<-sympdataframe
Dataset_I <- cbind(Dataset_I, Symptms)

#BMI
Dataset_I$BMI<-(Dataset_I$weight/(Dataset_I$height*Dataset_I$height))*703
Dataset_I$height<-NULL
Dataset_I$weight<-NULL
Dataset_I$BMI<-as.numeric(Dataset_I$BMI)

#Age
Dataset_I$age_grp<-ifelse((Dataset_I$age<50),'Group1',
                          ifelse((Dataset_I$age>=50 &                                       
                                    Dataset_I$age<=75),'Group2','Group5'))
Dataset_I$age_grp<-as.factor(Dataset_I$age_grp)
Dataset_I$age<-NULL

#Gleason Score
#Dataset_I$gleason_score<-as.numeric(Dataset_I$gleason_score)
Dataset_I$gleason_scr<-ifelse(is.element(Dataset_I$gleason_score,c("3","4","5","6")),'Low',
                              ifelse(is.element(Dataset_I$gleason_score,c("7","8","9")),'Medium',
                                     'High'))
Dataset_I$gleason_scr<-as.factor(Dataset_I$gleason_scr)
table(Dataset_I$survival_7_years, Dataset_I$gleason_scr)
Dataset_I$gleason_score<-NULL

#PSA Growth Rate 6 months
Dataset_I$psa6_rate<-100*(Dataset_I$psa_6_months-Dataset_I$psa_diagnosis)/Dataset_I$psa_diagnosis
hist(Dataset_I$psa6_rate, col=c("blue", "orange"))
#Tumor Growth Rate 1 year
Dataset_I$tumor6_rate<-100*(Dataset_I$tumor_6_months-Dataset_I$tumor_diagnosis)/Dataset_I$tumor_diagnosis
hist(Dataset_I$tumor6_rate, col=c("green", "yellow"))

#PSA Growth Rate 6 months
Dataset_I$psa_rate<-100*(Dataset_I$psa_1_year-Dataset_I$psa_diagnosis)/Dataset_I$psa_diagnosis
hist(Dataset_I$psa_rate, col=c("blue", "orange"))
#Tumor Growth Rate 1 year
Dataset_I$tumor_rate<-100*(Dataset_I$tumor_1_year-Dataset_I$tumor_diagnosis)/Dataset_I$tumor_diagnosis
hist(Dataset_I$tumor_rate, col=c("green", "yellow"))
Dataset_I$psa_1_year<-NULL
Dataset_I$psa_diagnosis<-NULL
Dataset_I$tumor_diagnosis<-NULL
Dataset_I$tumor_1_year<-NULL
Dataset_I$tumor_6_months<-NULL
Dataset_I$psa_6_months<-NULL

Dataset_fac1<-data.frame(lapply(Dataset_I[ ,c(22:37)], as.factor))
Dataset_I<-Dataset_I[,-c(22:37)]
Dataset_I<-cbind(Dataset_fac1, Dataset_I)
#Dataset_Im<-Dataset_I
#Dataset_I<-Dataset_Im

#BI-Variate analysis 
Train_cat<-Dataset_I[,-c(44,43,42,41,38)]
Train_num<-Dataset_I[,c(44,43,42,41,38,37)]
Train_num$survival_7_years<-as.numeric(Train_num$survival_7_years)
#Cor Matrix
r <- cor(Train_num, use="complete.obs")
round(r,2)
library(ggcorrplot)
ggcorrplot(r, hc.order = TRUE, type = "lower" , lab = T )

#T-Test
pvn<-data.frame(lapply(Train_num[,-6], function(x) t.test(x ~ Train_num$survival_7_years, var.equal = TRUE)$p.value))
View(round(pvn,6))
#Chi-Squared Test
pvc<-data.frame(lapply(Train_cat[,-37], function(x) chisq.test(table(x,Train_cat$survival_7_years))$p.value))
View(round(pvc,6))

Dataset_I$tea<-NULL
Dataset_I$side<-NULL
Dataset_I$psa6_rate<-NULL
Dataset_I$tumor6_rate<-NULL
Dataset_I$family_history<-NULL
Dataset_I$U01<-NULL
Dataset_I$U02<-NULL
Dataset_I$U03<-NULL
Dataset_I$U06<-NULL
Dataset_I$S07<-NULL
Dataset_I$O11<-NULL
Dataset_I$S04<-NULL

set.seed(999)
library(caret)
train.index <- createDataPartition(Dataset_I$survival_7_years, p = .8, list = FALSE)
TrainData1  <- Dataset_I[ train.index,]
TestData1<- Dataset_I[-train.index,]
table(TrainData1$survival_7_years)
table(TestData1$survival_7_years)
TrainData<-TrainData1
TestData<-TestData1
# 45 age, 21 DV, 12:19 thrpy

###Logistic Regression Model
null <- glm(survival_7_years ~ 1, data= TrainData,family="binomial") # only includes one variable
full <- glm(survival_7_years ~ ., data= TrainData,family="binomial") # includes all the variables
logitModel <- step(null, scope = list(lower = null, upper = full), direction = "both")
summary(logitModel)

Model<-glm(formula = survival_7_years ~ survival_1_year + n_score + 
             tumor_rate + rd_thrpy + m_score + gleason_scr + U05 + rad_rem + 
             S10 + brch_thrpy + stage + Total_Therapy + cry_thrpy + age_grp + 
             smoker + race + psa_rate, family = "binomial", data = TrainData)
alias(Model)   
car::vif(Model) 
#plot(Model)
summary(Model)

pred_prob <- predict(Model,newdata = TestData, type = "response")
library(ROCR)
pred <- prediction( predictions = pred_prob, TestData$survival_7_years)
perf <- performance(pred,"tpr","fpr")
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x-0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)}

print(opt.cut(perf, pred))
#Prediction and performance on TrainData
pre <- as.numeric(predict(Model,type="response")>0.4621201)
confusionMatrix(table(pre,TrainData$survival_7_years), positive = "1")
#Prediction and performance on TestData
pre <- as.numeric(predict(Model,newdata=TestData[,-27],type="response")>0.4642080)
confusionMatrix(table(pre,TestData$survival_7_years), positive = "1")
options(scipen = 99)
#Coefficients - Log of ODDS
exp(coefficients(Model))


