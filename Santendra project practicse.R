setwd("C:\\Users\\vkuma140\\Music\\Part-3\\Project 1 Santander Customer Satisfaction\\train.csv")
dir()

Train1<-read.csv("train.csv")
View(Train1)
dim(Train1)
str(Train1)

library(Amelia)
missmap(Train1[,-1], main = "Missing value vs observered")


sapply(Train1,function(x) length(unique(x)))

rem_col=subset(Train1,select = -c(ID,
                                  ind_var2_0,
                                  ind_var2,
                                  ind_var27_0,
                                  ind_var28_0,
                                  ind_var28,
                                  ind_var27,
                                  ind_var41,
                                  ind_var46_0,
                                  ind_var46,
                                  num_var27_0,
                                  num_var28_0,
                                  num_var28,
                                  num_var27,
                                  num_var41,
                                  num_var46_0,
                                  num_var46,
                                  saldo_var28,
                                  saldo_var27,
                                  saldo_var41,
                                  saldo_var46,
                                  imp_amort_var18_hace3,
                                  imp_amort_var34_hace3,
                                  imp_reemb_var13_hace3,
                                  imp_reemb_var33_hace3,
                                  imp_trasp_var17_out_hace3,
                                  imp_trasp_var33_out_hace3,
                                  num_var2_0_ult1,
                                  num_var2_ult1,
                                  num_reemb_var13_hace3,
                                  num_reemb_var33_hace3,
                                  num_trasp_var17_out_hace3,
                                  num_trasp_var33_out_hace3,
                                  saldo_var2_ult1,
                                  saldo_medio_var13_medio_hace3
))
View(rem_col)

sapply(rem_col, function(x) length(unique(x)))# Checking unique again value whether it is there or not

dim(rem_col)

library(psych)
library(mnormt)
library(GPArotation)

dtacor<-cor(rem_col)
eigen_val<-eigen(dtacor)$values
length(eigen_val[eigen_val>1])

fa_analysis<-fa(r=dtacor,nfactors = 68,rotate = "oblimin",fm='pa')

class(rem_col)
result<-data.frame(factor.scores( rem_col, fa_analysis)$scores)
View(result)
class(result)
dim(result)
result$TARGET<-rem_col[,c('TARGET')]
View(result)
dim(result)


sample.ind <- sample(2, nrow(result), replace = T, prob = c(0.75,0.25))
Train<-result[sample.ind==1,];dim(Train);View(Train)
###################################################################
Test<-result[sample.ind==2,];dim(Test);View(Test)


#%%%%%%%%%%%%%%%%%%%%%%%%%%% Applying Logostic Regression %%%%%%%%%%%%%%%%

# Now going for model---> Reject this model because Null deviance:  19073 is smaller than Residual deviance: 871031

#  model<-glm(TARGET~.,data = Train,family = binomial)
# # 
#  summary(model)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Trying another model:- SVM %%%%%%%%%%%%%%

library(e1071)
library(caret)
svm_model<-svm(TARGET~.,data = Train)
summary(svm_model)


pred_test<-predict(svm_model,newdata = Test)

pred_value<-ifelse(pred_test>=0.5,1,0)
str(pred_value)
a<-as.integer(pred_value)
str(a)
str(Test$TARGET)
class(Test)
class(Train)

confusionMatrix(a,Test$TARGET)



b<-table(factor(a, levels=min(Test$TARGET):max(Test$TARGET)), 
      factor(Test$TARGET, levels=min(Test$TARGET):max(Test$TARGET)))
b

print(sum(diag(b))/sum(b)) # R-0.9601004>0.85 better fit model


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

