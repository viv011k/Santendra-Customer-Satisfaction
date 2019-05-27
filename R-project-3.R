
setwd("C:\\Users\\vkuma140\\Music\\Part-3\\Project 1 Santander Customer Satisfaction\\train.csv")
dir()

Train1<-read.csv("train.csv")
View(Train1)


#W-col, X-col, ind_var27_0,ind_var28_0,ind_var28,ind_var27,ind_var41,ind_var46_0
#ind_var46,num_var27_0,num_var28_0, ind_var28,ind_var41,ind_var46_0, ind_var46, num_var27_0, 
#num_var28_0, num_var28,num_var27,  num_var46_0, num_var46, saldo_var28, saldo_var27, saldo_var41
#saldo_var46, imp_amort_var18_hace3,imp_reemb_var33_hace3, imp_amort_var34_hace3, imp_reemb_var13_hace3, 
#imp_trasp_var17_out_hace3, imp_trasp_var33_out_hace3, num_var2_0_ult1, num_var2_ult1 
# saldo_medio_var13_medio_hace3-------> all have zero values

# num_reemb_var13_hace3, num_reemb_var33_hace3,  num_trasp_var17_out_hace3, num_trasp_var17_out_ult1



library(Amelia)

dim(Train1)
str(Train1)



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
missmap(Train1[,-1], main = "Missing value vs observered")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# a<-Train1[,c(-1,-23,-24,-58,-59,-60,-61,-82,-86
#              ,-87,-133,-134,-135,-136,-157,-163,-164,-181,-182,-191,-194,-202,-204,-216,
#              -220,-226,-230,-243,-244,-285,-289,-297,-301,-309,-331)];dim(a);View(a)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sapply(a, function(x) length(unique(x)))
# is.data.frame(a)
# ncol(a)
View(rem_col)
dim(rem_col)


sample.ind <- sample(2, nrow(rem_col), replace = T, prob = c(0.75,0.25))
Traindata<-rem_col[sample.ind==1,];dim(Traindata)

View(Traindata)



Testdata<-rem_col[sample.ind==2,];dim(Testdata)
boxplot(rem_col)
View(Testdata)



datacor<-cor(Traindata);datacor
plot(datacor)
value1<-eigen(datacor)$values
length(value1[value1>1])

library(mnormt)
library(psych)

library(GPArotation)



fa_analysis <- fa(r = datacor, nfactors = 68, rotate = "oblimin",fm="pa")
fa_analysis

newdata<-data.frame(factor.scores(rem_col,fa_analysis)$scores)
View(newdata)
class(rem_col)
dim(newdata)
class(newdata)
newdata$TARGET<-rem_col[,c('TARGET')]
View(newdata)



# Here Logostics Regression is failed because Null deviance(25325) is less than Residual deviance(237960)
# we cannot accept the dataset.
model<-glm(TARGET~.,data = newdata,family = binomial)
summary(model)

# lets try with SVM

# Firstly we have to call the packages of the SVM

library(caret)

library(e1071)

model_svm<-svm(TARGET~.,newdata)
summary(model_svm)

test_pred<-predict(model_svm,newdata1)
factor(test_pred)
str(test_pred)

confusionMatrix((factor(test_pred,levels=min(newdata$TARGET):max(newdata$TARGET)))
                ,(factor(newdata$TARGET, levels=min(newdata$TARGET):max(newdata$TARGET))))
##########################################################################

# sapply(a, as.numeric(a))
# one_value_data<-subset(a,TARGET==1)
# View(one_value_data)
# dim(a)
# is.data.frame(one_value_data)
# # 
# # b<-sapply(one_value_data,as.numeric)
# # View(b)
# for (i in 1:ncol(one_value_data)) {
#   if (class(one_value_data[,i])=="integer")
#   {
#     class(one_value_data[,i])<-as.numeric(one_value_data[,i])
#   }
#   
# }
# 
# str(one_value_data)
# zero_value_data<-subset(a,TARGET==0)
# View(zero_value_data)
# c<-sapply(zero_value_data, as.numeric)
# View(c)
# 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setwd("C:\\Users\\vkuma140\\Music\\Part-3\\Project 1 Santander Customer Satisfaction\\test.csv")
dir()
Test1<-read.csv('test1.csv')
View(Test1)
str(Test1)
dim(Test1)
sapply(Test1[,-1], function(x) sum(is.na(x)))
sapply(Test1, function(x) length(unique(x)))

rem_Test_col=subset(Test1,select = -c(ID,
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
                                  saldo_medio_var13_medio_hace3,
                                  num_trasp_var17_out_ult1,
                                  saldo_medio_var29_hace3,
                                  num_reemb_var33_ult1,
                                  num_reemb_var17_hace3,
                                  imp_trasp_var17_out_ult1,
                                  imp_reemb_var33_ult1,
                                  imp_reemb_var17_hace3,
                                  delta_imp_reemb_var33_1y3,
                                  delta_num_trasp_var17_out_1y3,
                                  delta_imp_trasp_var17_out_1y3,
                                  delta_num_reemb_var33_1y3
                                  
                                  
                                  
                                  
                                  
))




# test<-Test1[,c(-1,-23,-24,-58,-59,-60,-61,-82,-86
#           ,-87,-133,-134,-135,-136,-157,-163,-164,-181,-182,-191,-194,-202,-204,-216,
#           -220,-226,-230,-243,-244,-285,-289,-297,-301,-309,-331)];dim(test);View(test)


sapply(test, as.numeric)
str(test)

# sd_var=c(rep(0,nrow(rem_Test_col)))
# for(i in 1:nrow(rem_Test_col))
# {
#   sd_var[i]=sd(rem_Test_col[,i])
# }
# 
# sd_var==0
# 
# summar_df<-sd(rem_Test_col$var3)
# summar_df


dim(rem_Test_col)

sapply(rem_Test_col,function(x)length(unique(x)))
is.data.frame(rem_Test_col)




install.packages("mnormt")
install.packages("psych")
install.packages("GPArotation")
library(mnormt)
library(psych)
library(GPArotation)



datacor1<-cor(rem_Test_col);datacor1
plot(datacor1)
value2<-eigen(datacor1)$values
length(value2[value2>1])

fa_analysis1 <- fa(r = datacor1, nfactors = 68, rotate = "oblimin",fm="pa")
fa_analysis1

newdata1<-factor.scores(rem_Test_col,fa_analysis1)$scores
View(newdata1)
dim(newdata1)







# sapply(Testdata, function(x) length(unique(x)))
# 
# rem_uniq_testdata<-subset(Testdata,select = -c(ind_var6,
#                                       ind_var18_0,
#                                       ind_var18,
#                                       ind_var34_0,
#                                       ind_var34,
#                                       ind_var29,
#                                       saldo_var6,
#                                       saldo_var18,
#                                       saldo_var29,
#                                       saldo_var34,
#                                       delta_imp_amort_var18_1y3,
#                                       delta_imp_amort_var34_1y3,
#                                       delta_imp_reemb_var33_1y3,
#                                       delta_imp_trasp_var17_out_1y3,
#                                       delta_imp_trasp_var33_out_1y3,
#                                       delta_num_trasp_var17_out_1y3,
#                                       delta_num_reemb_var33_1y3,
#                                       delta_num_trasp_var33_out_1y3,
#                                       imp_amort_var18_ult1,
#                                       imp_amort_var34_ult1,
#                                       imp_reemb_var17_hace3,
#                                       imp_reemb_var33_ult1,
#                                       imp_trasp_var17_in_ult1,
#                                       imp_trasp_var17_out_ult1,
#                                       imp_trasp_var33_in_ult1,
#                                       imp_trasp_var33_out_ult1,
#                                       num_meses_var29_ult3,
#                                       num_reemb_var17_hace3,
#                                       num_reemb_var33_ult1,
#                                       num_trasp_var17_in_ult1,
#                                       num_trasp_var17_out_ult1,
#                                       num_trasp_var33_in_ult1,
#                                       num_trasp_var33_out_ult1,
#                                       saldo_medio_var29_hace2,
#                                       saldo_medio_var29_hace3,
#                                       saldo_medio_var29_ult1,
#                                       saldo_medio_var29_ult3,
#                                       num_var6,
#                                       num_var18_0,
#                                       num_var18,
#                                       num_var34_0,
#                                       num_var34,
#                                       num_var29))
# dim(rem_uniq_testdata)
# View(rem_uniq_testdata)


##########################################################################

find_cor_test<-cor(rem_uniq_testdata)
eig_test_data<-eigen(find_cor_test)$values
length(eig_test_data[eig_test_data>1])

#factor_analy1<-fa(r=find_cor_test,nfactors = 68,rotate = "oblimin",fm="pa")

test_res<-data.frame(factor.scores(rem_uniq_testdata, factor_analy1)$scores)
class(test_res)
dim(test_res)
View(test_res)







