setwd("E:/")
ad=read.csv("test_lAUu6dG (1).csv",na.strings = c(" ","NA"),stringsAsFactors = FALSE)
head(ad,4)
View(ad)
bc=read.csv("train_ctrUa4K (1).csv",na.strings = c(" ","NA"),stringsAsFactors = FALSE)
head(bc,4)
View(bc)
colSums(is.na(ad))
summary(ad)
table(ad$Dependents)
ad$Gender=ifelse(ad$Gender=="Male" ,1,0)
as.numeric(ad$Gender)        
is.na(ad$Gender)
ad$Married=ifelse(ad$Married=="Yes",1,0)       
as.numeric(ad$Married)
 
table(ad$Education)
ad$Education=ifelse(ad$Education=="Graduate",1,0)

as.numeric(ad$Education)
table(ad$CoapplicantIncome)
table(ad$Credit_History)
 
is.na(ad$CoapplicantIncome)
table(ad$Self_Employed)

ad$Self_Employed=ifelse(ad$Self_Employed=="Yes",0,1) 
as.numeric(ad$Self_Employed)
str(ad)
is.na(ad$Credit_History)
table(ad$Property_Area)

str(bc)
bc$Loan_ID=as.numeric(bc$LoanAmount)
is.na(ad$LoanAmount)

table(ad$Loan_Amount_Term)
ad$LoanAmount[is.na(ad$Loan_Amount_Term)] = median(ad$Loan_Amount_Term, na.rm = T)

table(ad$Dependents)
table(ad$Property_Area)

ad=ad%>%
  mutate(Rural=as.numeric(Property_Area=="Rural"),
         Urban=as.numeric(Property_Area=="Urban"))

library(dplyr)
library(ggplot2)
dim(ad)
str(ad)
ad=ad[,-4]
ad=ad[,-11]
ad$Dependents=as.numeric(ad$Dependents)

table(ad$Dependents)

ad=ad%>%
  mutate(zero=ifelse(Dependents=="0",1,0),
         one=ifelse(Dependents=="1",1,0),
         two=ifelse(Dependents=="2",1,0),
         three=ifelse(Dependents=="3",1,0))
str(ad) 
ad=ad[,-11]
is.na(ad$Credit_History)
summary(ad)

ad$Credit_History[is.na(ad$Credit_History)] = median(ad$Credit_History, na.rm = T)
ad$LoanAmount[is.na(ad$LoanAmount)] = median(ad$LoanAmount, na.rm = T)
ad$Loan_Amount_Term[is.na(ad$Loan_Amount_Term)] = median(ad$Loan_Amount_Term, na.rm = T)
str(ad)
summary(bc)
#---------------train cleaning------------------
str(bc)
table(bc$Married)
is.na(bc$Gender)
bc$Married=ifelse(bc$Married=="Yes",1,0)
str(bc)
bc$Gender=ifelse(bc$Gender=="Male",1,0)
bc=bc%>%
  mutate(zero=ifelse(Dependents=="0",1,0),
         one=ifelse(Dependents=="1",1,0),
         two=ifelse(Dependents=="2",1,0))

table(bc$Dependents)


bc=bc[,-16]
str(bc)
summary(bc)
str(ad)
bc$Education=ifelse(bc$Education=="Graduate",1,0)
as.numeric(bc$Education)

bc$Self_Employed=ifelse(bc$Self_Employed=="Yes",0,1) 
as.numeric(bc$Self_Employed)

bc=bc%>%
  mutate(Rural=as.numeric(Property_Area=="Rural"),
         Urban=as.numeric(Property_Area=="Urban"))
str(bc)
table(bc$Loan_Status)
bc$Loan_Status=ifelse(bc$Loan_Status=="Y",1,0)
as.numeric(bc$Loan_Status)
summary(bc)
bc$Credit_History[is.na(bc$Credit_History)] = median(bc$Credit_History, na.rm = T)
bc$LoanAmount[is.na(bc$LoanAmount)] = median(bc$LoanAmount, na.rm = T)
bc$Loan_Amount_Term[is.na(bc$Loan_Amount_Term)] = median(bc$Loan_Amount_Term, na.rm = T)
str(bc)
bc=bc[,-11]
summary(bc)
#-------------------------data modeling----------------------------

fit=lm(Loan_Status~ .-Loan_ID,data = bc)
dc=step(fit)
summary(dc)

formula(dc)
fit=glm(Loan_Status ~ Married + CoapplicantIncome + Credit_History + 
            one + Rural + Urban,data = bc)

summary
table(bc$Loan_Status)
View(bc)
bc$predict=predict(fit,newdata=bc,type="response")
View(bc)

#-----------------rocr threshold------
library(ROCR)
ROCRpred = prediction(bc$predict,bc$Loan_Status)

ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


View(cutoff_data)
cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=seq(0,1,length=100)
View(cutoffs)

for(cutoff in cutoffs){
  predicted=as.numeric(bc$predict>cutoff)
  TP=sum(predicted==1 & bc$Loan_Status==1)
  FP=sum(predicted==1 & bc$Loan_Status==0)
  FN=sum(predicted==0 & bc$Loan_Status==1)
  TN=sum(predicted==0 & bc$Loan_Status==0)
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN))
}

cutoff_data=cutoff_data[-1,]            
View(cutoff_data)

cutoff_data$P=cutoff_data$FN+cutoff_data$TP
cutoff_data$N=cutoff_data$TN+cutoff_data$FP



cutoff_data=cutoff_data%>%
  mutate(Sn=TP/P,Sp=TN/N)%>%
  mutate(KS=abs(TP/P)-(FP/N)) %>%
  mutate(Accuracy =(TP + TN)/(P+N)) %>%
  #mutate(Lift=(TP/P)/(P+N)) %>%
  mutate(m=(8*FN+2+FP)/(P+N))
View(cutoff_data)


cutoff_data=cutoff_data%>%mutate(KS=as.numeric(KS))%>%na.omit()

KS_cutoff=cutoff_data$cutoff[which(cutoff_data$KS==max(cutoff_data$KS))][1]
KS_cutoff

max(KS_cutoff)


bc$score=as.numeric(bc$predict>KS_cutoff)

table(bc$Loan_Status,bc$score)

acc_train=(88+413)/(88+104+9+413)
acc_train

acc_train=(122 + 190)/(122+22+190+152)
acc_train
summary(fit)
#---------------train----------
str(ad)
bc=bc%>%
  mutate(bc$Married=ifelse(ad$Married=="Yes",1,0),)


ad$predict=predict(fit,newdata = ad,type = "response")
View(ad)


ad$loanstatus=ifelse(0.6363636<ad$predict,"Y","N")
View(ad)
str(ad)

YO=bc%>%
  cbind(bcLo,loanstatus)


View(ad)

library(dplyr)



submission=data.frame(ad$Loan_ID,ad$loanstatus)
View(submission)
write.csv(submission,file="submissions.csv",row.names = F)
getwd()


