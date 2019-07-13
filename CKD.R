
## Step 0  - Read in Data
#data=read.csv("C:\Users\Vinod\Desktop\Multivariate Analysis\CKD\Data/Final.csv")
data=read.csv("C:\Users\Vinod\Desktop\Multivariate Analysis\CKD\Data\NewAge.csv")


## Step 1 - Explore and relabel Data
class(data)
summary(data)
#out_sample=which(data$CKD=="#N/A")
out_sample=which(is.na(data$CKD)==1)


data_out=data[out_sample,]   ## the ones without a disease status
data_in=data[-out_sample,]   ## the ones with a disease status
summary(data_in)

#data_in=na.omit(data_in)

## Step 2  - Run the Logistic Regression with one variable
data_in$Age <- factor(data_in$Age)
data_in$Female <- factor(data_in$Female)
data_in$Unmarried <- factor(data_in$Unmarried)
data_in$Obese <- factor(data_in$Obese)
data_in$SBP <- factor(data_in$SBP)
data_in$HDL <- factor(data_in$HDL)
data_in$LDL <- factor(data_in$LDL)
data_in$White <- factor(data_in$White)
data_in$Hispa <- factor(data_in$Hispa)
data_in$Black <- factor(data_in$Black)
data_in$PVD <- factor(data_in$PVD)
data_in$Activity <- factor(data_in$Activity)
data_in$Hypertension <- factor(data_in$Hypertension)
data_in$Diabetes <- factor(data_in$Diabetes)
data_in$CVD <- factor(data_in$CVD)
data_in$Anemia <- factor(data_in$Anemia)


data_out$Age <- factor(data_out$Age)
data_out$Female <- factor(data_out$Female)
data_out$Unmarried <- factor(data_out$Unmarried)
data_out$Obese <- factor(data_out$Obese)
data_out$SBP <- factor(data_out$SBP)
data_out$HDL <- factor(data_out$HDL)
data_out$LDL <- factor(data_out$LDL)
data_out$White <- factor(data_out$White)
data_out$Hispa <- factor(data_out$Hispa)
data_out$Black <- factor(data_out$Black)
data_out$PVD <- factor(data_out$PVD)
data_out$Activity <- factor(data_out$Activity)
data_out$Hypertension <- factor(data_out$Hypertension)
data_out$Diabetes <- factor(data_out$Diabetes)
data_out$CVD <- factor(data_out$CVD)
data_out$Anemia <- factor(data_out$Anemia)

model=glm(CKD~.,family="binomial",data=data_in)
summary(model)

test <- predict(model, newdata = data_out, type = "response")
train <- predict(model, newdata = data_in, type = "response")
ckd.predict
min(ckd.predict)
max(ckd.predict)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(ckd.predict)
anova(model, test="Chisq")


classify=ifelse(train>.08,1,0)  # this is a threshold, we say if probability >50% , then say "yes"
PredictedProbabilities <- test

ckd.predict <- ifelse(test>.08,1,0) 
CKDDesignation <- ifelse(test>.08,"Y","N") 
OurFile <- cbind(PredictedProbabilities,CKDDesignation)


c_accuracy=function(actuals,classifications){
  df=data.frame(actuals,classifications);
  
  
  tp=nrow(df[df$classifications==1 & df$actuals==1,]);        
  fp=nrow(df[df$classifications==1 & df$actuals==0,]);
  fn=nrow(df[df$classifications==0 & df$actuals==1,]);
  tn=nrow(df[df$classifications==0 & df$actuals==0,]); 
  
  
  recall=tp/(tp+fn)
  precision=tp/(tp+fp)
  accuracy=(tp+tn)/(tp+fn+fp+tn)
  tpr=recall
  fpr=fp/(fp+tn)
  fmeasure=2*precision*recall/(precision+recall)
  scores=c(recall,precision,accuracy,tpr,fpr,fmeasure,tp,tn,fp,fn)
  names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure","tp","tn","fp","fn")
  
  #print(scores)
  return(round(scores,digits = 4));
}

c_accuracy(data_in$CKD,classify)  # to run this you must run my code below first.


acc=c_accuracy(data_in$CKD,classify)
c1=100   # penalize me  $100 for a false positive
c2=200  #  penalize me $200 for a false negatives
profit=(1300*acc[7])-acc[9]*c1

profit   ## my costs are $48,800  because I got hit with a ton of false negative costs 

par(mfrow=c(1,1))
library(pROC)
plot(roc(as.numeric(as.vector(data_in$CKD)), as.numeric(as.vector(classify))),
     col="blue")




data_out$CKD <- NULL
CKD <- ckd.predict
dataA <- cbind(data_out,CKD)



CKD <- data_in$CKD
data_in$CKD <- NULL
dataB <- cbind(data_in,CKD)

qlikdata <- rbind(dataB,dataA)

dataAge=read.csv("C:\Users\Vinod\Desktop\Multivariate Analysis\CKD\Data\LogisticRegressionData.csv")
agenumeric <- dataAge$Age

qlikdata <- cbind(qlikdata,agenumeric)
library(xlsx)
write.xlsx(qlikdata,"C:\Users\Vinod\Desktop\Multivariate Analysis\CKD\Data\qlikdata.xlsx")


write.xlsx(dataB,"C:\Users\Vinod\Desktop\Multivariate Analysis\CKD\Data\datab.xlsx")

write.xlsx(OurFile,"C:\Users\Vinod\Desktop\Multivariate Analysis\CKD\Data\OurFile.xlsx")


