#-------Main Aim of the file is to subset data till 428, Spit them to Train adn Test---------------#

#September 12th 
cases3 %>%
  group_by(Patient_ID,DX1,DX2,DX3,DX4,DX5,DX6,DX7,DX8,DX9)
mutate(DX1=428)
View(nn)
nn4<-case  
nn$dx
#DX1==428



nn4<-setDT(vf4)[,if(any(DX1=="428")) .SD[0:which.max(DX1=="428")]else .SD, by=Patient_ID]

nn4<-setDT(vf4)[,if(any(DX1=="428")) .SD[1:which.max(DX1=="428")+1]else .SD, by=Patient_ID]

nn4<-setDT()[,if(any(DX1=="428")) .SD[1:which.max(DX1=="428")+1]else .SD, by=Patient_ID]

nn41<-setDT(vf4)[,if(any(DX1=="428")) .SD[which.max(DX1=="428"):]else .SD, by=Patient_ID]

nn4<-NULL
vf4<-nn5
vf4$DX1 <- substr(vf4$DX1, 0, 3)
vf4$DX2 <- substr(vf4$DX2, 0, 3)
vf4$DX3 <- substr(vf4$DX3, 0, 3)
vf4$DX4 <- substr(vf4$DX4, 0, 3)
vf4$DX5 <- substr(vf4$DX5, 0, 3)
vf4$DX6 <- substr(vf4$DX6, 0, 3)
vf4$DX7 <- substr(vf4$DX7, 0, 3)
vf4$DX8 <- substr(vf4$DX8, 0, 3)
vf4$DX9 <- substr(vf4$DX9, 0, 3)
length(unique(unlist(vf4[12:19])))
k<-vf4$Patient_ID[vf4$DX1=="428"]
k<-as.character(k)
k
View(k)
require(data.table)
setDT(ll)[,.SD[cumsum(indc=='D')<=1], by=id]
k
#---Convert List to info##
vf4 <-vf4[vf4$Patient_ID %in%  k,]
View(vf4)
View


nn5<-nn4
nn5$DX1<-as.character(nn5$DX1)
nn5$DX2<-as.character(nn5$DX2)
nn5$DX3<-as.character(nn5$DX3)
nn5$DX4<-as.character(nn5$DX4)
nn5$DX5<-as.character(nn5$DX5)
nn5$DX6<-as.character(nn5$DX6)
nn5$DX7<-as.character(nn5$DX7)
nn5$DX8<-as.character(nn5$DX8)
nn5$DX9<-as.character(nn5$DX9)

nn6<-m[,c(2,8,14:22)]
View(nn6)
library(data.table)
setDT(nn6)
f<-melt(nn6, id = 1, measure=patterns("$"), 
        variable.name = "DX")

library(data.table)
l?dcasibrary(reshape2)
nn51$variable<-NULL
nn5<-NULL
View(nn51)
nn51$variable<-NULL
nn5<-NULL
View(nn51)
nn51<-as.data.table(nn51)

nn52<-dcast(f, Patient_ID~value)

View(nn52[1:10])

nn51[is.na(nn51)]<-0
nn51<-na.omit(nn51)

l<-names(nn52[,3:1008])

nn52$`0`<-NULL

l

nn53<-nn52
nn52
for (i in l) 
{
  if (sum(nn52$i)<30)
    nn52[i]<-NULL
}

f.subset<-nn4


k<-f.subset %>%
    group_by(Patient_ID,ADMDAT) %>%
    arrange(Patient_ID,desc(ADMDAT))

View(k)


#----------------for the predictive outcome after the heart failure subsetting-----------------#

library(dplyr)

l<-l[, test1 := c(0,sum(as.numeric(test))), Patient_ID]

nn5<-setDT(nn4)[order(Patient_ID),tail(.SD,2L),by=Patient_ID]

k<- l[,test1 := test+shift(test)]

View(k)

aggregate(. ~ Category, dataframe, sum)

l2<-aggregate(. ~Patient_ID,l1,max)

#just the first row

est2<-est2[!duplicated(est2$Patient_ID),]
View(est2)
require(data.table)
setDT(k)
m<-k[ , diff := ADMDAT[0] - shift(ADMDAT), by = Patient_ID] 

V#Remove the status 20 patients
k<-est2$Patient_ID[est2$STATUS==20]

#remove stuff from row

k1<-est2[!est2$Patient_ID %in% k,]

est3<-est3[!est3$Patient_ID %in% k,]
VIew(est3)
est3<-est2[,c(2,6,12,33,36,38,39)]

## 75% of the sample size
smp_size <- floor(0.75 * nrow(final1))
final1$X376[ddata1$X376==2]<-0

ddata1$X1_1<-NULL

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(final1)), size = smp_size)

train <- final1[train_ind, ]
test <- final1[-train_ind, ]

model1<-glm(V1~.,data = train,family = binomial(link = "logit"))


k<-predict(model1,test,type="response")

#Boston.rf=randomForest( ~ . , data = train)
library(ROCR)

pr <- prediction(k, test$V1)

prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(0,1)

library(caret)
# Use your model to make predictions, in this example newdata = training set, but replace with your test set    
pdata <- predict(model1, newdata = test, type = "response")

# use caret and compute a confusion matrix
confusionMatrix(data = as.numeric(pdata>0.5), reference = test$V1)

perf <- performance(pr,"tpr","fpr")

cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
head(cutoffs)

head(subset(cutoffs, fpr < 0.2))
(subset(cutoffs, tpr > 0.6 & fpr<0.4))



#Find difference between Rows
library(data.table)
#X2rows[, diff := ("ADMDAT" - shift("ADMDAT")), by = X2rows$Patient_ID,]
X2rows<-data.table(X2rows)
setkey(X2rows,Patient_ID, ADMDAT)
X2rows[, time.difference := ADMDAT - lag(ADMDAT, 1L), by = Patient_ID]

nn5<-nn4 %>% 
     group_by(Patient_ID) %>% 
     mutate(time.difference = ADMDAT - lag(ADMDAT))

write_rds(melteddx2dx9,"casteddx2dx9.rds")


nn4<- vf4 %>% 
    group_by(Patient_ID) %>% 
    mutate((which.min(vf4$DX1=="428"):last()))
library(dplyr)

nn4<-setDT(vf4)[,.SD[which.max(DX1=="428"):.N], by=Patient_ID]


nn5[, timediff := seq_len(.N), by = Patient_ID]



