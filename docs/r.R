?count.na
str(vf$CAUSE)
library(lubridate)
vf<-case
View(vf)
str(vf)

# Data - 07/02/2018-----
vf$ADMDAT <-ymd(vf$ADMDAT)
vf$DSCHDAT <-ymd(vf$DSCHDAT)
str(vf)
#------Removing Few Data----#
vf$patbdte<-NULL
vf$NEWDTD<-NULL
View(vf)
vf$TOTBIL<-NULL
vf$HOSP<-NULL
vf$Duration<-vf$DSCHDAT -vf$ADMDAT
vf$ADMDAT<-NULL
vf$DSCHDAT<-NULL
vf$DSCYR<-NULL
vf$YEAR<-NULL

vf2<-vf

str(vf)
vf$Duration<-as.numeric(vf$Duration)

View(vf$Duration)

hist(vf$Duration)

table(vf$duration)

#sum(is.na(vf$Patient_ID))    
#vf$age<-(vf$ADMISSION.DATE-vf$PATIENT.BIRTH.DATE)
#View(vf)
#str(vf$PATIENT.BIRTH.DATE)
#str(vf$ADMISSION.DATE)
#View(vf)
#library?(dplyr)
#str(vf$Patient_ID)
#str(vf)



vf2<-vf

#require(dplyr)

#vf2$diff <- vf2 %>%
  #group_by(vf2$Patient_ID) %>%
  #mutate(diff = diff(ADMDAT))

#vf2<-vf      

library(data.table)
vf4 <- as.data.table(vf)
setkey(vf4,Patient_ID)
#---------------------------Creating Diff vf4------------------------------------#
vf4[ , diff := c(NA, diff(ADMDAT)), by = Patient_ID]    
vf4$Patient_ID
hist(vf4$diff,xlim = c(0,1000),breaks="FD")
View(vf4)
require(dplyr)
vf4 %>%
  group_by(Patient_ID) %>%
  summarize(
    n_days=n(),
    n_30=sum(vf4$diff<30),
    n_60=sum(vf4$diff<60),
    n_90=sum(vf4$diff<90)
  )

View(vf4)



vf5<-vf4[which(diff<366)]
366),]
#Create Death
vf4$death <- ifelse(vf4$STATUS==20,1,0)
str(vf4)
View(vf4)

#July 5th 
#----------------------------Readmission-----------------------------------#
View(vf4)
vf4$X<-NULL
vf4$Patient_ID<-as.numeric(vf4$Patient_ID)
View(vf4)
vf4$CAUSE<-NULL
vf4$RACE<-as.numeric(vf4$race)
#--------------------------Create a Frequency table----------------------------#
vf4$HISPAN<-as.numeric(vf4$HISPAN)
vf4<-data.table(vf4)
vf4[,'freq-loc':=.N,by = Patient_ID]
View(vf4)


vf4$death
str(vf4$death)
sum(is.na(vf4$diff))

vf4$diff[is.na(vf4$diff)]<- 0

vf4$outcome<- ifelse(vf4$death==1,0,ifelse(vf4$`freq-loc`==0,2,1))

View(vf4)

vf5<-vf4

vf5$death<-NULL

vf5$death<-NULL
vf5$`freq-loc`<-NULL
write.csv(vf5,'july5.csv')
vf5$rec<-NULL
vf5$ADMDAT<-NULL
vf5$DSCHDAT<-NULL
View(vf5)

#-----------------------July 9th-------------------#

vf6<-vf5[,-c(9:14,16:22)]
View(vf6)
vf6$check<-NULL

ninja <- keygen()
pub <- pubkey(ninja)

msg <- serialize(vf6, NULL)
ciphertext <- simple_encrypt(msg, pub)
View(ciphertext)

#-------------------------July 10th--------------------#
anonymize <- function(x, algo="sha1"){
  unq_hashes <- vapply(unique(x), function(object) digest(object, algo=algo), FUN.VALUE="", USE.NAMES=TRUE)
  unname(unq_hashes[x])
}

cols_to_mask <- c("Patient_ID")
#cols_to_mask <- c("Patient_ID","DX1","DX2","DX3")

vf7[,cols_to_mask := lapply(.SD, anonymize),.SDcols=cols_to_mask,with=FALSE]

hashed_id <- function(x, salt) {
  y <- paste(x, salt)
  y <- sapply(y, function(X) digest(X, algo="sha1"))
  as.character(y)
}

sapply(vf7$Patient_ID,digest,algo="
       SHA1")


mydata$id <- hashed_id(mydata$raw_id, "somesalt1234")
gim<-vf3

set.seed(82)

f1<-gim[sample(nrow(gim),600,replace = FALSE ),]
f2<-randomForest(f1$outcome~.,data = F1)

vf4$diff[is.na(vf4$diff)]<- 0.1

table(cases)
#-----------------------July 11th-------------------------#
#vf7<-july9
#str(vf7$Patient_ID)
#View(vf7)
#x<-as.character(vf7$Patient_ID)
#x<-charToRaw(vf7$Patient_ID)

#encrypt <- function(x){
  # function assumes you already created a RSA key called key
  #x <- charToRaw(k)
  #e <- PKI.encrypt(x, key,"aes256")
  #return(e)
#}

#decrypt <- function(x) {
  # function assumes you already have a key called key
  #e <- PKI.decrypt(x, key,"aes256")
  #x <- rawToChar(e)
  #return(x)
#}


#vf7$bits <- map(vf7$Patient_ID, encrypt)

#library(PKI)

#k
    
#x<-as.character(x)
#x
#vf7$bits <- as.raw(vf7$Patient_ID)
#x<-as.integer(x)
#x
#key <- PKI.digest(charToRaw("hello"), "SHA256")
#ae <- PKI.encrypt(x, key, "aes256")
#ad <- PKI.decrypt(ae, key, "aes256")
#ae
#ad
#stopifnot(identical(x, ad))

#key <- as.raw(1:16)
#aes <- AES(key)
#e <- PKI.encrypt(x, key)
#library(PKI)
#aes$encrypt(x)
#f<-aes$decrypt(aes$encrypt(x), raw=TRUE)

#library(digest)
#-------------------------------------July 12th---------------------------#
 vf8<-vf7
 vf8$bits<-NULL
 write.csv(vf8,"vf8.csv")
 orig<-(,c(vf8$Patient_ID,vf8$SEX,vf8$DX1,vf8$DX2,vf8$DX3,vf8$PROC1,vf8$PRIME))

 orig<-vf8[c(1,2,5:8,10,11)]
 View(orig)
 write.csv(orig,"orig.csv")
 View(vf8)



c1 <- cut(vf8$AGE, breaks = c(0,20,40,60,80,100,120))
orig$agebin<-c1
orig$bin_categor<-as.numeric(orig$agebin)
orig$bin_categor<-as.numeric(orig$agebin)
View(orig)

vf8$DX1<-as.factor(vf8$DX1)
vf8$DX1
vf8$DX1<-as.numeric(vf8$DX1)
vf8$DX1
orig$d1<-vf8$DX1
vf8$DX2<-as.factor(vf8$DX2)
vf8$DX3<-as.factor(vf8$DX3)
vf8$DX2<-as.numeric(vf8$DX2)
vf8$DX3<-as.numeric(vf8$DX3)
#adding diagnosis to original file
orig$x1<-vf8$DX1
orig$x2<-vf8$DX2
orig$x3<-vf8$DX3

vf9$X5<-vf4$DX1
vf9$X6<-vf4$DX2
vf9$X7<-vf4$DX3
View(vf9)

vf8$bin<-orig$bin_categor
View(vf8)
vf8$PRIME<-as.factor(vf8$PRIME)
vf8$PRIME<-as.numeric(vf8$PRIME)
vf8$AGE<-NULL

orig$primecode<-vf8$PRIME
View(orig)
vf9<-vf8

names(vf9)<-c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16")
names(vf9)

#------------------------------------------------July 31st---------------------------------#
lm<-as.numeric(levels(orig$DX1))[orig$DX1]
Warning message:
  NAs introduced by coercion 
> sum(is.na(lm))
[1] 92955
#------------------------------------------------August 7th--------------------------------#
l<-as.character(vf4$DX1)
m<-as.character(vf4$DX2)
n<-as.character(vf4$DX3)
f<-as.data.frame(l,m,n)
k<-as.factor(f$`c(l, m, n)`)
k<-as.numeric(k)
r1<-k[1:6910365]
r2<-k[6910366:13820730]
r3<-k[13820731:20731095]
vf10$dx1<-r1
vf10$dx2<-r2
vf10$dx3<-r3
write.csv(vf10,"orig1.csv")
orig$y1<-vf10$dx1
orig$y2<-vf10$dx2
orig$y3<-vf10$dx3
vf11<-vf9
vf11$X5<-vf10$dx1
vf11$X6<-vf10$dx2
vf11$X7<-vf10$dx3
vf11$X8<-as.numeric(vf11$X8)
vf11$X8
View(orig)
str(vf11)
#--------------Replacing Naa-------------------------#
f1 %>%
  replace(is.na(.),999.99)
View(f1)


#---------------18.04.2017---------------------------#
vf3$DIAGNOSIS...PRINCIPAL.MAIN<-substr(vf3$DIAGNOSIS...PRINCIPAL.MAIN,0,3)
str(vf3)
vf4$death<-as.factor(vf4$death)
vf4<-vf3[,-c(10,11,36,39:46)]
View(vf4)
vf4<-vf4[,-c(35)]
str(vf4)

vf4$PROCEDURE...PRNCPL.MAIN.CODE<-as.factor(vf4$PROCEDURE...PRNCPL.MAIN.CODE)

vf4$PROCEDURE...2ND.MAIN.CODE<-as.factor(vf4$PROCEDURE...2ND.MAIN.CODE)
vf4$PROCEDURE...3RD.MAIN.CODE<-as.factor(vf4$PROCEDURE...3RD.MAIN.CODE)
vf4$PROCEDURE...4TH.MAIN.CODE<-as.factor(vf4$PROCEDURE...4TH.MAIN.CODE)
vf4$PROCEDURE...5TH.MAIN.CODE<-as.factor(vf4$PROCEDURE...5TH.MAIN.CODE)
vf4$PROCEDURE...6TH.MAIN.CODE<-as.factor(vf4$PROCEDURE...6TH.MAIN.CODE)
vf4$PROCEDURE...7TH.MAIN.CODE<-as.factor(vf4$PROCEDURE...7TH.MAIN.CODE)
vf4$PROCEDURE...8TH.MAIN.CODE<-as.factor(vf4$PROCEDURE...8TH.MAIN.CODE)
View(vf4)
vf4$DIV<-as.factor(vf4$DIV)
str(vf4)
vf4$Patient_ID<-as.character(vf4$Patient_ID)

vf4$DeathRNUM<-as.factor(vf4$DeathRNUM)
str(vf4$DeathRNUM)
table((vf4$))
vf4$DeathRNUM<-as.factor(vf4$DeathRNUM)
#-----------------22/04/2018------------------------#
vf4$outcome<-as.factor(vf4$outcome)
#substring------
vf4$DeathRNUM<- substr(vf4$DeathRNUM,0,4)
View(vf4)
vf4$DIV<-as.factor(vf4$DIV)
vf4$STATUS<-as.factor(vf4$STATUS)
str(vf4)
#--------------------using Dummy Variables---------------#
dummy_hf<-vf4
require(caret)
dumy<- dummyVars("~.", data = dummy_hf)
dumy
hf_dummies<- data.frame(predict(dumy,newdata=dummy_hf))

#----------------------------Cleaning dates-------------------#
hf_hash <- vf4[,-c(1,4,6,7,8,9,14)]
str(hf_hash)
View(hf_hash)
hf_hash$LOCATION<-as.factor(hf_hash$LOCATION)
str(hf_hash)
#--------------------Using Feature hashing-----------
predictorNames <- setdiff(names(hf_sample),hf_sample$outcome)

# change all NAs to #

hf_hash[is.na(hf_hash)] <- 999
str(hf_hash)

#---------------------------Doutb------------------~
set.seed(1234)
?sample
sample(floor)
#------------------------5000-------------------#
str(hf_hash)
x <- hf_sample$outcome
y <- 3
setdiff(x,y)
predictorNames <- setdiff(names(hf_sample),names(hf_sample$outcome))
predictorNames <- predictorNames[1:34]
objTrain[,predictorNames]
hf_sample<- hf_hash[sample(nrow(hf_hash),5000),]
split <- sample(nrow(hf_sample), floor(0.7*nrow(hf_sample)))
objTrain <-hf_hash[split,]
objTest <- hf_hash[-split,]

library(FeatureHashing)
#---------------------------Train---------------------------#
objTrain_hashed = hashed.model.matrix(~., data=objTrain[,predictorNames], hash.size=2^12,create.mapping = TRUE, transpose=FALSE)
objTrain_hashed = as(objTrain_hashed, "dgCMatrix")
#-----------------------Hashing Mapping-------------------#
mapping1<-hash.mapping(objTrain_hashed)
mean(duplicated(mapping1))
#--------------------------Test-------------------------------------------#
objTest_hashed = hashed.model.matrix(~., data=objTest[,predictorNames], hash.size=2^20, transpose=FALSE)
objTest_hashed = as(objTest_hashed, "dgCMatrix")

#-------------------------------Another way--------#
size<- hash.size(objTrain)
mat2<-  hashed.model.matrix(~., data=objTrain[,predictorNames], hash.size=size,create.mapping = TRUE, transpose=FALSE)
mapping2<-hash.mapping(mat2)
mean(duplicated(mapping2))

outcomeName <- 'outcome'
library(glmnet)
glmnetModel <- cv.glmnet(objTrain_hashed, objTrain[,outcomeName], 
                         family = "multinomial", type.measure = "auc")
glmnetPredict <- predict(glmnetModel, objTest_hashed, s="lambda.min")

glmnetPredict
confusionMatrix(objTest[,outcomeName],glmnetPredict)

deviance.glmnet(objTest[,outcomeName],glmnetPredict)

View(objTrain)
objTrain[,outcome]
#------------------------------unnceccasary check----------------------#
View(hf_sample)
hf_sample$LOCATION<- as.numeric(hf_sample$LOCATION)
hf_sample$LOCATION[is.na(hf_sample$LOCATION)]<-0.1
hf_sample[,c(2,3,10:25,28:30)]<- sapply(hf_sample[, c(2,3,10:25,28:30)],as.factor)
str(hf_sample)
hf_sample[is.na(hf_sample)] <- 0.1
sum(is.na(hf_sample))
str(hf_sample)
str(hf_sample)
hf_sample[,c()]

#--------------------------XGBoost-------------------------------------#
View(vf4)
set.seed(100)
finkk<- vf4[sample(nrow(vf4),4511),]
sum(finkk$outcome==1)
sum(finkk$outcome==0)
sum(finkk$outcome==2)

mkk<-finkk
set.seed(100)
train_ind<-sample(seq_len(nrow(mkk)),size=floor(0.75*nrow(mkk)))
train<-mkk[train_ind,]
test<-mkk[-train_ind,]

output<-recode(finkk$outcome,"death" <- 2,"readmitted"<-1,"Not Admitted" <-0)

finkk$outcome
recode()
View(finkk)

library(dplyr)


sparse.matrix.train= sparse.model.matrix(outcome~.-1, data = train)
sparse.matrix.test=  sparse.model.matrix(outcome~.-1, data = test)
output_vector = 
  
  xgb <- xgb.cv(data = sparse.matrix.train, 
                label = output_vector, 
                eta = 0.1,
                max_depth = 15, 
                nround=25, 
                subsample = 0.5,
                colsample_bytree = 0.5,
                seed = 1,
                eval_metric = "merror",
                objective = "multi:softprob",
                num_class = 12,
                nthread = 3
  )
xgb$
  xgb.model.one=    xgb.cv(data= sparse.matrix.train,     #train sparse matrix 
                           label= output_vector,    #output vector to be predicted 
                           eval.metric= 'logloss',     #model minimizes Root Mean Squared Error
                           
                           nfold= 10,
                           #tuning parameters
                           max.depth= 8,           
                           eta= 0.1,  #Learnin raet              
                           nthread = 5,             
                           subsample= 1,            
                           colsample_bytree= 0.5,   
                           lambda= 0.5,             
                           alpha= 0.5,             
                           min_child_weight= 3,     
                           nround= 100)   

mkk[is.na(mkk)]<-9999999
View(mkk)  

colMeans(is.na(mkk))

library(Matrix)
library(xgboost)
library(caret)
library(car)







colmeans(is.na(train1))





train1$outcome1<-as.factor(train1$outcome1)


#333
library(data.table)
str(train2)
str(mkk)
train2= data.table(mkk) #convert train set to data table format
test1= data.table(test) #convert test set to data table format
mkk<-mkk[,-c(mkk$DISCHARGE.DATE1)]
sparse.matrix.train= sparse.model.matrix(outcome~.-1, data = train) #converts train set factors to columns
sparse.matrix.test=  sparse.model.matrix(outcome~.-1, data = test1) #converts test set factors to columns
output_vector = as.factor(train2$outcome) #output vector to be predicted
train1$outcome1[train1$outcome==2]<-0
train1$outcome1[is.na(train1$outcome1)]=0
table(train1$outcome1)
t1<-(xgb.model.one)
t1<-as.data.frame(t1$evaluation_log)
mean(is.na(train1$outcome1))
label=train1$outcome1


xgb.model.one=    xgb.cv(data= sparse.matrix.train,     
                         eval.metric= 'logloss',
                         nfold= 10,
                         
                         max.depth= 8,           
                         eta= 0.1,  #Learnin raet              
                         nthread = 5,             
                         subsample= 1,            
                         colsample_bytree= 0.5,   
                         lambda= 0.5,             
                         alpha= 0.5,             
                         min_child_weight= 3,     
                         nround= 100)            

t2<-as.data.frame(xgb.model.two$evaluation_log)
str(mkk)
mkk$outcome<-as.factor(mkk$outcome)

xgb.model.two=    xgboost(data= sparse.matrix.train,     
                          label = output_vector,    
                          eval.metric= 'logloss',     
                          nfold= 10,
                          na.action="na.pass",
                          max.depth= 3,           
                          eta= 0.05,                
                          nthread = 5,             
                          subsample= 1,            
                          colsample_bytree= 0.5,   
                          lambda= 0.5,             
                          alpha= 0.5,              
                          min_child_weight= 3,     
                          nround= 100 )  
xgb.model.two$evaluation_log
xgb.model.two$params
table(label)
str(label)
str(mkk)
mkk$outcome<-as.factor(mkk$outcome)
t3<-xgb.model.best$evaluation_log
xgb.model.best=   xgboost(data= sparse.matrix.train,     #train sparse matrix 
                          label = labels1,          #output vector to be predicted 
                          eval.metric= 'logloss',        #model minimizes Root Mean Squared Error
                          #regression
                          #tuning parameters
                          max.depth= 8,            
                          eta= 0.1,                
                          nthread = 5,             
                          colsample_bytree= 0.5,   
                          lambda= 0.5,             
                          alpha= 0.5,              
                          min_child_weight= 3,     
                          nround= 30)              

?xgboost
plot(data.frame(t1$train_logloss_mean),t1$iter,col='black',ylab='CV logloss Error',xlab='# of trees')
lines(data.frame(xgb.model.one)[,3],type='l',col='red')
lines(data.frame(xgb.model.two)[,3],type='l',col='blue')
lines(t3$train_logloss,type='l',col='blue')
?plot

importance = xgb.importance(feature_names = sparse.matrix.train@Dimnames[[2]], 
                            model = xgb.model.two)  #Grab all important features
xgb.plot.importance(importance[1:3])  #Plot for top 1


str(Random_Forest)
Random_Forest <- data.matrix('0' = c(628,34), '1' = c(65,597))
XG_Boost<-data.frame('0' = c(631,31), '1' = c(61,601))
(accuracy <- sum(diag(Random_Forest)) / sum(Random_Forest))

labels1<-output_vector[1:3372]


colMeans(is.na(finkk))



colMeans(is.na(train2))

train2<-train1[ , apply(train1, 2, function(x) !any(is.na(x)))]
newdf <- t(na.omit(t(train1)))
train2<-as.data.frame(newdf)
str(train2)


mkk<-finkk[,-which(colMeans(is.na(finkk))>0.5)]
mkk[is.na(mkk)] = 0.1

colMeans(is.na(mkk))

mkk$CAUSE[is.na(mkk$CAUSE)]<- "jk"

mkk<-finkk[,-which(rowMeans(is.na(finkk))>0.9)]

sum(rowMeans(is.na(finkk))>0.55)


colMeans(is.na(mkk))


mkk$CAUSE<-as.numeric(mkk$CAUSE)
mkk$NEWDTD<-as.numeric(mkk$NEWDTD)
mkk$NEWDTD[is.na(mkk$NEWDTD)]<-0.1
mkk$DeathRNUM<-as.numeric(mkk$DeathRNUM)
View(mkk)

colMeans(is.na(mkk))
View(mkk)
#######
library(Matrix)
?matrix::
  train_v2 <- Matrix(mkk, sparse = T)
test_v2 <- Matrix::Matrix(model_mat_v1[-train_idx,], sparse = T)
xgb_v2 <- xgboost(data = train_v2, label = train_out_vec, nrounds = 20, verbose = F, missing = NA)
preds_v2 <- predict(xgb_v2, newdata = test_v2, missing = NA)




mkk$SEX<-as.numeric(mkk$SEX)
> str(mkk)

mkk$TOTBIL <-NULL

mkk$RACE<-as.numeric(mkk$RACE)
mkk$SOURCE<-as.numeric(mkk$SOURCE)
mkk$DIAGNOSIS...2ND.MAIN.CODE<-as.numeric(mkk$DIAGNOSIS...2ND.MAIN.CODE)
mkk$DIAGNOSIS...3RD.MAIN.CODE<-as.numeric(mkk$DIAGNOSIS...3RD.MAIN.CODE)
mkk$ZIP <-as.numeric(mkk$ZIP)
mkk$PATIENT.BIRTH.DATE <-NULL

mkk$DIAGNOSIS...4TH.MAIN.CODE<-as.numeric(mkk$DIAGNOSIS...4TH.MAIN.CODE)
mkk$DIAGNOSIS...5TH.MAIN.CODE<-as.numeric(mkk$DIAGNOSIS...5TH.MAIN.CODE)
mkk$DIAGNOSIS...6TH.MAIN.CODE<-as.numeric(mkk$DIAGNOSIS...6TH.MAIN.CODE)

mkk$HISPAN<-as.numeric(mkk$HISPAN)

importance_matrix<-xgb.importance(feature_names = sparse.matrix.train@Dimnames[[2]],model = xgb.model.two)

xgb.plot.importance(importance_matrix[1:8])

xgb.ggplot.importance()

str(mkk)

mkk$CAUSE<-as.numeric(mkk$CAUSE)
mkk$SEX<-as.numeric(mkk$SEX)
mkk$DIAGNOSIS...7TH.MAIN.CODE<-as.numeric(mkk$DIAGNOSIS...7TH.MAIN.CODE)
mkk$DIAGNOSIS...8TH.MAIN.CODE<-as.numeric(mkk$DIAGNOSIS...8TH.MAIN.CODE)
str(mkk)

View(mkk)


mkk<-mkk[,-c(1,2,5,8,9,10,19:26,28:34,40:46)]
View(mkk)
mkk<-mkk[,-c(25)]
View(mkk)

str(mkk)
mkk$ZIP<-as.numeric(mkk$ZIP)
mkk$RECDID<-as.numeric(mkk$RECDID)
View(mkk)
mkk<-mkk[,-c(18:20)]
mkk$PRIME<-as.numeric(mkk$PRIME)
str(mkk)
mkk$Patient_ID<-as.numeric(mkk$Patient_ID)
str(mkk)


write.csv(mkk,file="mkk.csv")


str(vf4)
View(vf4)


#------------------------------Clean vf4---------####
vf4$TOTBIL<-NULL
View(vf4)
vf4<-vf4[,-c(1,2,5,8,9,10,19:26,28:34,40:46,53)]
View(vf4)
vf4<-vf4[,-c(18,19,20)]

vf4(is.na(vf4))<- 9999999
View(vf4)
write.csv(vf4,file = "fullfile.csv")

#June 3#
test$ADMDAT<-NULL
test$DSCHDAT<-NULL

str(test$RACE)
test$RACE<-as.numeric(test$RACE)
View(test)
View(vf4)
View(test)
test$TOTBIL<-NULL
View(test)
table(test$RACE)



#####
library(PKI)
library(purrr)
key <- PKI.genRSAkey(2048)

# create some helper functions
encrypt <- function(x){
  # function assumes you already created a RSA key called key
  x <- charToRaw(x)
  e <- PKI.encrypt(x, key)
  return(e)
}

decrypt <- function(x) {
  # function assumes you already have a key called key
  e <- PKI.decrypt(x, key)
  x <- rawToChar(e)
  return(x)
}

#  encrypt the first name
df$ip <- map(df$fname, encrypt)

str(df)
