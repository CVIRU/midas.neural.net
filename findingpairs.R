library(dplyr)
library(data.table)
gc()

vf4<-case[,c(1,7,8,11,13:21,38)]
View(vf4)
case<-NULL

library(dplyr)
nn5<-vf4 %>% 
  group_by(Patient_ID) %>% 
  mutate(time.difference = ADMDAT - lag(DSCHDAT))


nn5$time.difference[is.na(nn5$time.difference)]<-99999

nn5<-as.data.table(nn5)
nn5<-nn5[!nn5$time.difference<0]

nn41<-setDT(vf4)[,if(any(DX1=="428")) .SD[0:(which.max(DX1=="428")-1)]else .SD, by=Patient_ID]
vf5<-nn41[,c(1,6:13)]

View(vf4)
#--------------------To censor the data-----------------#
#----------------------Melt to get Patient_Ids where dx2-dx9 have 428 to remove those ID-----------#
vf51<-melt(vf5,id.vars= 1)
k2<-vf51$Patient_ID[!vf51$value==428]

View(vf51)
#------------------------------------Not Substringing----------------------------------------------#
k3<-k2[,c(1,5:13)]
k31<- melt(k3,id.vars= 1)
View(k31)
k31$variable<-NULL

View(k2)
k32<-dcast(k31, Patient_ID~value)
View(k32)
#----------------------Subsetting only Heart Failure Patients--------------------------------------#
nn5 <-nn5[nn5$Patient_ID %in%  k,]

View(nn5)

#---------------------------ICD9 and Substring first three values----------------------------------#

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

View(vf4)

vf4$DSCHDAT<-NULL
vf4$time.difference<-NULL
#
------------------

nn41<-setDT(vf4)[,if(any(DX1=="428")) .SD[0:(which.max(DX1=="428")-1)]else .SD, by=Patient_ID]

nn51<-melt(vf4,id.vars= c(1:2))
View(nn51)

gc()

vf4<-`428before(censored)`[,c(1,5:13)]

vf4<-data.table(vf4)

vf4<-melt(vf4,id.vars = 1)
View(vf4)

l<- nn51 %>%
    group_by(Patient_ID,desc(ADMDAT))


k<-NULL

gc()
library(data.table)
l<-data.table(l)
k<-melt(l,id.vars = 1)


vf5<-NULL

vf5<- vf4 %>% 
      group_by(Patient_ID) %>%
      arrange(value, .by_group=TRUE)

View(vf5)

#-------------------------------Finding Pairs----------------------------------------------#

library(dplyr)  

View(vf5)

m <- vf5 %>% group_by(Patient_ID) %>% do(data.frame(t(combn(.$value, 2))))

m<-m[!(m$X1==m$X2)]
library(data.table)
m<-data.table(m)
gc()
m<-m[!(m$X1==m$X2)]
gc()
m$x3<-paste0(m$X1,m$X2)
gc()
View(m)
str(vf5)

k<- vf5 %>%
    group_by(Patient_ID) %>%
    combn(.$value,3)
