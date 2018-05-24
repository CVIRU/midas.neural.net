# |------------------------------------------------------------------------|
# | Project:  Neural Network                                               |
# | Script:   Make the data set for the analysis                           |
# | Authors:  Mohammad, Vachan, Davit Sargsyan                             |   
# | Created:  05/23/2018                                                   |
# | Modified:                                                              |
# |------------------------------------------------------------------------|
# sink(file = "tmp/midas15_neural_net_data_v1.txt")
date()

# Header----
require(data.table)
require(ggplot2)
require(icd)
require(knitr)

# Part I: Load data----
# The dataset was created using
# '.../midas/source/export_midas_from_csv_to_rdata_v4.R' script
system.time(load("E:/MIDAS/midas15_clean.RData"))
midas15

# Number of patients
length(unique(midas15$Patient_ID))
# 4,446,438

# Remove ER patients----
# 1 = inpatient
# 2 = ER outpatient
# 3 = same day surgery (SDS) outpatient
# 4 = other outpatient (non-ER and non-SDS)
# 5 = non-ER outpatient (3 or 4)
midas15 <- midas15[midas15$ADM_TYPE %in% c(1, 3:5),]

# Record identifier
midas15$rec <- factor(1:nrow(midas15),
                      levels = 1:nrow(midas15))

# Sort----
setkey(midas15,
       Patient_ID,
       ADMDAT)
summary(midas15)
gc()

# Part II: exclusions----
# Load ICD-9 list----
# l1 <- fread("data/icd9_map_2018-05-24.csv",
#             colClasses = c("character"))
# 
# l1 <- as.comorbidity_map(list(excl = l1$code))
# l1
load("data/icd9_map_2018-05-24.RData")


# Separate diagnoses----
dx <- data.table(midas15[, c("rec",
                             "Patient_ID")],
                 midas15[, DX1:DX9])
head(dx)

# Convert diagnoses codes to comorbidities-----
dtt <- list()
for(i in 1:9){
  dtt[[i]] <- comorbid(x = dx,
                       map = l1,
                       visit_name = "Patient_ID",
                       icd_name = names(dx)[i + 2])
}
head(dtt[[1]])
colSums(dtt[[1]])

comorb <- data.table(apply(Reduce("+", dtt),
                           MARGIN = 2,
                           function(a){
                             a > 0
                           }))
head(comorb)
kable(format(addmargins(table(comorb)),
             big.mark = ","))
  # |      |FALSE     |TRUE   |Sum       |
  # |:-----|:---------|:------|:---------|
  # |FALSE |3,428,113 |16,415 |3,444,528 |
  # |TRUE  |408,916   |3,604  |412,520   |
  # |Sum   |3,837,029 |20,019 |3,857,048 |

# Keep all records of patients who were admited for AMI or HF at least once----
id.keep <- rownames(dtt[[1]])[rowSums(comorb) > 0]
dt1 <- subset(midas15,
              Patient_ID %in% id.keep)
head(dt1)

# sessionInfo()
# sink()