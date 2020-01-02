rm(list=ls())
gc()

setwd("C:\\Users\\woneu\\OneDrive\\Desktop\\ISDS577\\data cleaning") #Please change this to your local path
#install.packages('fastDummies')
#install.packages('scales')
#install.packages("randomForest")
#install.packages("caret")
require(readr)
dat = read_csv('traffic_holiday.csv')
par(mfrow=c(1, 1))
dat = data.frame(dat[,c('Time Occurred','MO Codes','Victim Age','Victim Sex','Victim Descent','Council Districts')])
class(dat)
head(dat)
dat$dui <-grepl("3038|3039",dat$`MO.Codes`,perl=TRUE)
head(dat)
table(dat$dui)
mean(is.na(dat$`Time.Occurred`)) # no missing

# MO Codes
mean(is.na(dat$`MO.Codes`)) # 17.41% missing
dat = dat[-which(is.na(dat$"MO.Codes")),]

# Victim Age - num
mean(is.na(dat$`Victim.Age`)) # 16.32% missing 
dat = dat[-which(is.na(dat$`Victim.Age`)),]
#Convert Continuous to Categorical variables
dat$'Victim.Age'<-cut(dat$'Victim.Age', breaks = c(0,20,35,50,100),labels = c("A","B","C","D"))
table(dat$`Victim.Age`)

# Victim Sex - category
mean(is.na(dat$`Victim.Sex`)) # 0.2% missing
dat = dat[-which(is.na(dat$`Victim.Sex`)),]
table(dat$`Victim.Sex`) 
dat$`Victim.Sex`[which(dat$`Victim.Sex` %in% c("H","N"))] = "X"
table(dat$dui)
# Victim Descent - category
mean(is.na(dat$`Victim.Descent`)) # 0.124% missing
dat = dat[-which(is.na(dat$`Victim.Descent`)),]
sort(table(dat$`Victim.Descent`)) # many small categories, will combine them into Other_Descent
# H, W, B, O, and others
dat$`Victim.Descent`[which(dat$`Victim.Descent` %in% c("-","A","C","D","F","G","I","J","K","L","P","S","U","V","X","Z"))] = "Other_Descent"
table(dat$dui)
# Council Districts
boxplot(dat$`Council.Districts`,main="Council.Districts")
mean(is.na(dat$`Council.Districts`))
dat = dat[-which(is.na(dat$`Council.Districts`)),]
table(dat$`Council.Districts`)
dim(dat) #332182x7

# check whether it contains 3038 DUI felony or 3039 Misdemeanor
#dat$dui <-grepl("3038|3039",dat$`MO.Codes`,perl=TRUE)
dat = dat[,-which(colnames(dat) == "MO.Codes")]
head(dat)
table(dat$dui)
#factoring
dat$`dui` = as.factor(dat$`dui`)
dat$`Victim.Age` = as.factor(dat$`Victim.Age`)
dat$`Victim.Sex` = as.factor(dat$`Victim.Sex`)
dat$`Victim.Descent` = as.factor(dat$`Victim.Descent`)
dat$`Council.Districts` = as.factor(dat$`Council.Districts`)
typeof(dat)
class(dat)
# normalize data and making dummies
require(fastDummies)
require("scales")
#normalizaion
dat$`Time.ccurred.rescaled` <-as.vector(rescale(dat$`Time.Occurred`))

dat = dat[,-which(colnames(dat) == "Time.Occurred")]
dat = dummy_cols(dat, c("Victim.Age"))
dat = dat[,-which(colnames(dat) == "Victim.Age")]
dat = dummy_cols(dat, c("Victim.Sex"))
dat = dat[,-which(colnames(dat) == "Victim.Sex")]
dat = dummy_cols(dat, c("Victim.Descent"))
dat = dat[,-which(colnames(dat) == "Victim.Descent")]
dat = dummy_cols(dat, c("Council.Districts"))
dat = dat[,-which(colnames(dat) == "Council.Districts")]



##===============================Creating Data Partitions=================================##

#data partition
set.seed(1) 
# seperating data by dui=TRUE and FALSE
dat_y1 <- dat[dat$dui== TRUE,]
dat_y0 <- dat[!dat$dui==TRUE,]

# select 500 from dat_y1(y=1) data for training data
id_train_y1 = sample(nrow(dat_y1),500)
# rest of dat_y1 625 belongs to test data 
id_test_y1 =setdiff(1:nrow(dat_y1),id_train_y1)
dat_train_y1 <- dat_y1[id_train_y1,]
dat_test_y1 <- dat_y1[id_test_y1,]
# select 500 from dat_y0(y=0) for training data
id_train_y0 = sample(nrow(dat_y0),500)
id_test_y0 <- setdiff(1:nrow(dat_y0),id_train_y0)
dat_train_y0 <- dat_y0[id_train_y0,]
dat_test_y0 <- dat_y0[id_test_y0,]

train<- rbind(dat_train_y0, dat_train_y1) 
test <- rbind(dat_test_y0, dat_test_y1)
dim(train)
dim(test)
table(test$dui)

##=============================== RandomForest =================================##
require("randomForest")
class(train)
as.data.frame(train)
trainForest <- randomForest(dui ~.,data=train, ntree = 500, mtry = 4, nodesize = 5, importaince = TRUE)

# variable importance plot
varImpPlot(trainForest, type =NULL)
# confusion matrix
train_pred <- predict(trainForest,test)
head(train_pred)
require(caret)
confusionMatrix(train_pred, test$dui)