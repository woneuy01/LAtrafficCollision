rm(list=ls())
gc()

setwd("C:\\Users\\woneuy01\\Desktop\\577") #Please change this to your local path
# install.packages('fastDummies')
# install.packages('scales')
# install.packages("randomForest")
# install.packages("caret")
# install.packages("DMwR")
# install.packages("readr")
#install.packages("groupdata2")
#install.packages("e1071")
#install.packages("raster")

require(readr)
dat = read_csv('traffic-collision-data-from-2010-to-present.csv')
par(mfrow=c(1, 1))
dat = data.frame(dat[,c('Time Occurred','MO Codes','Victim Age','Victim Sex','Victim Descent','Council Districts')])


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
# Council Districts
boxplot(dat$`Council.Districts`,main="Council.Districts")
mean(is.na(dat$`Council.Districts`))
dat = dat[-which(is.na(dat$`Council.Districts`)),]
table(dat$`Council.Districts`)
dim(dat) #332182x7

#check whether it contains 3029 Hit and Run felony 3030 misdemeaner
dat$hit <-grepl("3029",dat$`MO.Codes`,perl=TRUE)
dat = dat[,-which(colnames(dat) == "MO.Codes")]

#factoring
dat$`hit` = as.factor(dat$`hit`)
dat$`Victim.Age` = as.factor(dat$`Victim.Age`)
dat$`Victim.Sex` = as.factor(dat$`Victim.Sex`)
dat$`Victim.Descent` = as.factor(dat$`Victim.Descent`)
dat$`Council.Districts` = as.factor(dat$`Council.Districts`)

# normalize data and making dummies
require(fastDummies)
require("scales")
#normalizaion
dat$Time.Occurred<-as.numeric(dat$`Time.Occurred`)
typeof(dat$Time.Occurred)
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
table(dat$hit)


##===============================Creating Data Partitions=================================##
#data partition
set.seed(1)
# seperating data by dui=TRUE and FALSE
dat_y1 <- dat[dat$hit== TRUE,]
dat_y0 <- dat[!dat$hit==TRUE,]

# select 10000 from dat_y1(y=1) data for training data
id_train_y1 = sample(nrow(dat_y1),5000)
# rest of dat_y1 625 belongs to test data
id_test_y1 =setdiff(1:nrow(dat_y1),id_train_y1)
dat_train_y1 <- dat_y1[id_train_y1,]
dat_test_y1 <- dat_y1[id_test_y1,]
# select 10000 from dat_y0(y=0) for training data
id_train_y0 = sample(nrow(dat_y0),5000)
id_test_y0 <- setdiff(1:nrow(dat_y0),id_train_y0)
dat_train_y0 <- dat_y0[id_train_y0,]
dat_test_y0 <- dat_y0[id_test_y0,]

train<- rbind(dat_train_y0, dat_train_y1)
test <- rbind(dat_test_y0, dat_test_y1)
dim(train)
dim(test)
table(train$hit)
table(test$hit)

a <- ggplot(train, aes(x = Time.ccurred.rescaled,fill=hit))
a +  geom_dotplot(binaxis = "x", 
                  stackdir = "centerwhole", 
                  method="dotdensity",
                  stackgroups = T,
                  binpositions="all", binwidth = 0.01)

##############################################
### Data Partition INTO TRAING 60% AND VALIDATION 40% RANDOMLY 
### with UPSAMPLING and SMOTE
##############################################
# set.seed(1)
# n = nrow(dat)
# ind.train = sample(n, floor(n*.1))
# ind.val = setdiff(1:n, ind.train)
# train <- dat[ind.train,]
# test <- dat[-ind.train,]
# table(train$hit)
# table(test$hit)
# dim(train)
# dim(test)
# head(train)

# #data partition
# set.seed(1)
# 
# require("groupdata2")
# upsample_d <-upsample(train, cat_col="dui")
# table(upsample_d$dui)
# #SMOTE
# require(DMwR)
# #smote_d<- SMOTE(dui~., data = train)
# smoted_d<- SMOTE(dui~., train, perc.over=700, perc.under=100, k=5, learner=NULL)
# table(smote_d$dui)
# table(test$dui)
# 
# train<- rbind(dat_train_y0, dat_train_y1)
# test <- rbind(dat_test_y0, dat_test_y1)
# dim(train)
# dim(test)
# table(test$dui)
# head(smote_d)
##=============================== RandomForest =================================##
require("randomForest")
class(train)
as.data.frame(train)
trainForest <- randomForest(hit ~.,data=train, ntree = 500, mtry = 4, nodesize = 5, importaince = TRUE)
table(trainForest$predicted)
# variable importance plot
varImpPlot(trainForest, type =NULL)
# confusion matrix
#train_pred <- predict(trainForest,test)
require(caret)
require(e1071)
#test$hit <-as.factor(test$hit)
#confusionMatrix(train_pred, test$hit)

# Predict using test data
#yhatfwd <- predict(trainForest,test)
train_pred<-as.numeric(train_pred)-1
typeof(trainForest)
head(trainForest)
names(trainForest)
trainForest$predicted <-as.integer(trainForest$predicted)-1
table()
yhatfwd = predict(trainForest$predicted, newdata = test, type='response')
#yhatfwd <- predict(trainForest,test)
yhatfwd<-as.numeric(yhatfwd)-1
hist(yhatfwd)
table(yhatfwd)
typeof(yhatfwd)
#cutoff
dichotomize = function(yhatfwd, cutoff=0.5) {
  out = rep(0, length(yhatfwd))
  out[yhatfwd > cutoff] = 1
  out
}
table(test$hit)
test$hit <-as.numeric(test$hit)-1

yhatfwd.class = dichotomize(yhatfwd, 0.5)
errfwd = mean(yhatfwd.class != test$hit) # misclassification error rate
errfwd
tail(test$hit)
table(yhatfwd)
table(yhatfwd.class)
table(test$hit)
table(yhatfwd.class, test$hit)
yhatfwd.clas<-as.numeric(yhatfwd.class)
#sensitivity
senfwd = function(y, yhatfwd) {
  ind.true1 = which(y== 1)
  mean( y[ind.true1] == yhatfwd[ind.true1] )
}

#specificity
spefwd = function(y, yhatfwd) {
  ind.true0 = which(y == 0)
  mean( y[ind.true0] == yhatfwd[ind.true0] )
}

senfwd(test$hit, yhatfwd.class)
spefwd(test$hit, yhatfwd.class)
# #cutoff
# dichotomize = function(yhatfwd, cutoff=0.5) {
#   out = rep(0, length(yhatfwd))
#   out[yhatfwd > cutoff] = 1
#   out
# }
# yhatfwd <-as.numeric(yhatfwd)
# table(test$hit)
# head(yhatfwd)
# yhatfwd.class = dichotomize(yhatfwd, 0.3)
# test$hit<-as.numeric(test$hit)
# errfwd = mean(yhatfwd.class != test$hit) # misclassification error rate
# errfwd
# table(yhatfwd.class)
# 
# #sensitivity
# senfwd = function(y, yhatfwd) {
#   ind.true1 = which(y == 1)
#   mean( y[ind.true1] == yhatfwd[ind.true1] )
# }
# 
# #specificity
# spefwd = function(y, yhatfwd) {
#   ind.true0 = which(y == 0)
#   mean( y[ind.true0] == yhatfwd[ind.true0] )
# }
# senfwd(test$hit, yhatfwd.class)
# spefwd(test$hit, yhatfwd.class)
# 





  
