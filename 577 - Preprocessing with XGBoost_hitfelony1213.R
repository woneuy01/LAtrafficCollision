rm(list=ls())
gc()

setwd("C:\\Users\\woneuy01\\Desktop\\577") #Please change this to your local path
# install.packages('fastDummies')
# install.packages('scales')
# install.packages("randomForest")
# install.packages("caret")
# install.packages("xgboost")
# install.packages("magrittr")
# install.packages("Matrix")
# install.packages("mlr")
# install.packages("parallelMap")
# install.packages("readr")
# install.packages("e1071", dep = TRUE)
# install.packages("ggplot2")
require(readr)
dat = read_csv('traffic-collision-data-from-2010-to-present.csv')
dim(dat)
par(mfrow=c(1, 1))
dat = data.frame(dat[,c('Time Occurred','MO Codes','Victim Age','Victim Sex','Victim Descent','Council Districts')])

#check whether it contains 3029 Hit and Run felony 3030 misdemeaner
dat$hit <-grepl("3029",dat$`MO.Codes`,perl=TRUE)

mean(is.na(dat$`Time.Occurred`)) # no missing
# MO Codes - num
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
sort(table(dat$`Victim.Descent`))
# H, W, B, O, and others
dat$`Victim.Descent`[which(dat$`Victim.Descent` %in% c("-","A","C","D","F","G","I","J","K","L","P","S","U","V","X","Z"))] = "Other_Descent"
table(dat$dui)
# Council Districts
boxplot(dat$`Council.Districts`,main="Council.Districts")
mean(is.na(dat$`Council.Districts`))
dat = dat[-which(is.na(dat$`Council.Districts`)),]
table(dat$`Council.Districts`)
# check whether it contains 3038 DUI felony or 3039 Misdemeanor
#dat$dui <-grepl("3038|3039",dat$`MO.Codes`,perl=TRUE)
dat = dat[,-which(colnames(dat) == "MO.Codes")]
require(ggplot2)

# #as.numeric(as.character(dat$Time.Occurred))
# dat2<-data.frame(dat1$Time.Occurred,dat1$Council.Districts) 
# head(dat2)
# #storage.mode(dat$Time.Occurred) <- "numeric" 
# ggplot(data=dat2, aes(dat2$Time.Occurred, dat2$Council.Districts)) + 
#   geom_point() + 
#   scale_x_continuous() + 
#   geom_jitter()
# ggplot(dat1)+geom_point(aes(y=dat1$Council.Districts,x=dat1$Time.Occurred)) + 
#   geom_jitter(aes(y=dat1$Council.Districts,x=dat1$Time.Occurred))  
#   
#   geom_tile()
# 
# typeof(dat$Time.Occurred)
# typeof (dat$Council.Districts)

# data.matrix(dat1)
# storage.mode(dat1) <- "numeric" 
# typeof(dat1$t)
# heatmap(dat2)
#factoring
dat$`hit` = as.factor(dat$`hit`)
dat$`Victim.Age` = as.factor(dat$`Victim.Age`)
dat$`Victim.Sex` = as.factor(dat$`Victim.Sex`)
dat$`Victim.Descent` = as.factor(dat$`Victim.Descent`)
dat$`Council.Districts` = as.factor(dat$`Council.Districts`)
head(dat)

# ggplot(dat,aes(x="Council.Districts",y="Time.Occurred")) +
#   geom_tile(aes(fill="Time.Occurred")) +
#   scale_fill_brewer(palette="Spectral") +
#   geom_text(aes(label="Council.Districts")) +
#   scale_y_reverse()
# normalize data and making dummies
require(fastDummies)
require("scales")
#normalizaion
dat$`Time.Occurred`<-as.numeric(dat$`Time.Occurred`)
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
head(dat)
typeof(dat)

##===============================Creating Data Partitions=================================##

#data partition
set.seed(1) 
#seperate data from DUI=TRUE and DUI=FALSE 
dat_y1 <- dat[dat$hit== TRUE,]
dat_y0 <- dat[!dat$hit==TRUE,]

# select 500 from dat_y1 data for training data
id_train_y1 = sample(nrow(dat_y1),5000)
# rest of dat_y1 625 becomes test data
id_test_y1 =setdiff(1:nrow(dat_y1),id_train_y1)
dat_train_y1 <- dat_y1[id_train_y1,]
dat_test_y1 <- dat_y1[id_test_y1,]
# select 500 from dat_y0 for training data
id_train_y0 = sample(nrow(dat_y0),5000)
id_test_y0 <- setdiff(1:nrow(dat_y0),id_train_y0)
dat_train_y0 <- dat_y0[id_train_y0,]
dat_test_y0 <- dat_y0[id_test_y0,]

train<- rbind(dat_train_y0, dat_train_y1)
test <- rbind(dat_test_y0, dat_test_y1)
table(train$hit)
#==================== XGBoost=========================================#
labels <- train$hit
ts_label <- test$hit
require(Matrix)
str(train)
new_tr <- sparse.model.matrix(hit~.-hit, data = train)
new_ts <- sparse.model.matrix(hit~.-hit, data = test)
#convert factor to numeric 
labels <- as.numeric(labels)-1
ts_label <- as.numeric(ts_label)-1
#preparing matrix 
require(xgboost)
dtrain <- xgb.DMatrix(data = as.matrix(new_tr),label = labels) 
dtest <- xgb.DMatrix(data = as.matrix(new_ts),label=ts_label)
#default parameters
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, 
               subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T,
                 stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)
xgbcv
# find min test_error_mean iteration number
min(xgbcv$evaluation_log[,4])
which(xgbcv$evaluation_log[,4]==0.4398)
#model training
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 22, watchlist = list(val=dtest,train=dtrain),
                   early_stop_round = 10, maximize = F , eval_metric = "error")
#model prediction
xgbpred <- predict (xgb1,dtest)
#manually use a cutoff value 0.5 since it return probability
xgbpred <- ifelse (xgbpred > 0.3,1,0)
require(caret)
confusionMatrix (table(xgbpred, ts_label))
#view variable importance plot
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20])

######################################################################################
####################XGBoost before tune###############################################
######################################################################################
#convert characters to factors
fact_col <- colnames(train)[sapply(train,is.character)]

for(i in fact_col) set(train,j=i,value = factor(train[[i]]))
for (i in fact_col) set(test,j=i,value = factor(test[[i]]))

#create tasks
require(mlr)
traintask <- makeClassifTask(data = train,target = "hit")
testtask <- makeClassifTask (data = test,target = "hit")

#do one hot encoding`<br/> 
traintask <- createDummyFeatures (obj = traintask) 
testtask <- createDummyFeatures (obj = testtask)

#create learner
lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list(
  objective="binary:logistic",
  eval_metric="error",
  nrounds=100L,
  eta=0.1
)

#set parameter space
params <- makeParamSet(
  makeDiscreteParam("booster",values = c("gbtree","gblinear")),
  makeIntegerParam("max_depth",lower = 3L,upper = 10L),
  makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
  makeNumericParam("subsample",lower = 0.5,upper = 1),
  makeNumericParam("colsample_bytree",lower = 0.5,upper = 1)
)

#set resampling strategy
rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)

#search strategy
ctrl <- makeTuneControlRandom(maxit = 10L)

#set parallel backend
require(parallel)
require(parallelMap)
parallelStartSocket(cpus = 2)

#parameter tuning
mytune <- tuneParams(learner = lrn
                     ,task = traintask
                     ,resampling = rdesc
                     ,measures = acc
                     ,par.set = params
                     ,control = ctrl
                     ,show.info = T)


mytune$y

#set hyperparameters
lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

#train model
xgmodel <- mlr::train(learner = lrn_tune,task = traintask)

#predict model
xgpred <- predict(xgmodel,testtask)

confusionMatrix(xgpred$data$response,xgpred$data$truth)
