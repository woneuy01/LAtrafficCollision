rm(list=ls())
gc()

setwd("C:\\Users\\woneuy01\\Desktop\\577") #Please change this to your local path
# install.packages('fastDummies')
# install.packages('scales')
# install.packages("randomForest")
# install.packages("caret")
# install.packages("DMwR")
# install.packages("readr")
# install.packages("groupdata2")
# install.packages("e1071")
# install.packages("ROCR")
# install.packages("Metrics")
#install.packages("pROC")

require(readr)
dat = read_csv('traffic-collision-data-from-2010-to-present.csv')
par(mfrow=c(1, 1))
dat = data.frame(dat[,c('Time Occurred','MO Codes','Victim Age','Victim Sex','Victim Descent','Council Districts')])


mean(is.na(dat$`Time.Occu  rred`)) # no missing
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
id_train_y1 = sample(nrow(dat_y1),10000)
# rest of dat_y1 625 belongs to test data
id_test_y1 =setdiff(1:nrow(dat_y1),id_train_y1)
dat_train_y1 <- dat_y1[id_train_y1,]
dat_test_y1 <- dat_y1[id_test_y1,]
# select 10000 from dat_y0(y=0) for training data
id_train_y0 = sample(nrow(dat_y0),10000)
id_test_y0 <- setdiff(1:nrow(dat_y0),id_train_y0)
dat_train_y0 <- dat_y0[id_train_y0,]
dat_test_y0 <- dat_y0[id_test_y0,]

train<- rbind(dat_train_y0, dat_train_y1)
test <- rbind(dat_test_y0, dat_test_y1)
dim(train)
dim(test)
table(train$hit)
table(test$hit)

#=========================Logistic regression=================================================##
# regression.
# logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial")
# options(scipen=999)
# summary(logit.reg)

min.model = glm(hit ~ 1, data = train, family = 'binomial')
max.model = glm(hit ~ ., data = train, family = 'binomial')
max.formula = formula(max.model)
min.formula = formula(min.model)

#### FORWARD----------------------------------------------------------------------------------------------
fwd = step(min.model, direction='forward', scope=max.formula) # it will print out models in each step
summary(fwd) # it will give you the final model
get.or = function(sobj, alpha=.05) {
  b = sobj$coef[-1, 'Estimate']
  se.b = sobj$coef[-1, 'Std. Error']
  pval = sobj$coef[-1, 'Pr(>|z|)']
  or = exp(b); se.or = exp(b)*se.b
  lb = b - qnorm(alpha/2)*se.b; lb.or = exp(lb)
  ub = b + qnorm(1-alpha/2)*se.b; ub.or = exp(ub)
  out = cbind(or, se.or, lb, ub, pval)
  colnames(out) = c('OR', 'SE', paste0((1-alpha)*100, '% CI, lower'),
                    paste0((1-alpha)*100, '% CI, upper'), 'p value')
  return(out)
}

get.or(summary(fwd))

# Predict using test data
yhatfwd = predict(fwd, newdata = test, type='response')

hist(yhatfwd)

#cutoff
dichotomize = function(yhatfwd, cutoff=0.5) {
  out = rep(0, length(yhatfwd))
  out[yhatfwd > cutoff] = 1
  out
}
table(test$hit)
test$hit <-as.numeric(test$hit)-1
yhatfwd.class = dichotomize(yhatfwd, 0.3)
errfwd = mean(yhatfwd.class != test$hit) # misclassification error rate
errfwd
tail(test$hit)
table(yhatfwd.class, test$hit)

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



