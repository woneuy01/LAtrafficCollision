rm(list=ls())
gc()

setwd("C:\\Users\\woneuy01\\Desktop") 
# install.packages('fastDummies')
# install.packages("useful")
# install.packages("randomForest")
# install.packages("caret")
# install.packages("factoextra")
# install.packages("stringr")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("readr")
require(readr)
dat = read_csv('traffic-collision-data-from-2010-to-present.csv')
mo_codes_names = read_csv('MO_Codes.csv')
head(mo_codes_names)
par(mfrow=c(1, 1))
dat = data.frame(dat[,c('MO Codes','Victim Age','Victim Sex','Victim Descent')])
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

# Victim Descent - category
mean(is.na(dat$`Victim.Descent`)) # 0.149% missing
dat = dat[-which(is.na(dat$`Victim.Descent`)),]
sort(table(dat$`Victim.Descent`)) # many small categories, will combine them into Other_Descent
# H, W, B, O, and others
dat$`Victim.Descent`[which(dat$`Victim.Descent` %in% c("-","A","C","D","F","G","I","J","K","L","P","S","U","V","X","Z"))] = "Other_Descent"

#factoring
dat$`Victim.Age` = as.factor(dat$`Victim.Age`)
dat$`Victim.Sex` = as.factor(dat$`Victim.Sex`)
dat$`Victim.Descent` = as.factor(dat$`Victim.Descent`)
dat <- dat[order(dat$MO.Codes),]
head(dat)
# normalize data and making dummies
# clustering will use 1)Victim.Age 2) Victim.Sex 3) Victim.Descent
require(fastDummies)
dat = dummy_cols(dat, c("Victim.Age"))
dat = dat[,-which(colnames(dat) == "Victim.Age")]
dat = dummy_cols(dat, c("Victim.Sex"))
dat = dat[,-which(colnames(dat) == "Victim.Sex")]
dat = dummy_cols(dat, c("Victim.Descent"))
dat = dat[,-which(colnames(dat) == "Victim.Descent")]
head(dat)
dat1 = dat[,-which(colnames(dat) == "MO.Codes")]
head(dat1)
##############################################
# Clustering
##############################################
#select 3 columns for numerical data (Time Occurred, Victim Age, Location)
#Location data is not deleted as other cleaned data

require("useful")
# Kmeans try cluster from 2 to 10 and repeat 20 times
kmeans1 <- FitKMeans(dat1, max.clusters=20, nstart=20, seed=1)
kmeans1
PlotHartigan(kmeans1)
#select 5 clusters
kmeans2 <- kmeans(x=dat1, centers = 5, nstart = 20)
#plot(kmeans2, data=dat1)
#plot(wineK3, data=wineTrain)
#The chart said cluster is addable until 20 but will not more since too much clustering may not practical.
require("factoextra")
fviz_cluster(kmeans2, data = dat1)
kmeans2$size
names(kmeans2)
cluster_dat1<-kmeans2$cluster
dat1<- cbind(dat1,cluster_dat1)
MO_Codes <- dat$MO.Codes
head(MO_Codes)
dat1<- cbind(dat1, MO_Codes)
dat1_nrow <-nrow(dat1)

require("ggplot2")
# 
# MO_sep_all <-as.numeric(str_split_fixed(dat1$MO_Codes," ",11))
# table_sort_MO_all<-sort(table(MO_sep_all), decreasing=TRUE)
# top_table_sort_MO_all<-head(table_sort_MO_all,10)
# m_all<- merge(top_table_sort_MO_all, mo_codes_names, by.x = "MO_sep_all", 
#             by.y="MO  CODES", all.x = TRUE)
# m_all<-m_all[order(m_all$Freq, decreasing=TRUE,na.last=NA),]

#devide data from cluster 1 to 5

dat_c1 <- dat1[which(dat1$cluster_dat1 == 1),]
dat_c2 <- dat1[which(dat1$cluster_dat1 == 2),]
dat_c3 <- dat1[which(dat1$cluster_dat1 == 3),]
dat_c4 <- dat1[which(dat1$cluster_dat1 == 4),]
dat_c5 <- dat1[which(dat1$cluster_dat1 == 5),]
ng1<-nrow(dat_c1) #98597
ng2<-nrow(dat_c2) #62375
ng3<-nrow(dat_c3) #79640
ng4<-nrow(dat_c4) #40665
ng5 <-nrow(dat_c5) #54743 total cluster1~5 336020
require(stringr)
#cluster 1
MO_sep <-as.numeric(str_split_fixed(dat_c1$MO_Codes," ",11))
gr1 <- setNames(c(1),c('group'))
MO_sep<- cbind(MO_sep, gr1)
sortg1<-sort(table(MO_sep),decreasing=TRUE)
gr1_per<-sortg1/ng1

#cluster 2
MO_sep2 <-as.numeric(str_split_fixed(dat_c2$MO_Codes," ",11))
gr2 <- setNames(c(2),c('group'))
MO_sep2<- cbind(MO_sep2, gr2)
sortg2<-sort(table(MO_sep2),decreasing=TRUE)
gr2_per<-sortg2/ng2
#cluster 3
MO_sep3 <-as.numeric(str_split_fixed(dat_c3$MO_Codes," ",11))
gr3 <- setNames(c(3),c('group'))
MO_sep3 <- cbind(MO_sep3, gr3)
sortg3<-sort(table(MO_sep3),decreasing=TRUE)
gr3_per<-sortg3/ng3
#cluster 4
MO_sep4 <-as.numeric(str_split_fixed(dat_c4$MO_Codes," ",11))
gr4 <- setNames(c(4),c('group'))
MO_sep4 <- cbind(MO_sep4, gr4)
sortg4<-sort(table(MO_sep4),decreasing=TRUE)
gr4_per<-sortg4/ng4
#cluster 5
MO_sep5 <-as.numeric(str_split_fixed(dat_c5$MO_Codes," ",11))
gr5 <- setNames(c(5),c('group'))
MO_sep5 <- cbind(MO_sep5, gr5)
sortg5<-sort(table(MO_sep5),decreasing=TRUE)
gr5_per<-sortg5/ng5
# combine dataframes
MO_sep_all <- data.frame(do.call("rbind", list(MO_sep,MO_sep2,MO_sep3, MO_sep4, MO_sep5)))
table1<-sort(table(MO_sep_all$MO_sep), decreasing=TRUE)

#select only top 20 MO codes
sort_table<-head(table1,20)
View(sort_table)
class(sort_table)
names(sort_table)
require(dplyr)
#vlookup with MO Code description excel
mer<- merge(sort_table, mo_codes_names, by.x = "Var1", 
            by.y="MO  CODES", all.x = TRUE)
head(mo_codes_names)
# order again
mer <- mer[order(mer$Freq,decreasing = TRUE),]
View(mer)

# reselect only top 12 
MO_sep_all <- MO_sep_all[which(MO_sep_all$MO_sep %in% c(3101,3701,3401, 3004,3037, 3030,3026,3028,3036,3035,3006)),]
View(MO_sep_all)
MO_sep_all$MO_sep = as.factor(MO_sep_all$MO_sep) 
MO_sep_all$gr1 = as.factor(MO_sep_all$gr1)
table_all<-sort(table(MO_sep_all$MO_sep)/dat1_nrow, decreasing=TRUE)
names(table_all)
View(table_all)
View(gr1_per)
mer1<- merge(table_all, gr1_per, by.x = "Var1", 
            by.y="MO_sep", all.x = TRUE)
mer1<-cbind(mer1,mer1$Freq.y-mer1$Freq.x)

mer2<- merge(table_all, gr2_per, by.x = "Var1", 
             by.y="MO_sep2", all.x = TRUE)
mer2<-cbind(mer2,mer2$Freq.y-mer2$Freq.x)

mer3<- merge(table_all, gr3_per, by.x = "Var1", 
             by.y="MO_sep3", all.x = TRUE)
mer3<-cbind(mer3,mer3$Freq.y-mer3$Freq.x)

mer4<- merge(table_all, gr4_per, by.x = "Var1", 
             by.y="MO_sep4", all.x = TRUE)
mer4<-cbind(mer4,mer4$Freq.y-mer4$Freq.x)

mer5<- merge(table_all, gr5_per, by.x = "Var1", 
             by.y="MO_sep5", all.x = TRUE)
mer5<-cbind(mer5,mer5$Freq.y-mer5$Freq.x)
names(mer1)

# aggregated graph
cbind(dt, df[, 1:2])
ggplot(data = MO_sep_all) +
  geom_bar(
    mapping = aes(x=MO_sep,fill=MO_sep))+
     scale_y_continuous(breaks=seq(0,300000,by=100000))
# set MO_codes as X
ggplot(data = MO_sep_all) +
  geom_bar(
    mapping = aes(x=MO_sep, fill = gr1), 
    position ="dodge"
  )
# set MO_codes as y
ggplot(data = MO_sep_all) +
  geom_bar(
    mapping = aes(x=gr1, fill = MO_sep), 
    position ="dodge"
  )+
ggtitle("Traffic violation by each clusters") +
  xlab("Cluster") + ylab("Count")+
  labs(fill = "MO Codes")

#Cluster1 VS population
ggplot(mer1, aes(x = Var1, y = `mer1$Freq.y - mer1$Freq.x`,fill = `mer1$Freq.y - mer1$Freq.x`)) +
  ggtitle("Cluster1 traffic violation VS population") +
  xlab("MO Code") + ylab("VS population")+
  labs(fill = "VS population")+
  geom_col()

#Cluster2 VS population
ggplot(mer2, aes(x = Var1, y = `mer2$Freq.y - mer2$Freq.x`,fill = `mer2$Freq.y - mer2$Freq.x`)) +
  ggtitle("Cluster2 traffic violation VS population") +
  xlab("MO Code") + ylab("VS population")+
  labs(fill = "VS population")+
  geom_col() 

#Cluster3 VS population
ggplot(mer3, aes(x = Var1, y = `mer3$Freq.y - mer3$Freq.x`,fill = `mer3$Freq.y - mer3$Freq.x`)) +
  ggtitle("Cluster3 traffic violation VS population") +
  xlab("MO Code") + ylab("VS population")+
  labs(fill = "VS population")+
  geom_col() 

#Cluster4 VS population
ggplot(mer4, aes(x = Var1, y = `mer4$Freq.y - mer4$Freq.x`,fill = `mer4$Freq.y - mer4$Freq.x`)) +
  ggtitle("Cluster4 traffic violation VS population") +
  xlab("MO Code") + ylab("VS population")+
  labs(fill = "VS population")+
  geom_col() 

#Cluster5 VS population
ggplot(mer5, aes(x = Var1, y = `mer5$Freq.y - mer5$Freq.x`,fill = `mer5$Freq.y - mer5$Freq.x`)) +
  ggtitle("Cluster5 traffic violation VS population") +
  xlab("MO Code") + ylab("VS population")+
  labs(fill = "VS population")+
  geom_col() 



# predict new data
# knnClust <- knn(train = df_iris$train[,-5], test = df_iris$test[,-5] , k = 1, cl = groups)
# knnClust

bp + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                       name="Experimental\nCondition",
                       breaks=c("ctrl", "trt1", "trt2"),
                       labels=c("Control", "Treatment 1", "Treatment 2"))

#fviz_cluster(k2, data = df)
require("factoextra")
# fviz_cluster(kmeans1, data = dat2_norm)
# clusplot(dat2_norm, kmeans1, lines=0, shade=TRUE, color=TRUE, labels=2,
#          plotchar = FALSE, span=TRUE,main=paste('Clusters of customers'),
#          xlab='Annual Income', ylab = 'Spending Scocre')


#Hierarchical clustering using 4 distance measures
# set.seed(1)
# n = nrow(dat2_norm)
# ind.sample = sample(n, floor(n*.05))
# length(ind.sample)
# dat2_sample <- dat2_norm[ind.sample,]
# dim(dat2_sample)
# require("stats")
# hc1 <- hclust(dist(dat2_sample))
# plot(hc1, labels=FALSE, main="5%sample")
# hc1_r<-rect.hclust(hc1, k=10, border="blue")
# hc2 <- hclust(dist(dat2_sample), method = 'complete')
# hc3 <- hclust(dist(dat2_sample), method = 'average')
# hc4 <- hclust(dist(dat2_sample), method = 'centroid')
#characterizing clusters

# member <- cutree(hc1,10)
# aggregate(dat2_sample, list(member), mean)
# distance <- dist(dat2_sample)


#Gap statistic, which compares ?????? ?????? 걸림 ?????????걸로 ??????
#the within-cluster dissimilarity for a clustering of the data with that of a bootstrapped sample of data
# Due to the heavy calculation ClusGap calculated with 10% sample of dat2(388332X4) data
# 
# install.packages("cluster")
# require(cluster)
# theGap <- clusGap(dat2_sample, FUNcluster=pam, K.max=5)
# gapDF <- as.data.frame(theGap$Tab)

# require("cluster")
# dim(dat2_sample)
# dim(hc1)



#silhouette plot
# require(cluster)
# install.packages("mdendro")
# require("mdendro")
# lnk1 <- linkage(dat2_sample, method="single", weighted=FALSE)
#plot(silhouette(cutree(hc1,10), distance)) #since too large data no silhouette availabe

#K-medoid(PAM) for categorical data
# pam1 <- pam(x=dat1_sample, k=5, keep.diss = TRUE, keep.data = TRUE)
# pam1$medoids
# head(pam1$medoids)
# clusplot(pam(x=dat1_sample, k=5, stand=TRUE))

# plot(pam1, which.plots = 3, main="")
# K-medoid (PAM cluster)
# install.packages("factoextra")
# require("factoextra")
# fviz_cluster(pam1, geom='point',
#              show.clust.cent = TRUE,
#              ellipse = TRUE)

# install.packages('data.table')
# # library(data.table)
# dat1 = data.frame(dat1)
# 
# # install.packages("rpart.plot")
# # library(rpart.plot)
# 
# fit = rpart(`Day.Off` ~ ., method="class", data=dat1[ind.train,], cp = 1e-2, minsplit=5)
# rpart.plot(fit, main = 'Full Tree')
# # Minimum Error Tree
# pfit = prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
# rpart.plot(pfit, main = 'Min Error Tree')
# 
# yhat = predict(pfit, dat1[ind.val,], type = "class")
# #install.packages("hydroGOF")
# require(hydroGOF)
# yhat = as.numeric(yhat)
# rmse(yhat, dat1[ind.val,]$Day.Off) #.9394237

