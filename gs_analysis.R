gs_shots<-read.csv(file.choose(),header=TRUE,encoding='latin-1')
head(gs_shots)
str(gs_shots)
attach(gs_shots)
library(ggplot2)

#convert minutes to numeric
library(lubridate)
gs_shots$minutes1<-as.numeric(gs_shots$MP) #>14 (around 40 minutes?)

#where is golden state shooting? 
ggplot(gs_shots,aes(x=X3PA,y=eFG.,color=Lineup))+geom_point()
cor.test(X3PA,eFG.) #-0.134 gsw lineups shooting more threes less eFG% relative to opponent

#lineups with curry 
curry<-gs_shots[grep("Curry",gs_shots$Lineup),]
summary(curry) #FTA=+2.05, TRB=+2.5, FGA=-0.9, PTS=+18.5, eFG=+0.11
#lineups with durant
durant<-gs_shots[grep("Durant",gs_shots$Lineup),]
summary(durant) #FTA=-2.1, TRB=+3.8, FGA=-1, PTS=+13.1, eFG%=+0.12
#lineups with thompson 
thompson<-gs_shots[grep("Thompson",gs_shots$Lineup),]
summary(thompson) #FTA=-3.7, TRB=+4.7, FGA=-2.8, PTS=+13.0, eFG%=+0.11  

### density visualizations?
library(ggplot2)
library(ggpubr)

fill <- "gold1"
line <- "goldenrod2"
# Point difference (>40 minutes played)
gs_shotsX<-subset(gs_shots,gs_shots$minutes1>14) #>40 minutes lineup time
p1<- ggplot(gs_shotsX, aes(x = gs_shotsX$PTS)) +
  geom_density(fill = fill, colour = line) +
  scale_x_continuous(name = "Points Differential",
                     breaks = seq(0, 200, 25),
                     limits=c(0, 200)) +
  scale_y_continuous(name = "Density") +
  ggtitle("Golden State Warriors Lineup Points Differential")+theme(plot.title = element_text(hjust = 0.5))
p1 

#gs shot classification
#if point differential is greater than 1 or not? 
gs_shots$over_under<-ifelse(gs_shots$PTS>12.5,1,0)
gs_shots$over_under<-as.numeric(gs_shots$over_under)

#k-means clustering*******
str(gs_shots)
names(gs_shots)
gs_subset<-subset(gs_shots,select=c(2,3,4,5,6,7,8,9,10,
                                    11,12,13,14,15,16,17,18,19,20,21,22,23,24,26))
names(gs_subset)
dim(gs_subset)

#normalize variables
library(caret)
gs_pred<-preProcess(gs_subset[,-24],method=c("center","scale")) #source: https://stackoverflow.com/questions/15215457/standardize-data-columns-in-r
gs_pred_new<-predict(gs_pred,gs_subset[,-24])
#merge with class
gs_final<-cbind(gs_pred_new,gs_subset[24])
#drop MP 'factor'
gs_final1<-subset(gs_final,select=-c(1))

test<-1:8
train.gc<-gs_final1[-test,]
test.gc<-gs_final1[test,] 

train.def<-gs_final1$over_under[-test]
test.def<-gs_final1$over_under[test]

#find all 'na' values in dataset?
colnames(gs_final1)[colSums(is.na(gs_final))>0]
sapply(gs_final1, function(x)all(is.na(x)))

library(class)
#use knn to identify classes correctly? 
knn_1<-knn(train.gc,test.gc,train.def,k=1) 
table(knn_1,test.def) # 0.5 correctly (as points diff>12.5)
knn_2<-knn(train.gc,test.gc,train.def,k=5)
table(knn_2,test.def) #0.5 














