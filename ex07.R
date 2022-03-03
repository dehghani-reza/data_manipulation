getwd()
library("moments")
library("MASS")
library("mice")

set.seed(1234)
x=rgamma(1000,shape = 1,rate = 1)
x
summary(x)
class(x)
dim(x)
str(x)

hist(x , probability =T,breaks = 15)
lines(density(x),col="red")

qqnorm(x,main = "qq plot",pch=20)
qqline(x,col="red")

skewness(x)
jarque.test(x)
kurtosis(x)
anscombe.test(x)

log_x=log(x)

hist(log_x,breaks = 15, col = "red",freq = F)
lines(density(log_x),col="blue")

qqnorm(log_x,main = "data log_x",pch=20)
qqline(x,col="red")

skewness(log_x)
kurtosis(log_x)

shapiro.test(log_x)
anscombe.test(log_x)


box_result = boxcox(x~1,lambda = seq(-5,5,0.1))
class(box_result)

box_result = data.frame(box_result$x,box_result$y)
class(box_result)
box_result
lambda=box_result[which(box_result$box_result.y==max(box_result$box_result.y)),1]

lambda

box_cox_x = (x^lambda-1)/lambda
hist(box_cox_x,breaks = 15,probability = T)
lines(density(box_cox_x),col="red")

qqnorm(box_cox_x)
qqline(box_cox_x,col="red")

skewness(box_cox_x)
kurtosis(box_cox_x)

shapiro.test(box_cox_x)
anscombe.test(box_cox_x)
jarque.test(box_cox_x)

#read to line 97
data <- read.csv("CS_01_02.csv", header = TRUE)
getwd()
setwd("E:/Computer/DataScience/sessionFiles/7")
dim(data)
summary(data)
head(data,n = 7)
data_1=data
data_1[data_1=="."]=NA
summary(data_1)
data_2=data_1
for (i in 2:10) {
  data_1[,i]=as.numeric(data_1[,i])  
}

for (i in 11:15) {
  data_1[,i]=as.factor(data_1[,i])  
}
class(data_1[,14])
summary(data_1)
data=data_1
summary(data)
p=c()
for (j in 1:15) {
  p[j]=sum(is.na(data[,j]))/nrow(data)*100
}
p
colnames(data)
summary_1=data.frame("variable_names" = colnames(data))
summary_1$mvs_freq=apply(data,2,function(x) sum(is.na(x)))
summary_1$mvs_perc=apply(data,2,function(x) sum(is.na(x))/nrow(data)*100)

summary_2=as.data.frame(table(apply(data, 1, function(x) sum(is.na(x)))))
View(summary_2)
colnames(summary_2) <- c("mvs_per_case", "mvs_freq")

summary_2$mvs_percentage=round(summary_2$mvs_freq/nrow(data)*100,digits = 3)

data_w_mvs=data[apply(data, 1, function(x) any(is.na(x))),]

data_w_mvs$mvs_count = apply(data_w_mvs,1,function(x)sum(is.na(x)))
data_1=data[-which(data$ID%in%c(210, 214, 233, 245, 261, 263)),]
dim(data_1)
dim(data)
View(data_1)
data_1=data_1[,-2]
mean(data_1$V3[is.na(data_1$V2)],na.rm = T)
mean(data_1$V3[!is.na(data_1$V2)],na.rm = T)

t.test(data_1$V3[is.na(data_1$V2)],
       data_1$V3[!is.na(data_1$V2)],
       alternative = "two.sided")
hist(data_1$V3,probability = T)
lines(density(data_1$V3,na.rm = T),col="red")
skewness(data_1$V3,na.rm = T)
kurtosis(data_1$V3,na.rm = T)
qqnorm(data_1$V3,na.rm = T)
qqline(data_1$V3,na.rm = T,col="red")
shapiro.test(data_1$V3)

data_1$ifV2mv <- ifelse(is.na(data_1$V2), 1, 0)
table("mv"=data_1$ifV2mv,"v10"=data_1$V10)
###--------------------------------
getwd()
setwd("E:/DataScience/sessionFiles/7")
library("moments")
library("MASS")
library("mice")
 set.seed(1234)
x = rgamma(1000,shape = 1,rate=1)
hist(x)
logx = log(x)
hist(logx)
kurtosis(logx)# kurtosis should be 3 for following normal distribution
skewness(logx)# skewness should be 0 for following normal distribution
jarque.test(logx)
anscombe.test(logx)
box_cox_x = boxcox(x~1,lambda = seq(-5,5,0.1))
class(box_cox_x)
df_box_x =data.frame(x=box_cox_x$x,y=box_cox_x$y)
best_power =df_box_x[which(df_box_x$y==max(df_box_x$y)),]$x
best_power
lamda_x = (((x^best_power)-1)/best_power)
hist(lamda_x)
anscombe.test(lamda_x)
jarque.test(lamda_x)
skewness(lamda_x)
kurtosis(lamda_x)
qqnorm(lamda_x)
qqline(lamda_x)

###---------------------
cs = read.csv("CS_01_02.csv",header = T)
View(cs)
summary(cs)
cs[cs=='.']=NA
for(i in 2:10){
  cs[,i]=as.numeric(cs[,i])
}
for(j in 11:15){
  cs[,j]=as.factor(cs[,j])
}
summary(cs)

class(cs)
sum_cs_case$n=apply(cs,1,function(x){sum(is.na(x))})
sum_cs_case=as.data.frame(cs$ID)
sum_cs_case$per=sum_cs_case$n/ncol(cs)*100
sum_cs_col=as.data.frame(colnames(cs))
sum_cs_col$num = apply(cs, 2, function(x){sum(is.na(x))})
sum_cs_col$per=sum_cs_col$num/nrow(cs)*100
## 204 207 210 233 261
removeCase = sum_cs_case[sum_cs_case$per>50,]$`cs$ID`
new_cs=cs[-which(cs$ID %in%c(204,207,210,233,261)),]
dim(new_cs)
View(new_cs)
View(sum_cs_col)
##remove column with highest MV
new_cs = new_cs[,-2]
## check relation between v3 and v5 NA the base col is v5
mean(new_cs$V3[is.na(new_cs$V5)],na.rm = T)
mean(new_cs$V3[!is.na(new_cs$V5)],na.rm = T)

boxplot(new_cs$V3[is.na(new_cs$V5)],new_cs$V3[!is.na(new_cs$V5)])

sum(!is.na((new_cs$V3[is.na(new_cs$V5)])))
sum(!is.na((new_cs$V3[!is.na(new_cs$V5)])))

t.test(new_cs$V3[is.na(new_cs$V5)],
       new_cs$V3[!is.na(new_cs$V5)],
       alternative = "two.sided")
#p-value = 0.8002 is significance
#so there isn't any relationship between V5 missing value and V3

## check relation between V10 and V5 NA the base col is v5

is.na(new_cs$V5)

table(v10=new_cs$V10,v5MV=ifelse(is.na(new_cs$V5),1,0))

chisq.test(table(v10=new_cs$V10,v5MV=ifelse(is.na(new_cs$V5),1,0)))

#p-value = 0.6275
#so there isn't any relationship between V5 missing value and V10



