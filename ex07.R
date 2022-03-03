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


#comparing all numeric data mean in two condition v5 is NA or not----
mean_df_on_v5 = data.frame(matrix(nrow = 8,ncol = 2))
row.names(mean_df_on_v5)=colnames(new_cs)[2:9]
colnames(mean_df_on_v5)=c("v5 na","v5")
for(i in 2:9){
  mean_df_on_v5$`v5 na`[i-1]=mean(new_cs[is.na(new_cs$V5),i],na.rm = T)
}
for(i in 2:9){
  mean_df_on_v5$v5[i-1]=mean(new_cs[!is.na(new_cs$V5),i],na.rm = T)
}
#remove v5
mean_df_on_v5=mean_df_on_v5[-4,]

mean_df_on_v5$`p.value`=rep.int(0,7)
for(i in c(2,4)){
  mean_df_on_v5$p.value[i-1]=(t.test(new_cs[is.na(new_cs$V5),i],
          new_cs[!is.na(new_cs$V5),i],
          alternative = "two.sided"))$p.value
}
for(i in 6:9){
  mean_df_on_v5$p.value[i-2]=(t.test(new_cs[is.na(new_cs$V5),i],
                                     new_cs[!is.na(new_cs$V5),i],
                                     alternative = "two.sided"))$p.value
}

mean_df_on_v5$is.relevant=ifelse(mean_df_on_v5$p.value>=0.05,F,T)
##all the numeric column have not any connection to V5 NA 

#comparing all factor data mean in two condition v5 is NA or not----
ifelse(is.na(new_cs$V5),0,1)

for(i in 10:14){
  print(i)
  print(table(name=new_cs[,i],v5=ifelse(is.na(new_cs$V5),0,1)))
}
## chi square test 
chisq_test_v5=data.frame(matrix(nrow = 5,ncol = 2))
row.names(chisq_test_v5)=colnames(new_cs)[10:14]
colnames(chisq_test_v5)=c("chi sq","is relate")
for(i in 10:14){
  print(i)
  chisq_test_v5$`chi sq`[i-9]=((chisq.test(table(name=new_cs[,i],
                                    v5=ifelse(is.na(new_cs$V5),0,1))))
        $p.value)
}

chisq_test_v5$`is relate` =ifelse(chisq_test_v5>0.05,F,T) 
## None column is effect on v5 NA
## we can use imputation method which for MCAR(missing completing at random) condition
#method 1: complete case approach 
comp_cs=new_cs[apply(new_cs,MARGIN = 1,FUN = function(x){any(is.na(x))==F}),]
dim(comp_cs)
mean_imput_comp <- data.frame(apply(comp_cs[2:9], 2, mean))
colnames(mean_imput_comp) <- "all_case"
mean_imput_comp 

sd_imput_comp <- data.frame(apply(comp_cs[2:9], 2, sd))
colnames(sd_imput_comp) <- "all_case"
sd_imput_comp 
#Method 2: Mean Substitution
mean_sub_cs = new_cs
for(i in 2:9){
mean_sub_cs[is.na(mean_sub_cs[,i]),i]=mean(mean_sub_cs[,i],na.rm = T)
}

mean_imput_comp$mean=apply(mean_sub_cs[,2:9],2,mean)
sd_imput_comp$mean=apply(mean_sub_cs[,2:9],2,sd)
#method 3:Regression
reg_cs = new_cs
impl_reg = mice(reg_cs[,2:9],method = "norm.predict",m = 5)
reg_cs=complete(impl_reg)
mean_imput_comp$reg = apply(reg_cs,2,mean)
sd_imput_comp$reg = apply(reg_cs,2,sd)

## step 5 : correlation analysis----
comp_cor = cor(comp_cs[2:9])
mean_cor =cor(mean_sub_cs[2:9])
reg_cor =cor(reg_cs)
ave_cor = (comp_cor+mean_cor+reg_cor)/3

#but we can get to this conclusion that complete case isn't good for this 
#data set because its make case half and have lot differ compare to two other 
#method
best_ave_cor = (mean_cor+reg_cor)/2







