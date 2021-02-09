#setting working_directory and reading the file

df=file.path(directory,"hiring_challenge.csv")
hiring_challenge=read.csv(file = df,header = TRUE,na.strings = "?")
View(hiring_challenge)

#EDA:
#checking duplicates,na and null values
any(is.na(hiring_challenge))

anyDuplicated(hiring_challenge)

is.null(hiring_challenge)

sum(is.na.data.frame(hiring_challenge))

nrow(hiring_challenge)


#columns with numeric values which has na values are: c2,c14
hiring_challenge$C2[which(is.na(hiring_challenge$C2))]=28.46 #replacing na values with median
median(hiring_challenge$C2,na.rm = TRUE)

hiring_challenge$C14[which(is.na(hiring_challenge$C14))]=160 #replacing na values with median
median(hiring_challenge$C14,na.rm = TRUE)

#View(hiring_challengsummary(hiring_challenge)


#detecting outliers
boxplot.stats(hiring_challenge$C2)$out
boxplot.stats(hiring_challenge$c3)$out #no outliers
boxplot.stats(hiring_challenge$C8)$out
boxplot.stats(hiring_challenge$C11)$out
boxplot.stats(hiring_challenge$C14)$out
boxplot.stats(hiring_challenge$C15)$out


#which(hiring_challenge$C2 %in% c(x))
#hiring_challenge$C2[c(9,46,219,438,28,40, 45,61,106,142,173,218,384,529,29,119,152,184,213,235,269,301,435,528,571,91,131,158,165,207,222,297,346,349,406,486,503,517,540,551,574,586,587)]<-median(hiring_challenge$C2)


#find IQR :
summary(hiring_challenge$C2)
IQR_c2=34.75-22.67
IQR_c2#12.8
upfen_c2=34.75+1.5*IQR_c2
upfen_c2 #52.8,

summary(hiring_challenge$C8)
IQR_c8=1.0000-0.1650
IQR_c8#0.835
upfen_c8=1+1.5*IQR_c8
upfen_c8 #2.2525

summary(hiring_challenge$C11)
IQR_c11=3.000-0.0000
IQR_c11#3
upfen_c11=3+1.5*IQR_c11
upfen_c11 #7.5

summary(hiring_challenge$C14)
IQR_c14=272-80
IQR_c14#192
upfen_c14=272+1.5*IQR_c14
upfen_c14 #560

summary(hiring_challenge$C15)
IQR_c15=395-0
IQR_c15#395
upfen_c15=395+1.5*IQR_c15
upfen_c15 #987.5

# replacing oultier values of numeric columns by upfen value
cleaned_hiring=subset(hiring_challenge,C2<=52.8 & C8<=2.2425 & C11<=7.5 & C14<=560 & C15<=987.5)
boxplot(cleaned_df$C2,outline = FALSE)
boxplot(cleaned_df$C8,outline = FALSE)
boxplot(cleaned_df$C11,outline = FALSE)
boxplot(cleaned_df$C14,outline = FALSE)
boxplot(cleaned_df$C15,outline = FALSE)
colnames(cleaned_hiring)




#handling na vallues of categorical variable usinh knn alogirthm

#install.packages("VIM")
library(VIM)
#INSupdate.packages()

.libPaths()
??KNN

#replacing na values of categorical columns by knn method
cleaned_df=kNN(cleaned_hiring,variable = c("C1","C4","C5","C6","C7"),k=5)
View(cleaned_df)
colnames(cleaned_df)
sapply(cleaned_df,function(x)all(is.na(x)))


#corrlation plot
numeric_data=select(cleaned_df,is.numeric) #correlation plot
numeric_data
cor(numeric_data)
library(corrplot)
corrplot(numeric_data,method = "circle")

#univariate analysis
ggplot(cleaned_df) +
  aes(x = C1) +
  geom_bar(fill = "green")                     #histogram plot
ggplot(cleaned_df) +
  aes(x = C4) +
  geom_bar(fill = "green") 
ggplot(cleaned_df) +
  aes(x = C5) +
  geom_bar(fill = "green") 
ggplot(cleaned_df) +
  aes(x = C6) +
  geom_bar(fill = "green") 
ggplot(cleaned_df) +
  aes(x = C7) +
  geom_bar(fill = "green") 
ggplot(cleaned_df) +
  aes(x = C9) +
  geom_bar(fill = "green") 
ggplot(cleaned_df) +
  aes(x = C10) +
  geom_bar(fill = "green") 
ggplot(cleaned_df) +
  aes(x = C11) +
  geom_bar(fill = "green") 
ggplot(cleaned_df) +
  aes(x = C12) +
  geom_bar(fill = "green") 
ggplot(cleaned_df) +
  aes(x = C13) +
  geom_bar(fill = "green")    


#density plot for numeric variable
ggplot(cleaned_df) +
  aes(x = C2) +
  geom_density(adjust = 1L, fill = "blue") +
  theme_minimal() #most of the datapoints are skewed towards left.
#peak is over the value of 25. so concentation will lie between 25 to 30

ggplot(cleaned_df) +
  aes(x = C3) +                                                  
  geom_density(adjust = 1L, fill = "#0c4c8a") +
  theme_minimal() #skewed left .concentration will lie between 2.5 to 5. peak is over 2.5

ggplot(cleaned_df) +
  aes(x = C8) +
  geom_density(adjust = 1L, fill = "#0c4c8a") +
  theme_minimal() #skewwd left. concetration will be more likely at 0.25 to 0.5
ggplot(cleaned_df) +
  aes(x = C11) +
  geom_density(adjust = 2L, fill = "#0c4c8a")  #skewed left

  ggplot(cleaned_df) +
  aes(x = C14) +
  geom_density(adjust = 3L, fill = "#0c4c8a") + #skewed left. peak is at value 200. concentration will be more likely
    #between 200 to 300
  theme_minimal()
  
  ggplot(cleaned_df) +
    aes(x = C15) +
    geom_density(adjust = 9L, fill = "#0c4c8a") +
    theme_minimal() #skewed left
  
#bivariate analysis
  ggplot(cleaned_df) +
    aes(x = C1, fill = C1, weight = Hired) +
    geom_bar() +
    scale_fill_hue() +
    labs(x = "C1", y = "HIRED", title = " ") +
    theme_minimal()
  
  #barplot comparing numeric variable vs dep variable
  
  ggplot(cleaned_df) +
    aes(x = C4, fill = C4, weight = Hired) +
    geom_bar() +
    scale_fill_hue() +
    labs(x = "C4", y = "HIRED", title = " ") +
    theme_minimal()
  
  ggplot(cleaned_df) +
    aes(x = C5, fill = C5, weight = Hired) +
    geom_bar() +
    scale_fill_hue() +
    labs(x = "C5", y = "HIRED", title = " ") +
    theme_minimal()
  
  ggplot(cleaned_df) +
    aes(x = C6, fill = C6, weight = Hired) +
    geom_bar() +
    scale_fill_hue() +
    labs(x = "C6", y = "HIRED", title = " ") +
    theme_minimal()
  
  ggplot(cleaned_df) +
    aes(x = C7, fill = C7, weight = Hired) +
    geom_bar() +
    scale_fill_hue() +
    labs(x = "C7", y = "HIRED", title = " ") +
    theme_minimal()
  
  ggplot(cleaned_df) +
    aes(x = C7, fill = C7, weight = Hired) +
    geom_bar() +
    scale_fill_hue() +
    labs(x = "C7", y = "HIRED", title = " ") +
    theme_minimal()
  
  
  ggplot(cleaned_df) +
    aes(x = C9, fill = C9, weight = Hired) +
    geom_bar() +
    scale_fill_hue() +
    labs(x = "C9", y = "HIRED", title = " ") +
    theme_minimal()
  
  ggplot(cleaned_df) +
    aes(x = C10, fill = C10, weight = Hired) +
    geom_bar() +
    scale_fill_hue() +
    labs(x = "C10", y = "HIRED", title = " ") +
    theme_minimal()
  
  ggplot(cleaned_df) +
    aes(x = C12, fill = C12, weight = Hired) +
    geom_bar() +
    scale_fill_hue() +
    labs(x = "C12", y = "HIRED", title = " ") +
    theme_minimal()
  
  ggplot(cleaned_df) +
    aes(x = C13, fill = C13, weight = Hired) +
    geom_bar() +
    scale_fill_hue() +
    labs(x = "C13", y = "HIRED", title = " ") +
    theme_minimal()
  
  
#finding correlation between numeric variable and dependent variable:
cor(cleaned_df$C2,cleaned_df$Hired) #-0.05824658
cor(cleaned_df$C3,cleaned_df$Hired) #0.1218653
cor(cleaned_df$C8,cleaned_df$Hired) # 0.2773108
cor(cleaned_df$C11,cleaned_df$Hired) #0.3377351
cor(cleaned_df$C14,cleaned_df$Hired) #0.02823362
cor(cleaned_df$C15,cleaned_df$Hired) #0.2512331


prop=table(cleaned_df$Hired)/length(cleaned_df$Hired)
percent=prop*100
percent
library(dplyr)
#View(cleaned_df)
cleaned_df[cleaned_df$Hired==0,]$Hired="NOT HIRED"
cleaned_df[cleaned_df$Hired==1,]$Hired="HIRED"

x=subset(cleaned_df,select = c(2,3,8,11,14,15,16))
cor(x)
#observation:c3,c8,c11,c14,c15 shows negative correlation with target variable


#SCALING  /standardising with z-score
library(dplyr)

scaled_num_var=cleaned_df %>% select(C2,C3,C8,C11,C14,C15) #df contains scaled numeric variable
scaled_num_var=scale(scaled_num_var)
scaled_num_var
 #converting c9,c10,c12 to numeric
#two_lvl_cat=cleaned_df[c("C9","C10","C12","Hired")]
#two_lvl_cat$C9=as.factor(two_lvl_cat$C9)
#two_lvl_cat$C10=as.factor(two_lvl_cat$C10)
#two_lvl_cat$C12=as.factor(two_lvl_cat$C12)


#two_lvl_cat$C9=as.numeric(two_lvl_cat$C9)
#two_lvl_cat$C10=as.numeric(two_lvl_cat$C10)
#two_lvl_cat$C12=as.numeric(two_lvl_cat$C12)
rm(two_lvl_cat)

#creating dummy variables for catogical variables
library(dummies)
dummy_cat_var=subset(cleaned_df,select = c(1,4,5,6,7,9,10,12,13))
dummy_cat_var=dummy.data.frame(dummy_cat_var)


#finaL dataset for building the model:
final_df=cbind.data.frame(dummy_cat_var,scaled_num_var,cleaned_df$Hired)
colnames(final_df)[44]="Hired"
#ncol(final_df)
#final_df$Hired=as.factor(final_df$Hired)

#MODEL BUILDING

sample = sort(sample(nrow(final_df), nrow(final_df)*.7))
train=final_df[sample,]
train
test=final_df[-sample,]
test


log.model1 <- glm(Hired ~., data = train, family = binomial(link = "logit"))
summary(log.model)

log.model2=glm(Hired~C4u+C9f+C13p+C15,data = train,family = binomial(link = "logit"))
summary(log.model2)

library(caret)

#split <- createDataPartition(y = fina$Hired,p = 0.6,list = FALSE)
#new_train=train[split,]
#new_test=train[-split,]

model=glm(Hired~C4u+C9f+C13p+C15,data =train,family = binomial(link = "logit") )
summary(model)

log_pred=predict(model,newdata = test,type = "response")
log_pred_dr=ifelse(log_pred>0.5,1,0)
log_pred_dr
head(log_pred_dr,20)
View(log_pred_dr)

range(log_pred)
acc=table(test$Hired,log_pred>0.5)
acc
#precision:out of all predicted positive values, how many are acutaully positive:tp/(tp+fp)

p=16/(16+8) #0.64
#tpr:tp/(tp+fn)
recall=16/(16+8) #0.6666667
#f score: harmonic mean of precision and recall:
2*((p*recall)/(p+recall))

sum(diag(acc))/sum(acc) #0.8648649

(16+78)/(78+9+8+16) #0.8468468 #accuracy
(9+8)/(66+20+12+4) #0.166667 
16/(16+8)#0.6666667 #TPR/RECALL/SENISTIVITY



#ROC AND AUC CURVE:
library(ROCR)
library(Metrics)
pred=prediction(log_pred,test$Hired)
perf=performance(pred,measure = "tpr",x.measure = "fpr")
perf
a=performance(pred,"acc")
plot(a)

auc(test$Hired,log_pred) #0.8213602

plot(perf,colorize=TRUE)

#setting the thresold:
predict=ifelse(log_pred>0.2,1,0) #0.794502  At thresold value=0.1, we can say there are more  true positive rate
pr=prediction(predict,test$Hired)
auc(test$Hired,predict)

 
# At thresold value=0.1,0.2,0.3,0.4,0.5,0.6, we can say there are more  true positive rate
# at thresold 0.7,0.8 there is a decrease in true positive rate,
# for more true positive rate that is for more sensitivity, thresold should be low
