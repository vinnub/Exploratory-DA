#Reading from txt
mydata = read.table("babies.txt")
mydata = mydata[2:1237,]
indx = c(1,2,4,5,6,8)
mydata[indx] <- lapply(mydata[indx], function(x) as.numeric(as.character(x)))
colnames(mydata) = c("BWT", "Gestation", "Parity", "Age", "Height", "Weight", "Smoke","BMI")
#Missing data
miss_ges = which(mydata$Gestation == 999 )
miss_smoke = which(mydata$Smoke == 9)
miss_age = which(mydata$Age == 99)
miss_weight = which(mydata$Weight == 999 )
miss_height = which(mydata$Height == 99)
miss_ges_smoke = union(miss_ges, miss_smoke)
miss_weight_height = union(miss_height, miss_weight)
miss_g_s_a = union(miss_ges_smoke, miss_age)
miss = union(miss_g_s_a, miss_weight_height)
#Final unfiltered(only missing data deleted) dataset 
final_data = mydata[- miss,]

#Unfiltered Smoker vs Non Smoker data 
index_smoke = which(final_data$Smoke ==1)
index_no_smoke = which(final_data$Smoke == 0 )
smoker_data = final_data[index_smoke,]
non_smoker_data = final_data[index_no_smoke,]

#Gestation Filtered Data 
index_normal_gest =  which(final_data$Gestation>= 259 & final_data$Gestation <= 294)
data_normal_gest = final_data[index_normal_gest,]
index_normal_bmi =  which(data_normal_gest$BMI>= 18.5 & data_normal_gest$BMI <= 25)
data_normal = data_normal_gest[index_normal_bmi,]

index_smoke_filtered = which(data_normal$Smoke ==1)
smoker_data_filtered = data_normal[index_smoke_filtered,]
non_smoker_data_filtered = data_normal[ - index_smoke_filtered,]

#Age filtered Data 
index_age_group_1 = which(data_normal$Age <= 19)
index_age_group_2 = which(data_normal$Age >= 20 & data_normal$Age <= 34)
index_age_group_3 = which(data_normal$Age >= 35)
data_age_group_1 = data_normal[index_age_group_1,]
data_age_group_2 = data_normal[index_age_group_2,]
data_age_group_3 = data_normal[index_age_group_3,]

index_smoke_group_1 = which(data_age_group_1$Smoke ==1)
index_smoke_group_2 = which(data_age_group_2$Smoke ==1)
index_smoke_group_3 = which(data_age_group_3$Smoke ==1)

smokers_data_group_1 = data_age_group_1[index_smoke_group_1,]
non_smokers_data_group_1 = data_age_group_1[ - index_smoke_group_1,]
smokers_data_group_2 = data_age_group_2[index_smoke_group_2,]
non_smokers_data_group_2 = data_age_group_2[ - index_smoke_group_2,]
smokers_data_group_3 = data_age_group_3[index_smoke_group_3,]
non_smokers_data_group_3 = data_age_group_3[ - index_smoke_group_3,]

#Printing summary in a file 
sink("summary")
print("Smokers all ages")
summary(smoker_data_filtered$BWT)
print("Non Smokers all ages")
summary(non_smoker_data_filtered$BWT)
print("Smokers ages <= 19")
summary(smokers_data_group_1$BWT)
print("Non Smokers ages <= 19")
summary(non_smokers_data_group_1$BWT)
print("Smokers ages 20-34")
summary(smokers_data_group_2$BWT)
print("Non Smokers ages 20-34")
summary(non_smokers_data_group_2$BWT)
print("Smokers ages >= 35")
summary(smokers_data_group_3$BWT)
print("Non Smokers ages >= 35")
summary(non_smokers_data_group_3$BWT)
sink()

#Generating Histograms 
sf = smokers_data_group_3$BWT
nsf = non_smokers_data_group_3$BWT
temp = 10
png()
hist(nsf, col= "red", xlim = c(50,180), ylim = c(0,0.05),freq = FALSE,breaks = temp, xlab = "BWT", main = "Smokers vs Non-smokers (all ages)" )
 hist(sf, col=rgb(0, 0, 1, 0.75) , xlim = c(50,180), ylim = c(0,0.05),freq = FALSE,breaks = temp, add = T)
legend('topright', c("Non-smokers", "Smokers"),fill=c("red", rgb(0,0,1)), cex = 1.5)
dev.off()

#Generating boxplots
png()
boxplot(sf, nsf, names = c("Smokers", "Non smokers"))
title("Smokers vs non smokers (Age group 3)")
dev.off()

#Generating qq-norm plots 
png()

par(mfrow=c(2,1))
qqnorm(smoker_data_filtered$BWT, main = "Smoker data")
qqline(smoker_data_filtered$BWT)
#title("Smoker data")
qqnorm(non_smoker_data_filtered$BWT, main = "Non-smoker data")
qqline(non_smoker_data_filtered$BWT)
#title("Non smoker data")
dev.off()