mydata = read.table("videodata.txt")
mydata = mydata[2:92,]
indx = c(1:15)
mydata[indx] <- lapply(mydata[indx], function(x) as.numeric(as.character(x)))
time = mydata$V1
playedornot = time
playedornot[playedornot != 0 ] = 1
mean(playedornot)
boot.population <- rep(playedornot, length.out = 314)
B = 400 # the number of bootstrap samples we want
boot.sample <- array(dim = c(B, 91))
for (i in 1:B) {
  boot.sample[i, ] <- sample(boot.population, size = 91, replace = FALSE)
}
boot.mean <- apply(X = boot.sample, MARGIN = 1, FUN = mean)
png()
hist(boot.mean, breaks = 20, probability = TRUE, density = 10, col = 2, border = 2)
lines(density(boot.mean, adjust = 2), col = 4)
dev.off()
png("scenario1_qq")
qqnorm(boot.mean)
qqline(boot.mean)
dev.off()
shapiro.test(boot.mean)
boot.sd <- sd(boot.mean)
mean(playedornot) + c(-1, 1)*1.96*boot.sd
freq = mydata$V4

summary(as.factor(freq))
#students expected to play during a normal week = 42.5 
summary(as.factor(playedornot))


boot.popindex <- rep(c(1:91), length.out = 314)
B = 400 # the number of bootstrap samples we want
boot.sampindex <- array(dim = c(B, 91))
boot.playedornot <- array(dim = c(B, 91))
boot.freq <- array(dim = c(B, 91))
expected_sum <- array(dim = c(B,1))
actual_sum <- array(dim = c(B, 1))
for (i in 1:B) {
  boot.sampindex[i, ] <- sample(boot.popindex, size = 91, replace = FALSE)
  boot.playedornot[i,] <- playedornot[boot.sampindex[i,]]
  boot.freq[i,] <- freq[boot.sampindex[i,]]
expected_sum[i] = length(which(boot.freq[i,]==1)) + length(which(boot.freq[i,]==2)) + 1/4*length(which(boot.freq[i,]==3)) + 1/24*length(which(boot.freq[i,]==4))
}
actual_sum = rowSums(boot.playedornot)
difference = actual_sum - expected_sum
png("scenario2.png")
hist(difference, breaks = 20, probability = TRUE, density = 10, col = 2, border = 2, main = "(Actual - Expected) #students")
lines(density(difference, adjust = 2), col = 3)
dev.off()

png("scenario2_qq.png")
qqnorm(difference)
qqline(difference)
dev.off()
shapiro.test(difference)

diff.sd <- sd(difference)
mean(difference) + c(-1, 1)*1.96*diff.sd

#Scenario 3
boot.time.population <- rep(time, length.out = 314)
boot.time <- array(dim = c(B, 91))
for (i in 1:B) {
  boot.time[i, ] <- sample(boot.time.population, size = 91, replace = FALSE)
}
avg.times = 1/91*(rowSums(boot.time))
png("scenario3.png")
hist(avg.times, breaks = 20, probability = TRUE, density = 10, col = 2, border = 2, main = "Average times")
lines(density(avg.times, adjust = 2), col = 3)
dev.off()

png("scenario3_qq.png")
qqnorm(avg.times)
qqline(avg.times)
dev.off()
shapiro.test(avg.times)

ss = var(time)
var.time.sample = ss*(314-91)/(91*(314))
sd.time.sample = sqrt(var.time.sample)
#Estimate from sample population
mean(time) + c(-1, 1)*1.96*sd.time.sample
#Estimate from simulation study 
mean(avg.times) +c(-1, 1)*1.96*sd(avg.times)

#Scenario 4
like = mydata$V2
like = like[like!= 99]
like = like[like!= 1]
like.bar = mean(like)
ss.like = var(like)
var.like.bar = ss.like*(314-89)/(89*(314))
sd.like.bar = sqrt(var.like.bar)
#Estimate of whether students like to play or not from sample population 
like.bar +c(-1, 1)*1.96*sd.like.bar

like_4 = like[like!= 5]
p = 0.27
se1 = sqrt(p*(1-p)*(314-82)/(81*(314)))
p +c(-1, 1)*1.96*se1

t = 0.01/(sqrt(1/82*((0.041)^2 + (0.0424)^2)))
p.value = 1 - pt(t, 162)

p = 0.33
se1 = sqrt(p*(1-p)*(314-91)/(90*(314)))
se1
p +c(-1, 1)*1.96*se1

t = 0.07/(sqrt(1/91*((0.0389)^2 + (0.0417)^2)))
t
p.value = 1 - pt(t, 180)
p.value

#Scenario 5 
like = mydata$V2
like_index = which(like != 1 & like != 99)
like_5 = like[like_index]
like_5[like_5 == 2|like_5 == 3] = 1
like_5[like_5 == 4|like_5 == 5] = 0
length(like_5)
unique(like_5)
sex = mydata$V7
sex_5 = sex[like_index]
work = mydata$V11
work_5 = work[like_index]
computer = mydata$V9
computer_5 = computer[like_index]
pc = mydata$V12
pc_5 = pc[like_index]

sex_5 = sex_5[- which(work_5 == 99)]
computer_5 = computer_5[- which(work_5 == 99)]
pc_5 = pc_5[- which(work_5 == 99)]
like_5 = like_5[- which(work_5 == 99)]
work_5 = work_5[- which(work_5 == 99)]
work_5[work_5 != 0] = 1

scenario5 = data.frame(like_5, pc_5, sex_5, work_5, computer_5)


data.tree <- tree(like_5 ~pc_5+sex_5+computer_5+work_5, data=scenario5)
png("scen5.png")
plot(data.tree, type="uniform")
text(data.tree)
dev.off()
CrossTable(like_5, pc_5, prop.r = F, prop.c = F, prop.t = F)


