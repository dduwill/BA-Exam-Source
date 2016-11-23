#Load .csv file
data <- read.csv("C:/Users/weiyi/Desktop/R/exam2/Q2/CasinoA.csv", header = T, stringsAsFactors = T)
str(data)
summary(data)

#Explore "Source"
data$Source
unique(data$Source)
table(data$Source) 

#Change the data type for Source to a factor variable
data$Source <- factor(data$Source)
str(data)

#Create dummy variables for each of the categories
data$AAA <- as.logical(0)
data$WALK <- as.logical(0)
data$WEB <- as.logical(0)
str(data)  

for (i in 1:nrow(data)){
  if (data$Source[i]=="AAA")
    data$AAA[i]<-1
  else if (data$Source[i]=="WALK")
    data$WALK[i]<-1
  else
    data$WEB[i]<-1}

View(data)   

model1 <- lm(data$Total.Spend ~ data$Gender + data$Age + data$AAA + data$WALK)
summary(model1)

model2 <- lm(data$Total.Spend ~ data$Age + data$WEB + data$AAA)
summary(model2)
model2
for (i in 1:nrow(data)){
  if (data$Source[i]=="WEB")
    data$WEB[i]<-1
  else if (data$Source[i]=="WALK")
    data$WALK[i]<-1
  else
    data$AAA[i]<-1}

View(data)  

model2 <- lm(data$Total.Spend ~ data$Gender + data$Age + data$WEB + data$WALK)
summary(model2)

#Correlation analysis
sapply(data, is.numeric)
num_data <- data[, sapply(data, is.numeric)]
cor(num_data, use = "complete.obs", method = "pearson")

#Linear regression model
model1 <- lm(data$Total.Spend ~ data$Gender + data$Age + data$WEB + data$WALK)
summary(model1)

model2 <- lm(data$Total.Spend ~ data$Age + data$AAA + data$WALK)
summary(model2)

#Subset age
age20_30 <- subset(data, data$Age <= 30)
summary(age20_30)
sum(age20_30$Total.Spend)

age30_40 <- subset(data, data$Age > 30 & data$Age <= 40)
summary(age30_40)
sum(age30_40$Total.Spend)

age40_50 <- subset(data, data$Age > 40 & data$Age <= 50)
summary(age40_50)
sum(age40_50$Total.Spend)

age50_60 <- subset(data, data$Age > 50 & data$Age <= 60)
summary(age50_60)
sum(age50_60$Total.Spend)

age60 <- subset(data, data$Age > 60)
summary(age60)
sum(age60$Total.Spend)

player.web <- subset(data, data$Source == "WEB")
summary(player.web)
player.aaa <- subset(data, data$Source == "AAA")
summary(player.aaa)
player.walk <- subset(data, data$Source == "WALK")
summary(player.walk)
