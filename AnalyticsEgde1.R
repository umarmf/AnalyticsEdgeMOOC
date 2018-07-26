hoursInYear = 365*24
hoursInYear
##All Variables
ls()

# Vectors
c(3,2,5,6,7)

Country = c("Brazil","USA","India") 
Country[2]

LifeExpantancy = c("65","66","67")
LifeExpantancy[2]

seq(1,100,2)

CountryData = data.frame(Country,LifeExpantancy)
CountryData

CountryData$Population = c(99,999,9999)

## Adding 2 Data Frames
Country = c("France","China","UK")
LifeExpantancy = c("55","45","37")
Population = c(45,45,45)

NewCountryData = data.frame(Country,LifeExpantancy,Population)
NewCountryData

AllCountryData = rbind(CountryData,NewCountryData)
AllCountryData

## Loading Files


WHO =read.csv("WHO.csv")
WHO
head(WHO)
str(WHO)
summary(WHO)

WHO_Europe = subset(WHO, Region=="Europe") 
str(WHO_Europe)
write.csv(WHO_Europe,"WHO_Europe.csv")
ls()
rm(WHO_Europe)

## Exploring the WHO data set
WHO$Under15
nrow(WHO)
mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)

which.min(WHO$Under15)
WHO$Country[which.min(WHO$Under15)]
which.max(WHO$Under15)
WHO$Country[124]

plot(WHO$GNI,WHO$FertilityRate)
Outliers = subset(WHO,GNI > 10000 & FertilityRate > 2.5)
nrow(Outliers)
Outliers[c("Country","GNI","FertilityRate")]

mean(WHO$Over60)
which.min(WHO$Over60)
WHO$Country[183]

which.max(WHO$LiteracyRate)
WHO$Country[44]
hist(WHO$CellularSubscribers)

boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab = "",ylab="Life Expetancy", main = "Life Expectancy of Countries by Region")
table(WHO$Region)

names(WHO)
tapply(WHO$Over60,WHO$Region,mean)
tapply(WHO$LiteracyRate,WHO$Region, min,na.rm="TRUE")
tapply(WHO$ChildMortality,WHO$Region,min,na.rm="TRUE")

## USDA National Nutrient Database for Standard Reference
getwd()
USDA = read.csv("USDA.csv")
str(USDA)
USDA$Sodium
which.max(USDA$Sodium)
names(USDA)
USDA$Description[265]

HighSodium = subset(USDA,Sodium >10000)
nrow(HighSodium)
HighSodium$Description
match("CAVIAR",USDA$Description)
USDA$Sodium[4154]
summary(USDA$Sodium)
sd(USDA$Sodium,na.rm="TRUE")

## Plots
plot(USDA$Protein,USDA$TotalFat,xlab="Protein",ylab="Fat",main = "Protein vs Fat", col = "red")
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of vitamin C levels", xlim=c(0,100), breaks =2000)
boxplot(USDA$Sugar, main = "Boxplot of Sugar Levels", ylab ="Sugar (g)")

## Adding Variables
USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = "TRUE"))
str(HighSodium)
str(USDA)
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm = "TRUE"))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm = "TRUE"))
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm = "TRUE"))

table(USDA$HighSodium, USDA$HighFat)

tapply(USDA$Iron, USDA$HighProtein, mean , na.rm = "T")
tapply(USDA$VitaminC, USDA$HighCarbs, summary , na.rm = T)
