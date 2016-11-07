#Combining the monthly tables Together

filepath = 'B:/UB_CS/DIC/DICProject/NYT/'
m1 = list.files(path = filepath, pattern = '*.csv')
completeTable = read.table('B:/UB_CS/DIC/DICProject/NYT/nyt1.csv',header = T, sep = ',')
for(i in 2:31){
  tempfilepath = paste(filepath,m1[i],sep = "")
  s2 = read.table(tempfilepath,header = T, sep = ',')
  completeTable = rbind(completeTable,s2)
}

head(completeTable)

#Converting Gender section to factor dataset
completeTable$Gender <- as.factor(completeTable$Gender)

#Click Through Rate vs Impressions based on AgeCateogry
filepath1 = 'B:/UB_CS/DIC/DICProject/NYT/nyt1.csv'
filepath2 = 'B:/UB_CS/DIC/DICProject/NYT/nyt10.csv'
filepath3 = 'B:/UB_CS/DIC/DICProject/NYT/nyt16.csv'
filepath4 = 'B:/UB_CS/DIC/DICProject/NYT/nyt22.csv'
filepath5 = 'B:/UB_CS/DIC/DICProject/NYT/nyt29.csv'

sampleTable = read.table(filepath1, header = T, sep = ',')
tempTable = read.table(filepath2, header = T, sep = ',')
sampleTable = rbind(sampleTable, tempTable)
tempTable = read.table(filepath3, header = T, sep = ',')
sampleTable = rbind(sampleTable, tempTable)
tempTable = read.table(filepath4, header = T, sep = ',')
sampleTable = rbind(sampleTable, tempTable)
tempTable = read.table(filepath5, header = T, sep = ',')
sampleTable = rbind(sampleTable, tempTable)
   
head(sampleTable)
sampleTable$agecat1 <-cut(sampleTable$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
install.packages('doBy')
install.packages('ggplot2')
library(ggplot2)
library('doBy')
sampleTable$hasimps <-cut(sampleTable$Impressions,c(-Inf,0,Inf))
ggplot(subset(sampleTable, sampleTable$Impressions>0), aes(x=Clicks/Impressions, colour=agecat1)) + geom_density()
ggplot(subset(sampleTable, sampleTable$Clicks>0), aes(x=agecat1, y=Clicks,fill=agecat1)) + geom_boxplot()
ggplot(subset(sampleTable, sampleTable$Clicks>0), aes(x=Clicks/Impressions,colour=agecat1)) + geom_density()



#Distributing the age category
completeTable$AgeCat = cut(completeTable$Age, breaks = c(-Inf,18,24,34,44,54,64,Inf), labels = c('<18','18-24','25-34','35-44','45-54','55-64','65+'))

#Calculating the ClickThorughRate
completeTable$CTR = (completeTable$Clicks/completeTable$Impressions) 

#Graph and statistics to show how many females and males <18 are logged IN
completeTable$Signed_In = as.factor(completeTable$Signed_In)
table1 = table(completeTable$Signed_In[completeTable$AgeCat == '<18'],completeTable$Gender[completeTable$AgeCat == '<18'])
table1
barplot(table1, beside = T, legend.text = c('Not-SighnedIn', 'SighnedIn'), xlab = 'Gender' , axes = F, las = 1, names.arg = c('F','M'), main = '<18 Females and Males sighned In Distribution')
box()


#Categorise Users Based on Thier Click Behavior
summary(completeTable$Clicks)
completeTable$UserClickBehav = cut(completeTable$Clicks, breaks = c(-Inf,0,Inf), labels = c('NoClicks','Clicks'))
levels(completeTable$UserClickBehav)
table2 <- table(completeTable$UserClickBehav[completeTable$Signed_In == 1],completeTable$Gender[completeTable$Signed_In == 1])
barplot(table2, beside = T, main = 'signed in and Click', las = 1 , xlab = 'Gender', names.arg = c('F','M'),legend.text = c('NoClicks','Clicks'))
box()

#Plotting Impressions based on User Click Behavior and Gender
boxplot(completeTable$Impressions~completeTable$UserClickBehav*completeTable$Gender,las = 2, main = 'Impressions~ [ClickBehavior And Gender]')


#summary of monthly data
summary(completeTable)