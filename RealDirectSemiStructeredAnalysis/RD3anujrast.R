install.packages('gdata')
library('gdata')

bk <- read.xls(perl = 'C:/Perl64/bin/perl.exe',xls = "rollingsales_brooklyn.xls",pattern="BOROUGH")
head(bk)
summary(bk)

#Converting sales price from factor to numeric
bk$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","",bk$SALE.PRICE))

#Converting the names to lower order
names(bk) <- tolower(names(bk))

bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",bk$gross.square.feet))
bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","",bk$land.square.feet))

bk$sale.date <- as.Date(bk$sale.date)
bk$year.built <- as.numeric(as.character(bk$year.built))

summary(bk$sale.price)
## do a bit of exploration to make sure there's not anything
## weird going on with sale prices

hist(bk$sale.price.n, main = 'hist(bk$sale.price.n)')
hist(bk$sale.price.n[bk$sale.price.n>0], main = 'hist(bk$sale.price.n[bk$sale.price.n>0])')
hist(bk$gross.sqft[bk$sale.price.n==0], main = 'hist(bk$gross.sqft[bk$sale.price.n==0])')

## keep only the actual sales
bk.sale <- bk[bk$sale.price.n!=0,]

plot(bk.sale$gross.sqft,bk.sale$sale.price.n, main = 'plot(bk.sale$gross.sqft,bk.sale$sale.price.n)')
plot(log(bk.sale$gross.sqft),log(bk.sale$sale.price.n), main = 'plot(log(bk.sale$gross.sqft),log(bk.sale$sale.price.n))')


## for now, let's look at 1-, 2-, and 3-family homes
bk.homes <- bk.sale[which(grepl("FAMILY", bk.sale$building.class.category)),]

plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n) ,main = 'plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n))')

## remove outliers that seem like they weren't actual sales
bk.homes$outliers <- (log(bk.homes$sale.price.n) <=5) + 0
bk.homes <- bk.homes[which(bk.homes$outliers==0),]

plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n), main = 'plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n))')

#Plotting the graph to view the change in property sale prices
plot(bk.sale$sale.date, bk.sale$sale.price.n ,  main = 'Variation in Property Prices', xlab = 'TimeSpan', ylab = 'sale prices', col = 'blue', pch = 15)
abline(lm(bk.sale$sale.date~bk.sale$sale.price.n), col = 'red')

#Determining the distribution of tax class category in Buildings
TaxClassvsBuildings <- table(bk.sale$building.class.category, bk.sale$tax.class.at.present)
TaxPayerDistribution <- table(bk.sale$tax.class.at.present)
barplot(TaxPayerDistribution, las = 1, main = 'Tax Payer Category Distribution')
box()

#Tax Class Category 1 Distribution in NeighbourHoods
TaxClassCategory1 <- table(bk.sale$neighborhood[bk.sale$tax.class.at.present == 1])
TaxCategoryDistribution <- subset(TaxClassCategory1, TaxClassCategory1>150)
barplot(TaxCategoryDistribution, las = 2, cex.names = 0.5, main = 'Areas With maximum Population of Tax Class Category1')
box()

#Analysing the sales prices for Tax Class Category1 where sales price are more than average
AreasWithHighPrice <- subset(bk.sale, bk.sale$sale.price.n> mean(bk.sale$sale.price.n) & bk.sale$tax.class.at.present == 1)
MoreThanAvgNeighbour <- table(AreasWithHighPrice$neighborhood)
Result1 <- subset(MoreThanAvgNeighbour, MoreThanAvgNeighbour > 50)
barplot(Result1, las = 2, cex.names = 0.5, main = 'Areas With above average prices occupied by Tax Category 1') 


#Analysing the sales prices for Tax Class Category1 where sales prices are least

AreasWithLeastPrice <- subset(bk.sale, bk.sale$sale.price.n < mean(bk.sale$sale.price.n) & bk.sale$tax.class.at.present == 1)
LessThanAvgNeighbour <- table(AreasWithLeastPrice$neighborhood)
Result2 <- subset(LessThanAvgNeighbour, LessThanAvgNeighbour > 50)
barplot(Result1, las = 2, cex.names = 0.5, main = 'Areas With less prices occupied by Tax Category 1') 

#Maximum Selling price Correspondng to TaxPayer
plot(bk.sale$tax.class.at.present, bk.sale$sale.price.n, las = 1, pch = 18, xlab = 'Tax payer Categrory', main = 'TaxPayerCategory vs Selling Price' )

#Mean Salary For Tax Class Category 1
taxCat1 = subset(bk.sale, bk.sale$tax.class.at.present == 1)
mean(taxCat1$sale.price.n)