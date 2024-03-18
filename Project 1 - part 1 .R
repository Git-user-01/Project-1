#setwd()
table1 <- read.csv("Table1.csv",header=TRUE)

n <- paste(rep("X", 64),sep="",as.character(seq(1958,2022,1))) 

# x3 is personal income (in millions of dollars) 

x <- table1[2,n]
x2 <-  x[[1]]
lapply(2:length(x), function(i) {x2 <<- rbind(x2, x[[i]])})
#data.frame(x2)
x3 <- as.numeric(as.character(unlist(x2)))

# y3 is Personal current transfer receipts (in millions of dollars)

y <- table1[5,n]
y2 <-  y[[1]]
lapply(2:length(y), function(i) {y2 <<- rbind(y2, y[[i]])})
#data.frame(y2)
y3 <- as.numeric(as.character(unlist(y2)))

# proportion of Personal current transfer receipts that make up personal income 

r1 <- y3/x3
years <- seq(1958,2022,1) 
cbind(years,r1)
barplot(r1,names.arg=years)

# figure 1 - US government transfers to individuals from all programs over time (in trillions of dollars)

table6 <- read.csv("Table6.csv",header=TRUE)
y3 <- table6$Current.transfer.receipts.of.individuals.from.governments..thousands.of.dollars

y4 <- y3/1000000000
plot(years,y4, ylab = "Transfers from government (Trillions of dollars)", xlab="years")

# figure 2 - US government transfers to individuals from all programs over time (in billions of dollars) adjusted for inflation in 2022 dollars 

table2 <- read.csv("Table2.csv",header=TRUE) # import annual CPI-U values (in 2022)

# a1 is the ratio of CPI in each of those years relative to the CPI in the year of 2022 
# table2$CPI.U[(2022-1958)+1] the CPI in 2022 is 292.655 
a1 <- (table2$CPI.U[(2022-1958)+1])/table2$CPI.U

# multiply a1 by y3 to get the real value of the personal current transfer receipts over time 
y5 <- y3*a1 

y6 <- y5/1000000
plot(years,y6, ylab= "Transfers adj. for inflation (2022, Billions of dollars)", xlab ="years")

# figure 3 - US government transfers to individuals from all programs over time (in billions of dollars) adjusted for inflation in 2010 dollars 

table2a <- read.table("cpiai.txt", header=TRUE, sep="") # import annual CPI-U values (in 2012)
years2 <- seq(1958,2010,1) 

# a11 is the ratio of CPI in each of those years relative to the CPI in the year of 2010
# table2a$Annual[(2010-1958)+1] the CPI in 2010 is 218.056 
a11 <- (table2a$Annual[(2010-1958)+1])/table2a$Annual

# multiply a1 by y3 to get the real value of the personal current transfer receipts over time 
y3a <- y3[1:((2010-1958)+1)]
y5a <- y3a*a11
y6a <- y5a/1000000
plot(years2, y6a, ylab= "Transfers adj. for inflation (2010, Billions of dollars)", xlab ="years")

# figure 4 - adjust for inflation and population growth in 2022 

table3 <- read.csv("Table3.csv",header=TRUE) # import values for the population of the US 
a2 <- table3$Population 

#y7 is real dollars (in 2022 money) per capita 
y7 <- y5*1000
y8 <- y7/a2
plot(years,y8)

alpha1 <- cbind(years,y8)

# the amount of entitlement transfers to individuals from the government has nonupled since 1958 
alpha1[(2022-1958)+1,2]/alpha1[1,2]

# figure 5 - adjust for inflation and population growth in 2010 

#y71 is real dollars (in 2010 money) per capita 
a22 <- a2[1:((2010-1958)+1)]
y7a <- y5a*1000
y8a <- y7a/a22
plot(years2,y8a)

alpha2 <- cbind(years2,y8a)

# the growth of entitlement transfers after adjusting for population and inflation is about 4 percent (3.5 %) per annum

y8a1 <- y8a[((1960-1958)+1):((2010-1958)+1)]
years3 <- years2[((1960-1958)+1):((2010-1958)+1)]

m1 <- lm(log(y8a1) ~ years3)
summary(m1)

# the amount of entitlement transfers to individuals from the government has septupled from 2010 to 1960 
alpha2[(2010-1958)+1,2]/alpha2[(1960-1958)+1,2]

# in 2010 alone the government oversaw a transfer of over $ 2.2 trillion in money, goods and services to recipient men, women and children in the US ($ 2260351000000)

y3a <- y3[((1960-1958)+1):((2010-1958)+1)]
y3a1 <- y3a[(2010-1960)+1]*1000

# in 2010 the burden of entitlement programs cost more than $7,200 for every man, woman and child ($7307) adjusted for inflation (in 2010 dollars)

y8a[(2010-1958)+1]

# in 2022 alone the government oversaw a transfer of over $ 3.84 trillion in money, goods and services to recipient men, women and children in the US ($3846653000000)

y31 <- y3[(2022-1958)+1]*1000

# in 2022 the burden of entitlement programs cost slightly less than $ 8600 for every man, woman and child ($ 8599.56) adjusted for inflation (in 2022 dollars)

y8[(2022-1958)+1]

# proportion of outlays that are accounted for by entitlements (2010)

outlays <- read.csv("federal-outlays.csv",header=TRUE)

y9a <- y3a*1000
y10a <- outlays$Outlays
y11a <- y10a[c(1:((2010-1958)+1))] 
y12a <- y11a*1000000
r2 <- y9a/y12a 
cbind(years2,y9a,y12a, r2)
plot(years2,r2, ylab= "Proportion of government outlays spent on entitlements", xlab ="years")

# proportion of outlays that are accounted for by entitlements (2022)

y9 <- y3*1000
y10a <- outlays$Outlays
y12 <- y10a*1000000
r3 <- y9/y12
cbind(years,y9,y12, r3)
plot(years,r3, ylab= "Proportion of government outlays spent on entitlements", xlab ="years")

table7 <- read.csv("Table 7.csv", header=TRUE)
y13 <- table7$Consumption

# 2010 ratio 
y13a <- y13[((1958-1940)+1):((2010-1940)+1)]
y14a <- y3a/1000000
cbind(years2,y13a,y14a)

# 2022 
y13b <- y13[((1958-1940)+1):((2022-1940)+1)]
y14b <- y3/1000000

c1 <- cbind(years,y13b,y14b)

# ratio in the 1960 of non-entitlement to entitlement spending 
(c1[((1960-1958)+1),2] - c1[((1960-1958)+1),3])/c1[((1960-1958)+1),3] 
# ratio in the 2010 of non-entitlement to entitlement spending 
(c1[((2010-1958)+1),2] - c1[((2010-1958)+1),3])/c1[((2010-1958)+1),3] 
# ratio in the 2022 of non-entitlement to entitlement spending 
(c1[((2022-1958)+1),2] - c1[((2022-1958)+1),3])/c1[((2022-1958)+1),3] 

c2 <- cbind(years,(y13b-y14b)/y13b,y14b/y13b)

y15b <- (y13b-y14b)/y13b 
y16b <- y14b/y13b 

plot(years, y15b)
points(years, y16b, col="blue")

table8 <- read.csv("Table 8.csv", header=TRUE)
y17 <- table8$total
y18 <- table8$medicad
y19 <- table8$Income.maintenance.benefits

# total amount spent on medicad and income mantaince benefits 
y17[(2010-1958)+1] # in 2010 
y17[(2022-1958)+1] # in 2022

# how much has spending on medicad and income mantaince benefits increased from 1960 to 2010 (26 times from my calculations though the book reports over 30 times)
y17a <- y17[1:((2010-1958)+1)]
y18a <- y18[1:((2010-1958)+1)]
y19a <- y19[1:((2010-1958)+1)]
cbind(years2,y17a)
y17a[(2010-1958)+1]/(y17a[(1960-1958)+1]*a11[(1960-1958)+1])

y17b <- (y17a*a11)/1000000
y18b <- (y18a*a11)/1000000
y19b <- (y19a*a11)/1000000
plot(years2,y17b)
points(years2,y18b,col="blue") # medicad
points(years2,y19b,col="red") # income mantaince benefits 

# how much has spending on medicad and income mantaince benefits increased from 1960 to 2020 (29 times from my calculations)
y17[(2020-1958)+1]/(y17[(1960-1958)+1]*a1[(1960-1958)+1])

y17c <- (y17*a1)/1000000
y18c <- (y18*a1)/1000000
y19c <- (y19*a1)/1000000
plot(years,y17c)
points(years,y18c,col="blue") # medicad
points(years,y19c,col="red") # income mantaince benefits






