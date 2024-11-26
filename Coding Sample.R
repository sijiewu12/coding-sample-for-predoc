Coding Sample R
##import data set
library(readxl)
mydata <- read_excel('/Users/wsjsmac/Desktop/23-24-1/Regression_Analysis/Report/data.xlsx')
#View(mydata)

head(mydata)
summary(mydata)
library(VIM)
aggr(mydata)


any(is.na(mydata))
is.na(mydata)


##Prbl 1 deal with missing values
library(visdat)
vis_dat(mydata)
vis_miss(mydata)

library(VIM)
matrixplot(mydata, sortby = "Year")


colnames(mydata)[2] <- "Y"
colnames(mydata)[3] <- "x1"
colnames(mydata)[4] <- "x2"
colnames(mydata)[5] <- "x3"
colnames(mydata)[6] <- "x4"
colnames(mydata)[7] <- "x5"
colnames(mydata)[8] <- "x6"
colnames(mydata)[9] <- "x7"
colnames(mydata)[10] <- "x8"


# install and show Hmisc 包
#install.packages("Hmisc")
library(Hmisc)

# EM Algorithmic interpolation
data_imputed_em <- aregImpute(Y~x1+x2+x3+x4+x5+x6+x7+x8, data = mydata)

# Output the data frame after interpolation
print(data_imputed_em)
data_imputed_em$imputed

# After checking the data frame, it is found that the data is inconsistent with the reality, and the following two methods are adopted (mean interpolation and regression interpolation).

mean(c(1832.9, 4753.8, 4517, 4753.8, 3120.6))



##### interpolation: mean
library(Hmisc)
#mydata$x2=impute(mydata$x2, mean)  # mean interpolation
#mydata$x6=impute(mydata$x6, mean)
#mydata$x7=impute(mydata$x7, mean)

mean(mydata$x2)
mean(mydata$x6)
mean(mydata$x7)
library(psych)
describe(mydata$x2)
describe(mydata$x6)
describe(mydata$x7)

vis_miss(mydata)
matrixplot(mydata, sortby = "Year")

summary(mydata)

#library(xlsx)
#write.table(mydata, "Desktop/23-24-1/Regression_Analysis/Report/data.txt")



#-----------------------
plot(mydata$Year[1:43],mydata$x2[1:43],main ="Fitting Graph of Year and Investment (Actual)", xlab="Year", ylab="Investment" )
plot(mydata$Year[1:43],log(mydata$x2[1:43]),main ="Fitting Graph of Year and Investment (log)", xlab="Year", ylab="Investment (log)")
result<-lm(log(mydata$x2[1:43])~mydata$Year[1:43])
result
summary(result)
abline(result)




xx <- mydata$Year[1:43]
yy <- log(mydata$x2[1:43])
yy
m1 <- lm(yy ~ xx + I(xx ^2))
m1
summary(m1)

library(ggplot2)
df1 <- data.frame(yy,xx)
p <- ggplot(df1, aes(xx, yy))+
  geom_point(shape = 1, size = 2, stroke = 1)
p

#p <- ggplot(df1, aes(x, log(y)))+
#  geom_point(shape = 1, size = 3, col="orange",stroke = 1)+
#  geom_smooth(method = "glm", col="red")

# Fit the quadratic polynomial regression model

p + 
  geom_smooth(method = "lm", formula = y ~ x + I(x ^ 2))+
  ggtitle("Quadratic Fitting Graph of Year and Investment (log)")+
  xlab("Year") + ylab("Investment (log)")


#----------------- Make predictions and complete missing values
m1 <- lm(yy ~ xx + I(xx ^ 2))
m1

newd <- data.frame(xx = 1979)
pred <- predict.lm(m1,newd)
pred
exp(pred)
mydata$x2[44] <-exp(pred)

newd <- data.frame(xx = 1978)
pred <- predict.lm(m1,newd)
pred
exp(pred)
mydata$x2[45] <-exp(pred)




xx <- mydata$Year
yy <- log(mydata$x2)
df1 <- data.frame(yy,xx)
p <- ggplot(df1, aes(xx, yy))+
  geom_point(shape = 1, size = 2, stroke = 1)
p + 
  geom_smooth(method = "lm", formula = y ~ x + I(x ^ 2))+
  ggtitle("Quadratic Fitting Graph of Year and Investment (log)")+
  xlab("Year") + ylab("Investment (log)")





#-------------
#x7
plot(mydata$Year[1:43],mydata$x7[1:43],main ="Fitting Graph of Year and Engel's Coefficient", xlab="Year", ylab="Engel's Coefficient (%)" )
result<-lm(mydata$x7[1:43]~mydata$Year[1:43])
summary(result)
abline(result)

library(sqldf)

xx <- as.list(sqldf("select Year from mydata where Year != 1979"))
yy <- as.list(sqldf("select x7 from mydata where Year != 1979"))

x22 <- xx$Year[1:44]
y22 <- yy$x7[1:44]
m2 <- lm(y22 ~ x22 + I(x22 ^2))
summary(m2)
# Found that the quadratic term is not significant, replaced by the cubic term
m3<- lm(y22 ~ x22 + I(x22 ^2) + I(x22 ^3))
summary(m3)

library(ggplot2)
df2 <- data.frame(y22,x22)
p <- ggplot(df2, aes(x22, y22))+
  geom_point(shape = 1, size = 2, stroke = 1)
p
p + 
  geom_smooth(method = "glm", formula = y ~ x + I(x ^ 2)+ I(x ^3))+
  ggtitle("Trinomial Fitting Graph of Year and Engel's Coefficient")+
  xlab("Year") + ylab("Engel's Coefficient")

#----------------- Make predictions and complete missing values

m3<- lm(y22 ~ x22 + I(x22 ^2) + I(x22 ^3))
m3

newd <- data.frame(x22 = 1979)
pred <- predict.lm(m3,newd)
pred
mydata$x7[44] <-pred
ggplot(data.frame(mydata$Year, mydata$x7), aes(mydata$Year, mydata$x7))+
  geom_point(shape = 1, size = 2, stroke = 1)+ 
  geom_smooth(method = "glm", formula = y ~ x + I(x ^ 2)+ I(x ^3))+
  ggtitle("Trinomial Fitting Graph of Year and Engel's Coefficient")+
  xlab("Year") + ylab("Engel's Coefficient")




#---------------
#x6
plot(mydata$Year[1:34],mydata$x6[1:34],main ="Fitting Graph of Year and PPI", xlab="Year", ylab="PPI (Last Year = 100)" )
result<-lm(mydata$x6[1:34]~mydata$Year[1:34])
summary(result)
abline(result)


xx <- as.list(sqldf("select Year from mydata where Year >= 1989"))
yy <- as.list(sqldf("select x6 from mydata where Year >= 1989"))
x44 <- xx$Year[1:34]
y44 <- yy$x6[1:34]

library(ggplot2)
df4 <- data.frame(y44,x44)
p <- ggplot(df4, aes(x44, y44))+
  geom_point(shape = 1, size = 2, stroke = 1)
p

m4<-lm(y44~x44)
m4

newd <- data.frame(x44 = 1988)
pred <- predict.lm(m4,newd)
pred
mydata$x6[44] <-pred


for (i in 1988:1977) {
  a = 2023-i
  newd <- data.frame(x44 = mydata$Year[a])
  pred <- predict.lm(m4,newd)
  pred
  mydata$x6[a] <-pred
  i = i-1
}

ggplot(data.frame(mydata$Year, mydata$x6), aes(mydata$Year, mydata$x6))+
  geom_point(shape = 1, size = 2, stroke = 1)+ 
  geom_smooth(method = "glm", formula = y ~ x)+
  ggtitle("Fitting Graph of Year and PPI")+
  xlab("Year") + ylab("PPI")







vis_miss(mydata)
matrixplot(mydata, sortby = "Year")

write.table(mydata, "Desktop/23-24-1/Regression_Analysis/Report/data.txt")


summary(mydata$Y)
summary(mydata$x1)
summary(mydata$x2)
summary(mydata$x3)
summary(mydata$x4)
summary(mydata$x5)
summary(mydata$x6)
summary(mydata$x7)
summary(mydata$x8)


###Prbl 2 time series analysis
# The logic is that GDP has a lag effect, so prove first; And then how do we solve this lag effect on the model - add it to the model

plot(mydata$Year[1:43],mydata$Y[1:43],main ="Graph of Year and GDP", xlab="Year", ylab="GDP" )


# First, GDP autoregression
GDP <- mydata$Y[45:1]
tsGDP <- ts(GDP)
plot(tsGDP)
a<-ar(tsGDP) ; a
b<-ar(tsGDP,method = "burg");b


acf(tsGDP)$acf
pacf(tsGDP)$acf


predict(a,10,n.ahead=5L)
tsp<-predict(a,n.ahead=50L)
tsp


# Secondly, the lag term of GDP is brought into the model regression
#2-stage lag
mm <- lm(log(Y)~ log(x1) +log(x2)+log(x3+701.4+1)+x4+log(x5)+x6+x7+log(x8), data = mydata)
summary(mm)

Y_1 = mydata$Y[2:45]
mydata2 <- data.frame(Year = mydata$Year[1:44], 
                      Y = mydata$Y[1:44],
                      x1 = mydata$x1[1:44],
                      x2 = mydata$x2[1:44],
                      x3 = mydata$x3[1:44],
                      x4 = mydata$x4[1:44],
                      x5 = mydata$x5[1:44],
                      x6 = mydata$x6[1:44],
                      x7 = mydata$x7[1:44],
                      x8 = mydata$x8[1:44],
                      Y_1 = mydata$Y[2:45])
mm2 <- lm(log(Y)~ log(Y_1)+log(x1) +log(x2)+log(x3+701.4+1)+x4+log(x5)+x6+x7+log(x8), data = mydata2)
summary(mm2)


Y_2 = mydata$Y[3:45]
mydata3 <- data.frame(Year = mydata$Year[1:43], 
                      Y = mydata$Y[1:43],
                      x1 = mydata$x1[1:43],
                      x2 = mydata$x2[1:43],
                      x3 = mydata$x3[1:43],
                      x4 = mydata$x4[1:43],
                      x5 = mydata$x5[1:43],
                      x6 = mydata$x6[1:43],
                      x7 = mydata$x7[1:43],
                      x8 = mydata$x8[1:43],
                      Y_1 = mydata$Y[2:44],
                      Y_2 = mydata$Y[3:45])
mm3 <- lm(log(Y)~ log(Y_2)+log(Y_1)+log(x1) +log(x2)+log(x3+701.4+1)+x4+log(x5)+x6+x7+log(x8), data = mydata3)
summary(mm3)


###Prbl 3 Granger Causality Test
library (lmtest)
#x1
result1 <- grangertest(log(Y)~log(x1), order = 1, data = mydata)
result1
result2 <- grangertest(log(x1) ~ log(Y), order = 1, data = mydata)
result2
result1 <- grangertest(Y~x1, order = 1, data = mydata)
result1
result2 <- grangertest(x1 ~ Y, order = 1, data = mydata)
result2
#x2
result3 <- grangertest(log(Y)~log(x2), order = 1, data = mydata)
result3
result4 <- grangertest(log(x2) ~ log(Y), order = 1, data = mydata)
result4
#x3
result5 <- grangertest(log(Y)~log(x3+705.1), order = 1, data = mydata)
result5
result6 <- grangertest(log(x3+705.1) ~ log(Y), order = 1, data = mydata)
result6
#x4
result7 <- grangertest(log(Y)~x4, order = 1, data = mydata)
result7
result8 <- grangertest(x4 ~ log(Y), order = 1, data = mydata)
result8
#x5
result9 <- grangertest(log(Y)~log(x5), order = 1, data = mydata)
result9
result10 <- grangertest(log(x5) ~ log(Y), order = 1, data = mydata)
result10
#x6
result11 <- grangertest(log(Y)~x6, order = 1, data = mydata)
result11
result12 <- grangertest(x6 ~ log(Y), order = 1, data = mydata)
result12
#x7
result13 <- grangertest(log(Y)~x7, order = 1, data = mydata)
result13
result14 <- grangertest(x7 ~ log(Y), order = 1, data = mydata)
result14
#x8
result15 <- grangertest(log(Y)~log(x8), order = 1, data = mydata)
result15
result16 <- grangertest(log(x8) ~ log(Y), order = 1, data = mydata)
result16

