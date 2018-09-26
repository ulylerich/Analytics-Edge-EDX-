IBM <- read.csv("IBMStock.csv", header = T)
GE <- read.csv("GEStock.csv", header = T)
Procter <- read.csv("ProcterGambleStock.csv", header = T)
Coca <- read.csv("CocaColaStock.csv", header = T)
Boeing <- read.csv("BoeingStock.csv", header = T)

#convert date format in eac data set
IBM$Date <- as.Date(IBM$Date, "%m/%d/%y")
GE$Date <-  as.Date(GE$Date, "%m/%d/%y")
Coca$Date <- as.Date(Coca$Date, "%m/%d/%y")
Procter$Date = as.Date(Procter$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

#mean stock price of IBM
mean(IBM$StockPrice)

#summary statistic of GE, Coca, Boeing, Procter
summary(GE)
summary(Coca)
summary(Boeing)
summary(Procter)

#standard deviation of procter stock price
sd(Procter$StockPrice)

# plot coca cola stock
plot (x= Coca$Date, y = Coca$StockPrice, type = "l" ,col = "red" )

#add Procter Gamble to the graph
lines(Procter$Date, Procter$StockPrice)

#add vertical line
abline(v=as.Date(c("1983-01-01")), lwd=1)

#stock price in period 1995-2005
plot(Coca$Date[301:432], Coca$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(IBM$Date, IBM$StockPrice, col = "blue")
lines(GE$Date, GE$StockPrice , col = "black")
lines(Boeing$Date, Boeing$StockPrice, col = "green")

#add vertical line
abline(v=as.Date(c("1997-09-01")), lwd=1)

which (IBM$Date == "1997-09-01")
Boeing$StockPrice[334]

#Group stock price and months with tapply
IBM$month <- months(IBM$Date)
tapply(IBM$StockPrice, months(IBM$Date) , mean)
tapply(Boeing$StockPrice, months(Boeing$Date) , mean)
tapply(Coca$StockPrice, months(Coca$Date) , mean)
tapply(GE$StockPrice, months(GE$Date) , mean)
tapply(Procter$StockPrice, months(Procter$Date) , mean)
mean(GE$StockPrice)
mean(Coca$StockPrice)
