setwd("C:/Users/drdre/Onedrive/INLS625")
arbuthnot <- read.csv("Arbuthnot.csv")
str(arbuthnot)
dim(arbuthnot)
names(arbuthnot)
arbuthnot$Males
plot(arbuthnot$Year,arbuthnot$Males)
plot(arbuthnot$Males, arbuthnot$Females)
plot(x=arbuthnot$Year, y=arbuthnot$Males, type='h')
plot(x=arbuthnot$Year, y=arbuthnot$Males, type='l')

arbuthnot$Males/(arbuthnot$Males+arbuthnot$Females)

plot(arbuthnot$Year,arbuthnot$Males/(arbuthnot$Males+arbuthnot$Females),
     type= 'l')

plot(arbuthnot$Year,arbuthnot$Males/(arbuthnot$Males+arbuthnot$Females),
     type= 'l', ylab='MaleRatio')

MaleRatio <- arbuthnot$Males/(arbuthnot$Males+arbuthnot$Females)
MaleRatio
