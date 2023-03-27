mydir = "/Users/wang.c/Desktop/wunshan district rain.csv"
poway.precip.monthly = read.csv(mydir)
head(poway.precip.monthly )
plot(poway.precip.monthly$Number,poway.precip.monthly$Rainfall,type="l",xlab = "",ylab="",xaxt="n",yaxt="n")  # "l" means plot it as a "line"
title(main="Rainfall at Wunshun District",xlab = "from 1960.1 to 2017.12", ylab = "Monthy rainfall in Wunshun district(cm)",line=2,cex=0.5)
axis(side=1,at=c(1,100,200,300,400,500,600,696), tck=-0.05,labels=c("1960.01","1968.04","1976.08","1984.12","1933.04","2001.08","2009.12","2017.12"))
axis(side=2,at=c(0,10,20,30,40,50,60), tck=-0.05,labels=c("0","10","20","30","40","50","60"))

poway.precip.yearly = aggregate(poway.precip.monthly$Rainfall,by=list(poway.precip.monthly$Month),FUN=sum)
names(poway.precip.yearly)
names(poway.precip.yearly) = c("Month","rainfall.cm")
plot(poway.precip.yearly$Month,poway.precip.yearly$rainfall.cm/5.8,ylim=c(0,150),type="l")  # "l" means plot it as a "line"
barplot(poway.precip.yearly$rainfall.cm,poway.precip.yearly$Month)
axis(side=1,at=c(1,2,3,4,5,6,7,8,9,10,11,12), tck=-0.05,labels=c("1","2","3","4","5","6","7","8","9","10","11","12"))


breaks <- seq(as.Date("1960"), length=58, by="year")  
years.breaks = as.numeric(format(breaks,"%Y"))
labels.water.year = years.breaks[2:length(breaks)]  # Why are we leaving off the first year in the water years label?
poway.precip.daily$wateryear <- cut(poway.precip.daily$Date, breaks,labels=labels.water.year)


x.annual.wy = aggregate(poway.precip.monthly$Rainfall,by=list(poway.precip.monthly$Year),FUN=sum)
names(x.annual.wy) = c("Year","Rainfall.cm")
lm.daily = lm(x.annual.wy$Rainfall.cm ~ x.annual.wy$Year)
summary(lm.daily)
plot(x.annual.wy$Year,x.annual.wy$Rainfall.cm,type="l",ylab="Maximum daily precip, inches",xlab="Water year",xlim = c(1960,2017))
abline(lm.daily)
title(main="Rainfall at Wunshun District from 1960-2017",line=2,cex=0.2)

