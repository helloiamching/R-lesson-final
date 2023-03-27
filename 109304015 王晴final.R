#1
#(a)
ID = seq(1,100)
midterm = sample(0:100,100,replace=T)
final = sample(40:100,100,replace = T)
diff <- midterm-final
which(diff<=10&diff>=(-10))
#(b)
which.max(midterm)
which.min(midterm)
which.max(final)
which.min(final)
#(c)
plot(midterm,final,xlim=c(0,100),ylim=c(40,100))
#(d)
#(甲)
for (i in 1:100) {
  if (diff[i]<=10&diff[i]>=(-10)){
    points(x=midterm[i],                       
           y=final[i], 
           pch=16,                  # 點的圖形
           col="red")
  }else{NA}
}
#(乙)
text(midterm[which.max(midterm)],final[which.max(midterm)],label= which.max(midterm),col="green", cex = 0.7)
text(midterm[which.min(midterm)],final[which.min(midterm)],label= which.min(midterm),col="green", cex = 0.7)
text(midterm[which.max(final)],final[which.max(final)],label= which.max(final),col="green", cex = 0.7)
text(midterm[which.min(final)],final[which.min(final)],label= which.min(final),col="green", cex = 0.7)
#(丙)
xmu<- mean(midterm)
ymu<- mean(final)
abline(v=xmu,col="blue",lty=2)
abline(h=ymu,col="blue",lty=2)

#2
#(a)
l <- 3
n <- 10
a <-1:n
x <- rbinom(n,size=n,prob =l/n)
y <- rpois(n,lambda = l)

plot(a,x,type = "l")
points(x=a,                       
       x, 
       pch=2,                  # 點的圖形
       col="black")
lines(a,y,type = "l",lty=2)
points(x=a,                       
       y, 
       pch=16,                  # 點的圖形
       col="black")
#(b)
mat2 <- matrix(c(1,1,2,2),2,2)
layout(mat2)
n <- 15
p2<-(plot(a,x,type = "l"))
points(x=a,                       
       x, 
       pch=2,                  # 點的圖形
       col="black")
lines(a,y,type = "l",lty=2)
points(x=a,                       
       y, 
       pch=16,                  # 點的圖形
       col="black")
n<-30
p3<-(plot(a,x,type = "l"))
points(x=a,                       
       x, 
       pch=2,                  # 點的圖形
       col="black")
lines(a,y,type = "l",lty=2)
points(x=a,                       
       y, 
       pch=16,                  # 點的圖形
       col="black")
#(c)
#當二項分布的 N 很大以及 p 很小的時候，此事件出現的次數可以用 Poisson 分布來逼近。

#3
set.seed(12345)
time = strptime(c("01/01/2020","12/31/2021"),"%m/%d/%Y")
shop.time = sample(seq(from = time[1],to = time[2],by = "day"),1000,replace = T)
price = sample(100:1000,1000,replace = T)
subject = sample(letters[1:20],1000,replace = T)
data = data.frame(shop.time,subject,price)
#(a)
arrange(data, subject,shop.time)         # 使用plyr包里的arrange函数
data[order(data$subject,data$shop.time),] 
#(b)
datanew <-data[order(data$subject,data$shop.time),]
boxplot(datanew$price[1:50],datanew$price[51:100],datanew$price[101:150],datanew$price[151:200],datanew$price[201:250],datanew$price[251:300],datanew$price[301:350],datanew$price[351:400],datanew$price[401:450],datanew$price[451:500],xaxt = "n")
axis(side=1,at=c(0,1,2,3,4,5,6,7,8,9,10), tck=-0.02,labels=c('0','1','2','3','4','5','6','7','8','9','10'))
#(c)
shop.avg <- for (i in 1:20) {
    mean(datanew$price[1*i:50*i])
  }
#(d)
plot(datanew$shop.time[1:50],datanew$price[1:50],type = "l",xaxt="n",yaxt="n")  # "l" means plot it as a "line"
points(x=datanew$shop.time[1:50],                       
       datanew$price[1:50], 
       pch=16,                  # 點的圖形
       col="black")
axis(side=2,at=c(200,400,600,800,10000), tck=-0.05,labels=c("200","400","600","800","10000"))
axis(side=1,at=c(1,32,50), tck=-0.05,labels=c("1","32","50"))
title(xlab="time",ylab ="cost" ,line=2,cex=0.5)
#4
x<-0
y<-0
plot(x,y,ann=F,xlim=c(-2,4),ylim=c(0,1),type = "l",xaxt="n",yaxt="n",main = "p",axes = F)
axis(side=2,at=c(1/6,1/2,1), tck=-0.05,labels=c("p(X<=x1)","","p(X<=x3)"))
axis(side=1,at=c(1,2,3), tck=-0.05,labels=c("x1","x2","x3"))
c=0
for (i in 0:3) {
  a=0+i
  b=1+i
  c=0+1/6*i+c
  segments(x0=a,y0 = c,x1=b,y1 =c)
}
e=0
for (i in 0:2) {
  g=1+i
  h=1+i
  e=0+1/6*i+e
  f=1/6+1/6*i+e
  segments(x0=g,y0 = e,x1=h,y1 =f,lty=2)
}
points(x=c(1,1,2,2,3,3),                       
       y=c(0,1/6,1/6,1/2,1/2,1), 
       pch=16,                  # 點的圖形
       col="black")






















