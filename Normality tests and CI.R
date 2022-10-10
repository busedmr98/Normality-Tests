
                                  ##DSM 5001##
                                   ##FINAL##
library(truncgof)
library(goftest)
# SORU 1
##Guven Araligi
x1=c(25,3 ,27 ,30,33 ,16 ,28,27 ,12 ,32 ,16) #Manometrik 
x2=c( 11 ,26, 18,16,20,12,8,26,12,17,14) #Seyreltme
n1=11
n2=11
a=0.05 #onem duzeyi
xbar1=mean(x1)
xbar2=mean(x2) 
s_x1=sd(x1)
s_x2=sd(x2)
#CI <- (xbar1-xbar2)-+t(a/2)*sp*sqrt((1/n1)+(1/n2))

t=qt(a/2,df=n1+n2-2,lower.tail = F) #2.08
sp = sqrt(((n1-1)*s_x1^2+(n2-1)*s_x2^2)/(n1+n2-2)) #7.91 (varyans esit)
margin <- t*sp*sqrt(1/n1 + 1/n2) #7.04

altsinir <- (xbar1-xbar2) - margin
altsinir

ustsinir <- (xbar1-xbar2) + margin
ustsinir

#P(altsinir<M1-M2<ustsinir)=0.95
#Aralıga 0 dahil oldugu icin ortalamalar arasinda anlamli bir fark olmayabilir.


#SORU 3
x<-c(1.542, 1.585, 1.532, 1.466, 1.499 ,1.611, 1.622, 1.466, 1.546, 1.494, 1.548, 1.626,
     1.440, 1.608 ,1.520 ,1.478, 1.542, 1.511, 1.459 ,1.533, 1.532, 1.523, 1.397, 1.487,
     1.598, 1.498, 1.600, 1.504, 1.545 ,1.558)
summary(x)
sd(x)
kutu<-boxplot(x,horizontal = T,main="Kutu Grafigi",col="gold")
m<-mean(x)
nx<-30
abline(v=m,lty=2)
kutu$out #sapan deger
text(kutu$out+0.02,1,kutu$out)
hist(x,col="blue",main="Histogram")
lines(density(x),col="red",lwd=3) #cok hafif sola carpiklik var. Normal dagilima yakin

set.seed(1)
shapiro.test(x) #0.67>0.05 Ho reddedilemedi. Veriler normal dagilima sahip
v.test(x, "pnorm",fit=list(mean=mean(x),sd=sd(x))) # 0.42
cvm.test(x,"pnorm",mean=mean(x),sd=sd(x)) #0.95 #Cramer-Von Mises Testi
ks.test(x, "pnorm",fit=list(mean=mean(x),sd=sd(x))) #0.62#Kolmogorov Smirnov
ad2.test(x,"pnorm",fit=list(mean=mean(x),sd=sd(x))) #0.73 #Anderson Darling Test
w2.test(x,"pnorm",fit=list(mean=mean(x),sd=sd(x))) #0.79
#Q-Q Plot
plot(x)
qqnorm(x, pch = 1, frame = T)
qqline(x, col = "steelblue", lwd = 3)

#Ortalama katran miktari icin guven araligi
#CI=mean(x)-z(a/2)*sd(x)*sqrt(nx)
a1=0.01
z1=qnorm(1-a1/2)
t1<-qt(alfa/2,df=nx-1,lower.tail = F)
margin1 <- t1*sd(x)/sqrt(nx)

altsinir1 <- mean(x) - margin1
altsinir1

ustsinir1 <- mean(x) + margin1
ustsinir1
 # P(altsinir<M<ustinir)=0.99

# Kitle Varyansi icin Guven Araligi %90 guven duzeyi
alfa<-0.1 
tablo_alt<-qchisq(alfa/2,nx-1,lower.tail = F)
tablo_ust<-qchisq(1-alfa/2,nx-1,lower.tail = F)
altsinir2<-(nx-1)*sd(x)^2/tabloalt
ustsinir2<-(nx-1)*sd(x)^2/tabloust

#P(altsinir2<Q^2<ustsinir2)<0.9


