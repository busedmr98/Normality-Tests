#12.soru
x<-c(1.25,1.95,19.15,7.39,2.42,4.85,1.52,6.90,0.26,7.00,
     10.89,11.68,8.85,4.85,0.33,0.16,1.67,0.89,16.27,1.59,0.44,1.95,13.62,
     19.39,11.19,4.80,4.76,6.13,10.17,12.32,1.90,14.71,6.94,5.75,4.54,5.65,11.47,20.69,13.18,92.41)
sort(x)
summary(x)
sd(x)
library(tidyverse)

qplot(x=pil_omru,y="",geom="boxplot",main="Kutu Grafigi",col=I("darkblue"),fill=I("orange"))


#normallik sinamasi
hist(x,freq=F,main="Histogram Grafigi",col="red",xlim=c(0,25),breaks=c(0,5,10,15,20,25,95))
lines(density(x),col="blue",lwd=2)  #saga carpik dagilim

shapiro.test(x) #p<0.05 Ho red. Veriler normal dağılıma sahip değil.
plot(x)
#Q-Q Plot
qqnorm(x, pch = 1, frame = T)
qqline(x, col = "steelblue", lwd = 3)

#Chebychev Teoremi 
# e) 1-1/(k)^2 , k=2.5
1-1/2.5^2

# f) 1-1/k^2=0.91
k= sqrt(1/(1-0.91))