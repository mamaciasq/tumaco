tumaco.pp<-read.table("/Users/martin/Dropbox/Proyectos/Peama Tumaco/tumacopp.csv", sep=";", header=T)
head(tumaco.pp)
afit <- glm(evento ~ b2, family="binomial", data=tumaco.pp)
summary(afit)

afit2 <- glm(evento ~ b2 + semestre, family="binomial", data=tumaco.pp)
summary(afit2)

t<-data.frame(afit2$coefficients)
y0<-t[1,1] + t[3,1]*ts0$time
y1<-t[1,1] + t[2,1] + t[3,1]*ts0$time
plot(ts0$time, logith0, type="p", ylab="Estimated Logit", xlab="Grade", 
     ylim=c(-4, 0), xlim=c(1, 6), col="red")
par(new=T) 
plot(ts1$time, logith1, type="p", ylab=" ", ylim=c(-4, 0), xlim=c(1, 6), xlab="", col="blue")
par(new=T)
plot(ts1$time, y0, type="l", ylab=" ", ylim=c(-4, 0), xlim=c(1, 6, xlab="", col="red")
par(new=T)
plot(ts1$time, y1, type="l", ylab=" ", ylim=c(-4, 0), xlim=c(1, 6), xlab="", col="blue")


afit3 <- glm(evento ~ b2 + factor(semestre), family="binomial", data=tumaco.pp)
summary(afit3)

t<-data.frame(cbind(y=afit3$linear.predictors, time=firstsex.pp$period, pt=firstsex.pp$pt))
t0<-t[t$pt==0,]
t0<-t0[order(t0$time),]
t1<-t[t$pt==1,]
t1<-t1[order(t1$time),]
plot(ts0$time, logith0, type="p", ylab="Estimated Logit", xlab="Grade", 
     ylim=c(-4, 0), xlim=c(1, 6), col="red")
par(new=T) 

plot(ts1$time, logith1, type="p", ylab=" ", ylim=c(-4, 0), xlim=c(1, 6), xlab="", col="blue")
par(new=T)
plot(t0$time, t0$y, type="l", ylab=" ", ylim=c(-4, 0), xlim=c(1, 6), xlab="", col="red")
par(new=T)
plot(t1$time, t1$y, type="l", ylab=" ", ylim=c(-4, 0), xlim=c(1, 6), xlab="", col="blue")

##

afit3<-glm(evento~b2 + factor(semestre), family="binomial", data=tumaco.pp)
t<-data.frame(cbind(y=afit3$linear.predictors, time=tumaco.pp$semestre, pt=tumaco.pp$b2))
t0<-t[t$pt==0,]
t0<-t0[order(t0$time),]
t1<-t[t$pt==1,]
t1<-t1[order(t1$time),]
plot(t0$time, t0$y, type="b", ylab=" ", ylim=c(-4, 0), xlim=c(1, 6), xlab="", col="red")
par(new=T)
plot(t1$time, t1$y, type="b", ylab=" ", ylim=c(-4, 0), xlim=c(1, 6), xlab="", col="blue")

##

afit4<-glm(evento~b2 + factor(semestre) + b2*factor(semestre), family="binomial", data=tumaco.pp)
t<-data.frame(cbind(y=exp(afit4$linear.predictors), time=tumaco.pp$semestre, pt=tumaco.pp$b2))
t0<-t[t$pt==0,]
t0<-t0[order(t0$time),]
t1<-t[t$pt==1,]
t1<-t1[order(t1$time),]
plot(t0$time, t0$y, type="b", ylab="Odds", ylim=c(0, 1), xlim=c(1, 6), xlab="", col="red")
par(new=T)
plot(t1$time, t1$y, type="b", ylab=" ", ylim=c(0, 1), xlim=c(1, 6), xlab="", col="blue")

##
t<-data.frame(cbind(y=afit4$fitted.values, time=tumaco.pp$semestre, pt=tumaco.pp$b2))
t0<-t[t$pt==0,]
t0<-t0[order(t0$time),]
t1<-t[t$pt==1,]
t1<-t1[order(t1$time),]
plot(t0$time, t0$y, type="b", ylab="Hazard", ylim=c(0, .5), xlim=c(1, 6), xlab="", col="red")
par(new=T)
plot(t1$time, t1$y, type="b", ylab=" ", ylim=c(0, .5), xlim=c(1, 6), xlab="", col="blue")

## Model A

modelA<-glm(evento~factor(semestre) - 1, family="binomial", data=tumaco.pp)
summary(modelA)

modelB<-glm(evento~factor(semestre) + b2 - 1, family="binomial", data=tumaco.pp)
summary(modelB)
anova(modelB)

modelC<-glm(evento~factor(semestre) + documento - 1, family="binomial", data=tumaco.pp)
summary(modelC)
anova(modelC)

modelD<-glm(evento~factor(semestre) + b2 + documento - 1, family="binomial", data=tumaco.pp)
summary(modelD)

modelD<-glm(evento~factor(semestre) + documento + b2 - 1, family="binomial", data=tumaco.pp)
summary(modelD)
anova(modelD)

modelA<-glm(evento~factor(semestre) - 1, family="binomial", data=tumaco.pp)
col0<-c(1:6)
col1<-c("s1", "s2", "s3", "s4", "s5", "s6")
col2<-exp(modelA$coefficients)
col3<- 1 /(1+exp(-modelA$coefficients))
tab11.4<-data.frame(time=col0, Predictor=col1, parameter=modelA$coefficients, 
                    fitted.odds=col2, fitted.hazard=col3, row.names=NULL)
tab11.4

##

modelB<-glm(evento~factor(semestre) + b2 - 1, family="binomial", data=tumaco.pp)
t<-data.frame(hazard=modelB$fitted.values, time=tumaco.pp$semestre, pt=tumaco.pp$b2)
t$logit<-log(t$hazard/(1-t$hazard))
ta<-aggregate(t, list(pt=t$pt, time=t$time),mean)
ta.0<-ta[ta$pt==0, ]
ta.1<-ta[ta$pt==1, ]
c1<-c(1:6)
c2<-ta.0$logit
c3<-ta.1$logit-ta.0$logit
c4<-ta.0$logit
c5<-ta.1$logit
c6<-ta.0$hazard
c7<-ta.1$hazard
tab11.5<-data.frame(time=c1, alpha=c2, beta=c3, logit_0=c4, logit_1=c5, hazard_0=c6, hazard_1=c7)
tab11.5$surv_0<-c(1:6)
tab11.5$surv_1<-c(1:6)
tab11.5$surv_0[1]<-1-tab11.5$hazard_0[1]
tab11.5$surv_1[1]<-1-tab11.5$hazard_1[1]
for(i in 2:6) {
  tab11.5$surv_0[i] = tab11.5$surv_0[i-1]*(1-tab11.5$hazard_0[i])
  tab11.5$surv_1[i] = tab11.5$surv_1[i-1]*(1-tab11.5$hazard_1[i])
}
tab11.5

## 

plot(tab11.5$time, tab11.5$logit_0, type="l", ylab="Fitted logit(hazard)", 
     ylim=c(-4, 0), xlim=c(1, 6), xlab="", col="red")
par(new=T)
plot(tab11.5$time, tab11.5$logit_1, type="l", ylab="", ylim=c(-4, 0), xlim=c(1, 6), xlab="", col="blue")
plot(tab11.5$time, tab11.5$hazard_0, type="l", ylab="Fitted hazard", 
     ylim=c(0, 0.5), xlim=c(1, 6), xlab="", col="red")
par(new=T)
plot(tab11.5$time, tab11.5$hazard_1, type="l", ylab="", ylim=c(0, 0.5), xlim=c(1, 6), xlab="", col="blue")
plot(tab11.5$time, tab11.5$surv_0, type="l", ylab="Fitted survival probability", 
     ylim=c(0, 1), xlim=c(1, 6), xlab="", col="red")
par(new=T)
plot(tab11.5$time, tab11.5$surv_1, type="l", ylab="", ylim=c(0,1), xlim=c(1, 6), xlab="", col="blue")
abline(h=c(.5), lty=2)


##

modelD<-glm(evento~factor(semestre) + b2 + documento - 1, family="binomial", data=tumaco.pp)
coeff<-data.frame(modelD$coefficients)
myt<-c(1:6)
h0_pas1<-c(1:6)
h0_pas0<-c(1:6)
h0_pasn1<-c(1:6)
h1_pas1<-c(1:6)
h1_pas0<-c(1:6)
h1_pasn1<-c(1:6)
for(i in 1:6) {
  myt[i]<-i+6
  h0_pas1[i]<-1/(1+ exp(-(coeff[i,] + coeff[8,])))
  h0_pas0[i]<-1/(1+ exp(-coeff[i,]))
  h0_pasn1[i]<-1/(1+ exp(-(coeff[i,] - coeff[8,])))
  h1_pas1[i]<-1/(1+ exp(-(coeff[i,] + coeff[8,] + coeff[7,])))
  h1_pas0[i]<-1/(1+ exp(-(coeff[i,] + coeff[7,])))
  h1_pasn1[i]<-1/(1+ exp(-(coeff[i,] - coeff[8,] + coeff[7,])))
}
f<-cbind(h0_pas1,h0_pas0,h0_pasn1, h1_pas1,h1_pas0,h1_pasn1)
matplot(myt, f, type="l", ylab="Fitted hazard", ylim=c(-50, 50), xlim=c(1, 6), 
        xlab="Grade", col=1:6, lty=1:6)
legend(6, .5, c("b2=0 pas=TI", "b2=0 pas=CC", "b2=0 pas=-1", 
                "b2=1 pas=TI", "b2=1 pas=CC", "b2=1 pas=-1"), 
       col=1:6, lty=1:6, pch = "*",
       ncol =3, cex = 1)