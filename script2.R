tumaco.pp<-read.table("data/tumaco.csv", sep=";", header=T)
head(tumaco.pp)
afit <- glm(EVENTO ~ GENERO, family="binomial", data=tumaco.pp)
summary(afit)

t<-data.frame(afit$coefficients)
p0<-t[1,1]
p1<-p0+t[2,1]

firstsex<-read.table("https://stats.idre.ucla.edu/stat/examples/alda/firstsex.csv", sep=",", header=T)
ts0 <- survfit( Surv(time, 1-censor)~ 1, conf.type="none", subset=(pt==0), data=firstsex)
ts1 <- survfit( Surv(time, 1-censor)~ 1, conf.type="none", subset=(pt==1), data=firstsex)
h0<-ts0$n.event/ts0$n.risk
h1<-ts1$n.event/ts1$n.risk

odds0<-h0/(1-h0)
odds1<-h1/(1-h1)
logith0<-log(odds0)
logith1<-log(odds1)
plot(ts0$time, logith0, type="p", ylab="Estimated Logit", xlab="Grade", 
  ylim=c(-4, 0), xlim=c(6, 12), col="red")
par(new=T) 
plot(ts1$time, logith1, type="p", ylab=" ", ylim=c(-4, 0), xlim=c(6, 12), xlab="", col="blue")
abline(h=c(p0), lty=1, col="red")
abline(h=c(p1), lty=1, col="blue")



afit2 <- glm(EVENTO ~ GENERO + PERIODO, family="binomial", data=tumaco.pp)
summary(afit2)

afit3 <- glm(EVENTO ~ GENERO + factor(PERIODO), family="binomial", data=tumaco.pp)
summary(afit3)