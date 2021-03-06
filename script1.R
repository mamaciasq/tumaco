library(survival)

# Leyendo datos crudos
tumaco <- read.table("/Users/martin/Dropbox/Proyectos/Peama Tumaco/tumaco.csv", sep=";", header=T)

# Ajustando funciones de riesgo estimadas y funciones de sobrevida
# por semestre, por sexo

ts0 <- survfit(Surv(time, 1-censor)~ 1, conf.type="none", 
  subset=(sexo==0),data = tumaco)

ts1 <- survfit(Surv(time, 1-censor)~ 1, conf.type="none", 
  subset=(sexo==1),data = tumaco)

h0 <- ts0$n.event/ts0$n.risk
h1 <- ts1$n.event/ts1$n.risk

# Gráfica de funciones de riesgo estimadas por semestre y por sexo
plot(ts0$time, h0, type="l", ylab="Estimated Hazard probability", 
  xlab="Semester", ylim=c(0.0, 0.1), xlim=c(1, 6), col="red")
par(new=T) 
plot(ts1$time, h1, type="l", ylab=" ", ylim=c(0.0, 0.1), xlim=c(1, 6), 
  xlab="", col="blue")

# Gráfica de funciones de sobrevida estimadas por semestre y por sexo

plot(ts0$time, ts0$surv, type = 'l', ylab = "Estimated Survival Function", 
  xlab = "Semester", ylim=c(0.75, 0.95), xlim=c(1, 6), col="red")
par(new=T)
plot(ts1$time, ts1$surv, type="l", ylab=" ", ylim=c(0.75, 0.95), xlim=c(1, 6), xlab="", col="blue")
abline(h=c(.8625), lty=2)

# Tabla de vida que describe el semestre de desvinculación, diferenciando
# por sexo

tab11.1.0<-cbind(time=ts0$time, nleft=ts0$n.risk, failed=ts0$n.event, hazard=h0, survival=ts0$surv)
tab11.1.1<-cbind(time=ts1$time, nleft=ts1$n.risk, failed=ts1$n.event, hazard=h1, survival=ts1$surv)
tab11.1 <-rbind(tab11.1.0, tab11.1.1)
tab11.1

tsall <- survfit( Surv(time, 1-censor)~ 1, conf.type="none", data=tumaco)
h<-tsall$n.event/tsall$n.risk
tab11.1.all<-cbind(time=tsall$time, nleft=tsall$n.risk, failed=tsall$n.event, hazard=h, survival=tsall$surv)
tab11.1.all

odds0<-h0/(1-h0)
odds1<-h1/(1-h1)
logith0<-log(odds0)
logith1<-log(odds1)
plot(ts0$time, h0, type="l", ylab="Estimated Hazard Probability", xlab="Grade", 
     ylim=c(0.0, 0.1), xlim=c(1, 6), col="red")
par(new=T) 
plot(ts1$time, h1, type="l", ylab=" ", ylim=c(0.0, 0.1), xlim=c(1, 5), xlab="", col="blue")
