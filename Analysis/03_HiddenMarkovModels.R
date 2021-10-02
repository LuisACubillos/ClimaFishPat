# Hidden Markov Change
library(depmixS4)

# Sardina comun
df2 <- read.csv("Data/data_demersal.csv",sep=";") 
names(df2)
colnames(df2) <- c("YY","SSP","Bt","St","Rt")

Spp <- "Raya_volantin_SUP"
Spp <- "Merluza_sur"
Spp <- "Merluza_cola"
Spp <- "Merluza_3A"
Spp <- "Congrio_sur"
Spp <- "Congrio_norte"
Spp <- "Bacalao"
df.raya <- df2[df2$SSP=="Raya_volantin_SUP",c(1,4,5)]
df.msur <- df2[df2$SSP=="Merluza_sur",c(1,4,5)]
df.mcol <- df2[df2$SSP=="Merluza_cola",c(1,4,5)]
df.m3a <- df2[df2$SSP=="Merluza_3A",c(1,4,5)]
df.cdn <- df2[df2$SSP=="Congrio_sur",c(1,4,5)]
df.cds <- df2[df2$SSP=="Congrio_norte",c(1,4,5)]
df.bac <- df2[df2$SSP=="Bacalao",c(1,4,5)]

#Alinea las series segun edad de reclutamiento
df.raya <- prep_tabla_srr(df.raya,tr=1)
df.msur <- prep_tabla_srr(df.msur,tr=1)
df.mcol <- prep_tabla_srr(df.mcol,tr=1)
df.m3a <- prep_tabla_srr(df.m3a,tr=2)
df.cdn <- prep_tabla_srr(df.cdn,tr=3)
df.cds <- prep_tabla_srr(df.cds,tr=3)
df.bac <- prep_tabla_srr(df.bac,tr=1)

# RAYA
range(df.raya$Rt)
p1 <- ggplot(data=df.raya,aes(x=YY,y=Rt))+
  geom_line()+
  geom_point()+
  scale_y_continuous(name = "Recruitment (millones)",limits = c(0,1))+
  mi.tema()
p1

df.raya$lnRt <- log(df.raya$Rt)
set.seed(532)
hhm1 <- depmix(lnRt ~ 1, nstates = 2,data=df.raya)
fm1 <- fit(hhm1)
prstate = apply(posterior(fm1)[,c("S1","S2")],1,which.max)
plot(prstate)
mu <- summary(fm1)[,1]
pred <- data.frame("Year"=df.raya$YY,"Pred"=mu[prstate],"Obs"=df.raya$lnRt)
p2 <- ggplot(data=pred)+
  geom_point(aes(x=Year,y=Obs))+
  geom_line(aes(x=Year,y=Pred))+
  mi.tema()
p2

# merluza del sur
range(df.msur$Rt)
p1 <- ggplot(data=df.msur,aes(x=YY,y=Rt))+
  geom_line()+
  geom_point()+
  scale_y_continuous(name = "Recruitment (millones)",limits = c(0,200))+
  mi.tema()
p1

df.msur$lnRt <- log(df.msur$Rt)
set.seed(532)
hhm1 <- depmix(lnRt ~ 1, nstates = 2,data=df.msur)
fm1 <- fit(hhm1)
prstate = apply(posterior(fm1)[,c("S1","S2")],1,which.max)
plot(prstate)
mu <- summary(fm1)[,1]
pred <- data.frame("Year"=df.msur$YY,"Pred"=mu[prstate],"Obs"=df.msur$lnRt)
p2 <- ggplot(data=pred)+
  geom_point(aes(x=Year,y=Obs))+
  geom_line(aes(x=Year,y=Pred))+
  mi.tema()
p2

# merluza de cola
range(df.mcol$Rt)
p1 <- ggplot(data=df.mcol,aes(x=YY,y=Rt))+
  geom_line()+
  geom_point()+
  scale_y_continuous(name = "Recruitment (millones)",limits = c(0,5657))+
  mi.tema()
p1

df.mcol$lnRt <- log(df.mcol$Rt)
set.seed(532)
hhm1 <- depmix(lnRt ~ 1, nstates = 2,data=df.mcol)
fm1 <- fit(hhm1)
prstate = apply(posterior(fm1)[,c("S1","S2")],1,which.max)
plot(prstate)
mu <- summary(fm1)[,1]
pred <- data.frame("Year"=df.mcol$YY,"Pred"=mu[prstate],"Obs"=df.mcol$lnRt)
p2 <- ggplot(data=pred)+
  geom_point(aes(x=Year,y=Obs))+
  geom_line(aes(x=Year,y=Pred))+
  scale_x_continuous(limits = c(1977,2017))+
  mi.tema()
p2


# merluza de 3 aletas
range(df.m3a$Rt)
p1 <- ggplot(data=df.m3a,aes(x=YY,y=Rt))+
  geom_line()+
  geom_point()+
  scale_y_continuous(name = "Recruitment (millones)",limits = c(0,1000))+
  mi.tema()
p1

df.m3a$lnRt <- log(df.m3a$Rt)
set.seed(532)
hhm1 <- depmix(lnRt ~ 1, nstates = 2,data=df.m3a)
fm1 <- fit(hhm1)
prstate = apply(posterior(fm1)[,c("S1","S2")],1,which.max)
plot(prstate)
mu <- summary(fm1)[,1]
pred <- data.frame("Year"=df.m3a$YY,"Pred"=mu[prstate],"Obs"=df.m3a$lnRt)
p2 <- ggplot(data=pred)+
  geom_point(aes(x=Year,y=Obs))+
  geom_line(aes(x=Year,y=Pred))+
  scale_x_continuous(limits = c(1977,2017))+
  mi.tema()
p2

# congrio dorado norte
range(df.cdn$Rt)
p1 <- ggplot(data=df.cdn,aes(x=YY,y=Rt))+
  geom_line()+
  geom_point()+
  scale_y_continuous(name = "Recruitment (millones)",limits = c(0,10))+
  mi.tema()
p1

df.cdn$lnRt <- log(df.cdn$Rt)
set.seed(532)
hhm1 <- depmix(lnRt ~ 1, nstates = 2,data=df.cdn)
fm1 <- fit(hhm1)
prstate = apply(posterior(fm1)[,c("S1","S2")],1,which.max)
plot(prstate)
mu <- summary(fm1)[,1]
pred <- data.frame("Year"=df.cdn$YY,"Pred"=mu[prstate],"Obs"=df.cdn$lnRt)
p2 <- ggplot(data=pred)+
  geom_point(aes(x=Year,y=Obs))+
  geom_line(aes(x=Year,y=Pred))+
  scale_x_continuous(limits = c(1977,2017))+
  mi.tema()
p2

# congrio dorado sur
range(df.cds$Rt)
p1 <- ggplot(data=df.cds,aes(x=YY,y=Rt))+
  geom_line()+
  geom_point()+
  scale_y_continuous(name = "Recruitment (millones)",limits = c(0,12))+
  mi.tema()
p1

df.cds$lnRt <- log(df.cds$Rt)
set.seed(532)
hhm1 <- depmix(lnRt ~ 1, nstates = 2,data=df.cds)
fm1 <- fit(hhm1)
prstate = apply(posterior(fm1)[,c("S1","S2")],1,which.max)
plot(prstate)
mu <- summary(fm1)[,1]
pred <- data.frame("Year"=df.cds$YY,"Pred"=mu[prstate],"Obs"=df.cds$lnRt)
p2 <- ggplot(data=pred)+
  geom_point(aes(x=Year,y=Obs))+
  geom_line(aes(x=Year,y=Pred))+
  scale_x_continuous(limits = c(1977,2017))+
  mi.tema()
p2

# bacalao
range(df.bac$Rt)
p1 <- ggplot(data=df.bac,aes(x=YY,y=Rt))+
  geom_line()+
  geom_point()+
  scale_y_continuous(name = "Recruitment (millones)",limits = c(0,10))+
  mi.tema()
p1

df.bac$lnRt <- log(df.bac$Rt)
set.seed(532)
hhm1 <- depmix(lnRt ~ 1, nstates = 2,data=df.bac)
fm1 <- fit(hhm1)
prstate = apply(posterior(fm1)[,c("S1","S2")],1,which.max)
plot(prstate)
mu <- summary(fm1)[,1]
pred <- data.frame("Year"=df.bac$YY,"Pred"=mu[prstate],"Obs"=df.bac$lnRt)
p2 <- ggplot(data=pred)+
  geom_point(aes(x=Year,y=Obs))+
  geom_line(aes(x=Year,y=Pred))+
  scale_x_continuous(limits = c(1977,2017))+
  mi.tema()
p2


library(markovchain)
stt =c("Hr","Lr")
P = matrix(c(1,0,0.066,0.934),nrow=2,byrow = TRUE,dimnames = list(stt,stt))
Pr = new("markovchain",
         states=stt,
         byrow=TRUE,
         transitionMatrix=P,
         name = "Recruitment")
Pr^2
steadyStates(Pr)
conditionalDistribution(Pr,"Lr")
conditionalDistribution(Pr,"Hr")
#Genera secuencia
outs <- markovchainSequence(n = 20, markovchain = Pr, t0 = "Lr")
outs
sequenceMatr <- createSequenceMatrix(outs, sanitize = FALSE)
mcFit <- markovchainFit(outs,method = "mle")
predict(mcFit$estimate,newdata="Hr",n.ahead=20)
mcFitBSP <- markovchainFit(data = outs, method = "bootstrap", nboot = 5, name = "Bootstrap Mc")