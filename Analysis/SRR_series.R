# Relaciones Stock-Recluta

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
# test de Chow
# raya volantin
#Alinea las series segun edad de reclutamiento
df.raya <- prep_tabla_srr(df.raya,tr=1)
df.msur <- prep_tabla_srr(df.msur,tr=1)
df.mcol <- prep_tabla_srr(df.mcol,tr=1)
df.m3a <- prep_tabla_srr(df.m3a,tr=2)
df.cdn <- prep_tabla_srr(df.cdn,tr=3)
df.cds <- prep_tabla_srr(df.cds,tr=3)
df.bac <- prep_tabla_srr(df.bac,tr=1)


yrs <- seq(1978,2014,1)

Rmsur=log(df.msur$Rt[2:38])
Rm3a=log(df.m3a$Rt[1:37])
Rcdn=log(df.cdn$Rt[1:37])
Rcds=log(df.cds$Rt[1:37])
Clima1 = df$PC1[9:45]
Clima2 = df$PC2[9:45]
library(rcompanion)
# RSR merluza de cola
range(df.mcol$YY)
range(df$YY)
df.mcol$Clima1 = df$PC1[16:47]
df.mcol$Clima2 = df$PC2[16:47]
#Ricker
names(df.mcol)
srrdt <- df.mcol
m0 <- glm(log(Rt) ~ offset(log(SSBt))+SSBt,data=srrdt)
m1 <- glm(log(Rt) ~ offset(log(SSBt))+SSBt+Clima1,data=srrdt)
AIC(m0)
AIC(m1)
summary(m0)
summary(m0)$coeff
(1-m0$deviance/m0$null.deviance)*100
AIC(m0)
nagelkerke(m0)
summary(m1)
summary(m1)$coeff
(1-m1$deviance/m1$null.deviance)*100
AIC(m1)
nagelkerke(m1)
#B&H
BHm0 <- glm(log(Rt) ~ I(1/SSBt),family=gaussian(link = "inverse"),data=srrdt)
BHm1 <- glm(log(Rt) ~ I(1/SSBt) + Clima1,family=gaussian(link = "inverse"),data=srrdt)
summary(BHm0)
summary(BHm0)$coeff
(1-BHm0$deviance/BHm0$null.deviance)*100
nagelkerke(BHm0)
AIC(BHm0)
summary(BHm1)
summary(BHm1)$coeff
(1-BHm1$deviance/BHm1$null.deviance)*100
nagelkerke(BHm1)
AIC(BHm1)

AIC(m0)
AIC(m1)
AIC(BHm0)
AIC(BHm1)
srrdt$BHpred.rt <- exp(1/(predict(BHm1,srrdt)))
p2 <- ggplot(data=srrdt)+
  geom_point(aes(x=YY,y=Rt),size=3)+
  #geom_line(aes(x=YY,y=Rikpred.rt),col="grey",size=1)+
  geom_line(aes(x=YY,y=BHpred.rt),col="blue",size=1)+
  scale_y_continuous(name = "Reclutamiento (miles)")+
  mi.tema()
p2

p2 <- ggplot(data=srrdt)+
  geom_point(aes(x=SSBt,y=Rt),size=3)+
  geom_line(aes(x=SSBt,y=BHpred.rt))+
  scale_y_continuous(name = "R",limits = c(0,6000))+
  scale_x_continuous(name = "SSB",limits = c(0,800))+
  mi.tema()
p2


