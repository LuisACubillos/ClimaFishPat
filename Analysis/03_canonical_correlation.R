
# Carga paquetes ----------------------------------------------------------
library(GGally)
library(CCA)
library(candisc)

clima <- read.csv("Data/Clima_anual_1970-2019.csv",sep=";")
names(clima)
yrs <- seq(1978,2015,1)

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



range(df.msur$YY)
range(df.m3a$YY)
range(df.cdn$YY)
range(df.cds$YY)
range(df.mcol$YY)

yrs <- seq(1978,2014,1)
Rmsur=log(df.msur$Rt[2:38])
Rm3a=log(df.m3a$Rt[1:37])
Rcdn=log(df.cdn$Rt[1:37])
Rcds=log(df.cds$Rt[1:37])
rec_dem1 <- data.frame(Rmsur,Rm3a,Rcdn,Rcds)
#Segunda seleccion
yrs <- seq(1985,2014,1)
Rmsur=log(df.msur$Rt[9:38])
Rm3a=log(df.m3a$Rt[8:37])
Rmcol=log(df.mcol$Rt[1:30])
Rcdn=log(df.cdn$Rt[8:37])
Rcds=log(df.cds$Rt[8:37])
rec_dem2 <- data.frame(Rmsur,Rm3a,Rcdn,Rcds,Rmcol)
# Analisis de correlación canonica ----------------------------------------
range(clima$YY)
# Selecciona los datos climaticos: años 1978-2014 formacion de la clase anual
amb1 <- clima[9:45,c(2,4,5,7)]
amb2 <- clima[16:45,c(2,4,5,7)]
# Selecciona los datos biologicos reclutamiento congrio norte y sur


# Correlación canónica ----------------------------------------------------
cc1 <- cancor(amb2,rec_dem2)
summary(cc1)
plot(cc1,smooth = TRUE)
cc1$coef$X
cc1$coef$Y

?p.asym

#Los coeficientes canónicos estandarizados se interpretan de una manera
#análoga a los coeficiecientes de regresión, pero en términos de desvación estándar.
coef(cc1,type="x",standardize = TRUE)
coef(cc1,type="y",standardize = TRUE)
#Componentes de carga ambiente
cc1$scores$X
#Componentes de carga especies
cc1$scores$Y
plot(cc1$scores$X[,1],cc1$scores$Y[,1])
#ålternbativa
plot(cc1,which = 1)
plot(cc1,which = 2)
plot(cc1,which = 3)
plot(cc1,which = 4)
heplot(cc1,xpd=TRUE)

redundancy(cc1)
m0 <- lm(cc1$scores$Y[,1] ~ cc1$scores$X[,1])
summary(m0)

# compute canonical loadings
#cc2 <- comput(ambiente, rec_dem1, cc1)
# display canonical loadings
#cc2[3:6]

ambiente <- amb2
## Correlacion
dim(ambiente)
cor(ambiente[,1],cc1$scores$X[,1])
cor(ambiente[,1],cc1$scores$X[,2])

cor(ambiente[,2],cc1$scores$X[,1])
cor(ambiente[,2],cc1$scores$X[,2])

cor(ambiente[,3],cc1$scores$X[,1])
cor(ambiente[,3],cc1$scores$X[,2])

cor(ambiente[,3],cc1$scores$X[,1])
cor(ambiente[,3],cc1$scores$X[,2])

cor(ambiente[,4],cc1$scores$X[,1])
cor(ambiente[,4],cc1$scores$X[,2])

#cor(ambiente[,5],cc1$scores$X[,1])
#cor(ambiente[,5],cc1$scores$X[,2])

spp1.cor <-numeric()
spp2.cor <-numeric()
spp3.cor <-numeric()
spp4.cor <-numeric()
spp5.cor <-numeric()
for(spp in 1:5){
  spp1.cor[spp] <-cor(rec_dem2[,spp],cc1$scores$Y[,1])
  spp2.cor[spp] <-cor(rec_dem2[,spp],cc1$scores$Y[,2])
  spp2.cor[spp] <-cor(rec_dem2[,spp],cc1$scores$Y[,3])
  spp2.cor[spp] <-cor(rec_dem2[,spp],cc1$scores$Y[,4])
}
name.spp <- names(rec_dem2)
name.spp2 <- name.spp[1:5]
out.corr <- data.frame(name.spp2,spp1.cor,spp2.cor)
out.corr

#yrs <- clima$YY[9:43]
plot(yrs,cc1$scores$X[,1],type="b",xlab="Year",ylab="Dimension 1",ylim=c(-3,3),las=1,cex.axis=1.5,cex.lab=1.5)
lines(yrs,cc1$scores$Y[,1],lty=2)
points(yrs,cc1$scores$Y[,1],pch=19)
abline(h=0,lty=3,lwd=0.8,col="grey")
legend("topleft",c("Climate","Recruitment"),pch=c(1,19),lty=c(1,2),cex=1.2)
box()

plot(yrs,cc1$scores$X[,2],type="b",xlab="Year",ylab="Dimension 2",ylim=c(-3,3),las=1,cex.axis=1.5,cex.lab=1.5)
lines(yrs,cc1$scores$Y[,2],lty=2)
points(yrs,cc1$scores$Y[,2],pch=19)
abline(h=0,lty=3,lwd=0.8,col="grey")
#legend("topright",c("Regional","Local"),pch=c(1,19),lty=c(1,2),cex=0.8)
box()
