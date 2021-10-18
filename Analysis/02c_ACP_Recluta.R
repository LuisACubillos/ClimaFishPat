
# ACP series de reclutamiento

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

# raya volantin
#Alinea las series segun edad de reclutamiento
df.raya <- prep_tabla_srr(df.raya,tr=1)
df.msur <- prep_tabla_srr(df.msur,tr=1)
df.mcol <- prep_tabla_srr(df.mcol,tr=1)
df.m3a <- prep_tabla_srr(df.m3a,tr=2)
df.cdn <- prep_tabla_srr(df.cdn,tr=3)
df.cds <- prep_tabla_srr(df.cds,tr=3)
df.bac <- prep_tabla_srr(df.bac,tr=1)

# Anomalias
an.msur2 <- (log(df.msur$Rt)-mean(log(df.msur$Rt)))/sd(log(df.msur$Rt))
an.mcol2 <- (log(df.mcol$Rt)-mean(log(df.mcol$Rt)))/sd(log(df.mcol$Rt))
an.m3a2 <- (log(df.m3a$Rt)-mean(log(df.m3a$Rt)))/sd(log(df.m3a$Rt))
an.cdn2 <- (log(df.cdn$Rt)-mean(log(df.cdn$Rt)))/sd(log(df.cdn$Rt))
an.cds2 <- (log(df.cds$Rt)-mean(log(df.cds$Rt)))/sd(log(df.cds$Rt))
an.bac2 <- (log(df.bac$Rt)-mean(log(df.bac$Rt)))/sd(log(df.bac$Rt))

# Seleccionar especies y años
# Periodo 1978 a 2014 
#
yrs <- seq(1978,2014,1)
anRmsur=an.msur2[2:38]
anRm3a=an.m3a2[1:37]
anRcdn=an.cdn2[1:37]
anRcds=an.cds2[1:37]

dfan1 <- data.frame(Year=yrs,aRmsur=anRmsur,aRm3a=anRm3a,aRcdn=anRcdn,aRcds=anRcds)

library(FactoMineR)
library(factoextra)

# Componentes principales -------------------------------------------------
dat2 <- dfan1[,c(2:5)]
rownames(dat2) <- dfan1[,1]
acp2 <- PCA(dat2,graph = FALSE,ncp=ncol(dat2))
x=1:length(acp2$eig[,1])
barplot(acp2$eig[,1],names.arg=x, main="Eigen values")
barplot(acp2$eig[,2],names.arg=x, main="Porcentaje de Inercia", xlab="N° Componentes",ylab="Inercia (%)")

plot.PCA(acp2, choix = "var", axes = c(1,2),new.plot=FALSE, title="", lim.cos2.var = 0.1)
plot.PCA(acp2, choix = "var", axes = c(1,3),new.plot=FALSE, title="", lim.cos2.var = 0.1)

fviz_pca_var(acp2)+
  mi.tema()

fviz_pca_var(acp2,col.var="contrib")+
  mi.tema()

eig.val <- get_eigenvalue(acp2)
fviz_eig(acp2)

df <- NULL
df <- data.frame(YY=dfan1[,1],PC1=acp2$ind$coord[,1],PC2=acp2$ind$coord[,2])
df$colour <- ifelse(df$PC1 < 0, "frío","cálido")
p1 <- ggplot(df,label="",aes(y=PC1,x=YY))+
  geom_bar(stat="identity",position="identity",aes(fill=colour))+
  scale_fill_manual(values = c(frío="#0000FF",cálido="#FF0000"))+
  geom_hline(yintercept = 0)+
  xlab("") +
  ylab("PC1")+
  theme_bw()+
  theme(legend.position = "none",strip.background = element_blank(),strip.placement = "outside")+
  mi.tema()
p1

df$colour <- ifelse(df$PC2 < 0, "frío","cálido")
p2 <- ggplot(df,label="",aes(y=PC2,x=YY))+
  geom_bar(stat="identity",position="identity",aes(fill=colour))+
  scale_fill_manual(values = c(frío="#0000FF",cálido="#FF0000"))+
  geom_hline(yintercept = 0)+
  xlab("") +
  ylab("PC1")+
  theme_bw()+
  theme(legend.position = "none",strip.background = element_blank(),strip.placement = "outside")+
  mi.tema()
p2

# PRIMER COMPONETE DE VAR CLIMA
climPC1 <- acp$ind$coord[9:45,1]
df$CP1 = climPC1
plot(df$CP1,df$PC1)
m70 <- lm(PC1~CP1,data=df)
summary(m70)
