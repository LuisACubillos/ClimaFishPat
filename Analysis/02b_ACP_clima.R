# Carga paquetes ----------------------------------------------------------
library(FactoMineR)
library(factoextra)

clima <- read.csv("Data/Clima_anual_1970-2019.csv",sep=";")
names(clima)
yrs <- seq(1970,2015,1)

# Componentes principales -------------------------------------------------
dat <-clima[,c(2,4:7)]
rownames(dat) <- clima[,1]
acp <- PCA(dat,graph = FALSE,ncp=ncol(dat))
x=1:length(acp$eig[,1])
barplot(acp$eig[,1],names.arg=x, main="Eigen values")
barplot(acp$eig[,2],names.arg=x, main="Porcentaje de Inercia", xlab="N° Componentes",ylab="Inercia (%)")

plot.PCA(acp, choix = "var", axes = c(1,2),new.plot=FALSE, title="", lim.cos2.var = 0.1)
plot.PCA(acp, choix = "var", axes = c(1,3),new.plot=FALSE, title="", lim.cos2.var = 0.1)

fviz_pca_var(acp)+
  mi.tema()

fviz_pca_var(acp,col.var="contrib")+
  mi.tema()

eig.val <- get_eigenvalue(acp)
fviz_eig(acp)
#row values
row <- get_pca(acp)
row
# Coordinates
head(row$coord)
# Cos2: quality on the factore map
row$cos2
# Contributions to the principal components
head(row$contrib)
fviz_pca(acp)
fviz_cos2(acp)

cor(acp$ind$coord[,1],dat[,1]) #HCI
cor(acp$ind$coord[,1],dat[,2]) #TPI
cor(acp$ind$coord[,1],dat[,3]) #SOI
cor(acp$ind$coord[,1],dat[,4]) #PDO
cor(acp$ind$coord[,1],dat[,5]) #SAM

cor(acp$ind$coord[,2],dat[,1]) #HCI
cor(acp$ind$coord[,2],ambiente[,2]) #TPI
cor(acp$ind$coord[,2],ambiente[,3]) #SOI
cor(acp$ind$coord[,2],ambiente[,4]) #PDO
cor(acp$ind$coord[,2],ambiente[,5]) #SAM

cor(acp$ind$coord[,3],ambiente[,1]) #HCI
cor(acp$ind$coord[,3],ambiente[,2]) #TPI
cor(acp$ind$coord[,3],ambiente[,3]) #SOI
cor(acp$ind$coord[,3],ambiente[,4]) #PDO
cor(acp$ind$coord[,3],ambiente[,5]) #SAM

plot(clima[,1],acp$ind$coord[,1],ty="s")
plot(clima[,1],acp$ind$coord[,2],ty="s")
plot(clima[,1],acp$ind$coord[,3],ty="s")

df <- NULL
df <- data.frame(YY=clima[,1],PC1=acp$ind$coord[,1],PC2=acp$ind$coord[,2])
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
  ylab("PC2")+
  theme_bw()+
  theme(legend.position = "none",strip.background = element_blank(),strip.placement = "outside")+
  mi.tema()
p2



#
yrs <- seq(1978,2014,1)
Rmsur=log(df.msur$Rt[2:38])
Rm3a=log(df.m3a$Rt[1:37])
Rcdn=log(df.cdn$Rt[1:37])
Rcds=log(df.cds$Rt[1:37])
Clima1 = df$PC1[9:45]
Clima2 = df$PC2[9:45]
#Segunda seleccion
yrs <- seq(1985,2014,1)
Rmsur=log(df.msur$Rt[9:38])
Rm3a=log(df.m3a$Rt[8:37])
Rmcol=log(df.mcol$Rt[1:30])
Rcdn=log(df.cdn$Rt[8:37])
Rcds=log(df.cds$Rt[8:37])
Clima1 = df$PC1[16:45]
Clima2 = df$PC2[16:45]

cor(Clima1,Rmsur)
cor(Clima1,Rm3a)
cor(Clima1,Rcdn)
cor(Clima1,Rcds)
cor(Clima1,Rmcol)
cor(Clima2,Rmsur)
cor(Clima2,Rm3a)
cor(Clima2,Rcdn)
cor(Clima2,Rcds)
cor(Clima2,Rmcol)
dtcor <- data.frame(PC1=Clima1,PC2=Clima2,Rmsur,Rm3a,Rcdn,Rcds)
dtcor <- data.frame(PC1=Clima1,PC2=Clima2,Rmsur,Rm3a,Rcdn,Rcds,Rmcol)

cordat=round(cor(dtcor, use="pairwise.complete.obs", method ="spearman"),2)
library(reshape2)
head(cordat)
melted_cordat <- melt(cordat)
head(melted_cordat)

ggplot(data = melted_cordat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+ theme(axis.text.x = element_text(angle = 90))

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cordat){
  cordat[upper.tri(cordat)] <- NA
  return(cordat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cordat){
  cordat[lower.tri(cordat)]<- NA
  return(cordat)
}
upper_tri <- get_upper_tri(cordat)
upper_tri

melted_cordat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
ggplot(data = melted_cordat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 18, hjust = 1),
        axis.text.y = element_text( size = 18),
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22))+
  coord_fixed()+ guides(fill = guide_colorbar(barheight = 15, barwidth = 2.2))+
  theme(legend.title = element_text(size=22))+
  theme(legend.text = element_text(size=20))






