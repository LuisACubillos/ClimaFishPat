
df2 <- read.csv("Data/data_demersal.csv",sep=";") 
names(df2)
colnames(df2) <- c("YY","SSP","Bt","St","Rt")
df2 <- df2[,c(1,2,4,5)]

Spp <- "Raya_volantin_SUP"
Spp <- "Merluza_sur"
Spp <- "Merluza_cola"
Spp <- "Merluza_3A"
Spp <- "Congrio_sur"
Spp <- "Congrio_norte"
Spp <- "Bacalao"
an.raya <- anom_fish(data=df2,Spp="Raya_volantin_SUP")
an.msur <- anom_fish(data=df2,Spp="Merluza_sur")
an.mcol <- anom_fish(data=df2,Spp="Merluza_cola")
an.m3a <- anom_fish(data=df2,Spp="Merluza_3A")
an.cdn <- anom_fish(data=df2,Spp="Congrio_norte")
an.cds <- anom_fish(data=df2,Spp="Congrio_sur")
an.bac <- anom_fish(data=df2,Spp="Bacalao")

# Test de pendientes
range(an.msur$yy)
range(an.mcol$yy)
range(an.m3a$yy)
range(an.cdn$yy)
range(an.cds$yy)
range(an.bac$yy)
yrs = an.msur$yy-an.msur$yy[1]+1
an.msur.rt <- an.msur$anom_rt[1:41]
an.mcol.rt <- c(rep(NA,8),an.mcol$anom_rt)
an.m3a.rt <- c(NA,an.m3a$anom_rt)
an.cdn.rt <- c(NA, an.cdn$anom_rt)
an.cds.rt <- c(NA, an.cds$anom_rt)
an.bac.rt <- c(rep(NA,15),an.bac$anom_rt,NA)

yrs <- rep(1:41,6)
Sp <- c(rep("Southern_hake",41),
        rep("Patagonian_grenadier",41),
        rep("Blue_within",41),
        rep("North_Kingclip",41),
        rep("South_Kingclip",41),
        rep("Toothfish",41))
an.rt  <- c(an.msur.rt,
            an.mcol.rt,
            an.m3a.rt,
            an.cdn.rt,
            an.cds.rt,
            an.bac.rt)

df.an.rt <- data.frame(Year=yrs,Species=Sp,Anomaly=an.rt)
head(df.an.rt)

library(lme4)
# Pendiente aleatoria
m0 <- lm(Anomaly ~ Year,data = df.an.rt)
summary(m0)
m1 <- lm(Anomaly ~ Year:Species,data = df.an.rt)
summary(m1)
confint(m1)
#Mixed effects
library(nlme)
m2 <- lmer(Anomaly ~ Year+(0+Year|Species),data=df.an.rt)
summary(m2)
confint(m2)
coef(m2)[1]
rr1<- ranef(m2)
dotplot.ranef.mer(rr1<- ranef(m2))

p1 <- ggCaterpillar(ranef(m2, condVar=TRUE), QQ=FALSE, likeDotplot=TRUE)
p1 <- p1 + mi.tema()
library(sjPlot)
tab_model(m2)
tab_model(m2,show.re.var= FALSE)


ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {
  require(ggplot2)
  f <- function(x) {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(y=unlist(x)[ord],
                       ci=1.96*se[ord],
                       nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                       ind=gl(ncol(x), nrow(x), labels=names(x)))
    
    if(QQ) {  ## normal QQ-plot
      p <- ggplot(pDf, aes(nQQ, y))
      p <- p + facet_wrap(~ ind, scales="free")
      p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
    } else {  ## caterpillar dotplot
      p <- ggplot(pDf, aes(ID, y)) + coord_flip()
      if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap(~ ind)
      } else {           ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales="free_y")
      }
      p <- p + xlab("Levels") + ylab("Random effects")
    }
    
    p <- p + theme(legend.position="none")
    p <- p + geom_hline(yintercept=0)
    p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
    p <- p + geom_point(aes(size=1.2), colour="blue") 
    return(p)
  }
  
  lapply(re, f)
}

