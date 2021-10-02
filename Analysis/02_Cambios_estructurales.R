library(strucchange)

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


range(df.msur$YY)
range(df.mcol$YY)
range(df.m3a$YY)
range(df.cdn$YY)
range(df.cds$YY)
range(df.bac$YY)

# Raya volantin
range(df.raya$YY)
y <- log(df.raya$Rt)
tsy <- ts(y,start=c(1979,1),frequency = 1)
plot(tsy)
#Prueba de Chow
m1 <- Fstats(tsy~1)
sctest(m1)
#El valor-p P=0.5637 (Ho=No hay cambios estructurales). Se rechaza Ho cuando P<0.05
#Si hay cambios estructurales, entonces
bp <- breakpoints(tsy~1)
bp
summary(bp)
plot(bp)
plot(tsy)
lines(bp)
iconf <- confint(bp)
lines(iconf)
##########

# Merluza del sur
range(df.msur$YY)
y <- log(df.msur$Rt)
tsy <- ts(y,start=c(1977,1),frequency = 1)
plot(tsy)
#Prueba de Chow
m1 <- Fstats(tsy~1)
sctest(m1)
#El valor-p P=0.5637 (Ho=No hay cambios estructurales). Se rechaza Ho cuando P<0.05
#Si hay cambios estructurales, entonces
bp <- breakpoints(tsy~1)
bp
summary(bp)
plot(bp)
plot(tsy)
lines(bp)
iconf <- confint(bp)
lines(iconf)

# Merluza de cola
range(df.mcol$YY)
y <- log(df.mcol$Rt)
tsy <- ts(y,start=c(1985,1),frequency = 1)
plot(tsy)
#Prueba de Chow
m1 <- Fstats(tsy~1)
sctest(m1)
#El valor-p P=0.5637 (Ho=No hay cambios estructurales). Se rechaza Ho cuando P<0.05
#Si hay cambios estructurales, entonces
bp <- breakpoints(tsy~1)
bp
summary(bp)
plot(bp)
plot(tsy)
lines(bp)
iconf <- confint(bp)
lines(iconf)

# Merluza de 3 aletas
range(df.m3a$YY)
y <- log(df.m3a$Rt)
tsy <- ts(y,start=c(1978,1),frequency = 1)
plot(tsy)
#Prueba de Chow
m1 <- Fstats(tsy~1)
sctest(m1)
#El valor-p P=0.5637 (Ho=No hay cambios estructurales). Se rechaza Ho cuando P<0.05
#Si hay cambios estructurales, entonces
bp <- breakpoints(tsy~1)
bp
summary(bp)
plot(bp)
plot(tsy)
lines(bp)
iconf <- confint(bp)
lines(iconf)

# Congrio norte
range(df.cdn$YY)
y <- log(df.cdn$Rt)
tsy <- ts(y,start=c(1978,1),frequency = 1)
plot(tsy)
#Prueba de Chow
m1 <- Fstats(tsy~1)
sctest(m1)
#El valor-p P=0.5637 (Ho=No hay cambios estructurales). Se rechaza Ho cuando P<0.05
#Si hay cambios estructurales, entonces
bp <- breakpoints(tsy~1)
bp
summary(bp)
plot(bp)
plot(tsy)
lines(bp)
iconf <- confint(bp)
lines(iconf)
# Congrio sur
range(df.cds$YY)
y <- log(df.cds$Rt)
tsy <- ts(y,start=c(1978,1),frequency = 1)
plot(tsy)
#Prueba de Chow
m1 <- Fstats(tsy~1)
sctest(m1)
#El valor-p P=0.5637 (Ho=No hay cambios estructurales). Se rechaza Ho cuando P<0.05
#Si hay cambios estructurales, entonces
bp <- breakpoints(tsy~1)
bp
summary(bp)
plot(bp)
plot(tsy)
lines(bp)
iconf <- confint(bp)
lines(iconf)

# bacalao
range(df.bac$YY)
y <- log(df.bac$Rt)
tsy <- ts(y,start=c(1992,1),frequency = 1)
plot(tsy)
#Prueba de Chow
m1 <- Fstats(tsy~1)
sctest(m1)
#El valor-p P=0.5637 (Ho=No hay cambios estructurales). Se rechaza Ho cuando P<0.05
#Si hay cambios estructurales, entonces
bp <- breakpoints(tsy~1)
bp
summary(bp)
plot(bp)
plot(tsy)
lines(bp)
iconf <- confint(bp)
lines(iconf)


