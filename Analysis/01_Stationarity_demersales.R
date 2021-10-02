#packages
library(tseries)
library(forecast)

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

# Raya
range(an.raya$yy)
ray <- ts(an.raya$anom_rt,start=c(1979,1),frequency = 1)
plot(ray)
adf.test(ray,alternative="stationary")
ray.dif <- diff(ray)
plot(ray.dif)
adf.test(ray.dif,alternative="stationary")
ray.dif2 <- diff(ray,differences = 2)
plot(ray.dif2)
adf.test(ray.dif2,alternative="stationary")


# Merluza del sur
range(an.msur$yy)
msur <- ts(an.msur$anom_rt,start=c(1977,1),frequency = 1)
plot(msur)
adf.test(msur,alternative="stationary")
msur.dif <- diff(msur)
plot(msur.dif)
adf.test(msur.dif,alternative="stationary")
msur.dif2 <- diff(msur,differences = 2)
plot(msur.dif2)
adf.test(msur.dif2,alternative="stationary")
acf(msur.dif2)
pacf(msur.dif2)
m5 <- arima(msur,order=c(1,2,1))
m5
tsdiag(m5)
Box.test(residuals(m5),type = "Ljung-Box")
pronmsur <- forecast::forecast(m5,h=5)
pronmsur
plot(pronmsur)

# Merluza_cola
range(an.mcol$yy)
mcol <- ts(an.mcol$anom_rt,start=c(1985,1),frequency = 1)
plot(mcol)
adf.test(mcol,alternative="stationary")
mcol.dif <- diff(mcol)
plot(mcol.dif)
adf.test(mcol.dif,alternative="stationary")
mcol.dif2 <- diff(mcol,differences = 2)
plot(mcol.dif2)
adf.test(mcol.dif2,alternative="stationary")

# Merluza 3A
range(an.m3a$yy)
m3a <- ts(an.m3a$anom_rt,start=c(1985,1),frequency = 1)
plot(m3a)
adf.test(m3a,alternative="stationary")
m3a.dif <- diff(m3a)
plot(m3a.dif)
adf.test(m3a.dif,alternative="stationary")
m3a.dif2 <- diff(m3a,differences = 2)
plot(m3a.dif2)
adf.test(m3a.dif2,alternative="stationary")
acf(m3a.dif2)
pacf(m3a.dif2)
m6 <- arima(m3a,order=c(1,2,1))
m6
tsdiag(m6)
Box.test(residuals(m6),type = "Ljung-Box")
pronm3a <- forecast::forecast(m6,h=5)
pronm3a
plot(pronm3a)

# Congrio Norte
range(an.cdn$yy)
cdn <- ts(an.cdn$anom_rt,start=c(1978,1),frequency = 1)
plot(cdn)
adf.test(cdn,alternative="stationary")
cdn.dif <- diff(cdn)
plot(cdn.dif)
adf.test(cdn.dif,alternative="stationary")
cdn.dif2 <- diff(cdn,differences = 2)
plot(cdn.dif2)
adf.test(cdn.dif2,alternative="stationary")
acf(cdn.dif2)
pacf(cdn.dif2)
m7 <- arima(cdn,order=c(0,2,1))
m7
tsdiag(m7)
Box.test(residuals(m7),type = "Ljung-Box")
proncdn <- forecast::forecast(m7,h=5)
proncdn
plot(proncdn)

# Congrio Sur
range(an.cds$yy)
cds <- ts(an.cds$anom_rt,start=c(1978,1),frequency = 1)
plot(cds)
adf.test(cds,alternative="stationary")
cds.dif <- diff(cds)
plot(cds.dif)
adf.test(cds.dif,alternative="stationary")
cds.dif2 <- diff(cds,differences = 2)
plot(cds.dif2)
adf.test(cds.dif2,alternative="stationary")

# Bacalao
range(an.bac$yy)
bac <- ts(an.bac$anom_rt,start=c(1992,1),frequency = 1)
plot(bac)
adf.test(bac,alternative="stationary")
bac.dif <- diff(bac)
plot(bac.dif)
adf.test(bac.dif,alternative="stationary")
acf(bac.dif)
pacf(bac.dif)
m8 <- arima(bac,order=c(2,1,3))
m8
tsdiag(m8)
Box.test(residuals(m8),type = "Ljung-Box")
pronbac <- forecast::forecast(m8,h=5)
pronbac
plot(pronbac)
