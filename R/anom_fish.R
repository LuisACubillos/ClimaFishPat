anom_fish <- function(data=df,Spp="Jurel"){
  df <- data
  df <- subset(df,SSP==Spp,c(YY,St,Rt))
  with(df,{
    spp <- rep(Spp,length(YY))
    anom_rt <- (log(Rt)-mean(log(Rt)))/sd(log(Rt))
    anom_st <- (log(St)-mean(log(St)))/sd(log(St))
    yy <- YY
    an_list <- data.frame(spp=spp,yy=yy,anom_rt=anom_rt,anom_st=anom_st)
  })
}
