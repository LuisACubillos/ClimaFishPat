prep_tabla_srr <- function(data,tr=1){
    n <- length(data$YY)-tr
    ri <- data$Rt[(tr+1):(n+tr)]
    si <- data$St[1:n]
    out <- data.frame(YY=data$YY[1:n],SSBt=si,Rt=ri)
    return(out)
}
