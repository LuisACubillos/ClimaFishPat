
# Carga funciones R y packages --------------------------------------------
code_dir <- "./R/"
file_path <- list.files(code_dir,full.names = T)
for(f_path in file_path){source(f_path,encoding = "UTF-8")}

library(ggplot2)
library(ggpubr)


