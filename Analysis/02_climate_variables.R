

clima <- read.csv("Data/Clima_anual_1970-2019.csv",sep=";")
names(clima)

# Grafica promedios anuales -----------------------------------------------
va <- clima
va$colour <- ifelse(va$SOI < 0, "frío","cálido")
vaf1 <- ggplot(va,label="",aes(y=SOI,x=YY))+
  geom_bar(stat="identity",position="identity",aes(fill=colour))+
  scale_fill_manual(values = c(frío="#0000FF",cálido="#FF0000"))+
  geom_hline(yintercept = 0)+
  xlab("") +
  ylab("Indice Oscilación del Sur")+
  theme_bw()+
  theme(legend.position = "none",strip.background = element_blank(),strip.placement = "outside")+
  mi.tema()
vaf1

va$colour <- ifelse(va$HCI < 0, "cálido","frío")
vaf2 <- ggplot(va,label="",aes(y=HCI,x=YY))+
  geom_bar(stat="identity",position="identity",aes(fill=colour))+
  scale_fill_manual(values = c(frío="#0000FF",cálido="#FF0000"))+
  geom_hline(yintercept = 0)+
  xlab("") +
  ylab("Indice Corriente de Humboldt")+
  theme_bw()+
  theme(legend.position = "none",strip.background = element_blank(),strip.placement = "outside")+
  mi.tema()
vaf2

va$colour <- ifelse(va$SAM < 0, "warm","cold")
vaf3 <- ggplot(va,label="",aes(y=SAM,x=YY))+
  geom_bar(stat="identity",position="identity",aes(fill=colour))+
  scale_fill_manual(values = c(warm="#0000FF",cold="#FF0000"))+
  geom_hline(yintercept = 0)+
  xlab("") +
  ylab("Indice de Oscilación Antartica")+
  theme_bw()+
  theme(legend.position = "none",strip.background = element_blank(),strip.placement = "outside")+
  mi.tema()
vaf3

va$colour <- ifelse(va$PDO < 0, "cold","warm")
vaf4 <- ggplot(va,label="",aes(y=PDO,x=YY))+
  geom_bar(stat="identity",position="identity",aes(fill=colour))+
  scale_fill_manual(values = c(warm="#FF0000",cold="#0000FF"))+
  geom_hline(yintercept = 0)+
  xlab("") +
  ylab("Pacific Decadal Oscillation")+
  theme_bw()+
  theme(legend.position = "none",strip.background = element_blank(),strip.placement = "outside")+
  mi.tema()
vaf4

va$colour <- ifelse(va$TPI < 0, "cold","warm")
vaf5 <- ggplot(va,label="",aes(y=TPI,x=YY))+
  geom_bar(stat="identity",position="identity",aes(fill=colour))+
  scale_fill_manual(values = c(warm="#FF0000",cold="#0000FF"))+
  geom_hline(yintercept = 0)+
  xlab("") +
  ylab("Tripole Index - IPO")+
  theme_bw()+
  theme(legend.position = "none",strip.background = element_blank(),strip.placement = "outside")+
  mi.tema()
vaf5

va$colour <- ifelse(va$NINO34 < 0, "cold","warm")
vaf6 <- ggplot(va,label="",aes(y=NINO34,x=YY))+
  geom_bar(stat="identity",position="identity",aes(fill=colour))+
  scale_fill_manual(values = c(warm="#FF0000",cold="#0000FF"))+
  geom_hline(yintercept = 0)+
  xlab("") +
  ylab("Niño 3+4")+
  theme_bw()+
  theme(legend.position = "none",strip.background = element_blank(),strip.placement = "outside")+
  mi.tema()
vaf6


