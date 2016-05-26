#Este script permite calcular las ventanas con el paquete GenWin para graficar el valor de 
#la diversidad genética de SNPs, obtenida con el programa vcftools
library(GenWin)
library(ggplot2)

#Cargar el archivo que contiene los valores de pi
pi <- read.delim("../../vcftools/data/Maices_all.sites.pi")
# Las ventanas deben ser calculadas por cromosoma, para que tenga sentido biológico
#Extraer los chromosomas 1 a 3
Ch01 <- pi [pi$CHROM==1,]
summary(Ch01)
Ch02 <- pi [pi$CHROM==2,]
summary(Ch01)
Ch03 <- pi [pi$CHROM==3,]
summary(Ch03)

#Calcular el número de ventanas para los cromosomas 1 a3
pi_Chr1 <- splineAnalyze(Ch01$PI,Ch01$POS,smoothness = 100, plotRaw = TRUE, plotWindows = TRUE, method = 1)

pi_Chr2 <- splineAnalyze(Ch02$PI,Ch02$POS,smoothness = 100, plotRaw = TRUE, plotWindows = TRUE, method = 3)

pi_Chr3 <- splineAnalyze(Ch03$PI,Ch03$POS,smoothness = 100, plotRaw = TRUE, plotWindows = TRUE, method = 3)

#Extraer data frames 
Chr1 <- pi_Chr1$windowData
Chr1 <-na.omit(Chr1)
Chr2 <- pi_Chr2$windowData
Chr2 <-na.omit(Chr2)
Chr3 <- pi_Chr3$windowData
Chr3 <-na.omit(Chr3)


#Graficar los valores del estadístico W para el Cromosoma 1
Chr1_plot <- ggplot(Chr1, aes(x=WindowStart, y=Wstat)) + geom_point(shape=1)
#De acuerdo al artículo del programa, para identificar los outliers se puede establecer un limite del 99.9
# Hacer una linea en el percentile 99.9
  Limit <- quantile(Chr1$Wstat,.999)
Chr1_plot <- Chr1_plot+ geom_hline(aes(yintercept=Limit), colour="#990000", linetype=1)+
  labs(title="Cromosoma 1")
Chr1_plot

#Graficar los valores del estadístico W para el cromosoma 2
Chr2_plot <- ggplot(Chr2, aes(x=WindowStart, y=Wstat)) + geom_point(shape=1)
#De acuerdo al artículo del programa, para identificar los outliers se puede establecer un limite del 99.9
# Hacer una linea en el percentile 99.9
Limit <- quantile(Chr2$Wstat,.999)
Chr2_plot <- Chr2_plot+ geom_hline(aes(yintercept=Limit), colour="#990000", linetype=1)+
  labs(title="Cromosoma 2")
Chr2_plot

#Graficar los valores del estadístico W PARA EL CROMOSOMA 3
Chr3_plot <- ggplot(Chr3, aes(x=WindowStart, y=Wstat)) + geom_point(shape=1)
#De acuerdo al artículo del programa, para identificar los outliers se puede establecer un limite del 99.9
# Hacer una linea en el percentile 99.9
  Limit <- quantile(Chr3$Wstat,.999)
Chr3_plot <- Chr3_plot+ geom_hline(aes(yintercept=Limit), colour="#990000", linetype=1)+
  labs(title="Cromosoma 3")
Chr3_plot
