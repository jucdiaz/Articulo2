## R-S ##############################################################################################################################
datos_RS<-read.table(file.choose(),header=F,sep=',')
colnames(datos_RS)<-c('tamanio_muestra','mu.b0','mu.b1','sigma.b0','sigma.b1','sigma.b2','p0.b0','p1.b0','p1.b1'
                              ,'mu.b0_verd','mu.b1_verd','sigma.b0_verd','sigma.b1_verd','sigma.b2_verd'
                              ,'p0.b0_verd','p1.b0_verd','p1.b1_verd')
							  
dim(datos_RS)
datos_RS$Error_porc_mu.b0<-abs((datos_RS$mu.b0-datos_RS$mu.b0_verd)/datos_RS$mu.b0_verd)
datos_RS$Error_porc_mu.b1<-abs((datos_RS$mu.b1-datos_RS$mu.b1_verd)/datos_RS$mu.b1_verd)

datos_RS$Error_porc_sigma.b0<-abs((datos_RS$sigma.b0-datos_RS$sigma.b0_verd)/datos_RS$sigma.b0_verd)
datos_RS$Error_porc_sigma.b1<-abs((datos_RS$sigma.b1-datos_RS$sigma.b1_verd)/datos_RS$sigma.b1_verd)
datos_RS$Error_porc_sigma.b2<-abs((datos_RS$sigma.b2-datos_RS$sigma.b2_verd)/datos_RS$sigma.b2_verd)

datos_RS$Error_porc_p0.b0<-abs((datos_RS$p0.b0-datos_RS$p0.b0_verd)/datos_RS$p0.b0_verd)

datos_RS$Error_porc_p1.b0<-abs((datos_RS$p1.b0-datos_RS$p1.b0_verd)/datos_RS$p1.b0_verd)
datos_RS$Error_porc_p1.b1<-abs((datos_RS$p1.b1-datos_RS$p1.b1_verd)/datos_RS$p1.b1_verd)

head(datos_RS)
max(datos_RS$tamanio_muestra)
table(datos_RS$tamanio_muestra)

median_mu.b0<-tapply(datos_RS$mu.b0,datos_RS$tamanio_muestra,median)
median_mu.b1<-tapply(datos_RS$mu.b1,datos_RS$tamanio_muestra,median)
median_sigma.b0<-tapply(datos_RS$sigma.b0,datos_RS$tamanio_muestra,median)
median_sigma.b1<-tapply(datos_RS$sigma.b1,datos_RS$tamanio_muestra,median)
median_sigma.b2<-tapply(datos_RS$sigma.b2,datos_RS$tamanio_muestra,median)
median_p0.b0<-tapply(datos_RS$p0.b0,datos_RS$tamanio_muestra,median)
median_p1.b0<-tapply(datos_RS$p1.b0,datos_RS$tamanio_muestra,median)
median_p1.b1<-tapply(datos_RS$p1.b1,datos_RS$tamanio_muestra,median)


par(mfrow=c(2,4),mar=c(5, 5, 4, 1))
plot(names(median_mu.b0),median_mu.b0,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'0')),main=expression(paste('ZOIP beta \n R-S Parámetro ',mu)),xlab='Tamaño de muestra')
abline(h=datos_RS$mu.b0_verd[1],col='red',lty=2)

plot(names(median_mu.b1),median_mu.b1,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'1')),main=expression(paste('ZOIP beta \n R-S Parámetro ',mu)),xlab='Tamaño de muestra')
abline(h=datos_RS$mu.b1_verd[1],col='red',lty=2)

plot(names(median_sigma.b0),median_sigma.b0,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'0')),main=expression(paste('ZOIP beta \n R-S Parámetro ',sigma)),xlab='Tamaño de muestra')
abline(h=datos_RS$sigma.b0_verd[1],col='red',lty=2)

plot(names(median_sigma.b1),median_sigma.b1,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'1')),main=expression(paste('ZOIP beta \n R-S Parámetro ',sigma)),xlab='Tamaño de muestra')
abline(h=datos_RS$sigma.b1_verd[1],col='red',lty=2)

plot(names(median_sigma.b2),median_sigma.b2,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'2')),main=expression(paste('ZOIP beta \n R-S Parámetro ',sigma)),xlab='Tamaño de muestra')
abline(h=datos_RS$sigma.b2_verd[1],col='red',lty=2)

plot(names(median_p0.b0),median_p0.b0,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'0')),main=expression(paste('ZOIP beta \n R-S Parámetro ',p0)),xlab='Tamaño de muestra')
abline(h=datos_RS$p0.b0_verd[1],col='red',lty=2)

plot(names(median_p1.b0),median_p1.b0,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'0')),main=expression(paste('ZOIP beta \n R-S Parámetro ',p1)),xlab='Tamaño de muestra')
abline(h=datos_RS$p1.b0_verd[1],col='red',lty=2)

plot(names(median_p1.b1),median_p1.b1,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'1')),main=expression(paste('ZOIP beta \n R-S Parámetro ',p1)),xlab='Tamaño de muestra')
abline(h=datos_RS$p1.b1_verd[1],col='red',lty=2)

## F-C ##############################################################################################################################
datos_FC<-read.table(file.choose(),header=F,sep=',')
colnames(datos_FC)<-c('tamanio_muestra','mu.b0','mu.b1','sigma.b0','sigma.b1','sigma.b2','p0.b0','p1.b0','p1.b1'
                              ,'mu.b0_verd','mu.b1_verd','sigma.b0_verd','sigma.b1_verd','sigma.b2_verd'
                              ,'p0.b0_verd','p1.b0_verd','p1.b1_verd')
							  

datos_FC$Error_porc_mu.b0<-abs((datos_FC$mu.b0-datos_FC$mu.b0_verd)/datos_FC$mu.b0)
datos_FC$Error_porc_mu.b1<-abs((datos_FC$mu.b1-datos_FC$mu.b1_verd)/datos_FC$mu.b1)

datos_FC$Error_porc_sigma.b0<-abs((datos_FC$sigma.b0-datos_FC$sigma.b0_verd)/datos_FC$sigma.b0)
datos_FC$Error_porc_sigma.b1<-abs((datos_FC$sigma.b1-datos_FC$sigma.b1_verd)/datos_FC$sigma.b1)
datos_FC$Error_porc_sigma.b2<-abs((datos_FC$sigma.b2-datos_FC$sigma.b2_verd)/datos_FC$sigma.b2)

datos_FC$Error_porc_p0.b0<-abs((datos_FC$p0.b0-datos_FC$p0.b0_verd)/datos_FC$p0.b0)

datos_FC$Error_porc_p1.b0<-abs((datos_FC$p1.b0-datos_FC$p1.b0_verd)/datos_FC$p1.b0)
datos_FC$Error_porc_p1.b1<-abs((datos_FC$p1.b1-datos_FC$p1.b1_verd)/datos_FC$p1.b1)

head(datos_FC)
max(datos_FC$tamanio_muestra)
table(datos_FC$tamanio_muestra)

median_mu.b0<-tapply(datos_FC$mu.b0,datos_FC$tamanio_muestra,median)
median_mu.b1<-tapply(datos_FC$mu.b1,datos_FC$tamanio_muestra,median)
median_sigma.b0<-tapply(datos_FC$sigma.b0,datos_FC$tamanio_muestra,median)
median_sigma.b1<-tapply(datos_FC$sigma.b1,datos_FC$tamanio_muestra,median)
median_sigma.b2<-tapply(datos_FC$sigma.b2,datos_FC$tamanio_muestra,median)
median_p0.b0<-tapply(datos_FC$p0.b0,datos_FC$tamanio_muestra,median)
median_p1.b0<-tapply(datos_FC$p1.b0,datos_FC$tamanio_muestra,median)
median_p1.b1<-tapply(datos_FC$p1.b1,datos_FC$tamanio_muestra,median)


par(mfrow=c(2,4),mar=c(5, 5, 4, 1))
plot(names(median_mu.b0),median_mu.b0,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'0')),main=expression(paste('ZOIP beta \n F-C Parámetro ',mu)),xlab='Tamaño de muestra')
abline(h=datos_FC$mu.b0_verd[1],col='red',lty=2)

plot(names(median_mu.b1),median_mu.b1,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'1')),main=expression(paste('ZOIP beta \n F-C Parámetro ',mu)),xlab='Tamaño de muestra')
abline(h=datos_FC$mu.b1_verd[1],col='red',lty=2)

plot(names(median_sigma.b0),median_sigma.b0,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'0')),main=expression(paste('ZOIP beta \n F-C Parámetro ',sigma)),xlab='Tamaño de muestra')
abline(h=datos_FC$sigma.b0_verd[1],col='red',lty=2)

plot(names(median_sigma.b1),median_sigma.b1,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'1')),main=expression(paste('ZOIP beta \n F-C Parámetro ',sigma)),xlab='Tamaño de muestra')
abline(h=datos_FC$sigma.b1_verd[1],col='red',lty=2)

plot(names(median_sigma.b2),median_sigma.b2,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'2')),main=expression(paste('ZOIP beta \n F-C Parámetro ',sigma)),xlab='Tamaño de muestra')
abline(h=datos_FC$sigma.b2_verd[1],col='red',lty=2)

plot(names(median_p0.b0),median_p0.b0,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'0')),main=expression(paste('ZOIP beta \n F-C Parámetro ',p0)),xlab='Tamaño de muestra')
abline(h=datos_FC$p0.b0_verd[1],col='red',lty=2)

plot(names(median_p1.b0),median_p1.b0,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'0')),main=expression(paste('ZOIP beta \n F-C Parámetro ',p1)),xlab='Tamaño de muestra')
abline(h=datos_FC$p1.b0_verd[1],col='red',lty=2)

plot(names(median_p1.b1),median_p1.b1,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'1')),main=expression(paste('ZOIP beta \n F-C Parámetro ',p1)),xlab='Tamaño de muestra')
abline(h=datos_FC$p1.b1_verd[1],col='red',lty=2)


## ORIGINAL ##############################################################################################################################
datos_Ori<-read.table(file.choose(),header=F,sep=',')
colnames(datos_Ori)<-c('tamanio_muestra','mu.b0','mu.b1','sigma.b0','sigma.b1','sigma.b2','p0.b0','p1.b0','p1.b1'
                              ,'mu.b0_verd','mu.b1_verd','sigma.b0_verd','sigma.b1_verd','sigma.b2_verd'
                              ,'p0.b0_verd','p1.b0_verd','p1.b1_verd')
							  

datos_Ori$Error_porc_mu.b0<-abs((datos_Ori$mu.b0-datos_Ori$mu.b0_verd)/datos_Ori$mu.b0)
datos_Ori$Error_porc_mu.b1<-abs((datos_Ori$mu.b1-datos_Ori$mu.b1_verd)/datos_Ori$mu.b1)

datos_Ori$Error_porc_sigma.b0<-abs((datos_Ori$sigma.b0-datos_Ori$sigma.b0_verd)/datos_Ori$sigma.b0)
datos_Ori$Error_porc_sigma.b1<-abs((datos_Ori$sigma.b1-datos_Ori$sigma.b1_verd)/datos_Ori$sigma.b1)
datos_Ori$Error_porc_sigma.b2<-abs((datos_Ori$sigma.b2-datos_Ori$sigma.b2_verd)/datos_Ori$sigma.b2)

datos_Ori$Error_porc_p0.b0<-abs((datos_Ori$p0.b0-datos_Ori$p0.b0_verd)/datos_Ori$p0.b0)

datos_Ori$Error_porc_p1.b0<-abs((datos_Ori$p1.b0-datos_Ori$p1.b0_verd)/datos_Ori$p1.b0)
datos_Ori$Error_porc_p1.b1<-abs((datos_Ori$p1.b1-datos_Ori$p1.b1_verd)/datos_Ori$p1.b1)

head(datos_Ori)
max(datos_Ori$tamanio_muestra)
table(datos_Ori$tamanio_muestra)

median_mu.b0<-tapply(datos_Ori$mu.b0,datos_Ori$tamanio_muestra,median)
median_mu.b1<-tapply(datos_Ori$mu.b1,datos_Ori$tamanio_muestra,median)
median_sigma.b0<-tapply(datos_Ori$sigma.b0,datos_Ori$tamanio_muestra,median)
median_sigma.b1<-tapply(datos_Ori$sigma.b1,datos_Ori$tamanio_muestra,median)
median_sigma.b2<-tapply(datos_Ori$sigma.b2,datos_Ori$tamanio_muestra,median)
median_p0.b0<-tapply(datos_Ori$p0.b0,datos_Ori$tamanio_muestra,median)
median_p1.b0<-tapply(datos_Ori$p1.b0,datos_Ori$tamanio_muestra,median)
median_p1.b1<-tapply(datos_Ori$p1.b1,datos_Ori$tamanio_muestra,median)


par(mfrow=c(2,4),mar=c(5, 5, 4, 1))
plot(names(median_mu.b0),median_mu.b0,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'0')),main=expression(paste('ZOIP beta \n Original Parámetro ',mu)),xlab='Tamaño de muestra')
abline(h=datos_Ori$mu.b0_verd[1],col='red',lty=2)

plot(names(median_mu.b1),median_mu.b1,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'1')),main=expression(paste('ZOIP beta \n Original Parámetro ',mu)),xlab='Tamaño de muestra')
abline(h=datos_Ori$mu.b1_verd[1],col='red',lty=2)

plot(names(median_sigma.b0),median_sigma.b0,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'0')),main=expression(paste('ZOIP beta \n Original Parámetro ',sigma)),xlab='Tamaño de muestra')
abline(h=datos_Ori$sigma.b0_verd[1],col='red',lty=2)

plot(names(median_sigma.b1),median_sigma.b1,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'1')),main=expression(paste('ZOIP beta \n Original Parámetro ',sigma)),xlab='Tamaño de muestra')
abline(h=datos_Ori$sigma.b1_verd[1],col='red',lty=2)

plot(names(median_sigma.b2),median_sigma.b2,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'2')),main=expression(paste('ZOIP beta \n Original Parámetro ',sigma)),xlab='Tamaño de muestra')
abline(h=datos_Ori$sigma.b2_verd[1],col='red',lty=2)

plot(names(median_p0.b0),median_p0.b0,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'0')),main=expression(paste('ZOIP beta \n Original Parámetro ',p0)),xlab='Tamaño de muestra')
abline(h=datos_Ori$p0.b0_verd[1],col='red',lty=2)

plot(names(median_p1.b0),median_p1.b0,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'0')),main=expression(paste('ZOIP beta \n Original Parámetro ',p1)),xlab='Tamaño de muestra')
abline(h=datos_Ori$p1.b0_verd[1],col='red',lty=2)

plot(names(median_p1.b1),median_p1.b1,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'1')),main=expression(paste('ZOIP beta \n Original Parámetro ',p1)),xlab='Tamaño de muestra')
abline(h=datos_Ori$p1.b1_verd[1],col='red',lty=2)


## simplex ##############################################################################################################################
datos_simplex<-read.table(file.choose(),header=F,sep=',')
colnames(datos_simplex)<-c('tamanio_muestra','mu.b0','mu.b1','sigma.b0','sigma.b1','sigma.b2','p0.b0','p1.b0','p1.b1'
                              ,'mu.b0_verd','mu.b1_verd','sigma.b0_verd','sigma.b1_verd','sigma.b2_verd'
                              ,'p0.b0_verd','p1.b0_verd','p1.b1_verd')
							  

datos_simplex$Error_porc_mu.b0<-abs((datos_simplex$mu.b0-datos_simplex$mu.b0_verd)/datos_simplex$mu.b0)
datos_simplex$Error_porc_mu.b1<-abs((datos_simplex$mu.b1-datos_simplex$mu.b1_verd)/datos_simplex$mu.b1)

datos_simplex$Error_porc_sigma.b0<-abs((datos_simplex$sigma.b0-datos_simplex$sigma.b0_verd)/datos_simplex$sigma.b0)
datos_simplex$Error_porc_sigma.b1<-abs((datos_simplex$sigma.b1-datos_simplex$sigma.b1_verd)/datos_simplex$sigma.b1)
datos_simplex$Error_porc_sigma.b2<-abs((datos_simplex$sigma.b2-datos_simplex$sigma.b2_verd)/datos_simplex$sigma.b2)

datos_simplex$Error_porc_p0.b0<-abs((datos_simplex$p0.b0-datos_simplex$p0.b0_verd)/datos_simplex$p0.b0)

datos_simplex$Error_porc_p1.b0<-abs((datos_simplex$p1.b0-datos_simplex$p1.b0_verd)/datos_simplex$p1.b0)
datos_simplex$Error_porc_p1.b1<-abs((datos_simplex$p1.b1-datos_simplex$p1.b1_verd)/datos_simplex$p1.b1)

head(datos_simplex)
max(datos_simplex$tamanio_muestra)
table(datos_simplex$tamanio_muestra)

median_mu.b0<-tapply(datos_simplex$mu.b0,datos_simplex$tamanio_muestra,median)
median_mu.b1<-tapply(datos_simplex$mu.b1,datos_simplex$tamanio_muestra,median)
median_sigma.b0<-tapply(datos_simplex$sigma.b0,datos_simplex$tamanio_muestra,median)
median_sigma.b1<-tapply(datos_simplex$sigma.b1,datos_simplex$tamanio_muestra,median)
median_sigma.b2<-tapply(datos_simplex$sigma.b2,datos_simplex$tamanio_muestra,median)
median_p0.b0<-tapply(datos_simplex$p0.b0,datos_simplex$tamanio_muestra,median)
median_p1.b0<-tapply(datos_simplex$p1.b0,datos_simplex$tamanio_muestra,median)
median_p1.b1<-tapply(datos_simplex$p1.b1,datos_simplex$tamanio_muestra,median)


par(mfrow=c(2,4),mar=c(5, 5, 4, 1))
plot(names(median_mu.b0),median_mu.b0,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'0')),main=expression(paste('ZOIP beta \n simplex Parámetro ',mu)),xlab='Tamaño de muestra')
abline(h=datos_simplex$mu.b0_verd[1],col='red',lty=2)

plot(names(median_mu.b1),median_mu.b1,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'1')),main=expression(paste('ZOIP beta \n simplex Parámetro ',mu)),xlab='Tamaño de muestra')
abline(h=datos_simplex$mu.b1_verd[1],col='red',lty=2)

plot(names(median_sigma.b0),median_sigma.b0,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'0')),main=expression(paste('ZOIP beta \n simplex Parámetro ',sigma)),xlab='Tamaño de muestra')
abline(h=datos_simplex$sigma.b0_verd[1],col='red',lty=2)

plot(names(median_sigma.b1),median_sigma.b1,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'1')),main=expression(paste('ZOIP beta \n simplex Parámetro ',sigma)),xlab='Tamaño de muestra')
abline(h=datos_simplex$sigma.b1_verd[1],col='red',lty=2)

plot(names(median_sigma.b2),median_sigma.b2,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'2')),main=expression(paste('ZOIP beta \n simplex Parámetro ',sigma)),xlab='Tamaño de muestra')
abline(h=datos_simplex$sigma.b2_verd[1],col='red',lty=2)

plot(names(median_p0.b0),median_p0.b0,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'0')),main=expression(paste('ZOIP beta \n simplex Parámetro ',p0)),xlab='Tamaño de muestra')
abline(h=datos_simplex$p0.b0_verd[1],col='red',lty=2)

plot(names(median_p1.b0),median_p1.b0,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'0')),main=expression(paste('ZOIP beta \n simplex Parámetro ',p1)),xlab='Tamaño de muestra')
abline(h=datos_simplex$p1.b0_verd[1],col='red',lty=2)

plot(names(median_p1.b1),median_p1.b1,type='l'
     , ylab=expression(paste('mediana ',hat(beta),'1')),main=expression(paste('ZOIP beta \n simplex Parámetro ',p1)),xlab='Tamaño de muestra')
abline(h=datos_simplex$p1.b1_verd[1],col='red',lty=2)

#----------------------------------------------------------------------------------------------------------------------------

error_mu.b0_C1<-tapply(datos_RS$Error_porc_mu.b0,datos_RS$tamanio_m,median)
error_mu.b1_C1<-tapply(datos_RS$Error_porc_mu.b1,datos_RS$tamanio_m,median)

error_sigma.b0_C1<-tapply(datos_RS$Error_porc_sigma.b0,datos_RS$tamanio_m,median)
error_sigma.b1_C1<-tapply(datos_RS$Error_porc_sigma.b1,datos_RS$tamanio_m,median)
error_sigma.b2_C1<-tapply(datos_RS$Error_porc_sigma.b2,datos_RS$tamanio_m,median)

error_p0.b0_C1<-tapply(datos_RS$Error_porc_p0.b0,datos_RS$tamanio_m,median)

error_p1.b0_C1<-tapply(datos_RS$Error_porc_p1.b0,datos_RS$tamanio_m,median)
error_p1.b1_C1<-tapply(datos_RS$Error_porc_p1.b1,datos_RS$tamanio_m,median)

median(error_mu.b0_C1)
median(error_mu.b1_C1)
median(error_sigma.b0_C1)
median(error_sigma.b1_C1)
median(error_sigma.b2_C1)
median(error_p0.b0_C1)
median(error_p1.b0_C1)
median(error_p1.b1_C1)
##########
error_mu.b0_C2<-tapply(datos_FC$Error_porc_mu.b0,datos_FC$tamanio_m,median)
error_mu.b1_C2<-tapply(datos_FC$Error_porc_mu.b1,datos_FC$tamanio_m,median)

error_sigma.b0_C2<-tapply(datos_FC$Error_porc_sigma.b0,datos_FC$tamanio_m,median)
error_sigma.b1_C2<-tapply(datos_FC$Error_porc_sigma.b1,datos_FC$tamanio_m,median)
error_sigma.b2_C2<-tapply(datos_FC$Error_porc_sigma.b2,datos_FC$tamanio_m,median)

error_p0.b0_C2<-tapply(datos_FC$Error_porc_p0.b0,datos_FC$tamanio_m,median)

error_p1.b0_C2<-tapply(datos_FC$Error_porc_p1.b0,datos_FC$tamanio_m,median)
error_p1.b1_C2<-tapply(datos_FC$Error_porc_p1.b1,datos_FC$tamanio_m,median)

median(error_mu.b0_C2)
median(error_mu.b1_C2)
median(error_sigma.b0_C2)
median(error_sigma.b1_C2)
median(error_sigma.b2_C2)
median(error_p0.b0_C2)
median(error_p1.b0_C2)
median(error_p1.b1_C2)
#########
error_mu.b0_C3<-tapply(datos_Ori$Error_porc_mu.b0,datos_Ori$tamanio_m,median)
error_mu.b1_C3<-tapply(datos_Ori$Error_porc_mu.b1,datos_Ori$tamanio_m,median)

error_sigma.b0_C3<-tapply(datos_Ori$Error_porc_sigma.b0,datos_Ori$tamanio_m,median)
error_sigma.b1_C3<-tapply(datos_Ori$Error_porc_sigma.b1,datos_Ori$tamanio_m,median)
error_sigma.b2_C3<-tapply(datos_Ori$Error_porc_sigma.b2,datos_Ori$tamanio_m,median)

error_p0.b0_C3<-tapply(datos_Ori$Error_porc_p0.b0,datos_Ori$tamanio_m,median)

error_p1.b0_C3<-tapply(datos_Ori$Error_porc_p1.b0,datos_Ori$tamanio_m,median)
error_p1.b1_C3<-tapply(datos_Ori$Error_porc_p1.b1,datos_Ori$tamanio_m,median)

median(error_mu.b0_C3)
median(error_mu.b1_C3)
median(error_sigma.b0_C3)
median(error_sigma.b1_C3)
median(error_sigma.b2_C3)
median(error_p0.b0_C3)
median(error_p1.b0_C3)
median(error_p1.b1_C3)

##########
error_mu.b0_C4<-tapply(datos_simplex$Error_porc_mu.b0,datos_simplex$tamanio_m,median)
error_mu.b1_C4<-tapply(datos_simplex$Error_porc_mu.b1,datos_simplex$tamanio_m,median)

error_sigma.b0_C4<-tapply(datos_simplex$Error_porc_sigma.b0,datos_simplex$tamanio_m,median)
error_sigma.b1_C4<-tapply(datos_simplex$Error_porc_sigma.b1,datos_simplex$tamanio_m,median)
error_sigma.b2_C4<-tapply(datos_simplex$Error_porc_sigma.b2,datos_simplex$tamanio_m,median)

error_p0.b0_C4<-tapply(datos_simplex$Error_porc_p0.b0,datos_simplex$tamanio_m,median)

error_p1.b0_C4<-tapply(datos_simplex$Error_porc_p1.b0,datos_simplex$tamanio_m,median)
error_p1.b1_C4<-tapply(datos_simplex$Error_porc_p1.b1,datos_simplex$tamanio_m,median)


median(error_mu.b0_C4)
median(error_mu.b1_C4)
median(error_sigma.b0_C4)
median(error_sigma.b1_C4)
median(error_sigma.b2_C4)
median(error_p0.b0_C4)
median(error_p1.b0_C4)
median(error_p1.b1_C4)

################Graficos
par(mfrow=c(2,4))
plot(names(error_mu.b0_C1),error_mu.b0_C1,type='l'
     ,xlab='Tamaño de muestra', ylab='Mape',ylim=c(0,1))
lines(names(error_mu.b0_C2),error_mu.b0_C2,col='chartreuse4')
lines(names(error_mu.b0_C3),error_mu.b0_C3,col='dodgerblue2')
lines(names(error_mu.b0_C4),error_mu.b0_C4,col='darkorange3')
abline(h=0,col='red',lty=2)
legend("topright", legend = c(expression(paste('MAPE ',mu,' ',beta,'0 Caso1')), expression(paste('MAPE ',mu,' ',beta,'0 Caso2')), expression(paste('MAPE ',mu,' ',beta,'0 Caso3')),expression(paste('MAPE ',mu,' ',beta,'0 Caso4'))),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'),bty='n')
	   
plot(names(error_mu.b1_C1),error_mu.b1_C1,type='l'
     ,xlab='Tamaño de muestra', ylab='Mape',ylim=c(0,1))
lines(names(error_mu.b1_C2),error_mu.b1_C2,col='chartreuse4')
lines(names(error_mu.b1_C3),error_mu.b1_C3,col='dodgerblue2')
lines(names(error_mu.b1_C4),error_mu.b1_C4,col='darkorange3')
abline(h=0,col='red',lty=2)
legend("topright", legend = c(expression(paste('MAPE ',mu,' ',beta,'0 Caso1')), expression(paste('MAPE ',mu,' ',beta,'0 Caso2')), expression(paste('MAPE ',mu,' ',beta,'0 Caso3')),expression(paste('MAPE ',mu,' ',beta,'0 Caso4'))),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'),bty='n')
	   
plot(names(error_sigma.b0_C1),error_sigma.b0_C1,type='l'
     ,xlab='Tamaño de muestra', ylab='Mape',ylim=c(0,1))
lines(names(error_sigma.b0_C2),error_sigma.b0_C2,col='chartreuse4')
lines(names(error_sigma.b0_C3),error_sigma.b0_C3,col='dodgerblue2')
lines(names(error_sigma.b0_C4),error_sigma.b0_C4,col='darkorange3')
abline(h=0,col='red',lty=2)
legend("topright", legend = c(expression(paste('MAPE ',sigma,' ',beta,'0 Caso1')), expression(paste('MAPE ',sigma,' ',beta,'0 Caso2')), expression(paste('MAPE ',sigma,' ',beta,'0 Caso3')),expression(paste('MAPE ',sigma,' ',beta,'0 Caso4'))),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'),bty='n')
	   
plot(names(error_sigma.b1_C1),error_sigma.b1_C1,type='l'
     ,xlab='Tamaño de muestra', ylab='Mape',ylim=c(0,1))
lines(names(error_sigma.b1_C2),error_sigma.b1_C2,col='chartreuse4')
lines(names(error_sigma.b1_C3),error_sigma.b1_C3,col='dodgerblue2')
lines(names(error_sigma.b1_C4),error_sigma.b1_C4,col='darkorange3')
abline(h=0,col='red',lty=2)
legend("topright", legend = c(expression(paste('MAPE ',sigma,' ',beta,'0 Caso1')), expression(paste('MAPE ',sigma,' ',beta,'0 Caso2')), expression(paste('MAPE ',sigma,' ',beta,'0 Caso3')),expression(paste('MAPE ',sigma,' ',beta,'0 Caso4'))),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'),bty='n')

plot(names(error_sigma.b2_C1),error_sigma.b2_C1,type='l'
     ,xlab='Tamaño de muestra', ylab='Mape',ylim=c(0,1))
lines(names(error_sigma.b2_C2),error_sigma.b2_C2,col='chartreuse4')
lines(names(error_sigma.b2_C3),error_sigma.b2_C3,col='dodgerblue2')
lines(names(error_sigma.b2_C4),error_sigma.b2_C4,col='darkorange3')
abline(h=0,col='red',lty=2)
legend("topright", legend = c(expression(paste('MAPE ',sigma,' ',beta,'0 Caso1')), expression(paste('MAPE ',sigma,' ',beta,'0 Caso2')), expression(paste('MAPE ',sigma,' ',beta,'0 Caso3')),expression(paste('MAPE ',sigma,' ',beta,'0 Caso4'))),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'),bty='n')

plot(names(error_p0.b0_C1),error_p0.b0_C1,type='l'
     ,xlab='Tamaño de muestra', ylab='Mape',ylim=c(0,1))
lines(names(error_p0.b0_C2),error_p0.b0_C2,col='chartreuse4')
lines(names(error_p0.b0_C3),error_p0.b0_C3,col='dodgerblue2')
lines(names(error_p0.b0_C4),error_p0.b0_C4,col='darkorange3')
abline(h=0,col='red',lty=2)
legend("topright", legend = c(expression(paste('MAPE ',sigma,' ',beta,'0 Caso1')), expression(paste('MAPE ',sigma,' ',beta,'0 Caso2')), expression(paste('MAPE ',sigma,' ',beta,'0 Caso3')),expression(paste('MAPE ',sigma,' ',beta,'0 Caso4'))),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'),bty='n')

plot(names(error_p1.b0_C1),error_p1.b0_C1,type='l'
     ,xlab='Tamaño de muestra', ylab='Mape',ylim=c(0,3))
lines(names(error_p1.b0_C2),error_p1.b0_C2,col='chartreuse4')
lines(names(error_p1.b0_C3),error_p1.b0_C3,col='dodgerblue2')
lines(names(error_p1.b0_C4),error_p1.b0_C4,col='darkorange3')
abline(h=0,col='red',lty=2)
legend("topright", legend = c(expression(paste('MAPE ',sigma,' ',beta,'0 Caso1')), expression(paste('MAPE ',sigma,' ',beta,'0 Caso2')), expression(paste('MAPE ',sigma,' ',beta,'0 Caso3')),expression(paste('MAPE ',sigma,' ',beta,'0 Caso4'))),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'),bty='n')

plot(names(error_p1.b1_C1),error_p1.b1_C1,type='l'
     ,xlab='Tamaño de muestra', ylab='Mape',ylim=c(0,1))
lines(names(error_p1.b1_C2),error_p1.b1_C2,col='chartreuse4')
lines(names(error_p1.b1_C3),error_p1.b1_C3,col='dodgerblue2')
lines(names(error_p1.b1_C4),error_p1.b1_C4,col='darkorange3')
abline(h=0,col='red',lty=2)
legend("topright", legend = c(expression(paste('MAPE ',sigma,' ',beta,'0 Caso1')), expression(paste('MAPE ',sigma,' ',beta,'0 Caso2')), expression(paste('MAPE ',sigma,' ',beta,'0 Caso3')),expression(paste('MAPE ',sigma,' ',beta,'0 Caso4'))),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'),bty='n')


error_C1<-apply(cbind(error_mu.b0_C1,error_mu.b1_C1,error_sigma.b0_C1,error_sigma.b1_C1,error_sigma.b2_C1,error_p0.b0_C1,error_p1.b0_C1,error_p1.b1_C1),1,median)
error_C2<-apply(cbind(error_mu.b0_C2,error_mu.b1_C2,error_sigma.b0_C2,error_sigma.b1_C2,error_sigma.b2_C2,error_p0.b0_C2,error_p1.b0_C2,error_p1.b1_C2),1,median)
error_C3<-apply(cbind(error_mu.b0_C3,error_mu.b1_C3,error_sigma.b0_C3,error_sigma.b1_C3,error_sigma.b2_C3,error_p0.b0_C3,error_p1.b0_C3,error_p1.b1_C3),1,median)
error_C4<-apply(cbind(error_mu.b0_C4,error_mu.b1_C4,error_sigma.b0_C4,error_sigma.b1_C4,error_sigma.b2_C4,error_p0.b0_C4,error_p1.b0_C4,error_p1.b1_C4),1,median)


mean(c(error_C1[72],error_C2[72],error_C3[72],error_C4[72]))

par(mfrow=c(1,1))
plot(names(error_C1),error_C1,type='l'
     ,xlab='Tamaño muestra', ylab='Mediana MAPE',ylim=c(0,1.1),las=1,main='Errores MAPE del estudio \n del simulación')
lines(names(error_C2),error_C2,col='chartreuse4')
lines(names(error_C3),error_C3,col='dodgerblue2')
lines(names(error_C4),error_C4,col='darkorange3')
#abline(h=0,col='red',lty=2)
legend("topright", legend = c("MAPE Caso R-S", "MAPE Caso F-C", "MAPE Caso original","MAPE Caso simplex" ),
       lty=1,col = c('black', 'chartreuse4', 'dodgerblue2', 'darkorange3'),bty='n')
	   
	   
