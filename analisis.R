
##################### Analisis #######################

freq(df$A.8, cumul = FALSE)
freq(df$B.1, cumul = FALSE)
df$edad4t <- cut(df$A.6,
                 breaks = c(min(df$A.6),
                   25, 35, max(df$A.6)))
freq(df$edad4t, report.nas = FALSE)

# RTA: 93% de las encuestadas son mujeres cis, mientras que solo el 3.57% son hombres 
# RTA: 39% de las encuestadas se dedican a la industria de software (implementacion, desarrollo, arquitectura), un 10.57% al disenio
# RTA: un 75% de los encuestados tiene entre 25-35 anios o menos, un 16% tiene entre 18-25 y el resto (24%) pertenecen a 35 a 56 anios 


####  Existe una relacion entre el genero y la especialidad elegida? -----

genero_espe <- table(df$A.8, df$B.1)
round(prop.table(genero_espe), 3)* 100

# RTA: existe una mayoria de mujeres con especialidad en software (implementacion, arquitectura) en un 36%. Luego disenio en un 10% y datos en un 9%. No hay una tendencia clara entre las especialidades menos frecuentes.  

summary(df$A.6)

#RTA: El promedio de edad registrado es de 31 anios 


################## Representacion grafica de tablas #############

mosaicplot(genero_espe, # Tabla de contingencias a graficar
           main= "Especialidad y Sexo",
           xlab="Sexo",
           ylab="Especialidad",
           col=c(2,3,4,5),
           cex.axis=0.3,
)




