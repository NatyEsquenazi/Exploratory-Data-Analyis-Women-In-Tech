
##################### Analisis #######################

freq(df$A.8, cumul = FALSE)

freq(df$B.1, cumul = FALSE)

df$edad4t <- cut(df$A.6,
                 breaks = c(min(df$A.6),
                   25, 35, max(df$A.6)))

freq(df$edad4t, report.nas = FALSE)

freq(df$A.9, cumul = FALSE)

freq(df$B.5, cumul = FALSE)

freq(df$B.7, cumul = FALSE)

# Genero: 93% de las encuestadas son mujeres cis, mientras que solo el 3.57% son hombres 
# Especialidad: 39% de las encuestadas se dedican a la industria de software (implementacion, desarrollo, arquitectura), un 10.57% al disenio. 
# Edad: un 75% de los encuestados tiene entre 25-35 anios o menos, un 16% tiene entre 18-25 y el resto (24%) pertenecen a 35 a 56 anios 
# Residencia: Un 40% reside en cordoba capital y un 8% en el interior 
# Laboral: 81% tiene vinculo laboral con una empresa
# Formacion: 25% tiene formacion en tecnologia a traves de cursos, el porcentaje de autodidactas tambien es alto en un 17%


####  Existe una relacion entre el genero y la especialidad elegida? -----

genero_espe <- table(df$A.8, df$B.1)
round(prop.table(genero_espe), 3)* 100

mosaicplot(genero_espe, 
           main= "Especialidad y Genero",
           xlab="Genero",
           ylab="Especialidad",
           col=c(2,3,4,5),
           cex.axis=0.3,
)

# RTA: existe una mayoria de mujeres con especialidad en software (implementacion, arquitectura) en un 36%. Luego disenio en un 10% y datos en un 9%. No hay una tendencia clara entre las especialidades menos frecuentes. 

#### Existe informalidad laboral entre los encuestados? ------

genero_lab <- table(df$B.5, df$A.8)
round(prop.table(genero_lab), 3)* 100

mosaicplot(genero_lab, 
           main= "Trabajo y Genero",
           xlab="Trabajo",
           ylab="Genero",
           col=c(2,3,4,5),
           cex.axis=1,
)

# RTA: 77% de las mujeres encuestadas tienen un vinculo laboral estable, demostrando bajo nivel de informalidad. 

#### Nivel educativo en tecnologia alcanzado por genero? ------

genero_edu <- table(df$B.7, df$A.8)
round(prop.table(genero_edu), 3)* 100

mosaicplot(genero_edu, 
           main= "Educacion y Genero",
           xlab="Educacion",
           ylab="Genero",
           col=c(2,3,4,5),
           cex.axis=0.50,
)

#24% de las mujeres encuestadas ha realizado cursos vinculados con tecnologia, el pocentaje de autodidactas es alto y el de ingenieras tambien. El resto de los generos se distribuye en diferentes porcentajes sobre la categoria de autodidacta. 














