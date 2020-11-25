
##################### Analisis ####################### -------------------------

freq(df$A.8, cumul = FALSE)

df$edad4t <- cut(df$A.6,
                 breaks = c(min(df$A.6),
                            25, 35, max(df$A.6)))

freq(df$edad4t, report.nas = FALSE)

freq(df$A.9, cumul = FALSE)

## Genero: 81% de las encuestadas se considera mujer, un 5.66% se considera mujer cis y 4.75% se considera femenino. La cantidad de hombres encuestados. . 
## Edad: un 58.6% de los encuestados tiene entre 25-35 anios, un 15% tiene entre 18-25 y el resto (26%) pertenecen a 35 a 56 anios 
## Residencia: Un 40% reside en cordoba capital y un 6.33% en el interior 

##################### Tablas de Contingencia ####################### -----------

## 1. Informalidad laboral y genero

empleo_genero<-table(df$B.5, df$A.8)
round(prop.table(empleo_genero), 3)* 100 
mosaicplot(empleo_genero,
           main= "Genero y Empleo",
           xlab="Empleo",
           ylab="Genero",
           cex.axis=0.30,
           las=2,
           col=c(2,3,4,5)
)

## Un 69.5% de las mujeres cuentan con un vinculo laboral en una empresa, mientras que un 11% es independiente. No se presenta informalidad laboral en el sector tecnologico por cuestiones de genero. 

## 2. Acercamiento a la tecnologia y genero, edad 

empleo_genero<-table(df$B.5, df$A.8)
round(prop.table(empleo_genero), 3)* 100 
mosaicplot(empleo_genero,
           main= "Genero y Empleo",
           xlab="Empleo",
           ylab="Genero",
           cex.axis=0.30,
           las=2,
           col=c(2,3,4,5)
)










