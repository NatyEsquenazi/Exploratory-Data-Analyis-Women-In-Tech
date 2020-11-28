
##################### Analisis ####################### -------------------------

freq(df$A.8, cumul = FALSE)

df$edad4t <- cut(df$A.6,
                 breaks = c(min(df$A.6),
                            25, 35, max(df$A.6)))

freq(df$edad4t, report.nas = FALSE)

freq(df$A.9, cumul = FALSE)

## Genero: 78.41% de las encuestadas se considera mujer, un 5.36% se considera femenino y 5.55% se considera mujer cis. La cantidad de hombres encuestados es 4.98% 
## Edad: un 57.33% de los encuestados tiene entre 25-35 anios, un 14.84% tiene entre 19-25 y el resto (27.83%) pertenecen a 35 a 99 anios 
## Residencia: Un 40.3% reside en cordoba capital y un 26.06% en CABA. 

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

## Un 65.4 de las mujeres cuentan con un vinculo laboral en una empresa, mientras que un 10.4% es independiente y solo el 2.8% tienen su propia empresa. No se presenta informalidad laboral en el sector tecnologico por cuestiones de genero. 

## Un 4.8 de los que se consideraron femenino tienen un vinculo laboral con una empresa y tambien las mujeres cis. 

## 2. Acercamiento a la tecnologia y genero

tech_genero<-table(df$B.7, df$A.8)
round(prop.table(tech_genero), 3)* 100 
mosaicplot(tech_genero,
           main= "Genero y Tecnologia",
           xlab="Tecnologia",
           ylab="Genero",
           cex.axis=0.40,
           las=2,
           col=c(2,3,4,5)
)

## Un 19.6% de las mujeres encuestadas se acercaron a la tecnologia a traves de cursos, el porcentaje de autodidactas es de 12.9%. Un 17.7% de las mujeres encuestadas se acercaron a la tecnologia a traves de la carrera de grado ingenieria. 
## Un 1.5% de los hombres es autodidacta y 1.7% de las mujeres cis a traves de cursos 

## 2. Acercamiento a la tecnologia y edad

tech_edad<-table(df$edad4t, df$B.7)
round(prop.table(tech_edad), 3)* 100 
mosaicplot(tech_edad,
           main= "Edad y Formacion",
           xlab="Edad",
           ylab="Formacion",
           cex.axis=0.40,
           las=2,
           col=c(2,3,4,5)
)

## La mayor concentracion de registros se encuentra en el rango de 25-35 anios. Un 10.2% de los encuestados entre 25-35 anios se acerco a la tecnologia siendo autodidacta, un 14.7% a traves de la cursos y un 11.7% en ingenieria y 10.2% analista.

## 3. Apariencia fisica 

freq(df$B.10)
fisico_genero<-table(df$A.8, df$B.10)
round(prop.table(fisico_genero), 3)* 100 

## Un 33.64% de los encuestados se sintieron juzgados por su apariencia fisica. Las mujeres se sintieron juzgadas por su apariencia fisica en un 25.9%. 

## 4. Opiniones tecnicas 

freq(df$B.21)
opinion_genero<-table(df$A.8, df$B.21)
round(prop.table(opinion_genero), 3)* 100 

## Las opiniones tecnicas de mujeres no se tuvieron en cuenta en un 24.6%

## 5. Comentarios discriminatorios 

freq(df$B.25)
comentario_genero<-table(df$A.8, df$B.25)
round(prop.table(comentario_genero), 3)* 100 

## Los encuestados que recibieron comentarios discriminatorios representan un 22.55%. Las mujeres que recibieron comentarios discriminatorios representan un 16.6%

## 6. Comentarios sexistas 

freq(df$B.26)
sexo_genero<-table(df$A.8, df$B.26)
round(prop.table(sexo_genero), 3)* 100 

## Los encuestados que recibieron comentarios discriminatorios representan un 22.55%.Las mujeres que recibieron comentarios comentarios con contenido sexual explicito representan un 17.7% 

## 7. Analisis de texto: B.31

tokenizado <- b_31 %>%
  unnest_tokens(word, B.31) %>%
  anti_join(my_stopwords)%>%
  count(word, sort = TRUE)

mywords <- data %>%
  filter(artist=="nekfeu") %>%
  dplyr::select(word) %>%
  group_by(word) %>%
  summarize(freq=n()) %>%
  arrange(freq) %>%
  tail(30)
































