
library(tidyverse)
library(dplyr)
library(readxl)
library(janitor)
library(funModeling)

survey<- read_excel("results-survey724261 (1).xlsx")

############### Exploracion ############### ----------------------------

# 1. Edad: 

#En la variable edad 6.04% de las observaciones son nulas.
df_status(survey$A.6)

plot_1 <- ggplot(data = survey, mapping = aes(x=A.6)) +
  geom_histogram(bins = 30) +
  labs(x = "Edad",
       title="Distribucion de edad",
       caption="Fuente: MET CBA")

#Elimino nulos 
survey<-subset(survey, !is.na(A.6))

#Me quedo con mayores a 18 anios
survey <- survey %>% 
  filter(A.6 >= 18)

# 2. Genero: 

df_status(survey$A.8)

#Elimino nulos 
survey<-subset(survey, !is.na(A.7))
survey<-subset(survey, !is.na(A.8))

survey <- survey %>% 
  mutate(A.7=as.character(A.7),
         A.7 = case_when(A.7 == 'Other' ~ '1',
                   A.7 == 'Estoy en proceso de definir mi identidad' ~ '2',
                   A.7 == 'No tengo género' ~ '3'))

survey <- survey %>% 
  mutate(A.8=as.character(A.8),
         A.8 = case_when(A.8 == 'mujer'~'1',
                   A.8 == 'Mujer'~'1',
                   A.8 == 'Femenino'~'1',
                   A.8 == 'femenino'~'1',
                   A.8 == 'MUJER'~'1',
                   A.8 == 'Mujer cis'~'1',
                   A.8 == 'Mujer Cis'~'1',
                   A.8 == 'Muje cis'~'1',
                   A.8 == 'mujer cis'~'1',
                   A.8 == 'MUJER CIS'~'1',
                   A.8 == 'Mujer CIS'~'1',
                   A.8 == 'Hombre'~'2',
                   A.8 == 'hombre'~'2',
                   A.8 == 'masculino'~'2',
                   A.8 == 'Masclino'~'2',
                   A.8 == 'varon'~'2',
                   A.8 == 'Lesbiana'~'3',
                   A.8 == 'Mujer cisgenero bisexual'~'4',
                   A.8 == 'no binarie'~'5',
                   A.8 == 'género no binario'~ '5',
                   A.8 == 'Mujer Trans'~'6',
                   A.8 == 'Varón trans'~'7',
                   A.8 == 'queer'~'8',
                   A.8 == 'siempre digo mujer, pero me considero nb también, así que no sé'~'9'))

# 3. Residencia 

unique(survey$A.9)
survey<-subset(survey, !is.na(A.9))

# 4. Especialidad: dejo los nulos xq pueden indicar no especialidad 

unique(survey$B.1)
survey <- survey %>% 
  mutate(B.1=as.character(B.1),
         B.1 = case_when(B.1 == 'Administración de sistemas'~'1',
                         B.1 == 'Análisis de datos'~'2',
                         B.1 == 'Desarrollo de software'~'3',
                         B.1 == 'Implementación de software'~'3',
                         B.1 == 'Diseño UX/UI'~'4',
                         B.1 == 'Testing'~'5',
                         B.1 == 'Seguridad informática'~'6',
                         B.1 == 'UX Research'~'7',
                         B.1 == 'Metodologías ágiles'~'8',
                         B.1 == 'Análisis funcional'~'9',
                         B.1 == 'Soporte técnico/al usuario'~'10',
                         B.1 == 'Calidad de software'~'11',
                         B.1 == 'Marketing Digital'~'12',
                         B.1 == 'Accesibilidad'~'13',
                         B.1 == 'Administración de bases de datos'~'14',
                         B.1 == 'Desarrollo/Estrategia de producto digital'~'15',
                         B.1 == 'Administración de redes y comunicaciones'~'16',
                         B.1 == 'Other'~ '0'))
unique(survey$B.2)


                         







































