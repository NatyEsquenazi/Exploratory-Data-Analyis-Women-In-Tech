
library(tidyverse)
library(dplyr)
library(readxl)
library(readr)
library(janitor)
library(funModeling)
library(tidyr)
library(summarytools)
library(MASS)

df <- read_excel("results-survey18-11-20.xlsx")
espe <- read_excel("espe.xlsx")

df <- survey %>% 
  select(-c(B.1, B.2))

df <- left_join(df, espe, by='A.1')

############### Exploracion ############### ----------------------------

# 1. Edad: 

#En la variable edad 6.04% de las observaciones son nulas.
df_status(df$A.6)

plot_1 <- ggplot(data = df, mapping = aes(x=A.6)) +
  geom_histogram(bins = 30) +
  labs(x = "Edad",
       title="Distribucion de edad",
       caption="Fuente: MET CBA")

#Elimino nulos 
df<-subset(df, !is.na(A.6))

#Me quedo con mayores a 18 anios
df <- df %>% 
  filter(A.6 >= 18)

# 2. Genero: 

df_status(df$A.8)

#Elimino nulos 
df<-subset(df, !is.na(A.7))
df<-subset(df, !is.na(A.8))

df <- df %>% 
  mutate(A.7=as.character(A.7),
         A.7 = case_when(A.7 == 'Other' ~ '1',
                   A.7 == 'Estoy en proceso de definir mi identidad' ~ '2',
                   A.7 == 'No tengo género' ~ '3'))

df <- df %>% 
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

unique(df$A.9)
df<-subset(df, !is.na(A.9))

# 4. Especialidad: dejo los nulos xq pueden indicar no especialidad 

#df <- df %>% 
 # mutate(B.1=as.character(B.1),
 #       B.1 = case_when(B.1 == 'Administración de sistemas'~'Sistemas',
#                         B.1 == 'Análisis de datos'~'Datos',
 #                        B.1 == 'Desarrollo de software'~'Software',
  #                       B.1 == 'Implementación de software'~'Software',
  #                       B.1 == 'Diseño UX/UI'~'Disenio',
  #                       B.1 == 'Testing'~'Testing',
  #                       B.1 == 'Seguridad informática'~'Seguridad',
  #                       B.1 == 'UX Research'~'Investigacion',
  #                       B.1 == 'Metodologías ágiles'~'Metodologias',
  #                       B.1 == 'Análisis funcional'~'Funcional',
  #                       B.1 == 'Soporte técnico/al usuario'~'Tecnico',
  #                       B.1 == 'Calidad de software'~'Software',
  #                       B.1 == 'Marketing Digital'~'Marketing',
  #                       B.1 == 'Accesibilidad'~'Accesibilidad',
  #                       B.1 == 'Administración de bases de datos'~'Datos',
  #                       B.1 == 'Desarrollo/Estrategia de producto digital'~'Producto',
  #                       B.1 == 'Administración de redes y comunicaciones'~'Comunicacion',
  #                       B.1 == 'Other'~ 'Otro'))

#survey <- survey %>% 
#  mutate(B.2=as.character(B.2),
#         B.2 = case_when(B.2 == 'Scrum master'~'Projecto',
#                         B.2 == 'ACCESIBILIDAD'~'Accesibilidad',
#                         B.2 == 'administracion'~'Adm',
#                         B.2 == 'administrativo'~'Adm',
#                         B.2 == 'ARQUITECTURA DE SOFTWARE'~'Software',
#                         B.2 == 'Auditoría de Sistemas'~'Sistemas',
#                         B.2 == 'Ciencia de datos'~'Datos',
#                         B.2 == 'Comunicación'~'Comunicacion',
#                         B.2 == 'Consultor BI/Soporte BI'~'Datos',
#                         B.2 == 'contadora'~'Profesional',
#                         B.2 == 'data entry'~'Datos',
#                         B.2 == 'Data science'~'Datos',
#                         B.2 == 'Data scientist'~'Datos',
#                         B.2 == 'Derecho informático'~'Datos',
#                         B.2 == 'Desarollador ETL'~'Datos',
#                         B.2 == 'Desarrollo de base de datos'~'Datos',
#                         B.2 == 'Desarrollo web'~'Web',
#                         B.2 == 'Desarollador ETL'~'Datos',
#                         B.2 == 'Devops, pero estudio programación y estoy estudiando front ahora'~'Devops',
#                         B.2 == 'DevOps/SRE'~'Devops',
#                         B.2 == 'Diseño de HW - Comunicaciones Digitales'~'Comunicacion',
#                         B.2 == 'Diseño gráfico'~'Disenio',
#                         B.2 == 'Educación + Tecnología'~'Projecto',
#                         B.2 == 'Gestion de Capital Humano' ~'rrhh',
#                         B.2 == 'it recruiter' ~ 'rrhh',
#                         B.2 == 'lider de proyecto'~'Projecto',
#                         B.2 == 'Líder de proyectos de software'~'Software',
#                         B.2 == 'Manager'~'Projecto',
#                         B.2 == 'Problem Solver'~'Projecto',
#                         B.2 == 'Product Management'~'Projecto',
#                         B.2 == 'Product Manager'~'Projecto',
#                         B.2 == 'Profesional'~'Profesional',
#                         B.2 == 'Programa manager'~'Projecto',
#                         B.2 == 'project management'~'Projecto',
#                         B.2 == 'Project Management'~'Projecto',
#                         B.2 == 'Project Manager '~'Projecto',
#                         B.2 == 'Redacción de contenidos digitales'~'Comunicacion',
#                         B.2 == 'rr '~'rrhh',
#                         B.2 == 'Seleccion'~'rrhh',
#                         B.2 == 'Soporte al negocio'~'Projecto',
#                         B.2 == 'Soporte Funcional de software'~'Software',
#                         B.2 == 'ceo'~'Projecto',
#                         B.2 == 'Testing, análisis funcional, soporte técnico/al usuario, Project  manager'~'Testing', 
#                         B.2 == 'Visual Design & Dirección de Arte '~'Disenio',
#                         B.2 == 'Análisis y desarrollo de sw'~ 'Software'))



# 5. Tipo de trabajo

unique(df$B.5)
df<-subset(df, !is.na(B.5))


# 5. Formacion

unique(df$B.7)
df<-subset(df, !is.na(B.7))
filter <- df %>% 
  select(B.7,B.8,B.9)


# 5. Apariencia, mas carga de trabajo, me quitaron tareas 

df_status(df$B.10)
df_status(df$B.11)
df_status(df$B.12)
df_status(df$B.13)
df_status(df$B.14)
df_status(df$B.15)
df_status(df$B.16)
df_status(df$B.17)
df_status(df$B.18)
df_status(df$B.19)
df_status(df$B.20)
df_status(df$B.21)
df_status(df$B.22)
df_status(df$B.23)
df_status(df$B.24)
df_status(df$B.25)
df_status(df$B.26)
df_status(df$B.27)
df_status(df$B.28)
df_status(df$B.29)
df_status(df$B.30)

df<-subset(df, !is.na(c(B.10, B.11,B.12,B.13, B.14, B.15, B.16, B.17,
                        B.18, B.19, B.20, B.21, B.22, B.23, B.24,
                        B.25, B.26, B.27, B.28, B.29, B.30)))

           
           
           
           











