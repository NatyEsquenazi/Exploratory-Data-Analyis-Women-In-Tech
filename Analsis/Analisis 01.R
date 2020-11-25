
########################### LIBRERIAS ######################## ----------------------
library(tidyverse)
library(dplyr)
library(readxl)
library(readr)
library(janitor)
library(tidyr)
library(summarytools)
library(MASS)

df<- read_csv("results-survey18-11-20.xlsx - ¿QUÉ TAN INCLUSIVO ES EL ECOS.csv")


########################### EXPLORACION ######################## ----------------------

# 1. Edad: 

#En la variable edad 6.04% de las observaciones son nulas.
df_status(df$A.6)

#Elimino nulos 
df<-subset(df, !is.na(A.6))

#Me quedo con mayores a 18 anios
df <- df %>% 
  filter(A.6 >= 18)

plot_1 <- ggplot(data = df, mapping = aes(x=A.6)) +
  geom_histogram(bins = 30) +
  labs(x = "Edad",
       title="Distribucion de edad",
       caption="Fuente: MET CBA")

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
                         A.8 == 'Femenino'~'2',
                         A.8 == 'femenino'~'2',
                         A.8 == 'MUJER'~'1',
                         A.8 == 'Mujer cis'~'3',
                         A.8 == 'Mujer Cis'~'3',
                         A.8 == 'Muje cis'~'3',
                         A.8 == 'mujer cis'~'3',
                         A.8 == 'MUJER CIS'~'3',
                         A.8 == 'Mujer CIS'~'3',
                         A.8 == 'Hombre'~'4',
                         A.8 == 'hombre'~'4',
                         A.8 == 'masculino'~'5',
                         A.8 == 'Masclino'~'5',
                         A.8 == 'varon'~'6',
                         A.8 == 'Lesbiana'~'7',
                         A.8 == 'Mujer cisgenero bisexual'~'8',
                         A.8 == 'no binarie'~'9',
                         A.8 == 'género no binario'~ '9',
                         A.8 == 'Mujer Trans'~'10',
                         A.8 == 'Varón trans'~'11',
                         A.8 == 'queer'~'12',
                         A.8 == 'siempre digo mujer, pero me considero nb también, así que no sé'~'13'))

# 3. Residencia 

unique(df$A.9)
df<-subset(df, !is.na(A.9))

# 4. Tipo de trabajo

unique(df$B.5)
df<-subset(df, !is.na(B.5))

# 5. Formacion

unique(df$B.7)
df<-subset(df, !is.na(B.7))










