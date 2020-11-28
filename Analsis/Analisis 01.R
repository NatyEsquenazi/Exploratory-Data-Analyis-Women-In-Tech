
########################### LIBRERIAS ######################## ----------------------
library(tidyverse)
library(dplyr)
library(readxl)
library(readr)
library(janitor)
library(tidyr)
library(summarytools)
library(MASS)
library(ggplot2)
library(tidytext)
library(quanteda)
library(readr)
library(ggwordcloud)
library(hrbrthemes)
library(tm)
library(proustr)
library(wordcloud2)

df <- read_csv("Bases/df.csv")
my_stopwords <- read_csv("Bases/my_stopwords.csv")

########################### EXPLORACION ######################## ----------------------

# 1. Edad: 

#En la variable edad 6.04% de las observaciones son nulas.
df_status(df$A.6)

#Elimino nulos 
df<-subset(df, !is.na(A.6))

#Me quedo con mayores a 18 anios
df <- df %>% 
  filter(A.6 >= 18 & A.6 < 99)

plot_1 <- ggplot(data = df, mapping = aes(x=A.6)) +
  geom_histogram(bins = 30) +
  labs(x = "Edad",
       title="Distribucion de edad",
       caption="Fuente: MET CBA")


# 2. Genero: 

unique(df$A.7)

#Elimino nulos 
df<-subset(df, !is.na(A.7))
df<-subset(df, !is.na(A.8))

#df <- df %>% 
#  mutate(A.7=as.character(A.7),
#         A.7 = case_when(A.7 == 'Other' ~ '1',
#                         A.7 == 'Estoy en proceso de definir mi identidad' ~ '2',
#                         A.7 == 'No tengo género' ~ '3'))

df <- df %>% 
  mutate(A.8=as.character(A.8),
         A.8 = case_when(A.8 == "mujer"~"1",
                         A.8 == "Mujer"~"1",
                         A.8 == "MUJER"~"1",
                         A.8 == "Femenino"~"2",
                         A.8 == "femenino"~"2",
                         A.8 == "femenina"~"2",
                         A.8 == "Género femenino"~"2",
                         A.8 == "Lesbiana"~"3",
                         A.8 == "mujer lesbiana"~"3",
                         A.8 == "Mujer lesbiana"~"3",
                         A.8 == "Mujer y me identifico como mujer y me gustan las mujeres"~"3",
                         A.8 == "Mujer cis"~"4",
                         A.8 == "Mujer Cis"~"4",
                         A.8 == "Muje cis"~"4",
                         A.8 == "mujer cis"~"4",
                         A.8 == "MUJER CIS"~"4",
                         A.8 == "Mujer CIS"~"4",
                         A.8 == "mujer cisgenero"~"4",
                         A.8 == "Mujer cisgenero bisexual"~"5",
                         A.8 == "Hombre"~"6",
                         A.8 == "hombre"~"6",
                         A.8 == "hombre cis"~"17",
                         A.8 == "Hombre cis"~"17",
                         A.8 == "masculino"~"11",
                         A.8 == "Masculino"~"11",
                         A.8 == "Masclino"~"11",
                         A.8 == "varon"~"12",
                         A.8 == "varón"~"12",
                         A.8 == "no binarie"~"7",
                         A.8 == "no binario"~"7",
                         A.8 == "género no binario"~"7",
                         A.8 == "Mujer Trans"~"8",
                         A.8 == "mujer (trans)"~"8",
                         A.8 == "Mujer trans"~"8",
                         A.8 == "queer"~"9",
                         A.8 == "siempre digo mujer, pero me considero nb también, así que no sé."~"10",
                         A.8 == "Varón trans"~"13",
                         A.8 == "gender-fluid"~"14",
                         A.8 == "gay"~"15",
                         A.8 == "Heterosexual cis"~"16",
                         A.8 == "heterosexual"~"16",
                         A.8 == "plurigeneralistic"~"17",
                         A.8 == "hombre, hetero, cis"~"18",
                         A.8 == "hombre gay cis"~"19",
                         A.8 == "Masculino, heterosexual"~"20",
                         A.8 == "Mujer Hetero"~"21"))

# 3. Residencia 

unique(df$A.9)
df<-subset(df, !is.na(A.9))

# 4. Tipo de trabajo

unique(df$B.5)
df<-subset(df, !is.na(B.5))

# 5. Formacion

unique(df$B.7)
df<-subset(df, !is.na(B.7))

# 6. Discriminacion 

unique(df$B.25)
freq(df$B.25)

# 7. Analisis de texto: B.31

palabras<-subset(df, !is.na(B.31))

b_31 <- dplyr::select(palabras, B.31)



























