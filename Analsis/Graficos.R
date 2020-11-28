
############################ Visualizaciones ######################### ------------------

## Tema predefinido 

mir_theme <- theme(plot.title = element_text(size = 18,family = "Source Sans Pro Semibold", 
                                             face = "italic", hjust = 0.5),
                   axis.line = element_line(color = "black", size = 1),
                   axis.title = element_text(size = 16),
                   axis.text = element_text(size = 14),
                   panel.background = element_rect(fill = NA),
                   plot.caption = element_text(size = 14),
                   legend.title = element_blank(),
                   legend.position = "bottom")

## 1. Genero

tab1 <- df %>% 
  group_by(A.8) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = round(n/sum(n) *100, 0))

plot1 <- ggplot(data = tab1, aes(x = A.8, y = perc))+
  geom_bar(stat = "identity", fill = "maroon", width = 0.7)+
  geom_text(aes(label = perc), size = 5, hjust = 0.5, vjust = -0.25)+
  mir_theme +
  labs(title = "Distribucion de Genero",
       x = "",y = "Porcentaje",
       caption = "MET: Mujeres en Tecnologia")+
  ylim(c(0,100))

## 2. Especialidad

tab2 <- df %>% 
  group_by(B.7) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = round(n / sum(n) *100,1))

plot2 <- ggplot(data = tab2, aes(x = B.7, y=perc)) +
  geom_bar(stat = "identity", fill = "maroon", width = 0.7) +
  geom_text(aes(label = perc), size = 4 , hjust = 0.5 , vjust = -0.25)+
  mir_theme +
  labs(title = "Distribucion de Especialidad",
       x = "", y = "Porcentaje",
       caption = "MET: Mujeres en Tecnologia")

## 3. Promedio de edad por Especialidad

tab5 <- df %>% 
  group_by(B.7) %>% 
  summarise(avg_age = round(mean(A.6, na.rm = T),1))

plot5 <- ggplot(data = tab5, aes(x = B.7, y=avg_age)) +
  geom_bar(stat = "identity", fill = "maroon", width = 0.7) +
  geom_text(aes(label = avg_age), size = 4 , hjust = 0.5 , vjust = -0.25)+
  mir_theme +
  labs(title = "Promedio de Especialidad por Edad",
       x = "", y = "Porcentaje",
       caption = "MET: Mujeres en Tecnologia")

## 4. Apariencia Fisica y Genero

tab3 <- df %>% 
  filter(B.10 != 'N/A') %>% 
  group_by(A.8, B.10) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(B.10) %>% 
  mutate(perc = round(n / sum(n) *100,1))

plot3 <- ggplot(data = tab3, aes(x = A.8, y=perc, fill = B.10)) +
  geom_bar(stat = "identity",  width = 0.7, position = "dodge") +
  geom_text(aes(label = perc), size = 4 , hjust = 0.5 , vjust = -0.25, 
            position = position_dodge(width = 0.7))+
  mir_theme +
  scale_fill_manual(values = c("grey", "maroon"))+
  labs(title = "Distribucion de Apariencia fisica por genero",
       x = "", y = "Porcentaje",
       caption = "")+
  ylim(c(0, 100))

## 5. Interrupcion y Genero

tab6 <- df %>% 
  filter(B.17 != 'N/A') %>% 
  group_by(A.8, B.17) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(B.17) %>% 
  mutate(perc = round(n / sum(n) *100,1))

plot6 <- ggplot(data = tab6, aes(x = A.8, y=perc, fill = B.17)) +
  geom_bar(stat = "identity",  width = 0.7, position = "dodge") +
  geom_text(aes(label = perc), size = 4 , hjust = 0.5 , vjust = -0.25, 
            position = position_dodge(width = 0.7))+
  mir_theme +
  scale_fill_manual(values = c("grey", "maroon"))+
  labs(title = "Distribucion de Interrupciones por genero",
       x = "", y = "Porcentaje",
       caption = "")+
  ylim(c(0, 100))

## 6. Acoso sexual y Genero

tab7 <- df %>% 
  filter(B.27 != 'N/A') %>% 
  group_by(A.8, B.27) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(B.27) %>% 
  mutate(perc = round(n / sum(n) *100,1))

plot7 <- ggplot(data = tab7, aes(x = A.8, y=perc, fill = B.27)) +
  geom_bar(stat = "identity",  width = 0.7, position = "dodge") +
  geom_text(aes(label = perc), size = 4 , hjust = 0.5 , vjust = -0.25, 
            position = position_dodge(width = 0.7))+
  mir_theme +
  scale_fill_manual(values = c("grey", "maroon"))+
  labs(title = "Acoso Sexual y genero",
       x = "", y = "Porcentaje",
       caption = "")+
  ylim(c(0, 85))

## 7. Analisis de texto: B.31

grafico_tokenizado <- tokenizado %>% 
  top_n(15) %>% 
  ggplot(aes(y = reorder(word, n), x = n)) + 
  geom_col(fill = "#2A8EBA") +
  labs(y = NULL, 
       x = "frecuencia",
       title = "Palabras más frecuentes discurso 2019") +
  theme_minimal()

textplot_wordcloud(tokenizado, rotation = 0.25, #rotacion de las palabras
                   color = rev(RColorBrewer::brewer.pal(10, "RdBu"))) #paleta de colores













