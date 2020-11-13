
##################### Analisis #######################

freq(survey$A.8)

# RTA: 93% de las encuestadas son mujeres cis, mientras que solo el 0.36% pertecen a categorias mujer/varon trans, bisexuales y queer. 

genero_espec <- table(survey$A.8, survey$B.1)

#RTA: el genero con la especialidad. De las mujeres encuestadas la mayoria tiene como especialidad desarrollo/implementacion de software. No hay una tendencia clara entre las especialidades menos frecuentes. 