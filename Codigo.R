library(readr)
library(tidyverse)
datos <- read_csv("mushroom_cleaned.csv")

colnames(datos) = c("cap_diameter","cap_shape", "gill_attachment", "gill_color", "stem_height", "stem_width", "stem_color", "season", "class")

datos %>% mutate(cap_shape = as.factor(cap_shape), 
                 gill_attachment = as.factor(gill_attachment),
                 gill_color = as.factor(gill_color),
                 stem_color = as.factor(stem_color))


str(datos)
unique(datos$cap_shape)
unique(datos$cap_shape)
unique(datos$gill_attachment)
unique(datos$gill_color)
unique(datos$stem_color)
summary(datos)

puntaje = matrix(0, ncol = 2, nrow = 100)

for (i in 1:100) {
  
set.seed(i)  
  
datos_sample = datos[sample(54035,100),]

puntaje[i,2] = i

modelo1 <- glm(class ~ cap_diameter, family=binomial(link="logit"), data = datos_sample) 

if(pchisq(modelo1$null.deviance-modelo1$deviance,1)>.95){
  puntaje[i,1] = puntaje[i,1] + 1
}

modelo2 <- glm(class ~ cap_shape, family=binomial(link="logit"), data = datos_sample) 

if(pchisq(modelo2$null.deviance-modelo2$deviance,length(unique(datos_sample$cap_shape))-1)>.95){
  puntaje[i,1] = puntaje[i,1] + 1
}

modelo3 <- glm(class ~ gill_attachment, family=binomial(link="logit"), data = datos_sample) 

if(pchisq(modelo3$null.deviance-modelo3$deviance,length(unique(datos_sample$gill_attachment))-1)>.95){
  puntaje[i,1] = puntaje[i,1] + 1
}



modelo4 <- glm(class ~ gill_color, family=binomial(link="logit"), data = datos_sample) 

if(pchisq(modelo4$null.deviance-modelo4$deviance,length(unique(datos_sample$gill_color))-1)>.95){
  puntaje[i,1] = puntaje[i,1] + 1
}



modelo5 <- glm(class ~ stem_height, family=binomial(link="logit"), data = datos_sample) 

if(pchisq(modelo5$null.deviance-modelo5$deviance,1)>.95){
  puntaje[i,1] = puntaje[i,1] + 1
}

modelo6 <- glm(class ~ stem_width, family=binomial(link="logit"), data = datos_sample) 

if(pchisq(modelo6$null.deviance-modelo6$deviance,1)>.95){
  puntaje[i,1] = puntaje[i,1] + 1
}

modelo7 <- glm(class ~ stem_color, family=binomial(link="logit"), data = datos_sample) 

if(pchisq(modelo7$null.deviance-modelo7$deviance,length(unique(datos_sample$stem_color))-1)>.95){
  puntaje[i,1] = puntaje[i,1] + 1
}


modelo8 <- glm(class ~ season, family=binomial(link="logit"), data = datos_sample) 

if(pchisq(modelo8$null.deviance-modelo8$deviance,1)>.95){
  puntaje[i,1] = puntaje[i,1] + 1
}

semilla = i



}


puntaje





