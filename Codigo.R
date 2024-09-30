library(readr)
library(tidyverse)
datos <- read_csv("mushroom_cleaned.csv")

colnames(datos) = c("cap_diameter","cap_shape", "gill_attachment", "gill_color", "stem_height", "stem_width", "stem_color", "season", "class")

datos %>% mutate(cap_shape = as.factor(cap_shape), 
                 gill_attachment = as.factor(gill_attachment),
                 gill_color = as.factor(gill_color),
                 stem_color = as.factor(stem_color),
                 season = as.factor(season))


unico1 = length(unique(datos$cap_shape))

unico2 = length(unique(datos$gill_attachment))

unico3 = length(unique(datos$gill_color))

unico4 = length(unique(datos$stem_color))

unico5 = length(unique(datos$season))


puntaje = as.data.frame(matrix(0, ncol = 4, nrow = 100))
colnames(puntaje) = c("Puntaje", "Semilla", "Variables", "Categorias_faltantes")
puntaje$Variables = ""

for (i in 1:100) {
  
set.seed(i)  
  
datos_sample = datos[sample(54035,100),]

puntaje[i,2] = i

modelo1 <- glm(class ~ cap_diameter, family=binomial(link="logit"), data = datos_sample) 

if(pchisq(modelo1$null.deviance-modelo1$deviance,1)>.95){
  puntaje[i,1] = puntaje[i,1] + 1
  puntaje[i,3] = paste(puntaje[i,3],"1")
}

modelo2 <- glm(class ~ cap_shape, family=binomial(link="logit"), data = datos_sample) 

if(pchisq(modelo2$null.deviance-modelo2$deviance,length(unique(datos_sample$cap_shape))-1)>.95){
  puntaje[i,1] = puntaje[i,1] + 1
  puntaje[i,3] = paste(puntaje[i,3],"2")
  puntaje[i,4] = paste(puntaje[i,4],as.character(unico1 - length(unique(datos_sample$cap_shape))))
}

modelo3 <- glm(class ~ gill_attachment, family=binomial(link="logit"), data = datos_sample) 

if(pchisq(modelo3$null.deviance-modelo3$deviance,length(unique(datos_sample$gill_attachment))-1)>.95){
  puntaje[i,1] = puntaje[i,1] + 1
  puntaje[i,3] = paste(puntaje[i,3],"3")
  puntaje[i,4] = paste(puntaje[i,4],as.character(unico2 - length(unique(datos_sample$gill_attachment))))
}



modelo4 <- glm(class ~ gill_color, family=binomial(link="logit"), data = datos_sample) 

if(pchisq(modelo4$null.deviance-modelo4$deviance,length(unique(datos_sample$gill_color))-1)>.95){
  puntaje[i,1] = puntaje[i,1] + 1
  puntaje[i,3] = paste(puntaje[i,3],"4")
  puntaje[i,4] = paste(puntaje[i,4],as.character(unico3 - length(unique(datos_sample$gill_color))))
}



modelo5 <- glm(class ~ stem_height, family=binomial(link="logit"), data = datos_sample) 

if(pchisq(modelo5$null.deviance-modelo5$deviance,1)>.95){
  puntaje[i,1] = puntaje[i,1] + 1
  puntaje[i,3] = paste(puntaje[i,3],"5")
}

modelo6 <- glm(class ~ stem_width, family=binomial(link="logit"), data = datos_sample) 

if(pchisq(modelo6$null.deviance-modelo6$deviance,1)>.95){
  puntaje[i,1] = puntaje[i,1] + 1
  puntaje[i,3] = paste(puntaje[i,3],"6")
}

modelo7 <- glm(class ~ stem_color, family=binomial(link="logit"), data = datos_sample) 

if(pchisq(modelo7$null.deviance-modelo7$deviance,length(unique(datos_sample$stem_color))-1)>.95){
  puntaje[i,1] = puntaje[i,1] + 1
  puntaje[i,3] = paste(puntaje[i,3],"7")
  puntaje[i,4] = paste(puntaje[i,4],as.character(unico4 - length(unique(datos_sample$stem_color))))
}


modelo8 <- glm(class ~ season, family=binomial(link="logit"), data = datos_sample) 

if(pchisq(modelo8$null.deviance-modelo8$deviance,length(unique(datos_sample$season))-1)>.95){
  puntaje[i,1] = puntaje[i,1] + 1
  puntaje[i,3] = paste(puntaje[i,3],"8")
  puntaje[i,4] = paste(puntaje[i,4],as.character(unico5 - length(unique(datos_sample$season))))
}

semilla = i



}


puntaje






