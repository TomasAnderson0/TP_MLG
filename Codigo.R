# ---Librerias---
library(readr)
library(tidyverse)


# ---Datos---
data_original <- read_delim("secondary_data.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

hongos <- data_original %>% 
  select(class, `cap-diameter`, `cap-shape`, `gill-attachment`, `gill-color`, 
         `stem-height`, `stem-width`, `stem-color`, season) %>% 
  mutate(class = ifelse(class == "e", 1, 0)) %>% 
  replace_na(list(`gill-attachment` = "u"))

colnames(hongos) = c("class", "cap_diameter","cap_shape", "gill_attachment", 
                     "gill_color", "stem_height", "stem_width", "stem_color", 
                     "season")

hongos %>% mutate(cap_shape = as.factor(cap_shape), 
                 gill_attachment = as.factor(gill_attachment),
                 gill_color = as.factor(gill_color),
                 stem_color = as.factor(stem_color),
                 season = as.factor(season))


unico1 = length(unique(hongos$cap_shape))

unico2 = length(unique(hongos$gill_attachment))

unico3 = length(unique(hongos$gill_color))

unico4 = length(unique(hongos$stem_color))

unico5 = length(unique(hongos$season))



# ---Selección de variables---
# Vemos cuántas y cuáles variables son significativas a modo de inicio; nos 
# quedamos con aquellos modelos que tuvieron 3 variables significativas y los 
# comparamos para ver cuál ajusta mejor.

puntaje = as.data.frame(matrix(0, ncol = 4, nrow = 250))
colnames(puntaje) = c("Puntaje", "Semilla", "Variables", "Categorias_faltantes")
puntaje$Variables = ""

for (i in 1:250) {
  
set.seed(i)  
  
datos_sample = hongos[sample(61069,250),]

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
table(datos_sample$class)

# ---Comparación de los modelos---

# Aparto los modelos de 3 variables:
modelos <- puntaje[str_count(puntaje$Variables, "\\d+") == 3, ]

#Caso para semilla 4

set.seed(4)  

datos_sample = hongos[sample(61069,250),]

#Modelo inical

modelo1 <- glm(class ~ stem_width + stem_color + season, family = binomial(link="logit"),
               data = datos_sample)

summary(modelo1)

modelo2 <- glm(class ~ stem_color + season, family = binomial(link="logit"),
               data = datos_sample)
modelo3 <- glm(class ~ stem_width + season, family = binomial(link="logit"),
               data = datos_sample)
modelo4 <- glm(class ~ stem_width + stem_color, family = binomial(link="logit"),
               data = datos_sample)


comp1 = anova(modelo2, modelo1)
comp2 = anova(modelo3, modelo1)
comp3 = anova(modelo4, modelo1)
pchisq(comp1$Deviance[2],comp1$Df[2],lower.tail = F)
pchisq(comp2$Deviance[2],comp2$Df[2],lower.tail = F)
pchisq(comp3$Deviance[2],comp3$Df[2],lower.tail = F)


#Sacamos stem_width


modelo5 = glm(class ~ season + stem_color, family = binomial(link="logit"),
              data = datos_sample)

summary(modelo5)

modelo6 = glm(class ~ season, family = binomial(link="logit"),
             data = datos_sample)
modelo7 = glm(class ~  stem_color, family = binomial(link="logit"),
              data = datos_sample)

comp4 = anova(modelo6, modelo5)
comp5 = anova(modelo7, modelo5)
pchisq(comp4$Deviance[2],comp4$Df[2],lower.tail = F)
pchisq(comp5$Deviance[2],comp5$Df[2],lower.tail = F)

#Son significativas, añadir de a una las demas

modelo8 = glm(class ~  stem_color + season + cap_diameter, family = binomial(link="logit"),
              data = datos_sample)

modelo9 = glm(class ~  stem_color + season + cap_shape, family = binomial(link="logit"),
              data = datos_sample)

modelo10 = glm(class ~  stem_color + season + gill_attachment, family = binomial(link="logit"),
              data = datos_sample)

modelo11 = glm(class ~  stem_color + season + gill_color, family = binomial(link="logit"),
              data = datos_sample)

modelo12 = glm(class ~  stem_color + season + stem_height, family = binomial(link="logit"),
              data = datos_sample)

comp6 = anova(modelo5,modelo8)
comp7 = anova(modelo5,modelo9)
comp8 = anova(modelo5,modelo10)
comp9 = anova(modelo5,modelo11)
comp10 = anova(modelo5,modelo12)
pchisq(comp6$Deviance[2],comp6$Df[2],lower.tail = F)
pchisq(comp7$Deviance[2],comp7$Df[2],lower.tail = F)
pchisq(comp8$Deviance[2],comp8$Df[2],lower.tail = F)
pchisq(comp9$Deviance[2],comp9$Df[2],lower.tail = F)
pchisq(comp10$Deviance[2],comp10$Df[2],lower.tail = F)

#Añadimos cap_shape



modelo13 = glm(class ~  stem_color + season + cap_shape + cap_diameter, family = binomial(link="logit"),
              data = datos_sample)

modelo14 = glm(class ~  stem_color + season + cap_shape + gill_attachment, family = binomial(link="logit"),
               data = datos_sample)

modelo15 = glm(class ~  stem_color + season + cap_shape + gill_color, family = binomial(link="logit"),
               data = datos_sample)

modelo16 = glm(class ~  stem_color + season + cap_shape + stem_height, family = binomial(link="logit"),
               data = datos_sample)

modelo17 = glm(class ~  stem_color + season + cap_shape + stem_width, family = binomial(link="logit"),
               data = datos_sample)

comp11 = anova(modelo9,modelo13)
comp12 = anova(modelo9,modelo14)
comp13 = anova(modelo9,modelo15)
comp14 = anova(modelo9,modelo16)
comp15 = anova(modelo9,modelo17)
pchisq(comp11$Deviance[2],comp11$Df[2],lower.tail = F)
pchisq(comp12$Deviance[2],comp12$Df[2],lower.tail = F)
pchisq(comp13$Deviance[2],comp13$Df[2],lower.tail = F)
pchisq(comp14$Deviance[2],comp14$Df[2],lower.tail = F)
pchisq(comp15$Deviance[2],comp15$Df[2],lower.tail = F)


#Añadimos gill_attachment

modelo18 = glm(class ~  stem_color + season + cap_shape + gill_attachment + cap_diameter, family = binomial(link="logit"),
               data = datos_sample)

modelo19 = glm(class ~  stem_color + season + cap_shape + gill_attachment + gill_color, family = binomial(link="logit"),
               data = datos_sample)

modelo20 = glm(class ~  stem_color + season + cap_shape + gill_attachment + stem_height, family = binomial(link="logit"),
               data = datos_sample)

modelo21 = glm(class ~  stem_color + season + cap_shape + gill_attachment + stem_width, family = binomial(link="logit"),
               data = datos_sample)

comp16 = anova(modelo14,modelo18)
comp17 = anova(modelo14,modelo19)
comp18 = anova(modelo14,modelo20)
comp19 = anova(modelo14,modelo21)
pchisq(comp16$Deviance[2],comp16$Df[2],lower.tail = F)
pchisq(comp17$Deviance[2],comp17$Df[2],lower.tail = F)
pchisq(comp18$Deviance[2],comp18$Df[2],lower.tail = F)
pchisq(comp19$Deviance[2],comp19$Df[2],lower.tail = F)


#Nos quedamos con el modelo14

#TO DO interacciones





# modelo2 <- glm(class ~ cap_shape + stem_color + season, family = binomial,
#                data = datos_sample)
# 
# modelo3 <- glm(class ~ cap_diameter + stem_width + stem_color, family = binomial,
#                data = datos_sample)
# 
# modelo4 <- glm(class ~ gill_attachment + stem_width + stem_color, family = binomial,
#                data = datos_sample)
# 
# modelo5 <- glm(class ~ gill_color + stem_color + season, family = binomial,
#                data = datos_sample)
# 
# modelo6 <- glm(class ~ gill_attachment + gill_color + stem_color, family = binomial,
#                data = datos_sample)
# 
# modelo7 <- glm(class ~ cap_diameter + cap_shape + stem_color, family = binomial,
#                data = datos_sample)
# 
# modelo8 <- glm(class ~ gill_color + stem_width + season, family = binomial,
#                data = datos_sample)
# 
# modelo9 <- glm(class ~ cap_diameter + stem_color + season, family = binomial,
#                data = datos_sample)
# 
# 
# comparacion <- data.frame(
#   akaike = c(AIC(modelo1), AIC(modelo2), AIC(modelo3), AIC(modelo4), AIC(modelo5), 
#             AIC(modelo6), AIC(modelo7), AIC(modelo8), AIC(modelo9)),
#   modelo = rep(1:9, 1)
# )

# Los modelos con AIC más chicos son el 4 y el 6 con 305.66 y 307.17 respectivamente
# mientras que el modelo 7 ya salta con un AIC de 329.
