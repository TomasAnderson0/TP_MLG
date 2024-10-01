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


modelo1 <- glm(class ~ stem_width + stem_color + season, family = binomial,
               data = datos_sample)

modelo2 <- glm(class ~ cap_shape + stem_color + season, family = binomial,
               data = datos_sample)

modelo3 <- glm(class ~ cap_diameter + stem_width + stem_color, family = binomial,
               data = datos_sample)

modelo4 <- glm(class ~ gill_attachment + stem_width + stem_color, family = binomial,
               data = datos_sample)

modelo5 <- glm(class ~ gill_color + stem_color + season, family = binomial,
               data = datos_sample)

modelo6 <- glm(class ~ gill_attachment + gill_color + stem_color, family = binomial,
               data = datos_sample)

modelo7 <- glm(class ~ cap_diameter + cap_shape + stem_color, family = binomial,
               data = datos_sample)

modelo8 <- glm(class ~ gill_color + stem_width + season, family = binomial,
               data = datos_sample)

modelo9 <- glm(class ~ cap_diameter + stem_color + season, family = binomial,
               data = datos_sample)


comparacion <- data.frame(
  residual_d = c(modelo1$deviance, modelo2$deviance, modelo3$deviance,
               modelo4$deviance, modelo5$deviance, modelo6$deviance,
               modelo7$deviance, modelo8$deviance, modelo9$deviance),
  df = c(modelo1$df.residual, modelo2$df.residual, modelo3$df.residual, 
         modelo4$df.residual, modelo5$df.residual, modelo6$df.residual,
         modelo7$df.residual, modelo8$df.residual, modelo9$df.residual)
)


combinaciones <- combn(1:9, 2)

resultados <- data.frame(
  dif_deviance = numeric(ncol(combinaciones)),
  dif_df = numeric(ncol(combinaciones)),
  prob_asoc = numeric(ncol(combinaciones)),
  modelos = character(ncol(combinaciones))
)


for (k in 1:ncol(combinaciones)) {
  i <- combinaciones[1, k]
  j <- combinaciones[2, k]
  
  
  resultados$dif_deviance[k] <- comparacion$residual_d[i] - comparacion$residual_d[j]
  resultados$dif_df[k] <- abs(comparacion$df[i] - comparacion$df[j])
  resultados$prob_asoc[k] <- pchisq(resultados$dif_deviance[k], resultados$dif_df[k], lower.tail = FALSE)
  resultados$modelos[k] <- paste0(i, "-", j)
}



mejores <- resultados %>% 
  filter(prob_asoc <= 0.06)
# Los modelos 8 y 9 quedan descartados porque ajustan practicamente lo mismo y 
# no son mejores que ningúno de los otros 7.

