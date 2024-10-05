# ---Librerias---
library(readr)
library(tidyverse)
library(ggplot2)


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

hongos = hongos %>% mutate(cap_shape = as.factor(cap_shape), 
                 gill_attachment = as.factor(gill_attachment),
                 gill_color = as.factor(gill_color),
                 stem_color = as.factor(stem_color),
                 season = as.factor(season))

## Recategorizacion

hongos = hongos %>% mutate(gill_color = case_when(gill_color == "e" | gill_color == "y" | gill_color == "o" | gill_color == "p" ~ "Calidos",
                                                  gill_color == "u" | gill_color == "r" ~ "Frios",
                                                  gill_color == "w" | gill_color == "g" | gill_color == "b" ~ "Claros",
                                                  gill_color == "n" | gill_color == "k" ~ "Oscuros",
                                                  gill_color == "f" ~ "Ninguno"),
                           gill_attachment = case_when(gill_attachment == "a" | gill_attachment == "x"~ "Adjuntas",
                                                       gill_attachment == "s" | gill_attachment == "p" | gill_attachment == "u" ~ "Otros",
                                                       gill_attachment == "e"  ~ "Libre",
                                                       gill_attachment == "f" ~ "Ninguno",
                                                       gill_attachment == "d"~ "Decurrente"))









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


# Vemos si las que ya estaban siguen siendo significativas

modelo13 = glm(class ~  stem_color + cap_shape, family = binomial(link="logit"),
              data = datos_sample)

modelo14 = glm(class ~   season + cap_shape, family = binomial(link="logit"),
               data = datos_sample)


comp11 = anova(modelo13, modelo9)
comp12 = anova(modelo14, modelo9)
pchisq(comp11$Deviance[2],comp11$Df[2],lower.tail = F)
pchisq(comp12$Deviance[2],comp12$Df[2],lower.tail = F)

# Siguen siendo signoficativas, proseguimos agregando variables

modelo15 = glm(class ~  stem_color + season + cap_shape + cap_diameter, family = binomial(link="logit"),
               data = datos_sample)

modelo16 = glm(class ~  stem_color + season + cap_shape + gill_attachment, family = binomial(link="logit"),
               data = datos_sample)

modelo17 = glm(class ~ stem_color + season + cap_shape + gill_color, family = binomial(link = "logit"),
               data = datos_sample)

modelo18 = glm(class ~  stem_color + season + cap_shape + stem_height, family = binomial(link="logit"),
               data = datos_sample)

modelo19 = glm(class ~  stem_color + season + cap_shape + stem_width, family = binomial(link="logit"),
               data = datos_sample)

comp13 = anova(modelo9,modelo15)
comp14 = anova(modelo9,modelo16)
comp15 = anova(modelo9,modelo17)
comp16 = anova(modelo9,modelo18)
comp17 = anova(modelo9,modelo19)
pchisq(comp13$Deviance[2],comp13$Df[2],lower.tail = F)
pchisq(comp14$Deviance[2],comp14$Df[2],lower.tail = F)
pchisq(comp15$Deviance[2],comp15$Df[2],lower.tail = F)
pchisq(comp16$Deviance[2],comp16$Df[2],lower.tail = F)
pchisq(comp17$Deviance[2],comp17$Df[2],lower.tail = F)


#Añadimos gill_attachment

# Verificamos, otra vez, que las que ya están sean aún significativas

modelo20 = glm(class ~  season + cap_shape + gill_attachment, family = binomial(link="logit"),
               data = datos_sample)

modelo21 = glm(class ~  stem_color + season + gill_attachment, family = binomial(link="logit"),
               data = datos_sample)

modelo22 = glm(class ~ stem_color + cap_shape + gill_attachment, family = binomial(link = "logit"),
               data = datos_sample)


comp18 = anova(modelo20, modelo16)
comp19 = anova(modelo21, modelo16)
comp20 = anova(modelo22, modelo16)
pchisq(comp18$Deviance[2],comp18$Df[2],lower.tail = F)
pchisq(comp19$Deviance[2],comp19$Df[2],lower.tail = F)
pchisq(comp20$Deviance[2],comp20$Df[2],lower.tail = F)

# Todas siguen siendo signficativas, probamos agregando más.

modelo23 = glm(class ~  stem_color + season + cap_shape + gill_attachment + cap_diameter, family = binomial(link="logit"),
               data = datos_sample)

modelo24 = glm(class ~  stem_color + season + cap_shape + gill_attachment + gill_color, family = binomial(link="logit"),
               data = datos_sample)

modelo25 = glm(class ~  stem_color + season + cap_shape + gill_attachment + stem_height, family = binomial(link="logit"),
               data = datos_sample)

modelo26 = glm(class ~  stem_color + season + cap_shape + gill_attachment + stem_width, family = binomial(link="logit"),
               data = datos_sample)

comp21 = anova(modelo16,modelo23)
comp22 = anova(modelo16,modelo24)
comp23 = anova(modelo16,modelo25)
comp24 = anova(modelo16,modelo26)
pchisq(comp21$Deviance[2],comp21$Df[2],lower.tail = F)
pchisq(comp22$Deviance[2],comp22$Df[2],lower.tail = F)
pchisq(comp23$Deviance[2],comp23$Df[2],lower.tail = F)
pchisq(comp24$Deviance[2],comp24$Df[2],lower.tail = F)


#Nos quedamos con el modelo16

# Las interacciones no se pueden hacer porque no están todos los cruces de 
# catgorías.



# Caso para semilla 205

set.seed(205)  

datos_sample = hongos[sample(61069,250),]
 
# Análisis descriptivo

ggplot(datos_sample) + 
  aes(x = factor(class), fill = factor(class)) + 
  geom_bar() +
  scale_fill_manual(values = c("slateblue", "orangered"), guide = "none") +
  scale_x_discrete(name = "Hongo", labels = c("Venenoso", "Comestible")) +
  scale_y_continuous(breaks = seq(0, 150, 25)) +
  labs(y = "Frecuencia", title = "Gráfico 1: Cantidad de hongos según categoría") +
  theme_bw()

ggplot(datos_sample) +
  aes(x = cap_shape, fill = factor(class)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("slateblue", "orangered"), guide = "none") +
  scale_x_discrete(name = "Sombrero", labels = c("b" = "campana", "c" = "cónico", "f" = "plano","x" = "convexo","s" = "hundido","o" = "otros", "p" = "esferico")) +
  labs(y = "Frecuencia", 
       title = "Gráfico 2; Cantidad de hongos según la forma del sombrero") +
  theme_bw()

ggplot(datos_sample) +
  aes(x = gill_attachment, fill = factor(class)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("slateblue", "orangered"), guide = "none") +
  scale_x_discrete(name = "Sombrero") +
  labs(y = "Frecuencia", 
       title = "Gráfico 3; Cantidad de hongos según") +
  theme_bw()

ggplot(datos_sample) +
  aes(x = gill_color, fill = factor(class)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("slateblue", "orangered"), guide = "none") +
  scale_x_discrete(name = "Sombrero") +
  labs(y = "Frecuencia", 
       title = "Gráfico 4; Cantidad de hongos según") +
  theme_bw()

datos_sample %>% 
  #saco la observacion de 60 para que se vea bien los boxplots
  filter(cap_diameter<50) %>% 
ggplot() +
  aes(y = cap_diameter, x = factor(class)) +
  geom_boxplot() + scale_x_discrete(name = "Hongo", labels = c("Venenoso", "Comestible")) 

# Barras apiladas

datos_sample %>% group_by(cap_shape, class) %>% count() %>% 
  ggplot() +
  aes(x = cap_shape, fill = factor(class), y = n) +
  geom_bar(position = "fill",  stat="identity") +
  scale_fill_manual(values = c("slateblue", "orangered"), guide = "none") +
  scale_x_discrete(name = "Sombrero", labels = c("b" = "campana", "c" = "cónico", "f" = "plano","x" = "convexo","s" = "hundido","o" = "otros", "p" = "esferico")) +
  labs(y = "Frecuencia", 
       title = "Gráfico 2; Cantidad de hongos según la forma del sombrero") +
  theme_bw()

datos_sample %>% group_by(gill_attachment, class) %>% count() %>% 
  ggplot() +
  aes(x = gill_attachment, fill = factor(class), y = n) +
  geom_bar(position = "fill",  stat="identity") +
  scale_fill_manual(values = c("slateblue", "orangered"), guide = "none") +
  scale_x_discrete(name = "Sombrero") +
  labs(y = "Frecuencia", 
       title = "Gráfico 3; Cantidad de hongos según") +
  theme_bw()

datos_sample %>% group_by(gill_color, class) %>% count() %>% 
  ggplot() +
  aes(x = gill_color, fill = factor(class), y = n) +
  geom_bar(position = "fill",  stat="identity") +
  scale_fill_manual(values = c("slateblue", "orangered"), guide = "none") +
  scale_x_discrete(name = "Sombrero") +
  labs(y = "Frecuencia", 
       title = "Gráfico 4; Cantidad de hongos según") +
  theme_bw()

#Modelo inical

modelo1 <- glm(class ~ cap_diameter + cap_shape + gill_attachment + gill_color, family = binomial(link="logit"),
               data = datos_sample)

modelo2 = glm(class ~  cap_shape  , family = binomial(link="logit"),
              data = datos_sample)

modelo3 = glm(class ~  gill_attachment , family = binomial(link="logit"),
              data = datos_sample)

modelo4 = glm(class ~  gill_color , family = binomial(link="logit"),
              data = datos_sample)


pchisq(modelo1$null.deviance-modelo1$deviance,1, lower.tail = F)
pchisq(modelo2$null.deviance-modelo2$deviance,length(unique(datos_sample$cap_shape))-1, lower.tail = F)
pchisq(modelo3$null.deviance-modelo3$deviance,length(unique(datos_sample$gill_attachment))-1, lower.tail = F)
pchisq(modelo4$null.deviance-modelo4$deviance,length(unique(datos_sample$gill_color))-1, lower.tail = F)



modelo5 <- glm(class ~ gill_color + cap_shape , family = binomial(link="logit"),
               data = datos_sample)

comp1 = anova(modelo2, modelo5)
pchisq(comp1$Deviance[2],comp1$Df[2],lower.tail = F)
comp2 = anova(modelo4, modelo5)
pchisq(comp2$Deviance[2],comp2$Df[2],lower.tail = F)

modelo6 <- glm(class ~ gill_attachment + cap_shape , family = binomial(link="logit"),
               data = datos_sample)

modelo7 <- glm(class ~ cap_diameter + cap_shape , family = binomial(link="logit"),
               data = datos_sample)



comp3 = anova(modelo2, modelo6)
pchisq(comp3$Deviance[2],comp3$Df[2],lower.tail = F)
comp4 = anova(modelo2, modelo7)
pchisq(comp4$Deviance[2],comp4$Df[2],lower.tail = F)


# Agrupamos los datos
datos_agrup <- datos_sample %>% 
  select(cap_shape, class) %>%
  group_by(cap_shape) %>% 
  summarise(n = n(), y = sum(class) ) 


# Vemos cuál enlace es mejor:

mod_logit <- glm(y/n ~ cap_shape, family = binomial(link = "logit"),
                 weights = n, data = datos_agrup)
summary(mod_logit)
mod_probit <- glm(y/n ~ cap_shape, family = binomial(link = "probit"),
                  weights = n,data = datos_agrup)
summary(mod_probit)
mod_cll <- glm(y/n ~ cap_shape, family = binomial(link = "cloglog"),
               weights = n,data = datos_agrup)
summary(mod_cll)

AIC(mod_logit, mod_probit, mod_cll) #Son los 3 iguales (aprieta el puño con rabia)

# Bondad de ajuste:
pchisq(mod_logit$deviance, mod_logit$df.residual, ncp = 0, 
       lower.tail = FALSE, log.p = FALSE)
pchisq(mod_probit$deviance, mod_probit$df.residual, ncp = 0, 
       lower.tail = FALSE, log.p = FALSE)
pchisq(mod_cll$deviance, mod_cll$df.residual, ncp = 0, 
       lower.tail = FALSE, log.p = FALSE)

# Al todos ajustar y tener el mismo AIC, nos quedamos con el enlace logit 
# (igual, preguntaría si está bien lo de que sea un modelo saturado)

# Gráfico de las "working responses"
z_logit <- resid(mod_logit, type = "working") + mod_logit$linear.predictor

ggplot(datos_agrup, aes(y = mod_logit$linear.predictor, x = z_logit)) + 
  geom_point(fill = "maroon", shape = 21, size = 3) +
  labs(title = "Working responses vs. predictor lineal", x = "Z" , y = expression(eta),
       subtitle = "Modelo logit") +
  geom_abline(intercept = 0, slope = 1) +
  theme_minimal()

# Hay relación lineal, probamos el test de (eta)^2

# 1) MODELO LOGIT
pred.logit<-predict(mod_logit)
datos_agrup$pred.2.logit<-pred.logit*pred.logit

mod_logit.2 <- glm(y/n ~ cap_shape + pred.2.logit, family=binomial(link="logit"), 
                    weights=n,data=datos_agrup)

summary(mod_logit.2)
anova(mod_logit, mod_logit.2, test="LRT") 

# Al tener los dos modelos el mismo valor de deviance, ajustan por igual; por ende
# el enlace es correcto.


modelo_final <- mod_logit
summary(modelo_final) # La categoría de referencia es "b" = campana


# Se calculan las RO estimadas.
