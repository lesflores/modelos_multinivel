# install.packages("readxl")
library(readxl)
library(tidyverse)

data <- read_excel("datos_prueba.xlsx") # Esta base de datos es simplificada
# y sirve como ejemplo para el ejercicio del modelo multinivel R

glimpse(data)
summary(data)

datos_estandarizados <- data

# Scale
datos_estandarizados$Intensity <- scale(data$Intensity)
datos_estandarizados$PIB_per <- scale(data$PIB_per)
datos_estandarizados$Equal <- scale(data$Equal)

# Factor 
datos_estandarizados$Genero_factor <- as.factor(datos_estandarizados$Genero)

# install.packages("lmer4")
library(lme4)
library(Matrix)
library(lmerTest)

#----------------------
# MODELO TRADICIONAL
#----------------------

# Tradicional

modelo_tradicional_1 <- lm(v2x_polyarchy ~ 1 + Intensity + 
                              Equal + PIB_per + Genero_factor, 
                            data = datos_estandarizados)

summary(modelo_tradicional_1)


# PRUEBAS MODELO TRADICIONAL

# normalidad
plot(density(modelo_tradicional_1$residuals))# sin normalidad
shapiro.test(resid(modelo_tradicional_1))

### heterocedasticidad
library(lmtest)
bptest(modelo_tradicional_1) # con heterocedasticidad
# si el valor p es mayor que el nivel 
# de significancia, se concluye que no hay evidencia 
# de heterocedasticidad.

# La heterocedasticidad se refiere a la situación en la que la varianza de los errores 
# (o residuos) no es constante a lo largo de las observaciones. 
# En un modelo de regresión ideal, esperamos que los residuos tengan una varianza constante 
# porque esto significa que el modelo está estimando los errores de manera uniforme a través 
# de todas las predicciones. 

### linealidad
resettest(modelo_tradicional_1, power = 2:3)
# Si el valor p es mayor que el nivel de significancia elegido, 
# se puede concluir que no hay suficiente evidencia para rechazar 
# la hipótesis nula y que el modelo es lineal en la variable predictora.

### independencia de residuos
dwtest(modelo_tradicional_1)
# El resultado de la prueba de Durbin-Watson 
# Si el valor p es mayor que el nivel de significancia elegido, 
# se puede concluir que no hay suficiente evidencia 
# para rechazar la hipótesis nula y que los residuos son independientes.

# La presencia de autocorrelación implica que hay información 
# en los residuos que el modelo no ha capturado.

#----------------------
# MODELO NULO
#----------------------

# Ajustar el modelo nulo,
# que no tiene las variables explicativas. 

modelo_nulo <- lmer(v2x_polyarchy ~ 1 + (1 | Country), REML = TRUE, data = datos_estandarizados)
icc(modelo_nulo)

#----------------------
# MODELO MULTINIVEL
#----------------------

modelo_multinivel_1 <- lmer(v2x_polyarchy ~ 1 + Intensity + 
                              Equal + PIB_per + Genero_factor + 
                              (1 + Intensity | Country),
                            REML = TRUE,
                            data = datos_estandarizados)

summary(modelo_multinivel_1)

anova(modelo_nulo, modelo_multinivel_1)

ranef(modelo_multinivel_1)

# PRUEBAS MODELO MULTINIVEL

residuos <- resid(modelo_multinivel_1)

# Estadísticas básicas
mean_residuos <- mean(residuos)
sd_residuos <- sd(residuos)

mean_residuos
sd_residuos

library(DHARMa) #Hartig (2022)

simulationOutput <- simulateResiduals(fittedModel = modelo_multinivel_1)
testDispersion(simulationOutput) # Un p-valor alto aquí significa (cercano a 1) que no hay diferencia significativa 
# entre la dispersión esperada y la observada, indicando que el modelo maneja adecuadamente la variabilidad en los datos. 

# Idealmente, la línea debe estar cerca del pico del histograma, 
# lo cual indicaría que la dispersión de los residuales del modelo se alinea bien 
# con la mayoría de las simulaciones. 

simulationOutput <- simulateResiduals(fittedModel = modelo_multinivel_1)
uniformityTest <- testUniformity(simulationOutput)

uniformityTest # mide la máxima desviación entre la distribución acumulativa de los residuales observados 
# y la distribución esperada. Un valor bajo, como 0.062319, indica que la diferencia máxima es pequeña.


# Gráfica

library(ggplot2)

# Tabla
random_effects <- ranef(modelo_multinivel_1)$Country
fixed_effects <- fixef(modelo_multinivel_1)
fixed_effect_intensity <- fixed_effects["Intensity"]
total_effects <- fixed_effect_intensity + random_effects$Intensity

df_effects <- data.frame(
  Country = rownames(random_effects),
  Random_Effect = random_effects$Intensity,
  Fixed_Effect = fixed_effect_intensity,
  Total_Effect = total_effects
)


df_effects

grafica_1 <- ggplot(df_effects, aes(x = reorder(Country, Total_Effect), y = Total_Effect)) + 
  geom_bar(stat = "identity", fill = "gray95", color = "black") + 
  geom_hline(yintercept = fixed_effect_intensity, linetype="dashed", color = "#404080", linewidth = 1) + 
  annotate("text", label = "Efecto Fijo", x=6, y=fixed_effect_intensity +0.002, color="#404080", size = 3) +
  labs(
    y = "Efecto total de la intensidad de conflictos",
    x = "País"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title = element_text(size = 11),
    title = element_text(size = 11, face = "bold")
  )


grafica_1
