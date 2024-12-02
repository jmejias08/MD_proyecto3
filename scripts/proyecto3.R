# Cargar librerías necesarias
library(tidyverse)
library(caret)
library(randomForest)
library(PerformanceAnalytics)


# 1. Cargar el Conjunto de datos
datos = read.csv("./files/datosLaboratorio04.csv")



head(datos)

str(datos)

summary(datos)


################################################################################


# 2. División de los datos en conjuntos de entrenamiento y prueba 

# Metodo utilizado: sample

# Establecer una semilla para reproducibilidad
set.seed(123)



tamano<-nrow(datos)
training<-round(tamano*0.7)
indices<-sample(1:tamano,size = training)

train_data<-datos[indices,]
test_data<-datos[-indices,]
chart.Correlation(train_data[,-5])

View(train_data)


#################################################################################

# 3 vamos a utlizar random forest

###############################################################################

# 4 entrenamiento del modelo

# Normalización de datos (excluyendo la variable objetivo)
preProc <- preProcess(train_data[, -ncol(train_data)], method = c("center", "scale"))
train_data_scaled <- predict(preProc, train_data)
test_data_scaled <- predict(preProc, test_data)

# Entrenar el modelo Random Forest

rf_model <- randomForest(
  day_minutes ~ ., 
  data = train_data_scaled, 
  importance = TRUE, 
  ntree = 500
)



# Resumen del modelo
print(rf_model)




################################################################################

# 5. Evaluacion del modelo


# Predicción en el conjunto de prueba
predictions <- predict(rf_model, test_data_scaled)

# Calcular métricas
mse <- mean((predictions - test_data_scaled$day_minutes)^2)
r2 <- 1 - (sum((predictions - test_data_scaled$day_minutes)^2) / 
             sum((mean(test_data_scaled$day_minutes) - test_data_scaled$day_minutes)^2))

cat("MSE: ", mse, "\n")
cat("R-squared: ", r2, "\n")

# Curva ROC no aplica directamente para regresión, pero podemos visualizar errores
plot(predictions, test_data_scaled$day_minutes, 
     main = "Predicción vs Valores Reales",
     xlab = "Predicción", ylab = "Real")
abline(0, 1, col = "red")

################################################################################

# 6 Ajuste de parametros

# Configurar búsqueda por malla
tune_grid <- expand.grid(mtry = c(2, 3, 5))

# Afinar el modelo
set.seed(123)
tuned_model <- train(
  day_minutes ~ ., 
  data = train_data_scaled, 
  method = "rf",
  tuneGrid = tune_grid,
  trControl = trainControl(method = "cv", number = 5)
)

# Resumen del modelo ajustado
print(tuned_model)

#################################################################################

# 7 Interpretacion del modelo

# Importancia de las variables
varImpPlot(rf_model)

varImpPlot(tuned_model)
















###############################################################################
# Excluir la variable objetivo de las características
caracteristicas <- setdiff(names(datos), "customer_service_calls")


# Verificar que haya divicion en los datos
dim(datos_entrenamiento)
dim(datos_prueba)
dim(datos)

View(datos_entrenamiento)
View(datos_prueba)


# Opcional: Verificar distribución de la variable objetivo
# para asegurar que la división mantiene la proporción
print("Distribución variable objetivo - Datos Originales:")
table(datos$customer_service_calls)
print("Distribución variable objetivo - Datos Entrenamiento:")
table(datos_entrenamiento$customer_service_calls)
print("Distribución variable objetivo - Datos Prueba:")
table(datos_prueba$customer_service_calls)



###################################################################################
# 3. Selección del modelo de Machine Learning 
# Se selecciona el RF debido a su versatibilidad de poder trabajar con variables
# cualitativas o cuantitativas

# Construir el modelo de Random Forest
# Modelo 1: Random Forest básico
modelo_rf1 <- randomForest(
      x = datos_entrenamiento[, caracteristicas], 
      y = datos_entrenamiento$customer_service_calls,
      ntree = 1000,  # Número de árboles
      importance = TRUE
)


predicciones_rf1 <- predict(modelo_rf1, datos_prueba[, caracteristicas])


# Evaluación de modelos
print("Modelo Random Forest Básico:")
confusion_rf1 <- confusionMatrix(predicciones_rf1, datos_prueba$customer_service_calls)
print(confusion_rf1)



################################################################################

