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
#chart.Correlation(train_data[,-5])

View(train_data)
View(test_data)


#################################################################################

# 3. Selección del modelo de Machine Learning

# vamos a utlizar random forest para resolver el problema
# el cual queremos trabajar sobre la cantidad de minutos diarios de 
# cada cliente, de este forma podriamos aplicarle promociones especiales
# a los clientes mas estables

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


#RMSE (Error Cuadrático Medio): Mide qué tan lejos están, en promedio, las 
#predicciones del modelo de los valores reales. Cuanto más bajo, mejor.

#R-cuadrado: Indica qué proporción de la variabilidad en los datos puede 
#ser explicada por el modelo. Valores más cercanos a 1 indican un mejor ajuste.

#MAE (Error Absoluto Medio): Mide el promedio de los errores absolutos. Cuanto más bajo, mejor.

#################################################################################

# 7 Interpretacion del modelo

# Importancia de las variables
varImpPlot(rf_model, main = "Importancia de las variables")

# Extraer importancia de las variables en forma tabular

#IncMSE: Este valor te indica cuánto empeora la predicción del modelo si quitas 
#una característica en particular. Si el valor es alto, significa que esa característica 
#es muy importante para hacer una buena predicción.

#IncNodePurity: Este valor mide qué tan bien una característica separa los datos
#en grupos diferentes. Si el valor es alto, quiere decir que esa característica 
#es buena para distinguir entre clientes que gastan mucho y poco.

variable_importance <- importance(rf_model)
print(variable_importance)


# Librería DALEX para SHAP en R
library(DALEX)

# Crear un explicador para el modelo
explainer <- explain(rf_model, data = train_data_scaled[, -ncol(train_data_scaled)], y = train_data_scaled$day_minutes)

# Calcular los valores SHAP
shap_values <- predict_parts(explainer, new_observation = test_data_scaled[1, ], type = "shap")
plot(shap_values)


###############################################################################

# Ejemplo de nuevos datos
new_data <- data.frame(
  account_length = c(0.5, 1.2),
  voicemail_message_count = c(1.0, -0.3),
  day_minutes = c(1.5, 0.8),
  day_calls = c(0.5, 0.9),
  day_charge = c(1.6, 0.7),
  evening_minutes = c(-0.1, 0.2),
  evening_calls = c(-0.2, 0.4),
  evening_charge = c(-0.1, 0.3),
  night_minutes = c(0.8, 1.0),
  night_calls = c(-0.4, 0.2),
  night_charge = c(0.9, 1.1),
  intl_minutes = c(0.1, -0.5),
  intl_calls = c(0.2, -0.4),
  intl_charge = c(0.1, -0.3),
  customer_service_calls = c(-0.5, 0.5)
)

# Normalizar los nuevos datos utilizando el preprocesamiento previamente creado
new_data_scaled <- predict(preProc, new_data)

# Realizar predicciones
numeric_predictions <- predict(rf_model, new_data_scaled)

# Mostrar resultados
print("Predicciones numéricas:")
print(numeric_predictions)














