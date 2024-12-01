library(randomForest)  # Para trabajar con Random Forest
library(caret)       # Para preprocesamiento de datos y evaluación de modelos


# 1. Cargar el Conjunto de datos
data = read.csv("./files/datosLaboratorio04.csv")

head(data)
summary(data)


# 2. División de los datos en conjuntos de entrenamiento y prueba 

# Metodo utilizado: sample

# Establecer una semilla para reproducibilidad
set.seed(123)

# Calcular el índice de división (por ejemplo, 80% entrenamiento, 20% prueba)
indices_entrenamiento <- sample(1:nrow(data), 0.8 * nrow(data))

# Crear conjuntos de entrenamiento y prueba
datos_entrenamiento <- data[indices_entrenamiento, ]
datos_prueba <- data[-indices_entrenamiento, ]

# Verificar que haya divicion en los datos
dim(datos_entrenamiento)
dim(datos_prueba)
dim(data)


# 3. Selección del modelo de Machine Learning 

# Cargar las librerías necesarias






# Supongamos que queremos predecir la variable "target" (ajusta esto según tu caso)

# Construir el modelo de Random Forest
modelo_rf <- randomForest(account_length ~ ., data = datos_entrenamiento)

# Imprimir información sobre el modelo
print(modelo_rf)

# Hacer predicciones en el conjunto de prueba
predicciones <- predict(modelo_rf, newdata = datos_prueba)

# Evaluar el modelo utilizando una matriz de confusión
matriz_confusion <- confusionMatrix(predicciones, datos_prueba$target)
print(matriz_confusion)

# Visualizar la importancia de las variables
varImpPlot(modelo_rf)
