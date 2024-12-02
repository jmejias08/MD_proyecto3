      # Para preprocesamiento de datos y evaluación de modelos
# Bibliotecas necesarias para análisis de datos y modelado
library(rpart)      # Árboles de decisión
library(ROCR)       # Evaluación de rendimiento de modelos
library(caret)      # Herramientas para entrenamiento de modelos
library(randomForest) # Algoritmo Random Forest
library(dplyr)      # Manipulación de datos
library(PerformanceAnalytics) # Gráficos de correlación
library(pROC)       # Curva ROC

# 1. Cargar el Conjunto de datos
datos = read.csv("./files/datosLaboratorio04.csv")

head(datos)
summary(datos)


################################################################################


# 2. División de los datos en conjuntos de entrenamiento y prueba 

# Metodo utilizado: sample

# Establecer una semilla para reproducibilidad
set.seed(123)

tamano <- nrow(datos)



# Calcular el índice de división (por ejemplo, 80% entrenamiento, 20% prueba)
indices_entrenamiento <- sample(1:nrow(datos), 0.8 * nrow(datos))

# Crear conjuntos de entrenamiento y prueba
datos_entrenamiento <- datos[indices_entrenamiento, ]
datos_prueba <- datos[-indices_entrenamiento, ]

# Análisis de correlación entre variables predictoras
chart.Correlation(datos_entrenamiento[,-5])

# Verificar que haya divicion en los datos
dim(datos_entrenamiento)
dim(datos_prueba)
dim(datos)

View(datos_entrenamiento)

###################################################################################
# 3. Selección del modelo de Machine Learning 

modelo<-randomForest(account_length~.,data=datos_entrenamiento, 
                     ntree=500)



################################################################################

