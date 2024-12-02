# Librerías necesarias
library(dplyr)
library(factoextra)
library(cluster)
library(gridExtra)

# 1. Cargar el conjunto de datos
df <- read.csv("Files/llamadas.csv", encoding = "UTF-8", header = TRUE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE)

# Seleccionar variables relevantes de llamadas
df_calls <- df %>% 
  select(day_minutes, day_calls, evening_minutes, evening_calls, night_minutes, night_calls, intl_minutes, intl_calls) 

# 2. Dividir los datos en entrenamiento y prueba
set.seed(123)
train_indices <- sample(1:nrow(df_calls), size = 0.7 * nrow(df_calls))
train_data <- df_calls[train_indices, ]
test_data <- df_calls[-train_indices, ]

# 3. Preprocesar los datos (escalar)
train_data_scaled <- scale(train_data)
test_data_scaled <- scale(test_data)

# 4. Determinar el número óptimo de clusters
set.seed(123)

# Método del codo
fviz_nbclust(train_data_scaled, kmeans, method = "wss") + 
  labs(title = "Elbow Method (WSS)")

# Método silhouette
fviz_nbclust(train_data_scaled, kmeans, method = "silhouette") + 
  labs(title = "Silhouette Method")

# 5. Entrenamiento del modelo K-Means con k = 4
kmeans_result <- kmeans(train_data_scaled, centers = 4, nstart = 25)

# 6. Evaluación del modelo
# Visualizar los clusters
fviz_cluster(kmeans_result, data = train_data_scaled, geom = "point") + 
  labs(title = "K-Means Clustering Result")

# Sumarización de las características de cada cluster
cluster_summary <- aggregate(train_data, by = list(kmeans_result$cluster), mean)
print("Resumen de cada cluster:")
print(cluster_summary)

# 7. Predicción sobre datos de prueba (asignación de clusters)
test_clusters <- kmeans(test_data_scaled, centers = kmeans_result$centers, nstart = 1)

# 8. Visualización de los resultados en el conjunto de prueba
fviz_cluster(test_clusters, data = test_data_scaled, geom = "point") +
  labs(title = "Test Data Clustering Result")

# Evaluación del modelo
print(paste("Inercia total (totss):", kmeans_result$totss))
print(paste("Inercia inter-grupos (betweenss):", kmeans_result$betweenss))
print(paste("Inercia intra-grupos total (tot.withinss):", kmeans_result$tot.withinss))
print(paste("Inercia  intra-grupos para cada cluster.(tot.withinss):", kmeans_result$withinss))


# Visualización de métricas comparativas con diferentes números de clusters
k2 <- kmeans(train_data_scaled, centers = 2, nstart = 25)
k3 <- kmeans(train_data_scaled, centers = 3, nstart = 25)
k4 <- kmeans(train_data_scaled, centers = 4, nstart = 25)
k5 <- kmeans(train_data_scaled, centers = 5, nstart = 25)

p1 <- fviz_cluster(k2, geom = "point", data = train_data_scaled) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = train_data_scaled) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = train_data_scaled) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = train_data_scaled) + ggtitle("k = 5")

grid.arrange(p1)
grid.arrange(p1, p2, p3, p4, nrow = 2)



