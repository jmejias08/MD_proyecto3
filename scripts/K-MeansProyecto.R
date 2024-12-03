# Cargar librerías necesarias
library(dplyr) 
# Para manipulación de datos
library(factoextra)  # Para visualización de resultados de clustering
library(cluster)     # Para análisis de clusters
library(gridExtra)   # Para organizar múltiples gráficos

# 1. Cargar el conjunto de datos
# Leer datos desde un archivo CSV con las configuraciones necesarias
df <- read.csv("Files/llamadas.csv", encoding = "UTF-8", header = TRUE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE)

# Seleccionar las columnas que se usarán para el clustering
df_calls <- df %>% 
  select(day_minutes, day_calls, evening_minutes, evening_calls, night_minutes, night_calls, intl_minutes, intl_calls)

# 2. Dividir los datos en entrenamiento y prueba
set.seed(123)  # Fijar semilla para reproducibilidad
# Dividir los índices para seleccionar datos de entrenamiento (70%)
train_indices <- sample(1:nrow(df_calls), size = 0.7 * nrow(df_calls))
# Crear conjunto de datos de entrenamiento y prueba
train_data <- df_calls[train_indices, ]
test_data <- df_calls[-train_indices, ]

# 3. Preprocesar los datos
# Escalar los datos para que todas las variables tengan magnitud similar
train_data_scaled <- scale(train_data)
test_data_scaled <- scale(test_data)

# 4. Determinar el número óptimo de clusters
set.seed(123)  # Fijar semilla para consistencia en los resultados

# Método del codo (WSS - Within Sum of Squares)
# Ayuda a encontrar el número óptimo de clusters analizando la inercia intra-grupos
fviz_nbclust(train_data_scaled, kmeans, method = "wss") + 
  labs(title = "Método del codo: Determinación del número óptimo de clusters")

# Método Silhouette
# Evalúa qué tan bien separados están los clusters y la cohesión de los datos
fviz_nbclust(train_data_scaled, kmeans, method = "silhouette") + 
  labs(title = "Método Silhouette: Evaluación de calidad de clusters")

# 5. Entrenamiento del modelo K-means con k = 4
# Crear el modelo de clustering K-means con 4 clusters
# nstart = 25 significa que se probarán 25 inicializaciones de centroides y se tomará la mejor
kmeans_result <- kmeans(train_data_scaled, centers = 4, nstart = 25)

# 6. Evaluación del modelo
# 6. Visualización de clusters en datos de entrenamiento
# Cambiar los nombres de los clusters por algo más entendible para los analistas
fviz_cluster(
  kmeans_result,
  data = train_data_scaled,
  geom = "point",
  ellipse.type = "convex",  # Dibujar un área convexa alrededor de cada cluster
  palette = "jco",         # Cambiar la paleta de colores para mejor visualización
  ggtheme = theme_minimal() # Aplicar un tema limpio
) + 
  labs(
    title = "Distribución de clientes según grupo (Entrenamiento)",
    x = "Componente principal 1 (PC1)",
    y = "Componente principal 2 (PC2)"
  )

# 7. Visualización de clusters en datos de prueba
# Similar a la visualización de entrenamiento pero para el conjunto de prueba
fviz_cluster(
  test_clusters,
  data = test_data_scaled,
  geom = "point",
  ellipse.type = "convex",  # Mostrar las áreas de cada cluster
  palette = "jco",
  ggtheme = theme_minimal()
) +
  labs(
    title = "Distribución de clientes según grupo (Prueba)",
    x = "Componente principal 1 (PC1)",
    y = "Componente principal 2 (PC2)"
  )

# 8. Comparación de modelos con diferentes números de clusters
# Mejora visual en los títulos y etiquetas para identificar claramente los clusters
p1 <- fviz_cluster(k2, geom = "point", data = train_data_scaled) + 
  ggtitle("Clientes segmentados en 2 grupos") +
  theme_minimal()

p2 <- fviz_cluster(k3, geom = "point", data = train_data_scaled) + 
  ggtitle("Clientes segmentados en 3 grupos") +
  theme_minimal()

p3 <- fviz_cluster(k4, geom = "point", data = train_data_scaled) + 
  ggtitle("Clientes segmentados en 4 grupos") +
  theme_minimal()

p4 <- fviz_cluster(k5, geom = "point", data = train_data_scaled) + 
  ggtitle("Clientes segmentados en 5 grupos") +
  theme_minimal()

# Mostrar todos los gráficos de comparación con títulos mejorados
grid.arrange(p1, p2, p3, p4, nrow = 2)


#Ejemplo personal de varible de peso en los clusters
#Observación directa de los datos escalados
boxplot(train_data_scaled[, "day_minutes"] ~ kmeans_result$cluster, main = "Diferencias por cluster en Day Minutes")

