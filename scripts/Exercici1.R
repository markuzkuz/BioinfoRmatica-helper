#actividad 1: 
library(ggplot2)
install.packages('pheatmap')
install.packages( 'complexHeatmap')
library(pheatmap)

getwd()
data <- read.csv('data/expresion_genes.csv', header = TRUE, row.names = 1)
data
summary(data)

#Ejercicio uno:
------------------------------------
# Lista de genes de interés
genes <- c("AQ_ALOX5", "AQ_CD274", "AQ_CHKA", "AQ_CSF2", "AQ_FOXO3", 
           "AQ_IL6", "AQ_LDHA", "AQ_LIF", "AQ_MAPK1", "AQ_NOS2", 
           "AQ_IFNG", "AQ_PDCD1", "AQ_PPARG", "AQ_TGFB1", "AQ_TNF")

# Seleccionamos las columnas de interés: tratamiento y genes
data_sel <- data[, c("trat", genes)]
data_sel

# Crear gráficos de caja individuales para cada gen
bp1 <- ggplot(data_sel, aes(x = trat, y = AQ_ALOX5, fill = trat)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Tratamiento")


bp2 <- ggplot(data_sel, aes(x = trat, y = AQ_CD274, fill = trat)) +
  geom_boxplot() +  
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(x = NULL)  # Quitar el título del eje x


bp3 <- ggplot(data_sel, aes(x = trat, y = AQ_CHKA, fill = trat)) +
  geom_boxplot() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(x = NULL)  # Quitar el título del eje x


bp4 <- ggplot(data_sel, aes(x=trat, y= AQ_CSF2, fill = trat)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = NULL)  # Quitar el título del eje x


bp5 <- ggplot(data_sel, aes(x=trat, y= AQ_FOXO3, fill = trat)) +
  geom_boxplot() +
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(x = NULL)  # Quitar el título del eje x


bp6 <- ggplot(data_sel, aes(x=trat, y= AQ_IL6, fill = trat)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = NULL)  # Quitar el título del eje x


bp7 <- ggplot(data_sel, aes(x=trat, y= AQ_LDHA, fill = trat)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = NULL)  # Quitar el título del eje x


bp8 <- ggplot(data_sel, aes(x=trat, y= AQ_LIF, fill = trat)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = NULL)  # Quitar el título del eje x


bp9 <- ggplot(data_sel, aes(x=trat, y= AQ_MAPK1, fill = trat)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = NULL)  # Quitar el título del eje x


bp10 <- ggplot(data_sel, aes(x=trat, y= AQ_NOS2, fill = trat)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = NULL)  # Quitar el título del eje x


bp11 <- ggplot(data_sel, aes(x=trat, y= AQ_IFNG, fill = trat)) +
  geom_boxplot() +
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(x = NULL)  # Quitar el título del eje x


bp12 <- ggplot(data_sel, aes(x=trat, y= AQ_PDCD1, fill = trat)) +
  geom_boxplot() +
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(x = NULL)  # Quitar el título del eje x


bp13 <- ggplot(data_sel, aes(x=trat, y= AQ_PPARG, fill = trat)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = NULL)  # Quitar el título del eje x


bp14 <- ggplot(data_sel, aes(x=trat, y= AQ_TGFB1, fill = trat)) +
  geom_boxplot() +
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(x = NULL)  # Quitar el título del eje x


bp15 <- ggplot(data_sel, aes(x=trat, y= AQ_TNF, fill = trat)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = NULL)  # Quitar el título del eje x


#Ahora con patchwork
library(patchwork)

# Usar patchwork para combinar los gráficos en un solo panel
combined_plot <- bp1 + bp2 + bp3 + bp4 + bp5 + bp6 + bp7 +
  bp8 + bp9 + bp10 + bp11 + bp12 + bp13 + bp14 + bp15 + plot_layout(3)  # Ajusta ncol para controlar el número de columnas

# Mostrar el gráfico combinado
combined_plot

#Otra forma de hacerlo más optima:
# Lista de genes de interés
genes <- c("AQ_ALOX5", "AQ_CD274", "AQ_CHKA", "AQ_CSF2", "AQ_FOXO3", 
           "AQ_IL6", "AQ_LDHA", "AQ_LIF", "AQ_MAPK1", "AQ_NOS2", 
           "AQ_IFNG", "AQ_PDCD1", "AQ_PPARG", "AQ_TGFB1", "AQ_TNF")

# Seleccionamos las columnas de interés: tratamiento y genes
data_sel <- data[, c("trat", genes)]

# Crear una lista vacía para almacenar los gráficos individuales
plot_list <- list()

# Generar un boxplot para cada gen y agregarlo a la lista
for (gen in genes) {
  p <- ggplot(data_sel, aes(x = trat, y = .data[[gen]], fill = trat)) +
    geom_boxplot() + 
    theme_minimal() +
    theme(legend.position = "none") +
    labs(x = NULL)  # Quitar el título del eje x
  
  # Agregar cada gráfico a la lista
  plot_list[[gen]] <- p
}

# Usar patchwork para combinar los gráficos en un solo panel
combined_plot <- wrap_plots(plot_list, ncol = 3)  # Ajusta ncol para controlar el número de columnas

# Mostrar el gráfico combinado
combined_plot

-----------------
#Ejercicio 2: 
# Lista de parámetros bioquímicos
  parametros <- c("glucosa", "leucocitos", "linfocitos", "neutrofilos", 
                  "chol", "hdl", "hierro", "igA", "igE", "igG", "igN", 
                  "ldl", "pcr", "transferrina", "trigliceridos", "cpk")



# Crear una lista para almacenar los gráficos
plot_list <- list()

# Generar un histograma con densidad para cada parámetro
for (parametro in parametros) {
  p <- ggplot(data, aes(x = .data[[parametro]])) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
    geom_density(color = "red", size = 1) +
    labs(y = NULL)
    theme_minimal()
  
  # Agregar cada gráfico a la lista
  plot_list[[parametro]] <- p
}

# Usar patchwork para mostrar los gráficos juntos
combined_plot <- wrap_plots(plot_list, ncol = 4)
combined_plot

-----
#Exercici 3: 
install.packages("dplyr")
library(dplyr)

# Seleccionar únicamente las columnas de expresión génica (AQ) del dataframe
data_AQ_pa <- data %>%
  select(starts_with("AQ"))  # Filtrar columnas que inicien con "AQ"

# Verificar que solo se seleccionaron las columnas correctas
head(data_AQ_pa)

# Cargar librerías necesarias
install.packages('pheatmap')
library(pheatmap)
library(dplyr)

# Fijar semilla para reproducibilidad
set.seed(1995)

# Escalar los datos de expresión génica para que sean comparables
data_scaled <- scale(data_AQ_pa)


# Crear el heatmap
pheatmap(
  data_scaled,
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  clustering_method = "ward.D2",
  scale = "row",
  color = colorRampPalette(c("darkgreen","white","purple"))(50),
  main = "Mapa de calor de la expresión génica (AQ)",
  fontsize_row = 6,
  fontsize_col = 6,
  cutree_rows = 4,
  cutree_cols = 4
)

#-- prueva grupos

# Escalar los datos de expresión génica para que sean comparables
data_scaled <- scale(data_AQ_pa)

# Realizar el clustering en filas y columnas y asignar los grupos
row_clusters <- cutree(hclust(dist(data_scaled), method = "ward.D2"), k = 4)  # 4 grupos de filas
col_clusters <- cutree(hclust(dist(t(data_scaled)), method = "ward.D2"), k = 4)  # 4 grupos de columnas

# Crear anotaciones de filas y columnas para los grupos
annotation_row <- data.frame(Grupo = factor(row_clusters))
annotation_col <- data.frame(Grupo = factor(col_clusters))

# Crear el heatmap con las anotaciones
heatmap <- pheatmap(
  data_scaled,
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  clustering_method = "ward.D2",
  scale = "row",
  color = colorRampPalette(c("darkgreen", "white", "purple"))(50),
  main = "Mapa de calor de la expresión génica (AQ)",
  fontsize_row = 6,
  fontsize_col = 6,
  cutree_rows = 4,
  cutree_cols = 4,
  annotation_row = annotation_row,  # Agregar anotación de grupos de filas
  annotation_col = annotation_col   # Agregar anotación de grupos de columnas
)

# Mostrar el heatmap
heatmap

