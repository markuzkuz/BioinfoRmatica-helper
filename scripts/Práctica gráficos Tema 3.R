rm(list=ls())

path <- "/Users/vic/Library/CloudStorage/GoogleDrive-vdelaopascual@gmail.com/Mi unidad/MU en Bioinformática (UNIR 2023)/Actividades (AAAA-MM-DD)/Actividad 1_junio2024 update"

setwd(path)

df <- read.csv("Dataset expresión genes.csv")
str(df, list.len = 500)

library(ggplot2)
library(dplyr)
library(pheatmap)





#### ---- Histogramas ---- ####
ggplot(df, aes(x = edad, fill = sexo)) +
  geom_histogram(color = "black", alpha = 0.4, position = "identity") +
  labs(title = "Distribución de la Edad", x = "Edad (años)", y = "Frecuencia") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



#### ---- Diagramas de barras ---- ####
ggplot(df, aes(x = sexo)) +
  geom_bar(fill = c("#FF9999", "#99CCFF"), color = "black") +
  labs(title = "Distribución por Sexo", x = "Sexo", y = "Conteo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



#### ---- Box plots ---- ####
ggplot(df, aes(x = trat, y = glucosa, fill = sexo)) +
  geom_boxplot(outlier.color = "black", outlier.size = 2, width = 0.7) +
  labs(title = "Distribución de Glucosa por Sexo", x = "Sexo", y = "Nivel de Glucosa (mg/dL)") +
  scale_fill_manual(values = c("lightpink", "lightblue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



#### ---- Scatter plots ---- ####
# Simple Scatter Plot
ggplot(df, aes(x = glucosa, y = chol)) +
  geom_point(color = "#4682B4", alpha = 0.7, size = 3) +
  labs(title = "Relación entre Glucosa y Colesterol", x = "Glucosa (mg/dL)", y = "Colesterol (mg/dL)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# Scatter Plot by Gender
ggplot(df, aes(x = glucosa, y = chol, color = sexo)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "Relación entre Glucosa y Colesterol por Sexo", x = "Glucosa (mg/dL)", y = "Colesterol (mg/dL)") +
  scale_color_manual(values = c("lightpink", "lightblue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  guides(color = guide_legend(title = "Sexo"))


#### ---- Pie charts ---- ####  no hacer
# Crear tabla de frecuencias
tumor_counts <- df %>%
  count(tumor) %>%
  mutate(perc = n / sum(n) * 100, label = paste0(round(perc, 1), "%"))

# Graficar
ggplot(tumor_counts, aes(x = "", y = perc, fill = tumor)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribución de Tipos de Tumor") +
  theme_void() +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))
  
  
#### ---- Heatmaps ---- ####
# Seleccionar solo las columnas de expresión génica
bioquimica <- df %>% select("glucosa":"neutrofilos", "chol":"cpk", starts_with("AQ_"))
corr_matrix <- cor(bioquimica, method = "spearman")
corr_matrix


dim(corr_matrix)
corr_matrix_sel <- corr_matrix[17:62, 1:16]
corr_matrix_sel


# Graficar heatmap
pheatmap(corr_matrix_sel, 
         main = "Heatmap",
         cluster_cols = FALSE,
         cluster_rows = FALSE,
         color = colorRampPalette(c("blue", "white", "red"))(100))

set.seed(1995)
pheatmap(corr_matrix_sel, 
         main = "Heatmap",
         cluster_cols = TRUE,
         cluster_rows = TRUE,
         color = colorRampPalette(c("blue", "white", "red"))(100))


# Estratificamos por tratamiento
bioquimica_a <- df %>% 
  filter(trat=="tratA") %>%
  select("glucosa":"neutrofilos", "chol":"cpk", starts_with("AQ_"))

corr_matrix_a <- cor(bioquimica_a, method = "spearman")
corr_matrix_a

dim(corr_matrix_a)
corr_matrix_sel_a <- corr_matrix_a[17:62, 1:16]
corr_matrix_sel_a


bioquimica_b <- df %>% 
  filter(trat=="tratB") %>%
  select("glucosa":"neutrofilos", "chol":"cpk", starts_with("AQ_"))

corr_matrix_b <- cor(bioquimica_b, method = "spearman")
corr_matrix_b

dim(corr_matrix_b)
corr_matrix_sel_b <- corr_matrix_b[17:62, 1:16]
corr_matrix_sel_b


library(ComplexHeatmap)

set.seed(1995)
Heatmap(corr_matrix_sel_a,
        row_km = 2,
        column_km = 2,
        row_names_gp = gpar(fontsize = 8),
        color = colorRampPalette(c("blue", "white", "red"))(100))


set.seed(1995)
Heatmap(corr_matrix_sel_b,
        row_km = 0,
        column_km = 2,
        row_names_gp = gpar(fontsize = 8),
        color = colorRampPalette(c("blue", "white", "red"))(100))







