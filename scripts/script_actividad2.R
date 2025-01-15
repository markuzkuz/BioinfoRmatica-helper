setwd('/Users/marc/Documents/Màster Bioinformàtica/Assignatures/Estadística y R/Actividad 2/Actividad 2_Casas_Sebastián_Marc')
getwd()

install.packages("gtsummary")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("nortest")
install.packages("car")
install.packages("flextable")

# Cargar librerías
library(tidyverse)
library(gtsummary)
library(nortest)
library(car)
library(flextable)


data <- read.csv("./data/expresion genes.csv")

# Inspeccionar los datos
str(data)
gtsummary(data)
head(data)
colSums(is.na(data))


# Seleccionar solo las columnas de los genes (empiezan con "AQ_")
genes <- data %>% select(starts_with("AQ_"))


# Crear tabla para almacenar resultados
normality_results <- data.frame(
  Variable = colnames(genes),
  Test = NA,
  P_value = NA,
  Interpretación = NA
)

# Evaluar normalidad para cada gen
for (i in 1:ncol(genes)) {
  # Seleccionar columna del gen
  gene <- genes[[i]]
  
  # Aplicar test de normalidad
  if (nrow(data) <= 50) {
    # Shapiro-Wilk
    p_val <- shapiro.test(gene)$p.value
    test_name <- "Shapiro-Wilk"
  } else {
    # Anderson-Darling
    p_val <- ad.test(gene)$p.value
    test_name <- "Anderson-Darling"
  }
  
  # Guardar resultados
  normality_results$Test[i] <- test_name
  normality_results$P_value[i] <- round(p_val, 3)
  normality_results$Interpretación[i] <- ifelse(p_val < 0.05, "No Normal", "Normal")
}

str(genes)
summary(genes)
normality_results


# Crear tabla horizontal en formato flextable
tabla1 <- normality_results %>%
  flextable() %>%
  set_header_labels(
    Variable = "Variable",
    Test = "Test utilizado",
    P_value = "Valor p",
    Interpretación = "Interpretación"
  ) %>%
  colformat_double(j = 3, digits = 3) %>%  # P-values con 3 decimales
  autofit() %>%                              # Ajuste automático del tamaño
  align(align = "center", part = "all") %>%  # Centrar texto
  add_header_lines("Tabla 1. Pruebas de Normalidad para la Expresión Génica") %>%
  add_footer_lines("Notas: Test utilizados: Shapiro-Wilk (SW) y Anderson-Darling (AD). Valores p ≤ 0.05 indican distribución no normal.")

tabla1


# Ejercicio 2: 

# Seleccionar variables bioquímicas, síntomas y sociodemográficas entre numéricas y categóricas:

# Variables cuantitativas:

# Seleccionarlas
cuantitativas <- data %>%
  select(edad, glucosa, leucocitos, linfocitos, neutrofilos, pcr, trigliceridos,
         chol, hdl, ldl, hierro, igA, igE, igG, igN, transferrina, cpk, calidad_fisica, calidad_mental, score_dieta)

# Comprobación de normalidad
normalidad_cuantitativas <- data.frame(
  Variable = colnames(cuantitativas),
  Test = NA,
  P_value = NA,
  Interpretación = NA
)

for (i in 1:ncol(cuantitativas)) {
  variable <- cuantitativas[[i]]
  
  # Test de normalidad
  if (nrow(data) <= 50) {
    p_val <- shapiro.test(variable)$p.value
    test_name <- "Shapiro-Wilk"
  } else {
    p_val <- ad.test(variable)$p.value
    test_name <- "Anderson-Darling"
  }
  
  # Guardar resultados
  normalidad_cuantitativas$Test[i] <- test_name
  normalidad_cuantitativas$P_value[i] <- formatC(p_val, format = "e", digits = 3)
  normalidad_cuantitativas$Interpretación[i] <- ifelse(p_val < 0.05, "No Normal", "Normal")
}

# Verificar normalidad
print(normalidad_cuantitativas)

# Estadísticas descriptivas
descriptivos_cuantitativas <- cuantitativas %>%
  summarise(across(
    everything(), 
    # Función para seleccionar cálculo según normalidad
    ~ if (normalidad_cuantitativas$Interpretación[which(normalidad_cuantitativas$Variable == cur_column())] == "Normal") {
      sprintf("%.1f (%.1f)", mean(.), sd(.))  # Media (SD) si es normal
    } else {
      sprintf("%.1f (%.1f - %.1f)", median(.), quantile(., 0.25), quantile(., 0.75)) # Mediana (RIQ) si la distribución no es normal
    }
  )) 

print(descriptivos_cuantitativas)

# Variables Categóricas:

# Seleccionarlas
categoricas <- data %>%
  select(sexo, exfumador, hta, dm, alergia, cardiopatia, ETE, neumopatia, hepatopatia,
         colelitiasis, utolitiasis, ITU, renal, neuropatia, corticoides, trat, tumor, extension,
         tos, disnea, expect, secrecion, dolor_garg, escalofrios, fiebre, diarrea, nauseas,
         vomitos, cefalea, mareo, cansancio, anosmia, disgueusia, dolor_hueso, dolor_abdo, perd_ape)

# Crear tablas de frecuencias absolutas y relativas
descriptivos_categoricas <- map(categoricas, ~ {
  tabla <- table(.)
  prop <- prop.table(tabla)
  list(tabla = tabla, prop = prop)
})

# Mostrar frecuencias y proporciones
for (var in names(descriptivos_categoricas)) {
  cat("\nVariable:", var, "\n")
  print(descriptivos_categoricas[[var]]$tabla) # Frecuencia absoluta
  print(descriptivos_categoricas[[var]]$prop)  # Proporciones
}


#Tabla 2: 

# Seleccionar variables de interés
data_genes <- data %>%
  select(trat, tumor, starts_with("AQ_"))

# Crear tabla descriptiva con estratificación por tratamiento y tumor
tabla2 <- data_genes %>%
  tbl_strata(
    strata = trat,  # Nivel 1: Tratamiento (tratA y tratB)
    .tbl_fun = ~ .x %>%
      tbl_summary(
        by = tumor,  # Nivel 2: Tipo de tumor (CCR, CM, CP)
        statistic = all_continuous() ~ "{median} ({p25}, {p75})", # Mediana e IQR
        digits = all_continuous() ~ function(x) format(x, digits = 2, scientific = TRUE) # Notación científica
      ) %>%
      add_p(
        test = all_continuous() ~ "kruskal.test",                 # Test no paramétrico Kruskal-Wallis
        pvalue_fun = ~style_pvalue(.x, digits = 3)                # p-value con 3 decimales
      )
  ) %>%
  modify_header(label = "**Variable**") %>%                       # Añadir encabezado para variables
  modify_caption("**Tabla 2. Descriptivos según tratamiento y tipo de tumor**") # Añadir título

# Mostrar tabla en R
tabla2

# Tabla3: Descriptivos en función de la edad:
# Crear variable categórica para edad
data_genes <- data %>%
  mutate(edad_cat = ifelse(edad < median(edad, na.rm = TRUE), "< Mediana", "≥ Mediana")) %>%
  select(edad_cat, starts_with("AQ_"))

tabla3 <- data_genes %>%
  tbl_summary(
    by = edad_cat,
    statistic = all_continuous() ~ "{median} ({p25}, {p75})",
    digits = all_continuous() ~ function(x) format(x, digits = 2, scientific = TRUE)
  ) %>%
  add_p(
    test = all_continuous() ~ "kruskal.test", # Kruskal-Wallis
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  modify_header(label = "**Variable**") %>%
  modify_caption("**Tabla 3. Descriptivos según la edad**")

# Mostrar tabla
tabla3



