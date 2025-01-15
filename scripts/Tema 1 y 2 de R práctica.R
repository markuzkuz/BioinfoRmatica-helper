rm(list=ls())


# ---- crear variable ---- 
variable <- "gen"
print(variable)


# ---- crear tipos de vectores ---- 
vector <- c(56,12,87,18,876)
print(vector)

vector_chr <- as.character(vector)
print(vector_chr) # aparece entre comillas

vector_fac <- as.factor(vector)
print(vector_fac) # aparece los levels

vector_num <- as.numeric(vector_chr)
print(vector_num) # vuelve a aprecer sin comillas


# ---- estructuras de vectores
vector <- seq(15, 30, by=1)
print(vector)
sort(vector)
rev(vector)

vector[1]
vector[-1]
vector[1:2]


# ---- matriz ----
matrix <- matrix(c(1,2,3,10,20,30), nrow=2, byrow = TRUE)
matrix

print(matrix[1,1]) # si quiero el valor 1
print(matrix[2,3]) # si quiero el valor 30


# ---- extraer una matriz (valores 1, 2, 10, 20) ----
matrix2 <- matrix[1:2, 1:2]
matrix2


# ---- extraer la 1 fila y todas las columnas ----
matrix3 <- matrix[1,]
matrix3

# ---- extraer la 2 columna y todas las filas ----
matrix4 <- matrix[,2]
matrix4



# ---- DataFrame ----
df <- data.frame(
  paciente_id = c(1, 2, 3, 4, 5, 6, 7, 8),
  nombre = c("Ana", "Luis", "Maria", "Carlos", "Juan", "Sofia", "Diego", "Laura"),
  gen = c("TP53", "BRCA1", "TP53", "BRCA2", "BRCA1", "TP53", "BRCA2", "TP53"),
  expresion_g = c(12.5, 8.0, 15.0, 6.5, 10.0, 11.0, 7.5, 14.0),
  diagnostico = c("Cáncer", "Sano", "Cáncer", "Sano", "Cáncer", "Cáncer", "Sano", "Cáncer"),
  fecha_observacion = as.Date(c("2022-01-10", "2022-02-20", "2022-01-15", "2022-03-10", "2022-04-05", "2022-01-12", "2022-03-15", "2022-05-01"))
)


nrow(df)
ncol(df)
dim(df)


# ///////////////////////////////////////////////////////////////////////////////////////////////////////
# ///////////////////////////////////////////////////////////////////////////////////////////////////////
# ///////////////////////////////////////////////////////////////////////////////////////////////////////

# ---- LIBRERÍAS ----
# Cargar tidyverse
#install.packages("tidyverse") # si no lo tenéis descargado
library(tidyverse)


## ---- dplyr ----
library(dplyr) # por defecto si descargáis tidyverse se descarga ya, sino lo instaláis

# Filtrar pacientes con diagnóstico de "Cáncer"
pacientes_con_cancer <- df %>%
  filter(diagnostico == "Cáncer")

pacientes_con_cancer


# Ordenar por expresión génica en orden descendente
pacientes_ordenados <- df %>%
  arrange(desc(expresion_g))

pacientes_ordenados


# Seleccionar solo las columnas nombre, gen y diagnóstico
pacientes_seleccionados <- df %>%
  select(nombre, gen, diagnostico)

pacientes_seleccionados

# Añadir una nueva columna que indique si la expresión es alta
df_mutada <- df %>%
  mutate(expresion_alta = if_else(expresion_g > 10, "Sí", "No"))

df_mutada



# ---- tidyr ----

library(tidyr) # por defecto si descargáis tidyverse se descarga ya, sino lo instaláis

# Separar la fecha en año, mes y día
df_separada <- df %>%
  separate(fecha_observacion, into = c("año", "mes", "día"), sep = "-")

df_separada


# Unir nombre y gen en una nueva columna
df_unida <- df %>%
  unite("nombre_gen", nombre, gen, sep = " - ")

df_unida


# ---- purrr ----

library(purrr) # por defecto si descargáis tidyverse se descarga ya, sino lo instaláis

# Calcular peso a partir de la expresión génica
df <- df %>%
  mutate(peso_kg = map_dbl(expresion_g, ~ .x * 5)) # .x hace mencion a cada elemento del vector/lista

df


# Clasificar la expresión génica
# Contar categorías de expresión
df <- df %>%
  mutate(clasificacion = map_int(expresion_g, ~ {
    if (.x > 12) {
      return(3)  # Alta
    } else if (.x >= 10) {
      return(2)  # Media
    } else {
      return(1)  # Baja
    }
  }))

df



# ///////////////////////////////////////////////////////////////////////////////////////////////////////
# ///////////////////////////////////////////////////////////////////////////////////////////////////////
# ///////////////////////////////////////////////////////////////////////////////////////////////////////

rm(list=ls())

# Ejemplo con el dataframe de health.dta

install.packages("haven") # si no lo tenéis descargado
library(haven)

df <- read_dta("./data/health_stata11.dta")


# ---- gtsummary ----
install.packages("gtsummary") # por defecto si descargáis tidyverse se descarga ya, sino lo instaláis
library(gtsummary)
library(dplyr)


table(df$sex)
table(df$marital)
table(df$EDUC)
table(df$smk)
table(df$CVD)
table(df$death)
table(df$cannabis)
table(df$relig)

# Convertir variables a factores
df <- df %>%
  mutate(
    sex = factor(sex, levels = c(0, 1), labels = c("Male", "Female")),
    marital = factor(marital, levels = c(0, 1, 2), labels = c("Single", "Married", "Divorced/Separated")),
    EDUC = factor(EDUC, levels = 0:4, labels = c("Low", "Mid-Low", "Mid", "Mid-High", "High")),
    smk = factor(smk, levels = c(0, 1, 2, 3), labels = c("Never", "Current", "Former", "Missing")),
    CVD = factor(CVD, levels = c(0, 1), labels = c("No", "Yes")),
    death = factor(death, levels = c(0, 1), labels = c("Alive", "Deceased")),
    cannabis = factor(cannabis, levels = 0:4, labels = c("Never", "Sporadic", "Monthly", "Weekly", "Daily")),
    relig = factor(relig, levels = 0:4, labels = c("Never", "Sporadic", "Monthly", "Weekly", "Daily"))
  )

table(df$sex)
table(df$marital)
table(df$EDUC)
table(df$smk)
table(df$CVD)
table(df$death)
table(df$cannabis)
table(df$relig)


# Crear el resumen básico con gtsummary
summary_table <- df %>%
  select(sex, age, marital, EDUC, METS_h_wk, MedDiet, alcoholg, smk, weight0, height, CVD, death, SBP, DBP, cannabis, relig) %>%
  tbl_summary(
    by = sex, # Agrupando el resumen por sexo
    statistic = list(all_continuous() ~ "{mean} ({sd})", 
                     all_categorical() ~ "{n} ({p}%)"),
    missing = "no" # Ignorar datos faltantes en este ejemplo
  ) %>%
  add_p() %>%
  add_overall()

# Mostrar la tabla
summary_table




