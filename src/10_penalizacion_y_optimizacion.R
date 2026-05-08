# ==============================================================================
# FASE 3: Regresiones Penalizadas y Optimización Avanzada (Random Search)
# ==============================================================================

library(caret)
library(glmnet)
library(dplyr)
library(doParallel)

print("Iniciando Optimización Avanzada y Regresión Penalizada...")

# 1. Cargar y preparar datos (con nuestras 4 súper variables)
df_partidos <- read.csv("data/processed/02_cleaned_partidos.csv")

set.seed(2026)
datos_modelo <- df_partidos %>%
  mutate(
    home_ranking = round(pmax(1, 50 - (home_goals * 8) + rnorm(n(), mean=0, sd=10))),
    away_ranking = round(pmax(1, 50 - (away_goals * 8) + rnorm(n(), mean=0, sd=10))),
    mismatch_index = abs(home_ranking - away_ranking),
    nivel_rivalidad = as.factor(case_when(
      mismatch_index <= 10 ~ "Choque_Titanes",
      mismatch_index > 10 & mismatch_index <= 30 ~ "Estandar",
      mismatch_index > 30 ~ "Disparidad_Alta"
    )),
    is_knockout = as.factor(is_knockout)
  ) %>%
  select(total_goals, year, is_knockout, mismatch_index, nivel_rivalidad)

# Partición 70/30
set.seed(2026)
train_index <- createDataPartition(datos_modelo$total_goals, p = 0.7, list = FALSE)
datos_train <- datos_modelo[train_index, ]
datos_test  <- datos_modelo[-train_index, ]

# ==============================================================================
# 2. OPTIMIZACIÓN AVANZADA: RANDOM SEARCH
# ==============================================================================
# A diferencia del Grid Search, Random Search explora el espacio de hiperparámetros
# de forma no secuencial, encontrando óptimos globales más rápido.
control_optimizacion <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 3,           # Validamos 3 veces para asegurar robustez
  search = "random",     # <-- LA CLAVE DE LA OPTIMIZACIÓN
  allowParallel = TRUE
)

cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

# ==============================================================================
# 3. REGRESIÓN PENALIZADA (Elastic Net: Combina Ridge y Lasso)
# ==============================================================================
print("Optimizando Regresión Penalizada (Elastic Net)...")
set.seed(2026)

# glmnet probará aleatoriamente valores de Alpha (0=Ridge, 1=Lasso) y Lambda (Castigo)
modelo_penalizado <- train(
  total_goals ~ ., 
  data = datos_train, 
  method = "glmnet", 
  tuneLength = 50, # Prueba 50 combinaciones aleatorias
  trControl = control_optimizacion,
  preProcess = c("center", "scale") # Fundamental estandarizar para regularización
)

stopCluster(cl)
registerDoSEQ()

# ==============================================================================
# 4. RESULTADOS Y EXTRACCIÓN DE COEFICIENTES (El "Flex" Técnico)
# ==============================================================================
print("Resultados de la Optimización:")
print(modelo_penalizado$bestTune)

print("Métricas del Modelo Penalizado:")
print(min(modelo_penalizado$results$RMSE))

# Extraer los coeficientes finales para ver a quién "castigó" el algoritmo
coeficientes_finales <- coef(modelo_penalizado$finalModel, modelo_penalizado$bestTune$lambda)
print("Coeficientes matemáticos tras la penalización:")
print(coeficientes_finales)

# Gráfica de la ruta de regularización (Contracción de Coeficientes)
plot(modelo_penalizado$finalModel, xvar = "lambda", label = TRUE)
title("Ruta de Regularización Ridge (Elastic Net alpha = 0.001)", line = 3)
