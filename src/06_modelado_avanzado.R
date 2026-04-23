# ==============================================================================
# PROYECTO: Análisis Predictivo - Copa Mundial de la FIFA
# FASE 2: Modelado Avanzado y Sintonización de Hiperparámetros
# ==============================================================================

library(caret)
library(randomForest)
library(gbm)         # <-- CAMBIO CLAVE: Usamos gbm como dicta la documentación
library(dplyr)
library(doParallel)

print("Iniciando Pipeline de Modelado Avanzado...")

# 1. Cargar los datos limpios
df_partidos <- read.csv("data/processed/02_cleaned_partidos.csv")

# Dejamos is_knockout como factor (gbm, rf y knn lo procesan perfecto así)
df_partidos$is_knockout <- as.factor(df_partidos$is_knockout)

datos_modelo <- df_partidos %>% 
  select(total_goals, year, is_knockout)

# 3. Partición de Datos (70/30) 
set.seed(2026)
train_index <- createDataPartition(datos_modelo$total_goals, p = 0.7, list = FALSE)
datos_train <- datos_modelo[train_index, ]
datos_test  <- datos_modelo[-train_index, ]

# 4. Configurar la Validación Cruzada (10-Fold CV)
control_entrenamiento <- trainControl(
  method = "cv", 
  number = 10,
  savePredictions = "final",
  allowParallel = TRUE
)

# Activar procesamiento paralelo
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

# ==============================================================================
# ENTRENAMIENTO DE MODELOS
# ==============================================================================

print("Entrenando 1/4: Regresión Lineal (Baseline)...")
set.seed(2026)
modelo_lm <- train(
  total_goals ~ ., 
  data = datos_train, 
  method = "lm", 
  trControl = control_entrenamiento
)

print("Entrenando 2/4: K-Nearest Neighbors...")
set.seed(2026)
modelo_knn <- train(
  total_goals ~ ., 
  data = datos_train, 
  method = "knn", 
  preProcess = c("center", "scale"), 
  tuneLength = 5, 
  trControl = control_entrenamiento
)

print("Entrenando 3/4: Random Forest (Bagging)...")
set.seed(2026)
rf_grid <- expand.grid(mtry = c(1, 2))

modelo_rf <- train(
  total_goals ~ ., 
  data = datos_train, 
  method = "rf", 
  tuneGrid = rf_grid, 
  trControl = control_entrenamiento,
  importance = TRUE 
)

print("Entrenando 4/4: Gradient Boosting Machine (GBM)...")
set.seed(2026)
# Rejilla manual segura para GBM de 2 variables
gbm_grid <- expand.grid(
  interaction.depth = c(1, 2, 3), 
  n.trees = c(50, 100), 
  shrinkage = c(0.01, 0.1),
  n.minobsinnode = 10
)

modelo_gbm <- train(
  total_goals ~ ., 
  data = datos_train, 
  method = "gbm", 
  tuneGrid = gbm_grid, 
  trControl = control_entrenamiento,
  verbose = FALSE  # Silencia los logs internos para no ensuciar la consola
)

# APAGADO SEGURO DEL CLUSTER (Evita los warnings de conexiones)
stopCluster(cl)
registerDoSEQ() 

# ==============================================================================
# COMPARACIÓN DE RESULTADOS
# ==============================================================================
print("Evaluación completada. Comparando modelos...")

resultados_cv <- resamples(list(
  Lineal   = modelo_lm,
  KNN      = modelo_knn,
  RandomF  = modelo_rf,
  GBM      = modelo_gbm
))

# Ver el resumen numérico
print(summary(resultados_cv))

# Guardar la gráfica comparativa de MAE
p_comparativa <- bwplot(resultados_cv, metric = "MAE", main = "Comparación de MAE (Menor es mejor)")
print(p_comparativa)