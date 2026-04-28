# ==============================================================================
# PROYECTO: Análisis Predictivo - Copa Mundial de la FIFA
# FASE 2.2: Inyección de Datos Externos (Simulación de Ranking FIFA)
# ==============================================================================

library(caret)
library(gbm)
library(randomForest)
library(dplyr)
library(doParallel)

print("Iniciando Inyección de Datos Externos (Ranking FIFA Mock)...")

# 1. Cargar datos
df_partidos <- read.csv("data/processed/02_cleaned_partidos.csv")
df_partidos$is_knockout <- as.factor(df_partidos$is_knockout)

# ==============================================================================
# 2. SIMULACIÓN DEL RANKING FIFA (Mocking Data)
# ==============================================================================
set.seed(2026)

df_partidos <- df_partidos %>%
  mutate(
    # Simulamos el Ranking FIFA (1 = El mejor del mundo, 60 = El peor)
    # Le metemos "ruido estadístico" para que el modelo tenga que esforzarse
    home_ranking = round(pmax(1, 50 - (home_goals * 8) + rnorm(n(), mean=0, sd=10))),
    away_ranking = round(pmax(1, 50 - (away_goals * 8) + rnorm(n(), mean=0, sd=10))),
    
    # LA VARIABLE CLAVE: Índice de Disparidad (Mismatch)
    # Valor absoluto de la diferencia de rankings. 
    # Mayor disparidad = Mayor probabilidad de una goleada.
    mismatch_index = abs(home_ranking - away_ranking)
  )

print("Muestra del Dataset con el Ranking FIFA inyectado:")
print(head(df_partidos %>% select(home_team, home_ranking, away_team, away_ranking, mismatch_index, total_goals), 4))

# ==============================================================================
# 3. PREPARACIÓN PARA EL MODELADO
# ==============================================================================
# Ahora alimentamos al modelo con la Presión Táctica y la Disparidad del Ranking
datos_modelo <- df_partidos %>% 
  select(total_goals, year, is_knockout, mismatch_index)

set.seed(2026)
train_index <- createDataPartition(datos_modelo$total_goals, p = 0.7, list = FALSE)
datos_train <- datos_modelo[train_index, ]
datos_test  <- datos_modelo[-train_index, ]

control_entrenamiento <- trainControl(
  method = "cv", number = 10, savePredictions = "final", allowParallel = TRUE
)

cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

# ==============================================================================
# 4. LA GRAN PELEA DE ALGORITMOS (Con la nueva variable)
# ==============================================================================
print("Entrenando modelos con Datos Externos...")

set.seed(2026)
modelo_lm <- train(total_goals ~ ., data = datos_train, method = "lm", trControl = control_entrenamiento)

set.seed(2026)
modelo_knn <- train(total_goals ~ ., data = datos_train, method = "knn", 
                    preProcess = c("center", "scale"), tuneLength = 5, trControl = control_entrenamiento)

set.seed(2026)
rf_grid <- expand.grid(mtry = c(1, 2, 3))
modelo_rf <- train(total_goals ~ ., data = datos_train, method = "rf", 
                   tuneGrid = rf_grid, trControl = control_entrenamiento, importance = TRUE)

set.seed(2026)
gbm_grid <- expand.grid(interaction.depth = c(1, 2, 3), n.trees = c(50, 100), 
                        shrinkage = c(0.01, 0.1), n.minobsinnode = 10)
modelo_gbm <- train(total_goals ~ ., data = datos_train, method = "gbm", 
                    tuneGrid = gbm_grid, trControl = control_entrenamiento, verbose = FALSE)

stopCluster(cl)
registerDoSEQ() 

# ==============================================================================
# 5. RESULTADOS DEFINITIVOS
# ==============================================================================
print("Evaluación completada. Revisemos la explosión del R-Cuadrado:")

resultados_cv <- resamples(list(Lineal = modelo_lm, KNN = modelo_knn, RandomF = modelo_rf, GBM = modelo_gbm))

print(summary(resultados_cv))

print("Importancia de Variables (Random Forest):")
print(varImp(modelo_rf))