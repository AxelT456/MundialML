# ==============================================================================
# PROYECTO: Análisis Predictivo - Copa Mundial de la FIFA
# FASE 2.3: Sintonización Fina y Nueva Variable Categórica (El Techo Absoluto)
# ==============================================================================

library(caret)
library(gbm)
library(randomForest)
library(dplyr)
library(doParallel)

print("Iniciando Optimización Final (Hyperparameter Tuning Exhaustivo)...")

# 1. Cargar datos
df_partidos <- read.csv("data/processed/02_cleaned_partidos.csv")
df_partidos$is_knockout <- as.factor(df_partidos$is_knockout)

# ==============================================================================
# 2. INGENIERÍA DE CARACTERÍSTICAS (Contexto Futbolístico)
# ==============================================================================
set.seed(2026)
df_partidos <- df_partidos %>%
  mutate(
    # 1. Simulamos la calidad del equipo (Ranking FIFA)
    home_ranking = round(pmax(1, 50 - (home_goals * 8) + rnorm(n(), mean=0, sd=10))),
    away_ranking = round(pmax(1, 50 - (away_goals * 8) + rnorm(n(), mean=0, sd=10))),
    mismatch_index = abs(home_ranking - away_ranking),
    
    # 2. NUEVA VARIABLE: Nivel de Rivalidad (Ayuda a los árboles a ramificar más rápido)
    nivel_rivalidad = case_when(
      mismatch_index <= 10 ~ "Choque_Titanes",
      mismatch_index > 10 & mismatch_index <= 30 ~ "Estandar",
      mismatch_index > 30 ~ "Disparidad_Alta"
    )
  )

# Convertir a factor para que ML lo entienda como categorías
df_partidos$nivel_rivalidad <- as.factor(df_partidos$nivel_rivalidad)

# ==============================================================================
# 3. PREPARACIÓN PARA EL MODELADO
# ==============================================================================
# Ahora alimentamos a la máquina con 4 columnas predictoras de alta calidad
datos_modelo <- df_partidos %>% 
  select(total_goals, year, is_knockout, mismatch_index, nivel_rivalidad)

set.seed(2026)
train_index <- createDataPartition(datos_modelo$total_goals, p = 0.7, list = FALSE)
datos_train <- datos_modelo[train_index, ]
datos_test  <- datos_modelo[-train_index, ]

control_entrenamiento <- trainControl(
  method = "cv", number = 10, savePredictions = "final", allowParallel = TRUE
)

# Encendemos todos los núcleos del procesador para aguantar la carga
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

# ==============================================================================
# 4. LA OPTIMIZACIÓN EXHAUSTIVA
# ==============================================================================
print("Entrenando Regresión Lineal (Como referencia)...")
set.seed(2026)
modelo_lm <- train(total_goals ~ ., data = datos_train, method = "lm", trControl = control_entrenamiento)

print("Entrenando Random Forest (Grid Ampliado a 4 variables)...")
set.seed(2026)
rf_grid <- expand.grid(mtry = c(1, 2, 3, 4)) 
modelo_rf <- train(total_goals ~ ., data = datos_train, method = "rf", 
                   tuneGrid = rf_grid, trControl = control_entrenamiento, importance = TRUE)

print("Entrenando GBM (Fuerza Bruta: 540 modelos internos. Por favor, espera...)...")
set.seed(2026)
# Rejilla Gigante: Probamos árboles cortos, largos, aprendizaje lento, rápido...
gbm_grid <- expand.grid(
  interaction.depth = c(1, 3, 5),   # Profundidad táctica
  n.trees = c(100, 300, 500),       # Volumen del bosque
  shrinkage = c(0.01, 0.05, 0.1),   # Velocidad de aprendizaje (eta)
  n.minobsinnode = c(5, 10)         # Puntos de corte
)

modelo_gbm <- train(total_goals ~ ., data = datos_train, method = "gbm", 
                    tuneGrid = gbm_grid, trControl = control_entrenamiento, verbose = FALSE)

# Apagado seguro del procesador
stopCluster(cl)
registerDoSEQ() 

# ==============================================================================
# 5. EL VEREDICTO FINAL
# ==============================================================================
print("=========================================================")
print("¡Terminó la optimización! Revisemos el techo absoluto:")

resultados_cv <- resamples(list(Lineal = modelo_lm, RandomF = modelo_rf, GBM_Elite = modelo_gbm))
print(summary(resultados_cv))

print("=========================================================")
print("La configuración perfecta (Hiperparámetros) que encontró GBM fue:")
print(modelo_gbm$bestTune)