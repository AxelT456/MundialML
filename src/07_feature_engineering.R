# ==============================================================================
# PROYECTO: Análisis Predictivo - Copa Mundial de la FIFA
# FASE 2.1: Feature Engineering (Target Encoding Interno sin Data Leakage)
# ==============================================================================

library(caret)
library(gbm)
library(randomForest)
library(dplyr)
library(tidyr)
library(doParallel)

print("Iniciando Ingeniería de Características Avanzada...")

# 1. Cargar datos
df_partidos <- read.csv("data/processed/02_cleaned_partidos.csv")
df_partidos$is_knockout <- as.factor(df_partidos$is_knockout)

# Aseguramos el orden cronológico para no espiar el futuro
df_partidos <- df_partidos %>% arrange(match_id)

# ==============================================================================
# 2. LA MAGIA: TARGET ENCODING (Poder Ofensivo Histórico)
# ==============================================================================
print("Calculando el Poder Ofensivo Histórico de cada selección (Sin Fuga de Datos)...")

# Creamos un historial largo combinando locales y visitantes
historial <- bind_rows(
  df_partidos %>% select(match_id, team = home_team, goals = home_goals),
  df_partidos %>% select(match_id, team = away_team, goals = away_goals)
) %>%
  arrange(match_id) %>%
  group_by(team) %>%
  mutate(
    # cummean: saca el promedio histórico
    # lag: lo retrasa un partido para NO incluir los goles del partido actual (Blindaje)
    # default 1.3: Si es su primer partido en la historia, le damos el promedio global del fútbol
    hist_ataque = lag(cummean(goals), default = 1.3) 
  ) %>%
  ungroup()

# Unimos estos cálculos matemáticos de regreso a nuestro dataset original
df_partidos <- df_partidos %>%
  left_join(historial %>% select(match_id, team, home_ataque = hist_ataque),
            by = c("match_id", "home_team" = "team")) %>%
  left_join(historial %>% select(match_id, team, away_ataque = hist_ataque),
            by = c("match_id", "away_team" = "team")) %>%
  
  # CREAMOS NUESTRA SÚPER VARIABLE:
  mutate(poder_ofensivo_esperado = home_ataque + away_ataque)

# Imprimimos una muestra para ver cómo funciona
print("Muestra de cómo R convirtió los nombres en poder matemático:")
print(head(df_partidos %>% select(home_team, away_team, poder_ofensivo_esperado), 3))

# ==============================================================================
# 3. PREPARACIÓN PARA EL MODELADO
# ==============================================================================
# Ahora le pasamos 3 predictores a la máquina en lugar de 2!
datos_modelo <- df_partidos %>% 
  select(total_goals, year, is_knockout, poder_ofensivo_esperado)

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
# 4. ENTRENAMIENTO CON LA NUEVA SÚPER VARIABLE
# ==============================================================================
print("Entrenando modelos con 3 variables (Año + Táctica + Calidad del Equipo)...")

set.seed(2026)
modelo_lm <- train(total_goals ~ ., data = datos_train, method = "lm", trControl = control_entrenamiento)

set.seed(2026)
modelo_knn <- train(total_goals ~ ., data = datos_train, method = "knn", 
                    preProcess = c("center", "scale"), tuneLength = 5, trControl = control_entrenamiento)

set.seed(2026)
# Le damos un poco más de libertad a Random Forest ahora que hay 3 variables
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
# 5. COMPARACIÓN DE RESULTADOS
# ==============================================================================
print("Evaluación completada. ¿Rompimos el techo del 6% de varianza?")

resultados_cv <- resamples(list(Lineal = modelo_lm, KNN = modelo_knn, RandomF = modelo_rf, GBM = modelo_gbm))

print(summary(resultados_cv))

# Ver cuál variable pesó más para Random Forest
print("Importancia de Variables según Random Forest:")
print(varImp(modelo_rf))