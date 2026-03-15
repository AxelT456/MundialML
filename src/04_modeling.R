# ==============================================================================
# PROYECTO: Análisis Predictivo - Copa Mundial de la FIFA
# FASE 4: Modelado y Evaluación (Regresión Lineal Múltiple)
# ARCHIVO: src/04_modeling.R
# ==============================================================================

print("Iniciando Fase de Modelado Predictivo...")

# 1. Cargar los datos limpios
df <- read.csv("data/processed/02_cleaned_partidos.csv")
df$is_knockout <- as.factor(df$is_knockout)

# 2. Partición de Datos (70% Entrenamiento, 30% Prueba)
# Usamos set.seed para que los resultados sean reproducibles en tu presentación
set.seed(2026) 
print("Particionando datos en Entrenamiento (Train) y Prueba (Test)...")

indice_entrenamiento <- sample(1:nrow(df), size = 0.7 * nrow(df))
datos_entrenamiento <- df[indice_entrenamiento, ]
datos_prueba <- df[-indice_entrenamiento, ]

print(paste("Partidos para entrenar:", nrow(datos_entrenamiento)))
print(paste("Partidos para probar el modelo:", nrow(datos_prueba)))

# 3. Entrenamiento del Modelo (Regresión Lineal Múltiple)
print("Entrenando algoritmo de Regresión Lineal...")
# La fórmula: total_goals en función (~) del año y la fase eliminatoria
modelo_regresion <- lm(total_goals ~ year + is_knockout, data = datos_entrenamiento)

# Imprimir el resumen estadístico para el reporte
print("=== RESUMEN ESTADÍSTICO DEL MODELO ===")
print(summary(modelo_regresion))

# 4. Evaluación del Rendimiento (Testing)
print("Evaluando el modelo con datos de prueba (Test)...")
# Usamos el modelo entrenado para predecir los goles del 30% de datos aislados
predicciones <- predict(modelo_regresion, newdata = datos_prueba)

# Calcular métricas de error (RMSE y MAE)
error_absoluto_medio <- mean(abs(predicciones - datos_prueba$total_goals))
error_cuadratico_medio <- sqrt(mean((predicciones - datos_prueba$total_goals)^2))

print("=== MÉTRICAS DE RENDIMIENTO FINAL ===")
print(paste("Error Absoluto Medio (MAE):", round(error_absoluto_medio, 4), "goles"))
print(paste("Error Cuadrático Medio (RMSE):", round(error_cuadratico_medio, 4), "goles"))

# Guardar el modelo entrenado por si se requiere consumir desde una API después
if(!dir.exists("outputs/models")) dir.create("outputs/models", recursive = TRUE)
saveRDS(modelo_regresion, "outputs/models/regresion_lineal_goles.rds")
print("Modelo guardado exitosamente en outputs/models/")