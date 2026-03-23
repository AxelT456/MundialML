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

# ==============================================================================
# GRÁFICAS PARA LA SECCIÓN DE DATA MINING (MODELADO)
# ==============================================================================
library(ggplot2)

print("Generando gráficas de evaluación de modelos...")

# ------------------------------------------------------------------------------
# Gráfica A: Análisis de Residuos (Regresión Lineal)
# ------------------------------------------------------------------------------
# Extraemos las predicciones y los errores del modelo de entrenamiento
datos_residuales <- data.frame(
  Predicciones = predict(modelo_regresion),
  Residuos = residuals(modelo_regresion)
)

p_residuos <- ggplot(datos_residuales, aes(x = Predicciones, y = Residuos)) +
  geom_point(alpha = 0.5, color = "darkblue", size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "orange", linewidth = 1) +
  theme_minimal() +
  labs(title = "Análisis de Residuos (Regresión Lineal Múltiple)",
       subtitle = "Línea Roja: Cero Error | Puntos: Partidos Individuales",
       x = "Goles Predichos por el Modelo",
       y = "Error Residual (Real - Predicho)")

ggsave("outputs/figures/10_residuos_lineal.png", plot = p_residuos, width = 8, height = 5)

# ------------------------------------------------------------------------------
# Gráfica B: Optimización de Hiperparámetros (K-NN)
# ------------------------------------------------------------------------------
# El paquete caret se integra nativamente con ggplot para plotear el entrenamiento
p_knn <- ggplot(modelo_knn) +
  theme_minimal() +
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen", linewidth = 1) +
  labs(title = "Curva de Optimización de Hiperparámetros (K-NN)",
       subtitle = "Validación Cruzada (5-Fold Cross Validation)",
       x = "Número de Vecinos (K)",
       y = "Error Cuadrático Medio (RMSE)")

ggsave("outputs/figures/11_optimizacion_knn.png", plot = p_knn, width = 8, height = 5)

print("¡Gráficas de Data Mining generadas con éxito!")