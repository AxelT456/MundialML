# Cargar librería para modelos avanzados y validación cruzada
library(caret)

print("Entrenando algoritmo K-NN para Regresión...")
set.seed(2026)

# Definir el control de entrenamiento (Cross-Validation de 5 pliegues)
control_entrenamiento <- trainControl(method = "cv", number = 5)

# Definir la rejilla de hiperparámetros (probar con 5, 10, 15, 20 y 25 vecinos)
rejilla_knn <- expand.grid(k = c(5, 10, 15, 20, 25))

# Entrenar el modelo K-NN
modelo_knn <- train(total_goals ~ year + is_knockout, 
                    data = datos_entrenamiento, 
                    method = "knn", 
                    trControl = control_entrenamiento, 
                    tuneGrid = rejilla_knn)

# Mostrar los resultados de la validación cruzada
print("=== RESUMEN DEL MODELO K-NN ===")
print(modelo_knn)

# Evaluar el modelo K-NN con los datos de Prueba (Test) aislados
predicciones_knn <- predict(modelo_knn, newdata = datos_prueba)
mae_knn <- mean(abs(predicciones_knn - datos_prueba$total_goals))
rmse_knn <- sqrt(mean((predicciones_knn - datos_prueba$total_goals)^2))

print("=== MÉTRICAS DE RENDIMIENTO K-NN ===")
print(paste("MAE K-NN:", round(mae_knn, 4), "goles"))
print(paste("RMSE K-NN:", round(rmse_knn, 4), "goles"))

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