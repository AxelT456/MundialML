# ==============================================================================
# PROYECTO: Análisis Predictivo - Copa Mundial de la FIFA
# FASE 3: Análisis Exploratorio de Datos (EDA) Completo
# ARCHIVO: src/03_eda.R
# ==============================================================================

library(ggplot2)
library(corrplot)
library(factoextra)

print("Iniciando Análisis Exploratorio (EDA)...")

# 1. Cargar los datos limpios de la fase anterior
# Asegúrate de que la ruta coincida con donde guardaste el CSV en el paso 2
df_partidos <- read.csv("data/processed/02_cleaned_partidos.csv")

# Asegurarnos de que las variables categóricas sean factores para poder graficarlas
df_partidos$is_knockout <- as.factor(df_partidos$is_knockout)
df_partidos$stage <- as.factor(df_partidos$stage)

# Crear directorio para guardar las imágenes si no existe
if(!dir.exists("outputs/figures")) dir.create("outputs/figures", recursive = TRUE)

# ------------------------------------------------------------------------------
# GRÁFICA 1: Histograma de Distribución
# ------------------------------------------------------------------------------
print("Generando 1/6: Histograma de Goles...")
p1 <- ggplot(df_partidos, aes(x = total_goals)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución del Total de Goles por Partido (1974 - 2022)",
       x = "Goles Totales (Local + Visitante)",
       y = "Frecuencia (Cantidad de Partidos)")

ggsave("outputs/figures/01_distribucion_goles.png", plot = p1, width = 8, height = 5)

# ------------------------------------------------------------------------------
# GRÁFICA 2: Boxplot por Fases
# ------------------------------------------------------------------------------
print("Generando 2/6: Boxplot de Fases...")
p2 <- ggplot(df_partidos, aes(x = is_knockout, y = total_goals, fill = is_knockout)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("0" = "lightgreen", "1" = "salmon"), 
                    labels = c("Fase de Grupos", "Eliminatorias")) +
  theme_minimal() +
  labs(title = "Volumen Ofensivo: Grupos vs Eliminatorias",
       x = "Tipo de Partido (0 = Grupos, 1 = Eliminatoria)",
       y = "Goles Totales") +
  theme(legend.position = "none")

ggsave("outputs/figures/02_boxplot_fases.png", plot = p2, width = 8, height = 5)

# ------------------------------------------------------------------------------
# GRÁFICA 3: Gráfico de Violín y Outliers
# ------------------------------------------------------------------------------
print("Generando 3/6: Gráfico de Violín y Outliers...")
p5 <- ggplot(df_partidos, aes(x = is_knockout, y = total_goals, fill = is_knockout)) +
  geom_violin(alpha = 0.5, trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", color = "black") + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "lightcoral"), 
                    labels = c("Grupos", "Eliminatorias")) +
  theme_minimal() +
  labs(title = "Densidad de Goles y Análisis de Outliers",
       x = "Tipo de Partido", y = "Goles Totales") +
  theme(legend.position = "none")

ggsave("outputs/figures/05_violin_outliers.png", plot = p5, width = 8, height = 5)

# ------------------------------------------------------------------------------
# GRÁFICA 4: Tendencia Histórica (Regresión Lineal Simple)
# ------------------------------------------------------------------------------
print("Generando 4/6: Tendencia Histórica...")
# Agrupamos por año para sacar el promedio
promedios_por_ano <- aggregate(total_goals ~ year, data = df_partidos, FUN = mean)

p3 <- ggplot(promedios_por_ano, aes(x = year, y = total_goals)) +
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) + 
  theme_minimal() +
  labs(title = "Evolución Histórica del Promedio de Goles por Partido",
       x = "Año del Mundial",
       y = "Promedio de Goles")

ggsave("outputs/figures/03_tendencia_historica.png", plot = p3, width = 8, height = 5)

# ------------------------------------------------------------------------------
# GRÁFICA 5: Análisis de Componentes Principales (PCA)
# ------------------------------------------------------------------------------
print("Generando 5/6: PCA Biplot...")
# Seleccionamos las columnas estrictamente numéricas para el PCA
datos_pca <- df_partidos[, c("year", "home_goals", "away_goals")]
pca_res <- prcomp(datos_pca, scale. = TRUE)

p6 <- fviz_pca_biplot(pca_res,
                      geom.ind = "point",
                      habillage = df_partidos$is_knockout, 
                      palette = c("#00AFBB", "#FC4E07"),
                      addEllipses = TRUE,
                      col.var = "black",
                      repel = TRUE) +
  labs(title = "Biplot PCA: Dispersión de Variables Ofensivas e Históricas")

ggsave("outputs/figures/06_pca_biplot.png", plot = p6, width = 8, height = 6)

# ------------------------------------------------------------------------------
# GRÁFICA 6: Matriz de Correlación
# ------------------------------------------------------------------------------
print("Generando 6/6: Matriz de Correlación...")
datos_numericos <- df_partidos[, c("year", "home_goals", "away_goals", "total_goals")]
matriz_cor <- cor(datos_numericos)

# Exportar con png() ya que corrplot no es compatible con ggsave()
png("outputs/figures/04_matriz_correlacion.png", width = 800, height = 800, res = 150)
corrplot(matriz_cor, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Matriz de Correlación de Pearson",
         mar=c(0,0,2,0))
dev.off()

print("¡EDA 100% completado! Revisa la carpeta outputs/figures/ para ver tus 6 gráficas.")

library(GGally)
# ------------------------------------------------------------------------------
# GRÁFICA 7: Matriz de Dispersión Multivariada (Pairplot)
# ------------------------------------------------------------------------------
print("Generando 7: Pairplot Multivariado (Esto puede tardar unos segundos)...")

# Seleccionamos todas las variables clave
datos_pairplot <- df_partidos[, c("year", "home_goals", "away_goals", "total_goals", "is_knockout")]

# Generamos la súper matriz
p7 <- ggpairs(datos_pairplot, 
              aes(color = is_knockout, alpha = 0.6),
              title = "Matriz de Dispersión Cruzada (Pairplot) por Fase del Torneo",
              lower = list(continuous = wrap("points", alpha = 0.3, size=0.5)),
              diag = list(continuous = wrap("densityDiag", alpha=0.5))) +
  theme_minimal()

ggsave("outputs/figures/07_pairplot_multivariado.png", plot = p7, width = 10, height = 8)

# ------------------------------------------------------------------------------
# GRÁFICAS 8 y 9: Violines Individuales (Local y Visitante)
# ------------------------------------------------------------------------------
print("Generando 8 y 9: Violines de Goles Locales y Visitantes...")

# Violín de Goles Locales
p8 <- ggplot(df_partidos, aes(x = is_knockout, y = home_goals, fill = is_knockout)) +
  geom_violin(alpha = 0.5, trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", color = "black") + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "lightcoral"), labels = c("Grupos", "Eliminatorias")) +
  theme_minimal() +
  labs(title = "Densidad de Goles Locales por Fase", x = "Fase del Torneo", y = "Goles del Local") +
  theme(legend.position = "none")

ggsave("outputs/figures/08_violin_home.png", plot = p8, width = 8, height = 5)

# Violín de Goles Visitantes
p9 <- ggplot(df_partidos, aes(x = is_knockout, y = away_goals, fill = is_knockout)) +
  geom_violin(alpha = 0.5, trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", color = "black") + 
  scale_fill_manual(values = c("0" = "lightgreen", "1" = "orange"), labels = c("Grupos", "Eliminatorias")) +
  theme_minimal() +
  labs(title = "Densidad de Goles Visitantes por Fase", x = "Fase del Torneo", y = "Goles del Visitante") +
  theme(legend.position = "none")

ggsave("outputs/figures/09_violin_away.png", plot = p9, width = 8, height = 5)

print("¡Nuevas gráficas generadas exitosamente!")

# ------------------------------------------------------------------------------
# 3.1 ANÁLISIS UNIVARIADO: VARIABLE OBJETIVO (CÓDIGO ACTUALIZADO)
# ------------------------------------------------------------------------------
print("Generando Análisis Univariado de la Variable Objetivo...")

p_objetivo <- ggplot(df_partidos, aes(x = total_goals)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1, alpha = 0.5) +
  geom_vline(aes(xintercept = mean(total_goals)), color = "blue", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(total_goals)), color = "green", linetype = "dashed", linewidth = 1) +
  theme_minimal() +
  labs(title = "Análisis Univariado: Distribución de Goles Totales",
       subtitle = "Línea Azul: Media | Línea Verde: Mediana | Curva Roja: Densidad de Probabilidad",
       x = "Goles Totales (Local + Visitante)",
       y = "Densidad")

ggsave("outputs/figures/01_univariado_objetivo.png", plot = p_objetivo, width = 8, height = 5)

# ------------------------------------------------------------------------------
# 3.2 DETECCIÓN MATEMÁTICA DE OUTLIERS
# ------------------------------------------------------------------------------
print("=== ANÁLISIS MATEMÁTICO DE VALORES ATÍPICOS (OUTLIERS) ===")
outliers_goles <- boxplot.stats(df_partidos$total_goals)$out
print(paste("Cantidad de partidos con marcadores atípicos detectados:", length(outliers_goles)))

# ------------------------------------------------------------------------------
# 3.3 ANÁLISIS BIVARIADO: PREDICTORA VS OBJETIVO (CÓDIGO ACTUALIZADO)
# ------------------------------------------------------------------------------
print("Generando Análisis Bivariado (Dispersión y Tendencia)...")

p_bivariado <- ggplot(df_partidos, aes(x = year, y = total_goals)) +
  geom_jitter(alpha = 0.3, width = 1, height = 0.2, color = "darkgray") + 
  # Se actualizó 'size' por 'linewidth'
  geom_smooth(method = "lm", color = "darkred", fill = "salmon", se = TRUE, linewidth = 1) +
  theme_minimal() +
  labs(title = "Análisis Bivariado: Impacto del Tiempo en el Volumen Ofensivo",
       subtitle = "Tendencia de Regresión Lineal con Intervalos de Confianza (95%)",
       x = "Año del Mundial",
       y = "Goles Totales")

ggsave("outputs/figures/03_bivariado_tendencia.png", plot = p_bivariado, width = 8, height = 5)


# ------------------------------------------------------------------------------
# FORZAR GRÁFICA 3.3: BIVARIADO (A prueba de fallos)
# ------------------------------------------------------------------------------
library(ggplot2)

print("Generando y mostrando Análisis Bivariado...")

p_bivariado <- ggplot(df_partidos, aes(x = year, y = total_goals)) +
  geom_jitter(alpha = 0.5, width = 0.5, height = 0.2, color = "darkgray") + 
  # Quitamos la fórmula manual para evitar errores, el aviso de R no afecta en nada
  geom_smooth(method = "lm", color = "darkred", fill = "salmon", se = TRUE) +
  theme_minimal() +
  labs(title = "Análisis Bivariado: Impacto del Tiempo en el Volumen Ofensivo",
       subtitle = "Tendencia de Regresión Lineal con Intervalos de Confianza (95%)",
       x = "Año del Mundial",
       y = "Goles Totales")

# 1. Esto forzará a RStudio a mostrarla en tu ventana inferior derecha (Plots)
print(p_bivariado)

# 2. Esto la guardará a la fuerza
ggsave("outputs/figures/03_bivariado_tendencia.png", plot = p_bivariado, width = 8, height = 5)

print("¡Si la viste en la pantalla, ya debe estar en tu carpeta!")