# ==============================================================================
# PROYECTO: Análisis Predictivo - Copa Mundial de la FIFA
# FASE 3: Análisis Exploratorio de Datos (EDA) Completo
# ARCHIVO: src/03_eda.R
# ==============================================================================

# Cargar todas las librerías necesarias
library(ggplot2)
library(corrplot)
library(factoextra)
library(GGally)
library(dplyr) # Nueva librería para manipular datos (Auditoría)

print("Iniciando Análisis Exploratorio (EDA)...")

# 1. Cargar los datos limpios de la fase anterior
df_partidos <- read.csv("data/processed/02_cleaned_partidos.csv")

# Asegurarnos de que las variables categóricas sean factores
df_partidos$is_knockout <- as.factor(df_partidos$is_knockout)
df_partidos$stage <- as.factor(df_partidos$stage)

# Crear directorio para guardar las imágenes si no existe
if(!dir.exists("outputs/figures")) dir.create("outputs/figures", recursive = TRUE)

# ==============================================================================
# FASE 0: AUDITORÍA DE DATOS (DATA PROFILING)
# ==============================================================================
print("Generando gráficas de Auditoría Univariada (Data Profiling)...")

# 0.1 Auditoría de 'stage' (Justifica la Ley de Miller)
png("outputs/figures/00a_univariado_stage.png", width = 800, height = 600, res = 120)
p_stage <- ggplot(df_partidos, aes(x = reorder(stage, stage, function(x)-length(x)), fill = stage)) +
  geom_bar(color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución de Fases del Torneo (Original)",
       x = "Fase del Torneo (Stage)", y = "Cantidad de Partidos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
print(p_stage)
dev.off()

# 0.2 Auditoría de 'home_team' (Justifica Alta Cardinalidad)
top_equipos <- df_partidos %>% count(home_team) %>% top_n(15, n)
png("outputs/figures/00b_univariado_equipos.png", width = 800, height = 600, res = 120)
p_equipos <- ggplot(top_equipos, aes(x = reorder(home_team, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", alpha = 0.8) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 15 Equipos Locales (Alta Cardinalidad)",
       x = "Equipo (home_team)", y = "Frecuencia")
print(p_equipos)
dev.off()

# 0.3 Auditoría de 'year' (Continuidad Temporal)
png("outputs/figures/00c_univariado_year.png", width = 800, height = 600, res = 120)
p_year <- ggplot(df_partidos, aes(x = year)) +
  geom_histogram(binwidth = 4, fill = "darkgreen", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución Temporal de los Partidos",
       x = "Año del Mundial", y = "Cantidad de Partidos")
print(p_year)
dev.off()

# 0.4 Auditoría de 'is_knockout' (Post-Transformación)
png("outputs/figures/00d_univariado_knockout.png", width = 800, height = 600, res = 120)
p_knockout <- ggplot(df_partidos, aes(x = as.factor(is_knockout), fill = as.factor(is_knockout))) +
  geom_bar(color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("lightgreen", "salmon")) +
  theme_minimal() +
  labs(title = "Distribución Binaria Post-Transformación",
       x = "is_knockout (0 = Grupos, 1 = Eliminatoria)", y = "Frecuencia") +
  theme(legend.position = "none")
print(p_knockout)
dev.off()

# ==============================================================================
# FASE 1: ANÁLISIS UNIVARIADO Y BIVARIADO (Variables Principales)
# ==============================================================================
print("Generando Gráficas Principales...")

# 1.1 Distribución de la Variable Objetivo
p_objetivo <- ggplot(df_partidos, aes(x = total_goals)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1, alpha = 0.5) +
  geom_vline(aes(xintercept = mean(total_goals)), color = "blue", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(total_goals)), color = "green", linetype = "dashed", linewidth = 1) +
  theme_minimal() +
  labs(title = "Análisis Univariado: Distribución de Goles Totales",
       subtitle = "Línea Azul: Media | Línea Verde: Mediana | Curva Roja: Densidad de Probabilidad",
       x = "Goles Totales (Local + Visitante)", y = "Densidad")
ggsave("outputs/figures/01_univariado_objetivo.png", plot = p_objetivo, width = 8, height = 5)

# 1.2 Detección Matemática de Outliers
print("=== ANÁLISIS MATEMÁTICO DE VALORES ATÍPICOS (OUTLIERS) ===")
outliers_goles <- boxplot.stats(df_partidos$total_goals)$out
print(paste("Cantidad de partidos con marcadores atípicos detectados:", length(outliers_goles)))

# 1.3 Boxplot por Fases
p2 <- ggplot(df_partidos, aes(x = is_knockout, y = total_goals, fill = is_knockout)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("0" = "lightgreen", "1" = "salmon"), 
                    labels = c("Fase de Grupos", "Eliminatorias")) +
  theme_minimal() +
  labs(title = "Volumen Ofensivo: Grupos vs Eliminatorias",
       x = "Tipo de Partido (0 = Grupos, 1 = Eliminatoria)", y = "Goles Totales") +
  theme(legend.position = "none")
ggsave("outputs/figures/02_boxplot_fases.png", plot = p2, width = 8, height = 5)

# 1.4 Análisis Bivariado (Dispersión y Tendencia Definitiva)
p_bivariado <- ggplot(df_partidos, aes(x = year, y = total_goals)) +
  geom_jitter(alpha = 0.5, width = 0.5, height = 0.2, color = "darkgray") + 
  geom_smooth(method = "lm", color = "darkred", fill = "salmon", se = TRUE) +
  theme_minimal() +
  labs(title = "Análisis Bivariado: Impacto del Tiempo en el Volumen Ofensivo",
       subtitle = "Tendencia de Regresión Lineal con Intervalos de Confianza (95%)",
       x = "Año del Mundial", y = "Goles Totales")
print(p_bivariado) # Forzar visualización en RStudio
ggsave("outputs/figures/03_bivariado_tendencia.png", plot = p_bivariado, width = 8, height = 5)

# ==============================================================================
# FASE 2: ANÁLISIS MULTIVARIADO Y AUDITORÍA DE MODELO
# ==============================================================================
print("Generando PCA y Correlaciones...")

# 2.1 Matriz de Correlación
datos_numericos <- df_partidos[, c("year", "home_goals", "away_goals", "total_goals")]
matriz_cor <- cor(datos_numericos)
png("outputs/figures/04_matriz_correlacion.png", width = 800, height = 800, res = 150)
corrplot(matriz_cor, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Matriz de Correlación de Pearson", mar=c(0,0,2,0))
dev.off()

# 2.2 Gráfico de Violín y Outliers
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

# 2.3 Análisis de Componentes Principales (PCA)
datos_pca <- df_partidos[, c("year", "home_goals", "away_goals")]
pca_res <- prcomp(datos_pca, scale. = TRUE)
p6 <- fviz_pca_biplot(pca_res, geom.ind = "point", habillage = df_partidos$is_knockout, 
                      palette = c("#00AFBB", "#FC4E07"), addEllipses = TRUE,
                      col.var = "black", repel = TRUE) +
  labs(title = "Biplot PCA: Dispersión de Variables Ofensivas e Históricas")
ggsave("outputs/figures/06_pca_biplot.png", plot = p6, width = 8, height = 6)

# 2.4 Matriz de Dispersión Cruzada (Pairplot)
datos_pairplot <- df_partidos[, c("year", "home_goals", "away_goals", "total_goals", "is_knockout")]
p7 <- ggpairs(datos_pairplot, 
              aes(color = is_knockout, alpha = 0.6),
              title = "Matriz de Dispersión Cruzada (Pairplot) por Fase del Torneo",
              lower = list(continuous = wrap("points", alpha = 0.3, size=0.5)),
              diag = list(continuous = wrap("densityDiag", alpha=0.5))) +
  theme_minimal()
ggsave("outputs/figures/07_pairplot_multivariado.png", plot = p7, width = 10, height = 8)

# 2.5 Violines Individuales (Home y Away)
p8 <- ggplot(df_partidos, aes(x = is_knockout, y = home_goals, fill = is_knockout)) +
  geom_violin(alpha = 0.5, trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", color = "black") + 
  scale_fill_manual(values = c("0" = "lightblue", "1" = "lightcoral")) +
  theme_minimal() + labs(title = "Densidad de Goles Locales por Fase", x = "Fase", y = "Goles Local") +
  theme(legend.position = "none")
ggsave("outputs/figures/08_violin_home.png", plot = p8, width = 8, height = 5)

p9 <- ggplot(df_partidos, aes(x = is_knockout, y = away_goals, fill = is_knockout)) +
  geom_violin(alpha = 0.5, trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", color = "black") + 
  scale_fill_manual(values = c("0" = "lightgreen", "1" = "orange")) +
  theme_minimal() + labs(title = "Densidad de Goles Visitantes por Fase", x = "Fase", y = "Goles Visitante") +
  theme(legend.position = "none")
ggsave("outputs/figures/09_violin_away.png", plot = p9, width = 8, height = 5)

print("¡EDA 100% completado con Auditoría Univariada! Revisa la carpeta outputs/figures/")