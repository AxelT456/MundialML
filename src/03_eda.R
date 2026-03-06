# ==============================================================================
# PROYECTO: Análisis Predictivo - Copa Mundial de la FIFA
# FASE 3: Análisis Exploratorio de Datos (EDA)
# ARCHIVO: src/03_eda.R
# ==============================================================================

# Cargar librería para gráficos profesionales
library(ggplot2)

print("Iniciando Análisis Exploratorio (EDA)...")

# 1. Cargar los datos limpios
df <- read.csv("data/processed/02_cleaned_partidos.csv")
df$is_knockout <- as.factor(df$is_knockout)

# Crear directorio para guardar las imágenes si no existe
if(!dir.exists("outputs/figures")) dir.create("outputs/figures", recursive = TRUE)

# ------------------------------------------------------------------------------
# DIMENSIÓN 1: OBSERVA (Distribución Unidimensional)
# Pregunta: ¿Cuál es el comportamiento normal de los goles en un partido?
# ------------------------------------------------------------------------------
print("Generando Histograma de Goles...")
p1 <- ggplot(df, aes(x = total_goals)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución del Total de Goles por Partido (1974 - 2022)",
       x = "Goles Totales (Local + Visitante)",
       y = "Frecuencia (Cantidad de Partidos)")

ggsave("outputs/figures/01_distribucion_goles.png", plot = p1, width = 8, height = 5)

# ------------------------------------------------------------------------------
# DIMENSIÓN 2: RELACIONES (Análisis Bidimensional)
# Pregunta: ¿Se anotan menos goles en las fases eliminatorias (vida o muerte)?
# ------------------------------------------------------------------------------
print("Generando Boxplot (Fase de Grupos vs Eliminatorias)...")
p2 <- ggplot(df, aes(x = is_knockout, y = total_goals, fill = is_knockout)) +
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
# DIMENSIÓN 3: CORRELACIÓN (Tendencia Histórica)
# Pregunta: ¿El fútbol se ha vuelto más defensivo con el paso de los años?
# ------------------------------------------------------------------------------
print("Generando Gráfico de Tendencia Histórica...")
# Agrupamos el promedio de goles por año
promedios_por_ano <- aggregate(total_goals ~ year, data = df, FUN = mean)

p3 <- ggplot(promedios_por_ano, aes(x = year, y = total_goals)) +
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) + # Línea de tendencia lineal
  theme_minimal() +
  labs(title = "Evolución Histórica del Promedio de Goles por Partido",
       x = "Año del Mundial",
       y = "Promedio de Goles")

ggsave("outputs/figures/03_tendencia_historica.png", plot = p3, width = 8, height = 5)

# ------------------------------------------------------------------------------
# DIMENSIÓN 4: SIGNIFICACIÓN Y MATRIZ DE CORRELACIÓN
# ------------------------------------------------------------------------------
# Seleccionamos solo las variables numéricas para la matriz
print("Generando Matriz de Correlación...")
library(reshape2) # Asegúrate de tener instalado este paquete (install.packages("reshape2"))

datos_numericos <- df[, c("year", "home_goals", "away_goals", "total_goals")]
matriz_cor <- cor(datos_numericos)

# Transformar para ggplot
melted_cor <- melt(matriz_cor)

p4 <- ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlación\nde Pearson") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  labs(title = "Matriz de Correlación de Variables Numéricas", x = "", y = "")

ggsave("outputs/figures/04_matriz_correlacion.png", plot = p4, width = 6, height = 6)
print("¡Matriz de correlación generada!")

print("¡EDA completado! Revisa la carpeta outputs/figures/ para ver tus gráficas.")