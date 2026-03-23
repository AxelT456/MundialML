# ==============================================================================
# PROYECTO: Análisis Predictivo - Copa Mundial de la FIFA
# FASE 2: Limpieza de Datos (Metodología SFCA)
# ARCHIVO: src/02_cleaning.R
# ==============================================================================

print("Iniciando Fase de Limpieza (SFCA)...")

# 1. Cargar el snapshot del ETL anterior
ruta_entrada <- "data/raw/world_cup_last_50_years.csv"
df <- read.csv(ruta_entrada, stringsAsFactors = FALSE)

# ------------------------------------------------------------------------------
# 2. FIX (Corregir)
# ------------------------------------------------------------------------------
# Verificar si hay valores NA (nulos) perdidos 
total_nulos <- sum(is.na(df))
print(paste("Total de valores nulos encontrados:", total_nulos))

# ------------------------------------------------------------------------------
# 3. CONVERT (Convertir)
# ------------------------------------------------------------------------------
print("Convirtiendo tipos de datos a estructuras matemáticas...")

# Convertir la columna 'date' a un formato de Fecha nativo de R
df$date <- as.Date(df$date, format = "%Y-%m-%d")

# Convertir texto a Factores (Categorías estadísticas para Machine Learning)
df$stage <- as.factor(df$stage)
df$home_team <- as.factor(df$home_team)
df$away_team <- as.factor(df$away_team)
df$winner <- as.factor(df$winner)

# ------------------------------------------------------------------------------
# 4. ADAPT (Adaptar / Ingeniería de Características)
# ------------------------------------------------------------------------------
print("Aplicando Ingeniería de Características (Feature Engineering)...")

# Creación de la Variable Objetivo (Target) para la Regresión Lineal
df$total_goals <- df$home_goals + df$away_goals

# Simplificación de Categorías 
# Reducimos las múltiples fases a un factor binario (1 = Eliminatoria, 0 = Fase de Grupos)
df$is_knockout <- ifelse(grepl("Group", df$stage), 0, 1)
df$is_knockout <- as.factor(df$is_knockout)

# ------------------------------------------------------------------------------
# 5. GUARDAR DATOS LIMPIOS (Carga al Data Mart)
# ------------------------------------------------------------------------------
# Crear el directorio si no existe para evitar errores
if(!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

ruta_salida <- "data/processed/02_cleaned_partidos.csv"
write.csv(df, ruta_salida, row.names = FALSE)

print("Datos limpios y transformados guardados con éxito en data/processed/")
print("=== NUEVA ESTRUCTURA LISTA PARA EL MODELO ===")
str(df)