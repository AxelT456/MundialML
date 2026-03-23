# ==============================================================================
# PROYECTO: Análisis Predictivo - Copa Mundial de la FIFA
# FASE 1: ETL (Extract, Transform, Load)
# ARCHIVO: src/01_etl.R
# ==============================================================================

# 1. EXTRACCIÓN (Extract)
# Cargar los datos crudos desde la carpeta correspondiente
print("Iniciando extracción de datos...")
rutas_partidos <- "data/raw/world_cup_last_50_years.csv"
rutas_torneos <- "data/raw/FIFA_World_Cup_Results_All_Time_20260210_051012.csv"

df_partidos <- read.csv(rutas_partidos, stringsAsFactors = FALSE)
df_torneos <- read.csv(rutas_torneos, stringsAsFactors = FALSE)

# 2. TRANSFORMACIÓN INICIAL (Transform)
# Verificación básica de consistencia y dimensiones
print(paste("Partidos cargados:", nrow(df_partidos), "registros."))
print(paste("Torneos cargados:", nrow(df_torneos), "registros."))

# 3. CARGA (Load)
# Guardar un snapshot inicial en la carpeta de procesados para la Fase 2 (Limpieza)
ruta_salida <- "data/processed/01_etl_partidos_snapshot.csv"
write.csv(df_partidos, ruta_salida, row.names = FALSE)
print("Fase ETL completada exitosamente. Datos guardados en data/processed/")

# Exploración rápida para consola
str(df_partidos)
summary(df_partidos)


# 1. Cargar los datasets satelitales
torneos <- read.csv("data/raw/FIFA_World_Cup_Results_All_Time_20260210_051012.csv", stringsAsFactors = FALSE)
campeones <- read.csv("data/raw/FIFA_World_Cup_Champions_20260210_051012.csv", stringsAsFactors = FALSE)
anfitriones <- read.csv("data/raw/FIFA_World_Cup_Hosts_20260210_051012.csv", stringsAsFactors = FALSE)

# 2. Imprimir las dimensiones (Filas y Columnas) para tu justificación
print("=== DIMENSIONES DE LOS DATASETS SATELITALES ===")
print(paste("Torneos Históricos:", nrow(torneos), "filas y", ncol(torneos), "columnas"))
print(paste("Tabla de Campeones:", nrow(campeones), "filas y", ncol(campeones), "columnas"))
print(paste("Tabla de Anfitriones:", nrow(anfitriones), "filas y", ncol(anfitriones), "columnas"))

# 3. Ver qué variables contiene el dataset histórico (el más grande de los tres)
print("=== ESTRUCTURA DEL DATASET DE TORNEOS ===")
str(torneos)

