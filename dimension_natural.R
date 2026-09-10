# ==============================================================================
# SCRIPT 04: NORMALIZACIÓN E INTEGRACIÓN DE LA DIMENSIÓN NATURAL (ReSVI)
# ==============================================================================

library(sf)
library(tidyverse)

# ==============================================================================
# 1. CARGA DE DATOS (Malla con estadísticas zonales calculadas en SIG)
# ==============================================================================
malla_nat <- st_read("C:/Users/MASTER_TIG/Desktop/tfm/capas/dimension_natural/malla_natural.gpkg")

# ==============================================================================
# 2. FUNCIÓN DE NORMALIZACIÓN METODOLOGÍA ReSVI
# ==============================================================================
# Algoritmo de transformación: Control de NAs, Z-score y escalado Min-Max (0-1)
normalizar_natural_resvi <- function(columna) {
  # Imputación de valores nulos a 0
  columna <- if_else(is.na(columna), 0, columna)
  
  # Control de varianza nula
  if(sd(columna, na.rm = TRUE) == 0) {
    return(rep(0, length(columna)))
  }
  
  # Cálculo de Z-score y reescalado
  z_score <- (columna - mean(columna, na.rm = TRUE)) / sd(columna, na.rm = TRUE)
  min_z <- min(z_score, na.rm = TRUE)
  max_z <- max(z_score, na.rm = TRUE)
  
  return((z_score - min_z) / (max_z - min_z))
}

# ==============================================================================
# 3. NORMALIZACIÓN DE VARIABLES BIOCLIMÁTICAS Y DE COMBUSTIBLE
# ==============================================================================
malla_nat_normalizada <- malla_nat %>%
  mutate(
    v_combustible = normalizar_natural_resvi(pesos_combus),
    v_resiliencia = normalizar_natural_resvi(resimean),
    v_valor_ecol  = normalizar_natural_resvi(levmean)
  )

# ==============================================================================
# 4. CÁLCULO DEL SUBÍNDICE DE VULNERABILIDAD NATURAL
# ==============================================================================
malla_nat_normalizada <- malla_nat_normalizada %>%
  rowwise() %>%
  mutate(
    ReSVI_Natural = mean(c(v_combustible, v_resiliencia, v_valor_ecol), na.rm = TRUE)
  ) %>%
  ungroup()

# Exportación del GeoPackage final para su representación cartográfica
st_write(malla_nat_normalizada, "C:/Users/MASTER_TIG/Desktop/tfm/capas/malla_naturaleza_resvi_final.gpkg", delete_dsn = TRUE)