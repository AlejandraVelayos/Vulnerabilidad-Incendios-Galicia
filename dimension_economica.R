# ==============================================================================
# SCRIPT 05: OPERATIVIZACIÓN Y CÁLCULO DE LA DIMENSIÓN ECONÓMICA (ReSVI)
# ==============================================================================

library(sf)
library(tidyverse)

# ==============================================================================
# 1. CARGA DE DATOS ESPACIALES
# ==============================================================================
carpeta_capas <- "C:/Users/MASTER_TIG/Desktop/tfm/capas/dimension_economica/"
malla_econ <- st_read(paste0(carpeta_capas, "malla_economica.gpkg"))

# ==============================================================================
# 2. FUNCIÓN DE TRANSFORMACIÓN BINARIA (ReSVI)
# ==============================================================================
# Ecuaciones adaptadas a presencia/ausencia (Ref: Documento D1.4, pp. 61-62)
# Transforma identificadores numéricos en 1 (presencia) y valores nulos/cero en 0 (ausencia)
limpiar_binario_resvi <- function(columna) {
  valor_0_1 <- if_else(!is.na(columna) & columna > 0, 1, 0)
  return(valor_0_1)
}

# ==============================================================================
# 3. NORMALIZACIÓN DE VARIABLES ESTRATÉGICAS
# ==============================================================================
# Aplicación a las cuatro infraestructuras críticas evaluadas
malla_econ_normalizada <- malla_econ %>%
  mutate(
    v_torres       = limpiar_binario_resvi(torres),
    v_lineas       = limpiar_binario_resvi(lineas),
    v_convencional = limpiar_binario_resvi(convencional),
    v_tren_ave     = limpiar_binario_resvi(tren_ave)
  )

# ==============================================================================
# 4. CÁLCULO DEL SUBÍNDICE DE VULNERABILIDAD ECONÓMICA
# ==============================================================================
# Cuantas más infraestructuras converjan espacialmente en una celda, mayor será el índice
malla_econ_normalizada <- malla_econ_normalizada %>%
  rowwise() %>%
  mutate(
    ReSVI_Economico = mean(c(v_torres, v_lineas, v_convencional, v_tren_ave), na.rm = TRUE)
  ) %>%
  ungroup()

# ==============================================================================
# 5. EXPORTACIÓN DE RESULTADOS
# ==============================================================================
st_write(malla_econ_normalizada, paste0(carpeta_capas, "malla_economica_resvi_final.gpkg"), delete_dsn = TRUE)