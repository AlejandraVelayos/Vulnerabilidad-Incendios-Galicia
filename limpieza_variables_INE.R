# ==============================================================================
# SCRIPT 01: DEPURACIÓN Y NORMALIZACIÓN DE VARIABLES SOCIALES (INE)
# ==============================================================================

# 1. CARGA DE LIBRERÍAS
library(tidyverse)
library(readxl)

# ==============================================================================
# A. DEPURACIÓN DE DATOS DE POBLACIÓN TOTAL
# ==============================================================================
# Lectura y unificación de archivos provinciales
archivos_pob <- list.files("F:/tfm/capas/poblacion_seccion_censal/", pattern = "\\.xlsx$", full.names = TRUE)
poblacion_galicia <- archivos_pob %>%
  map_dfr(~ read_excel(.x, col_types = "text")) %>%
  filter(!is.na(cusec)) %>%
  mutate(Total = ifelse(is.na(Total), "0", Total))

# ==============================================================================
# B. CLASIFICACIÓN DE GRUPOS DE EDAD (Infantil y Mayores)
# ==============================================================================
archivos_edad <- list.files("D:/tfm/capas/Grupos_Edad/", pattern = "\\.csv$", full.names = TRUE)
grupos_edad <- archivos_edad %>%
  map_dfr(~ read.csv2(.x, fileEncoding = "UTF-8", colClasses = c(cusec = "character"), check.names = FALSE)) %>%
  rename_all(tolower) %>%
  drop_na() %>%
  filter(cusec != "" & edad != "" & total != "") %>%
  mutate(
    total_num = as.numeric(total),
    grupo = case_when(
      edad %in% c("De 0 a 4 años", "De 5 a 9 años", "De 10 a 14 años") ~ "poblacion_infantil",
      edad %in% c("De 65 a 69 años", "De 70 a 74 años", "De 75 a 79 años", 
                  "De 80 a 84 años", "De 85 a 89 años", "De 90 a 94 años", 
                  "De 95 a 99 años", "100 y más años") ~ "poblacion_mayores",
      TRUE ~ "otros"
    )
  ) %>%
  group_by(cusec, grupo) %>%
  summarise(total_habitantes = sum(total_num, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = grupo, values_from = total_habitantes, values_fill = 0) %>%
  select(cusec, poblacion_infantil, poblacion_mayores)

# ==============================================================================
# C. NORMALIZACIÓN DE NIVEL FORMATIVO (Estudios básicos)
# ==============================================================================
archivos_estudios <- list.files("E:/tfm/capas/renta_persona/", pattern = "\\.xlsx?$", full.names = TRUE)
estudios_limpios <- archivos_estudios %>%
  map_dfr(~ read_excel(.x, col_types = "text", na = c("NA", "null", ""))) %>%
  filter(!is.na(Secciones), Secciones != "") %>%
  mutate(
    Total = as.numeric(gsub("\\.", "", Total)),
    Total = ifelse(is.na(Total), 0, Total)
  ) %>%
  filter(!is.na(Total)) %>%
  pivot_wider(names_from = `Nivel de formación alcanzado`, values_from = Total, values_fill = 0) %>%
  rename(cusec = Secciones, pob_total_estudios = Total, pob_primaria_o_menos = `Educación primaria e inferior`) %>%
  mutate(
    porcen_estudios_bajos = ifelse(pob_total_estudios > 0, (pob_primaria_o_menos / pob_total_estudios) * 100, 0)
  )

# ==============================================================================
# D. TRATAMIENTO DE LA VARIABLE RENTA MEDIA
# ==============================================================================
archivos_renta <- list.files("E:/tfm/capas/renta_persona/", pattern = "a_coruña|lugo|ourense|pontevedra", full.names = TRUE)
renta_limpia <- archivos_renta %>%
  map_dfr(function(archivo) {
    tabla <- read.csv2(archivo, colClasses = "character")[, 1:2]
    colnames(tabla) <- c("cusec", "renta")
    return(tabla)
  }) %>%
  filter(cusec != "", !is.na(cusec)) %>%
  mutate(
    renta = as.numeric(gsub("\\.", "", renta)),
    renta = ifelse(is.na(renta), 0, renta)
  )