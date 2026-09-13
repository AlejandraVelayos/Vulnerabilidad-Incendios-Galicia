# Evaluación Integrada de la Vulnerabilidad frente a Incendios Forestales en Galicia: Aplicacion del indice ReSVI bajo el Enfoque de Capitales Integrado (ICA).

Trabajo de Fin de Máster — Máster Universitario en Tecnologías de la Información Geográfica (Universidad de Alcalá).

---

## Resumen del Proyecto
Este repositorio contiene el flujo metodológico, consultas espaciales y scripts desarrollados para evaluar la vulnerabilidad del territorio gallego frente a incendios forestales sobre una malla regular de 1 × 1 km. 

El trabajo adapta el índice **ReSVI (Relative Social Vulnerability Index)** mediante el enfoque de **Capitales Integrados (ICA)**, combinando cuatro dimensiones ponderadas al 25 %:
* **Capital Social:** Exposición y sensibilidad poblacional mediante modelo dasimétrico a partir de microdatos censales (INE) y Catastro.
* **Capital Económico:** Exposición de infraestructuras críticas (red ferroviaria, tendidos eléctricos).
* **Capital Natural:** Susceptibilidad del combustible vegetal mitigada por el valor de conservación ecológica (Índice LEV).
* **Capital de Prevención:** Cobertura de infraestructuras defensivas, puntos de agua, tiempos de respuesta operativa de extinción, cortafuegos y red viaria.

---

## Trabajo Técnico Realizado
1. **Modelado dasimétrico:** Desagregación espacial de población residente asignada a edificaciones residenciales catastrales en RStudio.
2. **Álgebra de mapas y normalización:** Normalización min-max de variables y cálculo de subíndices provinciales en QGIS y base de datos espacial.
3. **Validación empírica:** Contraste espacial del ReSVI frente al registro histórico de incendios (2000–2023) y Zonas de Alto Riesgo (ZAR/PLADIGA).

---

## Principales Conclusiones
* **Asimetría territorial:** En A Coruña y Pontevedra la vulnerabilidad está dominada por la exposición social y económica (eje atlántico denso). En Ourense y Lugo, el índice está traccionado por el severo déficit preventivo y defensivo.
* **La paradoja del riesgo:** Las áreas con mayor recurrencia histórica de incendios y superficie quemada en el interior coinciden con niveles de ReSVI medios/bajos debido al despoblamiento rural y abandono agroforestal.

---

## Estructura del Código
* `sql/`: Cálculo de los cuatro subíndices, métricas provinciales y diagrama de flujo metodológico (Mermaid).
* `scripts/`: Scripts en R para crear el modelo dasimétrico, análisis estadístico, normalización y generación de gráficos.

---

## Software
* QGIS 3.x
* R / RStudio
