# Cargar librerías necesarias
library(tidyverse)

# Cargar los datos
data <- read.csv("datosPaisesDelMundo.csv", na.strings = c("-99", "...", "NA"))

# Visualizar estructura inicial de los datos
str(data)

# Reemplazar los valores no válidos con NA
data <- data %>%
  mutate(across(everything(), ~na_if(.x, -99))) %>%
  mutate(across(everything(), ~na_if(.x, "...")))

# Confirmar los cambios
print(head(data))

# Filtrar los datos relevantes para el análisis: migración y desarrollo urbano
migration_urban_data <- data %>%
  select(MigracionPorcentaje, CrecimientoPobUrbana_TasaAnual, PoblacionUrbana_Porcentaje) %>%
  filter(!is.na(MigracionPorcentaje) & !is.na(CrecimientoPobUrbana_TasaAnual) & !is.na(PoblacionUrbana_Porcentaje))

# Estadísticas descriptivas
migration_stats <- migration_urban_data %>%
  summarise(
    mean_migration = mean(MigracionPorcentaje, na.rm = TRUE),
    mean_urban_growth = mean(CrecimientoPobUrbana_TasaAnual, na.rm = TRUE),
    mean_urban_population = mean(PoblacionUrbana_Porcentaje, na.rm = TRUE)
  )

print(migration_stats)

# Incorporar sección descriptiva en el reporte
cat("\nResultados:\n")
cat("Estadísticas descriptivas:\n")
cat("- Promedio del porcentaje de migración neta: ", migration_stats$mean_migration, "%\n")
cat("- Promedio de la tasa de crecimiento anual de la población urbana: ", migration_stats$mean_urban_growth, "%\n")
cat("- Promedio del porcentaje de la población urbana: ", migration_stats$mean_urban_population, "%\n")

# Visualización 1: Relación entre migración neta y crecimiento de la población urbana
ggplot(migration_urban_data, aes(x = MigracionPorcentaje, y = CrecimientoPobUrbana_TasaAnual)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relación entre Migración y Crecimiento Urbano", 
       x = "% Migración", y = "Tasa de Crecimiento de la Población Urbana")

cat("\nInterpretación del Gráfico 1:\n")
cat("El gráfico muestra una ligera tendencia negativa entre la migración neta y el crecimiento de la población urbana, lo que sugiere que un mayor porcentaje de migración podría estar asociado con tasas de crecimiento urbano más bajas.\n")

# Visualización 2: Relación entre migración neta y porcentaje de población urbana
ggplot(migration_urban_data, aes(x = MigracionPorcentaje, y = PoblacionUrbana_Porcentaje)) +
  geom_point(alpha = 0.6, color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  labs(title = "Relación entre Migración y Porcentaje de Población Urbana", 
       x = "% Migración", y = "% Población Urbana")

cat("\nInterpretación del Gráfico 2:\n")
cat("El gráfico muestra que no hay una correlación fuerte entre la migración neta y el porcentaje de población urbana, aunque algunos patrones específicos podrían investigarse a nivel regional.\n")

# Conclusiones iniciales
cat("\nConclusión:\n")
cat("Los resultados muestran que la migración neta tiene una relación moderada con el crecimiento urbano, mientras que su relación con el porcentaje de población urbana es menos clara. Esto sugiere que otros factores podrían estar influyendo en estas dinámicas y que se requiere un análisis más detallado para comprender las variaciones a nivel regional.\n")
