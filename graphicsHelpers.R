library(reshape2)

ggplot(migration_urban_data, aes(x = MigracionPorcentaje)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Distribución del Porcentaje de Migración Neta",
       x = "% Migración", y = "Densidad")

ggplot(data, aes(x = Region, y = MigracionPorcentaje)) +
  geom_boxplot(fill = "cyan", alpha = 0.7) +
  coord_flip() +
  labs(title = "Porcentaje de Migración Neta por Región",
       x = "Región", y = "% Migración")

corr_matrix <- cor(migration_urban_data, use = "complete.obs")
corr_melt <- melt(corr_matrix)
ggplot(corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Mapa de Correlaciones", x = "", y = "")


# Filtrar los datos relevantes para el análisis: migración, desarrollo urbano y región
migration_urban_data <- data %>%
  select(Region, MigracionPorcentaje, CrecimientoPobUrbana_TasaAnual, PoblacionUrbana_Porcentaje) %>%
  filter(!is.na(MigracionPorcentaje) & !is.na(CrecimientoPobUrbana_TasaAnual) & !is.na(PoblacionUrbana_Porcentaje) & !is.na(Region))


ggplot(migration_urban_data, aes(x = MigracionPorcentaje, y = CrecimientoPobUrbana_TasaAnual, color = Region)) +
  geom_point(alpha = 0.6) +
  labs(title = "Migración vs Crecimiento Urbano por Región",
       x = "% Migración", y = "Tasa de Crecimiento de la Población Urbana") +
  theme(legend.position = "bottom")
