# Inasistencias y rendimiento académico - 3.º año de secundaria
# Parte cuantitativa de una investigación del Ministerio de Educación de Córdoba
# sobre asistencia escolar en el nivel secundario.
# Los datos son confidenciales y no se incluyen en el repositorio.

library(dplyr)
library(ggplot2)

# ---- Datos ----
muestra <- read.csv("data/muestra.csv", sep = ";") %>%
  rename(inasistencias = TOTAL, promedio = PROMEDIO_GRAL) %>%
  mutate(across(c(inasistencias, promedio), ~ as.numeric(gsub(",", ".", .x))))

# ---- Correlación y modelo lineal ----
cor(muestra$inasistencias, muestra$promedio, use = "complete.obs")

modelo <- lm(promedio ~ inasistencias, data = muestra)
summary(modelo)

r2 <- summary(modelo)$r.squared
p  <- summary(modelo)$coefficients[2, 4]
etiqueta <- paste0("R² = ", round(r2, 3), "\n",
                   ifelse(p < 0.001, "p < 0.001", paste0("p = ", round(p, 3))))

ggplot(muestra, aes(inasistencias, promedio)) +
  geom_point(alpha = 0.2, color = "#2171b5") +
  geom_smooth(method = "lm", color = "red") +
  annotate("text", x = Inf, y = Inf, label = etiqueta, hjust = 1.1, vjust = 1.5) +
  labs(title = "Inasistencias y promedio general",
       subtitle = "Estudiantes de 3.º año de secundaria",
       x = "Total de inasistencias", y = "Promedio general anual") +
  theme_minimal()

# ---- Aprobados y desaprobados ----
muestra <- muestra %>%
  mutate(
    condicion = if_else(promedio >= 7, "Aprobado (≥ 7)", "Desaprobado (< 7)"),
    grupo_faltas = if_else(inasistencias <= 25, "25 o menos", "26 o más"),
    rango_faltas = cut(inasistencias, breaks = c(0, 25, 50, 75, 100, Inf),
                       labels = c("0-25", "26-50", "51-75", "76-100", "Más de 100"),
                       include.lowest = TRUE)
  )

# Inasistencias promedio según condición
muestra %>%
  filter(!is.na(condicion)) %>%
  group_by(condicion) %>%
  summarise(media_inasistencias = mean(inasistencias, na.rm = TRUE))

# Porcentaje de aprobados según cantidad de faltas
resumen <- muestra %>%
  filter(!is.na(condicion), !is.na(grupo_faltas)) %>%
  count(grupo_faltas, condicion) %>%
  group_by(grupo_faltas) %>%
  mutate(porcentaje = n / sum(n) * 100)

ggplot(resumen, aes(grupo_faltas, n, fill = condicion)) +
  geom_col() +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")),
            position = position_stack(vjust = 0.5), fontface = "bold") +
  scale_fill_manual(values = c("#1E90FF", "#87CEEB")) +
  labs(title = "Aprobación según cantidad de inasistencias",
       x = "Inasistencias", y = "Estudiantes", fill = NULL) +
  theme_minimal()

# Distribución del promedio por rango de faltas
muestra %>%
  filter(!is.na(rango_faltas), !is.na(promedio)) %>%
  ggplot(aes(rango_faltas, promedio, fill = rango_faltas)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = c("#C4484C", "#009ADA", "#64A70B", "#00457F", "#994275")) +
  labs(title = "Promedio general por rango de inasistencias",
       x = "Inasistencias", y = "Promedio general anual") +
  theme_minimal()
