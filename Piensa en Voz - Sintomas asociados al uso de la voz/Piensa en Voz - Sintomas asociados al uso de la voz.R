# Piensa en Voz - procesamiento del autorreporte diario de síntomas
# Programa de cuidado de la voz para docentes (Ministerio de Educación de Córdoba).
# Los docentes registran cada día los síntomas asociados al uso de la voz.
# Los datos son confidenciales y no se incluyen en el repositorio.

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

# ---- Datos ----
# Una fila por docente; las columnas de fecha tienen los síntomas del día
# separados por coma. Los datos personales se reemplazan por un id.
autorreporte <- read_excel("data/autorreporte.xlsx") %>%
  mutate(id_docente = row_number()) %>%
  select(-any_of(c("Apellido", "Nombre")))

cols_fecha <- names(autorreporte)[str_detect(names(autorreporte), "^\\d")]

# Gravedad de cada síntoma (1 = leve, 3 = grave)
gravedad_sintoma <- tribble(
  ~sintoma,                      ~gravedad,
  "Cansancio al hablar",         1,
  "Ardor y picor",               1,
  "Sensación de cuerpo extraño", 2,
  "Cambio en el tono",           2,
  "Falta de aire",               3,
  "Pérdida de voz",              3
)

# ---- Limpieza ----
autorreporte <- autorreporte %>%
  mutate(
    carga_horaria = factor(carga_horaria,
                           levels = c("de 5 a 10 horas", "de 11 a 20 horas",
                                      "de 21 a 30 horas", "más de 30 horas"))
  )

# Pasamos a formato largo: una fila por docente, día y síntoma
sintomas_long <- autorreporte %>%
  pivot_longer(all_of(cols_fecha), names_to = "fecha", values_to = "sintoma") %>%
  mutate(fecha = dmy(fecha)) %>%
  separate_longer_delim(sintoma, delim = ",") %>%
  mutate(
    sintoma = str_squish(sintoma),
    sintoma = str_replace(sintoma, regex("ardor,? ?y? ?picor", ignore_case = TRUE), "Ardor y picor"),
    sintoma = na_if(sintoma, "No tuve síntomas"),
    sintoma = na_if(sintoma, "")
  ) %>%
  filter(!is.na(sintoma)) %>%
  left_join(gravedad_sintoma, by = "sintoma")

# Síntomas sin categoría: revisar si hay que sumarlos a la tabla
sintomas_long %>% filter(is.na(gravedad)) %>% count(sintoma, sort = TRUE)

# ---- Indicadores ----
# Promedio de síntomas reportados por docente, según nivel y carga horaria
sintomas_docente <- sintomas_long %>%
  count(id_docente, name = "total_sintomas") %>%
  right_join(autorreporte %>% select(id_docente, nivel, carga_horaria, antiguedad),
             by = "id_docente") %>%
  mutate(total_sintomas = coalesce(total_sintomas, 0L))

sintomas_docente %>%
  group_by(nivel) %>%
  summarise(docentes = n(), promedio_sintomas = mean(total_sintomas))

sintomas_docente %>%
  group_by(carga_horaria) %>%
  summarise(docentes = n(), promedio_sintomas = mean(total_sintomas))

# Síntomas por gravedad para cada docente (sirve para detectar casos de riesgo)
gravedad_docente <- sintomas_long %>%
  filter(!is.na(gravedad)) %>%
  count(id_docente, gravedad) %>%
  pivot_wider(names_from = gravedad, values_from = n,
              names_prefix = "gravedad_", values_fill = 0)

docentes_riesgo <- gravedad_docente %>%
  filter(gravedad_3 >= 3)

# ---- Gráficos ----
# Evolución diaria de los síntomas reportados
sintomas_long %>%
  count(fecha, sintoma) %>%
  ggplot(aes(fecha, n, color = sintoma)) +
  geom_line() +
  labs(title = "Síntomas reportados por día", x = NULL,
       y = "Cantidad de reportes", color = NULL) +
  theme_minimal()

# Frecuencia de cada síntoma
sintomas_long %>%
  count(sintoma, sort = TRUE) %>%
  ggplot(aes(reorder(sintoma, n), n)) +
  geom_col(fill = "#009ADA") +
  coord_flip() +
  labs(title = "Frecuencia de síntomas", x = NULL, y = "Reportes") +
  theme_minimal()
