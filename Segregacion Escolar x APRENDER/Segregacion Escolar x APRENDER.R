# Segregación escolar según APRENDER 2019 - Santiago del Estero
# Socio-hackathon "Investigar en Sociales" 2024 (FCS-UNC) - 2.º premio
# Trabajo en equipo: G. Polizzi, X. Ruberto Villarreal y G. Strizzolo.
#
# Datos: base pública de estudiantes de APRENDER 2019 (secundaria),
# Ministerio de Capital Humano / Secretaría de Educación de la Nación.

library(dplyr)
library(ggplot2)

# ---- Datos ----
aprender <- read.csv("data/aprender_2019_estudiantes_secundaria.csv", sep = ";") %>%
  mutate(
    sector = recode(as.character(sector), `1` = "Estatal", `2` = "Privado"),
    ambito = recode(as.character(ambito), `1` = "Urbano", `2` = "Rural")
  )

# ---- País ----
aprender %>% count(sector) %>% mutate(porcentaje = n / sum(n) * 100)
aprender %>% count(ambito) %>% mutate(porcentaje = n / sum(n) * 100)

# Nivel socioeconómico del estudiante según sector
nse_sector <- aprender %>%
  filter(!is.na(isocioa)) %>%
  count(sector, isocioa) %>%
  group_by(sector) %>%
  mutate(porcentaje = n / sum(n) * 100)

ggplot(nse_sector, aes(isocioa, porcentaje, color = sector, group = sector)) +
  geom_line() +
  geom_point() +
  labs(title = "Nivel socioeconómico de los estudiantes según sector",
       x = "Nivel socioeconómico", y = "% de estudiantes", color = NULL) +
  theme_minimal()

# Desempeño en Matemática y Lengua según sector
desempeno <- function(datos, var) {
  datos %>%
    filter(!is.na({{ var }})) %>%
    count(sector, desempeno = {{ var }}) %>%
    group_by(sector) %>%
    mutate(porcentaje = n / sum(n) * 100)
}

desempeno(aprender, mdesemp)
desempeno(aprender, ldesemp)

# ---- Santiago del Estero ----
sde <- aprender %>% filter(cod_provincia == 86)  # código INDEC de Santiago del Estero

sde_nse <- sde %>%
  filter(!is.na(isocioa)) %>%
  count(sector, isocioa) %>%
  group_by(sector) %>%
  mutate(porcentaje = n / sum(n) * 100)

ggplot(sde_nse, aes(isocioa, porcentaje, fill = sector)) +
  geom_col(position = "dodge") +
  labs(title = "Santiago del Estero: nivel socioeconómico según sector",
       x = "Nivel socioeconómico", y = "% de estudiantes", fill = NULL) +
  theme_minimal()

# Desempeño en Matemática según nivel socioeconómico y sector
sde %>%
  filter(!is.na(isocioa), !is.na(mdesemp)) %>%
  count(sector, isocioa, mdesemp) %>%
  group_by(sector, isocioa) %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  ggplot(aes(isocioa, porcentaje, fill = mdesemp)) +
  geom_col() +
  facet_wrap(~ sector) +
  labs(title = "Desempeño en Matemática según NSE y sector",
       x = "Nivel socioeconómico", y = "%", fill = "Desempeño") +
  theme_minimal()
