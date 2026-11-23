# Escenarios de demanda educativa: caída de la natalidad x capacidad de las escuelas
# Dirección General de Planeamiento, Evaluación y Prospectiva Educativa - Córdoba
#
# Idea general:
#  1. Proyectar la matrícula de sala de 3 a 6.º grado por municipio/comuna:
#     las cohortes de 2025 avanzan un grado por año (1 a 1) y el ingreso a
#     sala de 3 sale de la población de 0 a 2 años (nov. 2025) por la cobertura
#     de sala de 3 de cada territorio. Inicial llega a 2028 y primaria a 2030.
#  2. Calcular la ocupación: matrícula proyectada / capacidad actual.
#  3. Clasificar cada municipio en cuatro escenarios según la caída de la
#     natalidad y la ocupación.
#
# Los datos son administrativos y confidenciales: no se incluyen.

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)

# ---- Parámetros ----
CORTE_OCUPACION <- 75   # % de la capacidad
GRADOS <- c("s3", "s4", "s5", "g1", "g2", "g3", "g4", "g5", "g6")
INICIAL <- c("s3", "s4", "s5")
PRIMARIA <- c("g1", "g2", "g3", "g4", "g5", "g6")

# Capacidad por sección (fija, la de 2025)
CAP_INDEPENDIENTE <- c(s3 = 20, s4 = 25, s5 = 30,
                       g1 = 25, g2 = 25, g3 = 25, g4 = 25, g5 = 25, g6 = 25)
CAP_COMPARTIDA <- 30  # multisala y plurigrado
CAP_MULTINIVEL <- 25  # inicial y primaria en la misma sección

# ---- Datos ----
# poblacion: id_territorio, territorio, departamento, edad_0 ... edad_3 (nov. 2025)
# matricula: id_territorio, grado, matricula (2025)
# secciones: id_territorio, id_seccion, tipo_seccion, grado
poblacion <- read_excel("data/poblacion_0a3.xlsx")
matricula <- read_excel("data/matricula_2025.xlsx")
secciones <- read_excel("data/secciones_2025.xlsx")

# ---- Caída de la natalidad ----
# Variación de los nacimientos recientes (0 y 1 año) respecto de los
# anteriores (2 y 3 años). Valores más negativos = más caída.
natalidad <- poblacion %>%
  mutate(var_natalidad = 100 * ((edad_0 + edad_1) / (edad_2 + edad_3) - 1))

# ---- Capacidad ----
# Las secciones compartidas reparten su capacidad en partes iguales
# entre las salas/grados que atienden.
capacidad <- secciones %>%
  group_by(id_territorio, id_seccion) %>%
  mutate(n_grados = n_distinct(grado)) %>%
  ungroup() %>%
  mutate(cap = case_when(
    tipo_seccion == "independiente" ~ CAP_INDEPENDIENTE[grado],
    tipo_seccion == "multinivel"    ~ CAP_MULTINIVEL / n_grados,
    TRUE                            ~ CAP_COMPARTIDA / n_grados
  )) %>%
  group_by(id_territorio, grado) %>%
  summarise(capacidad = sum(cap), .groups = "drop")

# ---- Proyección de matrícula ----
mat_ancha <- matricula %>%
  pivot_wider(names_from = grado, values_from = matricula, values_fill = 0)

# Cobertura de sala de 3 en 2025 (matrícula / población de 3 años)
cobertura <- mat_ancha %>%
  select(id_territorio, s3) %>%
  left_join(poblacion %>% select(id_territorio, edad_3), by = "id_territorio") %>%
  mutate(cob_s3 = if_else(edad_3 > 0, pmin(s3 / edad_3, 1), NA_real_))

# Ingreso a sala de 3: en 2026 entran los que tenían 2 años en nov. 2025, etc.
ingresos <- poblacion %>%
  left_join(cobertura %>% select(id_territorio, cob_s3), by = "id_territorio") %>%
  transmute(id_territorio,
            `2026` = edad_2 * cob_s3,
            `2027` = edad_1 * cob_s3,
            `2028` = edad_0 * cob_s3) %>%
  pivot_longer(-id_territorio, names_to = "anio_ingreso", values_to = "matricula") %>%
  mutate(anio_ingreso = as.integer(anio_ingreso))

# Cohortes observadas en 2025 y nuevas cohortes: cada una avanza un grado por año
cohortes_2025 <- matricula %>%
  mutate(pos_inicial = match(grado, GRADOS), anio_base = 2025L)

cohortes_nuevas <- ingresos %>%
  mutate(pos_inicial = 1L, anio_base = anio_ingreso) %>%
  select(-anio_ingreso)

proyeccion <- bind_rows(cohortes_2025, cohortes_nuevas) %>%
  select(id_territorio, pos_inicial, anio_base, matricula) %>%
  mutate(anio = map(anio_base, ~ .x:2030)) %>%
  unnest(anio) %>%
  mutate(pos = pos_inicial + (anio - anio_base)) %>%
  filter(pos <= length(GRADOS)) %>%
  mutate(grado = GRADOS[pos]) %>%
  group_by(id_territorio, anio, grado) %>%
  summarise(matricula = sum(matricula), .groups = "drop") %>%
  # inicial solo hasta 2028 (no hay población para ingresos posteriores)
  filter(!(grado %in% INICIAL & anio > 2028))

# ---- Ocupación por nivel ----
ocupacion <- proyeccion %>%
  left_join(capacidad, by = c("id_territorio", "grado")) %>%
  mutate(nivel = if_else(grado %in% INICIAL, "Inicial", "Primaria")) %>%
  group_by(id_territorio, nivel, anio) %>%
  summarise(matricula = sum(matricula),
            capacidad = sum(capacidad, na.rm = TRUE), .groups = "drop") %>%
  mutate(ocupacion = if_else(capacidad > 0, 100 * matricula / capacidad, NA_real_))

# ---- Escenarios ----
# Corte de natalidad: mediana de los municipios/comunas con escuelas
corte_natalidad <- natalidad %>%
  filter(id_territorio %in% capacidad$id_territorio) %>%
  pull(var_natalidad) %>%
  median(na.rm = TRUE)

escenarios <- ocupacion %>%
  left_join(natalidad %>% select(id_territorio, territorio, departamento, var_natalidad),
            by = "id_territorio") %>%
  mutate(escenario = case_when(
    is.na(var_natalidad) | is.na(ocupacion) ~ "Sin dato",
    var_natalidad <  corte_natalidad & ocupacion >= CORTE_OCUPACION ~ "Alta caída - escaso margen de absorción",
    var_natalidad >= corte_natalidad & ocupacion >= CORTE_OCUPACION ~ "Moderada caída - escaso margen de absorción",
    var_natalidad <  corte_natalidad ~ "Alta caída - amplio margen de absorción",
    TRUE                             ~ "Moderada caída - amplio margen de absorción"
  ))

# Cantidad de municipios/comunas por escenario, por nivel y año
escenarios %>%
  count(nivel, anio, escenario) %>%
  pivot_wider(names_from = anio, values_from = n, values_fill = 0)
