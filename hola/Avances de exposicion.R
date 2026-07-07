
# ============================================================
# ANALISIS ESTADISTICO - MASTITIS SUBCLINICA BOVINA
# Curso: Sistematizacion y Metodos Estadisticos
# Tema: Factores asociados a mastitis subclinica en vacas lecheras
# Articulo base: Khasanah et al. (2021) - Veterinary World
# ============================================================

# ------------------------------------------------------------
# 0. INDICACIONES GENERALES
# ------------------------------------------------------------
# 1) Colocar este script en la misma carpeta donde se encuentra:
#    base_mastitis_bovina_teorica.xlsx
# 2) Ejecutar todo el script desde el inicio.
# 3) Se creara automaticamente una carpeta llamada "resultados_mastitis_bovina".
# 4) Dentro de esa carpeta se guardaran tablas, graficos y respuestas a objetivos.

# ------------------------------------------------------------
# 1. INSTALAR Y CARGAR PAQUETES
# ------------------------------------------------------------

paquetes <- c(
  "readxl", "dplyr", "ggplot2", "janitor", "gtsummary",
  "broom", "epitools", "writexl", "forcats", "stringr"
)

instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]

if(length(instalar) > 0){
  install.packages(instalar, dependencies = TRUE)
}

library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)
library(gtsummary)
library(broom)
library(epitools)
library(writexl)
library(forcats)
library(stringr)

# ------------------------------------------------------------
# 2. CREAR CARPETA DE RESULTADOS
# ------------------------------------------------------------

carpeta_resultados <- "resultados_mastitis_bovina"

if(!dir.exists(carpeta_resultados)){
  dir.create(carpeta_resultados)
}

# ------------------------------------------------------------
# 3. IMPORTAR BASE DE DATOS
# ------------------------------------------------------------

archivo <- "base_mastitis_bovina_teorica.xlsx"

datos <- read_excel(archivo, sheet = "Base_Datos") %>%
  clean_names()

# Verificar primeras filas
head(datos)

# ------------------------------------------------------------
# 4. PREPARACION DE VARIABLES
# ------------------------------------------------------------
# Convertimos variables categoricas a factor.
# Ademas, definimos categorias de referencia para que la regresion
# logistica sea mas facil de interpretar.

datos <- datos %>%
  mutate(
    raza = factor(raza),
    etapa_lactancia = factor(
      etapa_lactancia,
      levels = c("Tardia (>7 meses)", "Temprana (1-2 meses)", "Media (3-6 meses)")
    ),
    higiene_ubre = factor(higiene_ubre, levels = c("Buena", "Mala")),
    limpieza_establo = factor(limpieza_establo, levels = c("Buena", "Moderada", "Mala")),
    limpieza_pezones_postordeno = factor(limpieza_pezones_postordeno, levels = c("Si", "No")),
    secado_pezones_postordeno = factor(secado_pezones_postordeno, levels = c("Si", "No")),
    pre_dipping_agua_tibia = factor(pre_dipping_agua_tibia, levels = c("Si", "No")),
    post_dipping_yodo = factor(post_dipping_yodo, levels = c("Si", "No")),
    antecedente_mastitis = factor(antecedente_mastitis, levels = c("No", "Si")),
    tipo_ordeno = factor(tipo_ordeno),
    ubicacion = factor(ubicacion),
    resultado_cmt = factor(resultado_cmt),
    mastitis_subclinica = factor(mastitis_subclinica, levels = c("No", "Si")),
    mastitis_binaria = ifelse(mastitis_subclinica == "Si", 1, 0)
  )

# ------------------------------------------------------------
# 5. REVISION GENERAL DE LA BASE
# ------------------------------------------------------------

dimension_base <- data.frame(
  indicador = c("Numero de vacas", "Numero de variables"),
  valor = c(nrow(datos), ncol(datos))
)

variables_con_na <- data.frame(
  variable = names(datos),
  n_perdidos = sapply(datos, function(x) sum(is.na(x)))
)

write_xlsx(
  list(
    dimension_base = dimension_base,
    variables_con_na = variables_con_na
  ),
  path = file.path(carpeta_resultados, "01_revision_base.xlsx")
)

# ------------------------------------------------------------
# OBJETIVO ESPECIFICO 1
# Describir las caracteristicas demograficas, productivas y de manejo.
# ------------------------------------------------------------

# Resumen para variables numericas
resumen_numerico <- datos %>%
  summarise(
    edad_media = mean(edad_anios, na.rm = TRUE),
    edad_de = sd(edad_anios, na.rm = TRUE),
    edad_mediana = median(edad_anios, na.rm = TRUE),
    edad_min = min(edad_anios, na.rm = TRUE),
    edad_max = max(edad_anios, na.rm = TRUE),

    partos_media = mean(n_partos, na.rm = TRUE),
    partos_de = sd(n_partos, na.rm = TRUE),
    partos_mediana = median(n_partos, na.rm = TRUE),
    partos_min = min(n_partos, na.rm = TRUE),
    partos_max = max(n_partos, na.rm = TRUE),

    leche_media = mean(produccion_leche_l_dia, na.rm = TRUE),
    leche_de = sd(produccion_leche_l_dia, na.rm = TRUE),
    leche_mediana = median(produccion_leche_l_dia, na.rm = TRUE),
    leche_min = min(produccion_leche_l_dia, na.rm = TRUE),
    leche_max = max(produccion_leche_l_dia, na.rm = TRUE)
  )

# Funcion para frecuencias
tabla_frecuencia <- function(variable){
  datos %>%
    tabyl({{ variable }}) %>%
    adorn_pct_formatting(digits = 1)
}

freq_raza <- tabla_frecuencia(raza)
freq_etapa <- tabla_frecuencia(etapa_lactancia)
freq_higiene_ubre <- tabla_frecuencia(higiene_ubre)
freq_limpieza_establo <- tabla_frecuencia(limpieza_establo)
freq_limpieza_pezones <- tabla_frecuencia(limpieza_pezones_postordeno)
freq_antecedente <- tabla_frecuencia(antecedente_mastitis)
freq_tipo_ordeno <- tabla_frecuencia(tipo_ordeno)
freq_ubicacion <- tabla_frecuencia(ubicacion)

write_xlsx(
  list(
    resumen_numerico = resumen_numerico,
    raza = freq_raza,
    etapa_lactancia = freq_etapa,
    higiene_ubre = freq_higiene_ubre,
    limpieza_establo = freq_limpieza_establo,
    limpieza_pezones = freq_limpieza_pezones,
    antecedente_mastitis = freq_antecedente,
    tipo_ordeno = freq_tipo_ordeno,
    ubicacion = freq_ubicacion
  ),
  path = file.path(carpeta_resultados, "02_objetivo_1_descriptivos.xlsx")
)

# GRAFICO 1: Histograma de edad
g1 <- ggplot(datos, aes(x = edad_anios)) +
  geom_histogram(bins = 6, color = "black", fill = "gray80") +
  labs(
    title = "Distribucion de la edad de las vacas",
    x = "Edad (anios)",
    y = "Frecuencia"
  ) +
  theme_minimal()

ggsave(file.path(carpeta_resultados, "grafico_01_histograma_edad.png"),
       g1, width = 7, height = 5, dpi = 300)

# GRAFICO 2: Histograma produccion de leche
g2 <- ggplot(datos, aes(x = produccion_leche_l_dia)) +
  geom_histogram(bins = 12, color = "black", fill = "gray80") +
  labs(
    title = "Distribucion de la produccion de leche",
    x = "Produccion de leche (L/dia)",
    y = "Frecuencia"
  ) +
  theme_minimal()

ggsave(file.path(carpeta_resultados, "grafico_02_histograma_produccion_leche.png"),
       g2, width = 7, height = 5, dpi = 300)

# GRAFICO 3: Barras por etapa de lactancia
g3 <- ggplot(datos, aes(x = etapa_lactancia)) +
  geom_bar(fill = "gray70", color = "black") +
  labs(
    title = "Distribucion segun etapa de lactancia",
    x = "Etapa de lactancia",
    y = "Numero de vacas"
  ) +
  theme_minimal()

ggsave(file.path(carpeta_resultados, "grafico_03_etapa_lactancia.png"),
       g3, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------
# OBJETIVO ESPECIFICO 2
# Determinar la prevalencia de mastitis subclinica.
# ------------------------------------------------------------

prevalencia_mastitis <- datos %>%
  tabyl(mastitis_subclinica) %>%
  adorn_pct_formatting(digits = 1)

write_xlsx(
  list(prevalencia_mastitis = prevalencia_mastitis),
  path = file.path(carpeta_resultados, "03_objetivo_2_prevalencia.xlsx")
)

# GRAFICO 4: Prevalencia de mastitis
g4 <- ggplot(datos, aes(x = mastitis_subclinica)) +
  geom_bar(fill = "gray70", color = "black") +
  labs(
    title = "Prevalencia de mastitis subclinica",
    x = "Mastitis subclinica",
    y = "Numero de vacas"
  ) +
  theme_minimal()

ggsave(file.path(carpeta_resultados, "grafico_04_prevalencia_mastitis.png"),
       g4, width = 7, height = 5, dpi = 300)

# ------------------------------------------------------------
# OBJETIVO ESPECIFICO 3
# Evaluar asociacion entre etapa de lactancia y mastitis subclinica.
# ------------------------------------------------------------

tabla_etapa_mastitis <- datos %>%
  tabyl(etapa_lactancia, mastitis_subclinica) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()

prueba_etapa <- chisq.test(table(datos$etapa_lactancia, datos$mastitis_subclinica))

resultado_chi_etapa <- data.frame(
  variable = "etapa_lactancia",
  chi_cuadrado = unname(prueba_etapa$statistic),
  grados_libertad = unname(prueba_etapa$parameter),
  p_valor = prueba_etapa$p.value
)

write_xlsx(
  list(
    tabla_cruzada = tabla_etapa_mastitis,
    prueba_chi_cuadrado = resultado_chi_etapa
  ),
  path = file.path(carpeta_resultados, "04_objetivo_3_etapa_lactancia.xlsx")
)

# GRAFICO 5: Mastitis segun etapa de lactancia
g5 <- ggplot(datos, aes(x = etapa_lactancia, fill = mastitis_subclinica)) +
  geom_bar(position = "fill", color = "black") +
  labs(
    title = "Proporcion de mastitis segun etapa de lactancia",
    x = "Etapa de lactancia",
    y = "Proporcion",
    fill = "Mastitis"
  ) +
  theme_minimal()

ggsave(file.path(carpeta_resultados, "grafico_05_mastitis_por_etapa.png"),
       g5, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------
# OBJETIVO ESPECIFICO 4
# Evaluar asociacion entre limpieza del establo y mastitis subclinica.
# ------------------------------------------------------------

tabla_establo_mastitis <- datos %>%
  tabyl(limpieza_establo, mastitis_subclinica) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()

prueba_establo <- chisq.test(table(datos$limpieza_establo, datos$mastitis_subclinica))

resultado_chi_establo <- data.frame(
  variable = "limpieza_establo",
  chi_cuadrado = unname(prueba_establo$statistic),
  grados_libertad = unname(prueba_establo$parameter),
  p_valor = prueba_establo$p.value
)

write_xlsx(
  list(
    tabla_cruzada = tabla_establo_mastitis,
    prueba_chi_cuadrado = resultado_chi_establo
  ),
  path = file.path(carpeta_resultados, "05_objetivo_4_limpieza_establo.xlsx")
)

# GRAFICO 6: Mastitis segun limpieza del establo
g6 <- ggplot(datos, aes(x = limpieza_establo, fill = mastitis_subclinica)) +
  geom_bar(position = "fill", color = "black") +
  labs(
    title = "Proporcion de mastitis segun limpieza del establo",
    x = "Limpieza del establo",
    y = "Proporcion",
    fill = "Mastitis"
  ) +
  theme_minimal()

ggsave(file.path(carpeta_resultados, "grafico_06_mastitis_por_limpieza_establo.png"),
       g6, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------
# OBJETIVO ESPECIFICO 5
# Evaluar asociacion entre limpieza de pezones post-ordeno y mastitis.
# ------------------------------------------------------------

tabla_pezones_mastitis <- datos %>%
  tabyl(limpieza_pezones_postordeno, mastitis_subclinica) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()

# En tablas 2x2 se puede usar Chi-cuadrado o Fisher.
tabla_2x2_pezones <- table(datos$limpieza_pezones_postordeno, datos$mastitis_subclinica)

prueba_pezones <- chisq.test(tabla_2x2_pezones)
fisher_pezones <- fisher.test(tabla_2x2_pezones)

or_pezones <- oddsratio(tabla_2x2_pezones)

resultado_pezones <- data.frame(
  variable = "limpieza_pezones_postordeno",
  chi_cuadrado = unname(prueba_pezones$statistic),
  p_chi_cuadrado = prueba_pezones$p.value,
  p_fisher = fisher_pezones$p.value
)

write_xlsx(
  list(
    tabla_cruzada = tabla_pezones_mastitis,
    prueba_estadistica = resultado_pezones,
    odds_ratio = as.data.frame(or_pezones$measure),
    p_values_or = as.data.frame(or_pezones$p.value)
  ),
  path = file.path(carpeta_resultados, "06_objetivo_5_limpieza_pezones.xlsx")
)

# GRAFICO 7: Mastitis segun limpieza de pezones
g7 <- ggplot(datos, aes(x = limpieza_pezones_postordeno, fill = mastitis_subclinica)) +
  geom_bar(position = "fill", color = "black") +
  labs(
    title = "Proporcion de mastitis segun limpieza de pezones post-ordeno",
    x = "Limpieza de pezones post-ordeno",
    y = "Proporcion",
    fill = "Mastitis"
  ) +
  theme_minimal()

ggsave(file.path(carpeta_resultados, "grafico_07_mastitis_por_limpieza_pezones.png"),
       g7, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------
# ANALISIS COMPLEMENTARIO
# Comparacion de produccion de leche segun mastitis.
# ------------------------------------------------------------

resumen_leche_por_mastitis <- datos %>%
  group_by(mastitis_subclinica) %>%
  summarise(
    n = n(),
    media = mean(produccion_leche_l_dia, na.rm = TRUE),
    de = sd(produccion_leche_l_dia, na.rm = TRUE),
    mediana = median(produccion_leche_l_dia, na.rm = TRUE),
    minimo = min(produccion_leche_l_dia, na.rm = TRUE),
    maximo = max(produccion_leche_l_dia, na.rm = TRUE)
  )

prueba_t_leche <- t.test(produccion_leche_l_dia ~ mastitis_subclinica, data = datos)

resultado_t_leche <- data.frame(
  comparacion = "produccion_leche_l_dia segun mastitis_subclinica",
  t = unname(prueba_t_leche$statistic),
  gl = unname(prueba_t_leche$parameter),
  p_valor = prueba_t_leche$p.value
)

write_xlsx(
  list(
    resumen_leche = resumen_leche_por_mastitis,
    prueba_t = resultado_t_leche
  ),
  path = file.path(carpeta_resultados, "07_analisis_complementario_produccion_leche.xlsx")
)

# GRAFICO 8: Boxplot de produccion de leche segun mastitis
g8 <- ggplot(datos, aes(x = mastitis_subclinica, y = produccion_leche_l_dia)) +
  geom_boxplot(fill = "gray80", color = "black") +
  labs(
    title = "Produccion de leche segun mastitis subclinica",
    x = "Mastitis subclinica",
    y = "Produccion de leche (L/dia)"
  ) +
  theme_minimal()

ggsave(file.path(carpeta_resultados, "grafico_08_boxplot_leche_por_mastitis.png"),
       g8, width = 7, height = 5, dpi = 300)

# ------------------------------------------------------------
# OBJETIVO ESPECIFICO 6
# Identificar factores asociados mediante regresion logistica.
# ------------------------------------------------------------
# Variable dependiente:
# mastitis_binaria: 1 = Si, 0 = No

modelo_logistico <- glm(
  mastitis_binaria ~ edad_anios +
    n_partos +
    produccion_leche_l_dia +
    etapa_lactancia +
    higiene_ubre +
    limpieza_establo +
    limpieza_pezones_postordeno +
    post_dipping_yodo +
    antecedente_mastitis,
  family = binomial(link = "logit"),
  data = datos
)

summary(modelo_logistico)

# OR ajustados con intervalo de confianza
or_ajustados <- tidy(modelo_logistico, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(
    interpretacion = case_when(
      p.value < 0.05 & estimate > 1 ~ "Factor asociado a mayor odds de mastitis",
      p.value < 0.05 & estimate < 1 ~ "Factor protector asociado a menor odds de mastitis",
      TRUE ~ "No significativo"
    )
  )

write_xlsx(
  list(
    or_ajustados = or_ajustados
  ),
  path = file.path(carpeta_resultados, "08_objetivo_6_regresion_logistica.xlsx")
)

# Tabla bonita de regresion en formato HTML
tabla_modelo <- tbl_regression(
  modelo_logistico,
  exponentiate = TRUE,
  label = list(
    edad_anios ~ "Edad (anios)",
    n_partos ~ "Numero de partos",
    produccion_leche_l_dia ~ "Produccion de leche (L/dia)",
    etapa_lactancia ~ "Etapa de lactancia",
    higiene_ubre ~ "Higiene de ubre",
    limpieza_establo ~ "Limpieza del establo",
    limpieza_pezones_postordeno ~ "Limpieza de pezones post-ordeno",
    post_dipping_yodo ~ "Post-dipping con yodo",
    antecedente_mastitis ~ "Antecedente de mastitis"
  )
)

# Guardar tabla como RDS por si se desea reutilizar
saveRDS(tabla_modelo, file.path(carpeta_resultados, "tabla_regresion_logistica.rds"))

# GRAFICO 9: Forest plot simple de OR ajustados
or_plot <- or_ajustados %>%
  filter(term != "(Intercept)") %>%
  mutate(term = str_replace_all(term, "_", " "))

g9 <- ggplot(or_plot, aes(x = estimate, y = fct_reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_log10() +
  labs(
    title = "Odds Ratio ajustados para mastitis subclinica",
    x = "OR ajustado (escala logaritmica)",
    y = "Variable"
  ) +
  theme_minimal()

ggsave(file.path(carpeta_resultados, "grafico_09_forest_plot_or_ajustados.png"),
       g9, width = 9, height = 6, dpi = 300)

# ------------------------------------------------------------
# TABLA RESUMEN GENERAL TIPO ARTICULO
# ------------------------------------------------------------

tabla_descriptiva_final <- datos %>%
  select(
    mastitis_subclinica,
    edad_anios,
    n_partos,
    produccion_leche_l_dia,
    raza,
    etapa_lactancia,
    higiene_ubre,
    limpieza_establo,
    limpieza_pezones_postordeno,
    post_dipping_yodo,
    antecedente_mastitis
  ) %>%
  tbl_summary(
    by = mastitis_subclinica,
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_p() %>%
  add_overall()

saveRDS(tabla_descriptiva_final, file.path(carpeta_resultados, "tabla_descriptiva_final.rds"))

# ------------------------------------------------------------
# RESPUESTAS AUTOMATICAS A LOS OBJETIVOS
# ------------------------------------------------------------
# Este bloque genera un archivo de texto con conclusiones guiadas.
# Los estudiantes deben revisar y complementar con redaccion propia.

prev_si <- datos %>%
  summarise(prevalencia = mean(mastitis_binaria) * 100) %>%
  pull(prevalencia)

p_etapa <- prueba_etapa$p.value
p_establo <- prueba_establo$p.value
p_pezones <- prueba_pezones$p.value
p_leche <- prueba_t_leche$p.value

sign_etapa <- ifelse(p_etapa < 0.05, "si se encontro asociacion estadisticamente significativa", "no se encontro asociacion estadisticamente significativa")
sign_establo <- ifelse(p_establo < 0.05, "si se encontro asociacion estadisticamente significativa", "no se encontro asociacion estadisticamente significativa")
sign_pezones <- ifelse(p_pezones < 0.05, "si se encontro asociacion estadisticamente significativa", "no se encontro asociacion estadisticamente significativa")
sign_leche <- ifelse(p_leche < 0.05, "si hubo diferencia estadisticamente significativa", "no hubo diferencia estadisticamente significativa")

vars_significativas <- or_ajustados %>%
  filter(term != "(Intercept)", p.value < 0.05) %>%
  pull(term)

if(length(vars_significativas) == 0){
  texto_vars <- "En el modelo multivariado no se identificaron variables estadisticamente significativas."
} else {
  texto_vars <- paste(
    "En el modelo multivariado, las variables con asociacion estadisticamente significativa fueron:",
    paste(vars_significativas, collapse = ", "),
    "."
  )
}

respuestas <- c(
  "RESPUESTAS A LOS OBJETIVOS",
  "==========================",
  "",
  "Objetivo general:",
  "Determinar los factores asociados a la presentacion de mastitis subclinica en vacas lecheras.",
  "",
  "OE1. Caracteristicas de la poblacion:",
  paste0("Se analizaron ", nrow(datos), " vacas lecheras. La edad promedio fue de ",
         round(mean(datos$edad_anios), 2), " anios, con una produccion promedio de ",
         round(mean(datos$produccion_leche_l_dia), 2), " L/dia."),
  "",
  "OE2. Prevalencia:",
  paste0("La prevalencia de mastitis subclinica fue de ", round(prev_si, 1), "%."),
  "",
  "OE3. Etapa de lactancia:",
  paste0("Al evaluar etapa de lactancia y mastitis subclinica, ", sign_etapa,
         " (p = ", round(p_etapa, 4), ")."),
  "",
  "OE4. Limpieza del establo:",
  paste0("Al evaluar limpieza del establo y mastitis subclinica, ", sign_establo,
         " (p = ", round(p_establo, 4), ")."),
  "",
  "OE5. Limpieza de pezones post-ordeno:",
  paste0("Al evaluar limpieza de pezones post-ordeno y mastitis subclinica, ", sign_pezones,
         " (p = ", round(p_pezones, 4), ")."),
  "",
  "Analisis complementario:",
  paste0("Al comparar la produccion de leche entre vacas con y sin mastitis, ", sign_leche,
         " (p = ", round(p_leche, 4), ")."),
  "",
  "OE6. Regresion logistica:",
  texto_vars,
  "",
  "Conclusion sugerida:",
  "La mastitis subclinica fue frecuente en la poblacion evaluada. Los resultados sugieren que las variables de lactancia, higiene y manejo del ordeno deben ser consideradas en programas de prevencion y control.",
  "",
  "Nota:",
  "Estas respuestas son generadas automaticamente por el script. Deben ser revisadas, interpretadas y redactadas con criterio academico por cada grupo."
)

writeLines(respuestas, con = file.path(carpeta_resultados, "09_respuestas_a_objetivos.txt"))

# ------------------------------------------------------------
# EXPORTAR BASE LIMPIA
# ------------------------------------------------------------

write_xlsx(
  list(base_limpia = datos),
  path = file.path(carpeta_resultados, "10_base_limpia_analizada.xlsx")
)

# ------------------------------------------------------------
# FIN DEL SCRIPT
# ------------------------------------------------------------

message("Analisis finalizado correctamente.")
message("Revisa la carpeta: ", carpeta_resultados)
