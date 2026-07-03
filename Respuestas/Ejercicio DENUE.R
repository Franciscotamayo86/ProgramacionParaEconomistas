title: "Productos metálicos en CDMX — Análisis DENUE"
author: "Tu nombre aquí"
date: "`r Sys.Date()`"
output:
  html_document:
  toc: true
toc_float: true
theme: flatly
highlight: tango
---

unzip("INEGI_DENUE_1.zip")

denue_ind <- read_csv("INEGI_DENUE_02072026.csv")

library(readr)
guess_encoding("INEGI_DENUE_02072026.csv")

---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, comment = "##")
```

```{r}
library(tidyverse)
```

---
  
  ## Carga de datos
  
  ```{r}
getwd()


file.exists("INEGI_DENUE_02072026.csv")

list.files()
denue <- read_csv("INEGI_DENUE_02072026.csv",
                  locale = locale(encoding = "LATIN1"),
                  show_col_types = FALSE)

nrow(denue)
```

> Fuente: DENUE (INEGI), directorio de unidades económicas, sector 332 — Fabricación de productos metálicos.

---
  
```{r}
# Total de establecimientos
nrow(denue)
```

> *"Hay 3,287 establecimientos de productos metálicos en CDMX."* → Esto no necesita gráfica. Una oración es suficiente.

### Distribución por alcaldía

```{r}
tabla_alcaldias <- denue %>%
  count(Municipio, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  rename(alcaldia = Municipio, establecimientos = n)

tabla_alcaldias
```

> Con 16 alcaldías, una tabla ya es informativa, pero una gráfica de barras hace el patrón geográfico evidente.
  
  ## Distribución por tamaño (estrato de personal)
  
  ```{r}
tabla_tamano <- denue %>%
  count(`Descripcion estrato personal ocupado`, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  rename(estrato = `Descripcion estrato personal ocupado`,
         establecimientos = n)

tabla_tamano
```

> *"El 76% de los establecimientos son micro (0 a 5 personas)."* → Esto podría ser una oración, pero debido al contraste entre estratos, una gráfica refuerza el mensaje.

### Distribución por actividad específica

```{r}
tabla_actividad <- denue %>%
  count(`Nombre de clase de la actividad`, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  rename(actividad = `Nombre de clase de la actividad`,
         establecimientos = n)

tabla_actividad
```

---
  
  ## Visualizaciones
  

### Gráfica 1 — Establecimientos por alcaldía

  
  ```{r fig.width=8, fig.height=6}
denue %>%
  count(Municipio, sort = TRUE) %>%
  mutate(Municipio = reorder(Municipio, n)) %>%
  ggplot(aes(x = n, y = Municipio)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n), hjust = -0.3, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Establecimientos de productos metálicos por alcaldía",
    subtitle = "CDMX, DENUE 2024 (n = 3,287)",
    x        = "Número de establecimientos",
    y        = NULL,
    caption  = "Fuente: INEGI, DENUE"
  ) +
  theme_minimal()

Mensaje: "Las manufacturas metálicas se concentran en Iztapalapa y Gustavo A. Madero."
```

  Datos adicionales:
- Barras horizontales → cada alcaldía.
- Eje x → número de establecimientos (con dato al final de cada barra).
- Eje y → alcaldía, ordenada de mayor a menor (Iztapalapa arriba).
- El título y subtítulo responden el qué y el cuánto, cosas que le dan profundidad a la gráfica.

---
  
  ### Gráfica 2 — Distribución por tamaño (micro vs. PyME vs. grande)
  
  Mensaje: "El sector es predominantemente micro."
  
  ```{r fig.width=7, fig.height=4}
denue %>%
  count(`Descripcion estrato personal ocupado`) %>%
  mutate(
    `Descripcion estrato personal ocupado` = reorder(
      `Descripcion estrato personal ocupado`, n
    ),
    pct = round(100 * n / sum(n), 1)
  ) %>%
  ggplot(aes(x = `Descripcion estrato personal ocupado`, y = n)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = paste0(n, " (", pct, "%)")),
            vjust = -0.4, size = 3.5) +
  labs(
    title    = "Estrato de personal ocupado",
    subtitle = "Predominan las microempresas (0 a 5 personas)",
    x        = NULL,
    y        = "Número de establecimientos"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
```

Datos relevantes
  
  - Eje x → cada estrato (etiqueta clara: "0 a 5 personas", etc.).
- Eje y → número de establecimientos.


  
  ---
  
  ## Gráfica 3 — Top 5 alcaldías
  
 Se prioriza la concentración en el sector dejando de lado aquellas alcaldías que no son relevantes.

```{r fig.width=7, fig.height=4}
top5 <- tabla_alcaldias %>%
  slice_max(establecimientos, n = 5)

otras <- tabla_alcaldias %>%
  slice_max(establecimientos, n = 5) %>%
  anti_join(tabla_alcaldias, by = "alcaldia")

grafica_top5 <- bind_rows(
  top5,
  tibble(
    alcaldia         = "Otras 11 alcaldías",
    establecimientos = sum(otras$establecimientos),
    pct              = round(100 * sum(otras$establecimientos) / sum(tabla_alcaldias$establecimientos), 1)
  )
)

grafica_top5 %>%
  mutate(alcaldia = reorder(alcaldia, establecimientos)) %>%
  ggplot(aes(x = alcaldia, y = establecimientos, fill = alcaldia == "Otras 11 alcaldías")) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(establecimientos, "\n(", pct, "%)")),
            vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "gray70")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Top 5 alcaldías que concentran más de la mitad del sector",
    subtitle = "Productos metálicos en CDMX",
    x        = NULL,
    y        = "Número de establecimientos"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
```

Datos relevantes:
  
  - Top 5 en azul, (Otras 11 alcaldías) en gris → diferenciación visual clara entre lo relevante y el resto.
- Hay etiquetas con número y porcentaje, por lo tanto, el lector no necesita hacer cuentas mentalmente.
- El título afirma el mensaje y no solo describe el contenido. (Concentración de la industria de productos metálicos)

---


---
  
Resumen:
  
  ## Pregunta 1: ¿Cuál es el mensaje? ¿Cabe en una oración?
  
  > "En CDMX existen 3,287 establecimientos de productos metálicos, concentrados en Iztapalapa, Gustavo A. Madero y Azcapotzalco; predominantemente micro (0-5 personas)."
  
  R= Sí cabe.

## Pregunta 2: ¿Podría ser una tabla o una oración?

- *"Hay 3,287 establecimientos"* → oración, no necesita gráfica.
- Distribución por alcaldía (16 valores) → la tabla ya funciona, pero la gráfica añade impacto visual.
- Distribución por tamaño (4 categorías) → la tabla funciona, pero el gráfico refuerza.
- Distribución por actividad (muchas categorías) → tabla es mejor que gráfica de barras con 30+ categorías.

## Pregunta 3: ¿Puede el lector señalar cada elemento y decir qué significa?

- Cada barra tiene etiqueta con número y porcentaje.
- Cada eje tiene título con unidades (o se omite cuando es obvio).
- El gráfico inicia en 0.
- El título de cada gráfica afirma el mensaje, no solo describe.


---
  
  ## Conclusión
  
  Mensaje final:
  
  > La fabricación de productos metálicos en CDMX es un sector micro y territorialmente concentrado: 3,287 establecimientos, 76% con 0–5 empleados, y casi la mitad ubicados en solo 4 alcaldías del oriente y norte (Iztapalapa, Gustavo A. Madero, Azcapotzalco y Venustiano Carranza).