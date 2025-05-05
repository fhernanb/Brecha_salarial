
# Loading the dataset -----------------------------------------------------
library(haven)
datos <- read_dta("Data/GEIH_Intermedia_step1.dta")

dim(datos)
colnames(datos)

# Some conversions --------------------------------------------------------

# To show the "ingresos" in millions
datos <- datos |> mutate(ingresos = inglabo / 1000000)


# Nota: por favor cambiar TODAS las variables CUALI en palabras, no usar numeros
# Los numeros se usaban hace muchos anos cuando los lenguajes de programacion
# no podian usar strings, la cosa ya cambio.
# Las variables CUANTI las pueden dejar como estan.

# To convert "estado_civil" in factor using words, not numbers.
datos <- datos %>%
  mutate(
    est_civ = case_when(estado_civil == 1 ~ "soltero",
                        estado_civil == 2 ~ "casado",
                        estado_civil == 3 ~ "blabla"
                        estado_civil == 4 ~ "jaja"
                        estado_civil == 5 ~ "jeje"
                        estado_civil == 6 ~ "blublu")
    )


# To convert "regimen_scc" in factor using words, not numbers.
datos <- datos %>%
  mutate(
    reg = case_when(regimen_scc == 1 ~ "blabla",
                    regimen_scc == 2 ~ "blabla"
                    regimen_scc == 3 ~ "blabla",
                    "agregar las lineas que falten")
  )

# To convert "educacion" in factor using words, not numbers.
datos <- datos %>%
  mutate(
    edu = case_when(educacion == 1 ~ "blabla",
                    educacion == 2 ~ "blabla"
                    educacion == 3 ~ "blabla",
                    "agregar las lineas que falten")
  )

# To convert "sexo" in factor using words, not numbers.
datos <- datos %>%
  mutate(sexo = ifelse(sexo == 0, "blabla", "bleble"))



# Exploratory analysis ----------------------------------------------------

library(dplyr)
library(ggplot2)

datos |> select(oficio) |> table()

ggplot(datos, aes(x=inglabo)) + 
  geom_density() + 
  xlab("Salary [million of pesos]")
