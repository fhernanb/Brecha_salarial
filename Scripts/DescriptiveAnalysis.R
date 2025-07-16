
# Loading the dataset -----------------------------------------------------
library(haven)
library(dplyr)

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
    est_civ = case_when(estado_civil == 1 ~ "No casado, vive con pareja hace menos de dos años",
                        estado_civil == 2 ~ "No casado, vive con pareja hace dos años o más",
                        estado_civil == 3 ~ "Casado",
                        estado_civil == 4 ~ "Divorciado",
                        estado_civil == 5 ~ "Viudo",
                        estado_civil == 6 ~ "soltero"),
  )


# To convert "regimen_scc" in factor using words, not numbers.
datos <- datos %>%
  mutate(
    reg = case_when(regimen_ssc == 1 ~ "Contributivo (eps)",
                    regimen_ssc == 2 ~ "Especial",
                    regimen_ssc == 3 ~ "Subsidiado",
                    regimen_ssc == 9 ~ "No sabe, no informa"),
        )

# To convert "educacion" in factor using words, not numbers.
datos <- datos %>%
  mutate(
    edu = case_when(educacion == 1 ~ "Ninguno",
                    educacion == 2 ~ "Preescolar",
                    educacion == 3 ~ "Básica primaria (1o - 5o)",
                    educacion == 4 ~ "Básica secundaria (6o - 9o)",
                    educacion == 5 ~ "Media académica (Bachillerato clásico",
                    educacion == 6 ~ "Media técnica (Bachillerato técnico)",
                    educacion == 7 ~ "Normalista",
                    educacion == 8 ~ "Técnica profesional",
                    educacion == 9 ~ "Tecnológica",
                    educacion == 10 ~ "Universitaria",
                    educacion == 11 ~ "Especialización",
                    educacion == 12 ~ "Maestría",
                    educacion == 13 ~ "Doctorado",
                    educacion == 99 ~ "No sabe, no informa",
  ))

  # To convert "dpto" in factor using words, not numbers.
  datos <- datos %>%
    mutate(
      dptos = case_when(dpto == 5 ~ "Antioquia",
                      dpto == 8 ~ "Atlántico",
                      dpto == 11 ~ "Bogotá D.C.",
                      dpto == 13 ~ "Bolívar",
                      dpto == 15 ~ "Boyocá",
                      dpto == 17 ~ "Caldas",
                      dpto == 18 ~ "Caquetá",
                      dpto == 19 ~ "Cauca",
                      dpto == 20 ~ "César",
                      dpto == 23 ~ "Córdoba",
                      dpto == 27 ~ "Chocó",
                      dpto == 41 ~ "Huila",
                      dpto == 44 ~ "La Guajira",
                      dpto == 47 ~ "Magdalena",
                      dpto == 50 ~ "Meta",
                      dpto == 52 ~ "Nariño",
                      dpto == 54 ~ "Norte de Santander",
                      dpto == 63 ~ "Quindío",
                      dpto == 66 ~ "Risaralda",
                      dpto == 68 ~ "Santander",
                      dpto == 70 ~ "Sucre",
                      dpto == 73 ~ "Tolima",
                      dpto == 76 ~ "Valle del Cauca",
                      dpto == 99 ~ "No identificado",
      ))
      
      

# To convert "sexo" in factor using words, not numbers.
datos <- datos %>%
  mutate(sexo = ifelse(sexo == 0, "Mujer", "Hombre"))

# To convert "salud" in factor using words, not numbers.
# Es afiliado, cotizante o beneficiario en salud
datos <- datos %>%
  mutate(salud = ifelse(salud == 0, "No", "Si"))

# To convert "Sabe_leer_escribir" in factor using words, not numbers.
# Es afiliado, cotizante o beneficiario en salud
datos <- datos %>%
  mutate(alfabetismo = ifelse(sabe_leer_escribir == 0, "No", "Si"))

# To convert "cotizante_pension" in factor using words, not numbers.
datos <- datos %>%
  mutate(
    pension = case_when(cotizante_pension == 1 ~ "Si cotiza",
                    cotizante_pension == 2 ~ "No cotiza",
                    cotizante_pension == 3 ~ "Ya es pensionado",
    ))

# To convert "oficio" in factor using words, not numbers.
datos <- datos %>%
  mutate(
    profesion = case_when(oficio == 2131 ~ "Biologos, Botánicos, zoólogos y afines",
                              oficio == 2141 ~ "Ingenieros industriales y de producción",
                              oficio == 2142 ~ "Ingenieros civiles",
                              oficio == 2143 ~ "Ingenieros medioambientales",
                              oficio == 2161 ~ "Arquitectos",
                              oficio == 2211 ~ "Médicos generales",
                              oficio == 2221 ~ "Profesionales en enfermeria",
                              oficio == 2261 ~ "Odontólogos",
                              oficio == 2310 ~ "Profesionales de instituciones de educación superior",
                              oficio == 2411 ~ "Contadores y auditores financieros",
                              oficio == 2431 ~ "Profesionales de la publicidad y la comercialización",
                              oficio == 2512 ~ "Desarrolladores de software",
                              oficio == 2611 ~ "Abogados",
                              oficio == 2631 ~ "Economistas",
                              oficio == 2634 ~ "Psicólogos",
      ))

# To convert "tipo_vivienda" in factor using words, not numbers.
datos <- datos %>%
  mutate(
    vivienda = case_when(tipo_vivienda == 1 ~ "Casa",
                    tipo_vivienda == 2 ~ "Apartamento",
                    tipo_vivienda == 3 ~ "Cuartos en inquilinato",
                    tipo_vivienda == 4 ~ "Cuartos en otra estructura",
                    tipo_vivienda == 5 ~ "Vivienda indigena",
                    tipo_vivienda == 6 ~ "Otra vivienda",
        ))    

# To convert "material_paredes" in factor using words, not numbers.
datos <- datos %>%
  mutate(
    mat_paredes = case_when(material_paredes == 1 ~ "Ladrillo, bloque, material prefabricado, pieda",
                    material_paredes == 2 ~ "Madera pulida",
                    material_paredes == 3 ~ "Adobe o tapia pisada",
                    material_paredes == 4 ~ "Bahareque",
                    material_paredes == 5 ~ "Madera burda, tabla, tablón",
                    material_paredes == 6 ~ "Guadua",
                    material_paredes == 7 ~ "Caña, otro tipo de material vegetal",
                    material_paredes == 8 ~ "zinc, tela, carton, latas, plástico",
                    material_paredes == 9 ~ "sin paredes",
        ))

# To convert "material_pisos" in factor using words, not numbers.
  datos <- datos %>%
    mutate(
      mat_pisos = case_when(material_paredes == 1 ~ "Ladrillo, bloque, material prefabricado, pieda",
                                material_pisos == 2 ~ "Madera pulida",
                                material_pisos == 3 ~ "Adobe o tapia pisada",
                                material_pisos == 4 ~ "Bahareque",
                                material_pisos == 5 ~ "Madera burda, tabla, tablón",
                                material_pisos == 6 ~ "Guadua",
                                material_pisos == 7 ~ "Caña, otro tipo de material vegetal",
        ))

# To convert "energia_electrica" in factor using words, not numbers.
# Es afiliado, cotizante o beneficiario en salud
datos <- datos %>%
  mutate(electricidad = ifelse(energia_electrica == 0, "No", "Si"))       

# To convert "estrato_tarifa" in factor using words, not numbers.
datos <- datos %>%
  mutate(
    estrato_tarif = case_when(estrato_tarifa == 0 ~ "conexión pirata",
                          estrato_tarifa == 1 ~ "Bajo - bajo",
                          estrato_tarifa == 2 ~ "Bajo",
                          estrato_tarifa == 3 ~ "Medio - bajo",
                          estrato_tarifa == 4 ~ "Medio",
                          estrato_tarifa == 5 ~ "Medio - alto",
                          estrato_tarifa == 6 ~ "alto",
                          estrato_tarifa == 9 ~ "No sabe o cuenta con planta electrica",
    ))

# To convert "gas_natural" in factor using words, not numbers.
# Es afiliado, cotizante o beneficiario en salud
datos <- datos %>%
  mutate(gas_natural = ifelse(gas_natural == 0, "No", "Si"))

# To convert "alcantarillado" in factor using words, not numbers.
# Es afiliado, cotizante o beneficiario en salud
datos <- datos %>%
  mutate(alcantarillado = ifelse(alcantarillado == 0, "No", "Si"))

# To convert "acueducto" in factor using words, not numbers.
# Es afiliado, cotizante o beneficiario en salud
datos <- datos %>%
  mutate(acueducto = ifelse(acueducto == 0, "No", "Si")) 
    
# To convert "tipo_empleo" in factor using words, not numbers.
datos <- datos %>%
  mutate(
    empleo = case_when(cotizante_pension == 1 ~ "Empleo Privado",
                              cotizante_pension == 2 ~ "Empleo Público",
                              cotizante_pension == 3 ~ "Empleador",
        ))
    
# To convert "trabaja_cuenta_propia" in factor using words, not numbers.
# Es afiliado, cotizante o beneficiario en salud
datos <- datos %>%
  mutate(independiente = ifelse(trabaja_cuenta_propia == 0, "No", "Si"))


# Saving the new dataset --------------------------------------------------

# Voy a guardar la nueva base de datos para poder usarla luego en los
# analisis posteriores

#saveRDS(datos, file="datos")
datos <- readRDS(file="Data/datos")


# Exploratory analysis ----------------------------------------------------

library(dplyr)
library(ggplot2)

datos |> select(oficio) |> table()

# Density for the response variable Y
ggplot(datos, aes(x=ingresos)) + 
  geom_density() + 
  xlab("Salary [million of pesos]") +
  ylab("Densidad")

# Boxplot between Y and X's
ggplot(datos, aes(x=edu, y=ingresos)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Estado civil") +
  ylab("Salary [million of pesos]")

ggplot(datos, aes(x=est_civ, y=log10(ingresos))) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Estado civil") +
  ylab("Logarithm of salary [million of pesos]")

# Scatterplot between Y and X's
ggplot(datos, aes(x=edad, y=ingresos)) +
  geom_point() +
  xlab("Edad") +
  ylab("Salary [million of pesos]")


 
# Scatterplot between Experiencia and Ingresos
ggplot(datos, aes(x=experiencia, y=ingresos)) +
  geom_point() +
  xlab("Meses de experiencia") +
  ylab("Salary [million of pesos]")
  

# Scatterplot Meses trabajados en el ultimo año and Ingresos
ggplot(datos, aes(x=meses_trabajados, y=ingresos)) +
  geom_point() +
  xlab("Meses Trabajados en el ultimo año") +
  ylab("Salary [million of pesos]"), 

# Scatterplot horas_trabajadas and Ingresos
ggplot(datos, aes(x=horas_trabajadas, y=ingresos)) +
  geom_point() +
  xlab("Horas trabajadas por semana") +
  ylab("Salary [million of pesos]")

# Scatterplot Ocupados and Ingresos
ggplot(datos, aes(x=ocupados, y=ingresos)) +
  geom_point() +
  xlab("Personas ocupadas") +
  ylab("Salary [million of pesos]")

# Scatterplot numero_personas and Ingresos
ggplot(datos, aes(x=numero_personas, y=ingresos)) +
  geom_point() +
  xlab("Número de personas en el hogar") +
  ylab("Salary [million of pesos]")

# Scatterplot pob_may18 and Ingresos
ggplot(datos, aes(x=pob_may18, y=ingresos)) +
  geom_point() +
  xlab("Personas mayores de 18 en el hogar") +
  ylab("Salary [million of pesos]")

# Scatterplot  and Ingresos
ggplot(datos, aes(x=pob, y=ingresos)) +
  geom_point() +
  xlab("Horas trabajadas por semana") +
  ylab("Salary [million of pesos]")

# Scatterplot menores 5 años en el hogar and Ingresos
ggplot(datos, aes(x=menores_5annos, y=ingresos)) +
  geom_point() +
  xlab("Personas menores a 5 años en el hogar") +
  ylab("Salary [million of pesos]")

# Scatterplot mayores 60 años en el hogar and Ingresos
ggplot(datos, aes(x=mayores_60annos, y=ingresos)) +
  geom_point() +
  xlab("Personas mayores a 60 años en el hogar") +
  ylab("Salary [million of pesos]")

# Scatterplot personas ocupadas en el hogar  and Ingresos
ggplot(datos, aes(x=n_ocupados , y=ingresos)) +
  geom_point() +
  xlab(" personas ocupadas en el hogar ") +
  ylab("Salary [million of pesos]")

# cualitativas -----------------------------------------------------------------

# Boxplot between Ingresos and regimen contributivo
ggplot(datos, aes(x=reg, y=ingresos)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Regimen contributivo") +
  ylab("Salary [million of pesos]")

# Boxplot between Ingresos and Educacion
ggplot(datos, aes(x=edu, y=ingresos)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab(" Nivel educativo ") +
  ylab("Salary [million of pesos]")

# Boxplot between Ingresos and Departamentos
ggplot(datos, aes(x=dptos, y=ingresos)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab(" dptos ") +
  ylab("Salary [million of pesos]")


# Boxplot between Ingresos and Alfabetismo
ggplot(datos, aes(x=alfabetismo, y=ingresos)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab(" Sabe leer y escribir ") +
  ylab("Salary [million of pesos]")

# Boxplot between Ingresos and sexo
ggplot(datos, aes(x=sexo, y=ingresos)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab(" Sexo ") +
  ylab("Salary [million of pesos]")

# Boxplot between Ingresos and Salud
ggplot(datos, aes(x=salud, y=ingresos)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab(" Salud ") +
  ylab("Salary [million of pesos]")

# Boxplot between Ingresos and Cotizante Pension
ggplot(datos, aes(x=pension, y=ingresos)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab(" Cotizante a Pension ") +
  ylab("Salary [million of pesos]")


# Boxplot between Ingresos and Profesion
ggplot(datos, aes(x=profesion, y=ingresos)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Profesiones") +
  ylab("Salary [million of pesos]")


# Boxplot between Ingresos and vivienda
ggplot(datos, aes(x=vivienda, y=ingresos)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Tipo de vivienda") +
  ylab("Salary [million of pesos]")

# Boxplot between Ingresos and Material de paredes
ggplot(datos, aes(x=mat_paredes, y=ingresos)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab(" Material paredes de la vivienda ") +
  ylab("Salary [million of pesos]")

