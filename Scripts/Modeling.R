
library(haven)
datos <- read_dta("Datos/GEIH_Intermedia_step1.dta")

# Some conversions --------------------------------------------------------

datos$inglabo <- datos$inglabo / 1000000 # To show the "ingresos" in millions

datos$estado_civil <- as.factor(datos$estado_civil)


# Filtering ---------------------------------------------------------------
library(dplyr)

# Filtering by "Desarrolladores de software" or 2512
datos |> filter(oficio == 2512) -> dt

# To quantify the number of NA in the dataframe

colSums(apply(dt, MARGIN=2, FUN=is.na))

# To drop rows with NA
dt <- na.omit(dt)


# Modeling ----------------------------------------------------------------
library(gamlss)

mod0 <- gamlss(inglabo ~ edad + estado_civil + experiencia + tipo_empleo, 
               family=GA, data=dt)

summary(mod0)
plot(mod0)
Rsq(mod0)

mod1 <- gamlss(inglabo ~ edad + estado_civil + experiencia + tipo_empleo, 
               sigma.fo = ~ edad + estado_civil + experiencia,
               family=NO, data=dt)

summary(mod1)
plot(mod1)
Rsq(mod1)


