
# Loading the modified data -----------------------------------------------
datos <- readRDS(file="Data/datos")

# Filtering ---------------------------------------------------------------
library(dplyr)

# Filtering by "Desarrolladores de software" or 2512
datos |> filter(oficio == 2512) -> dt

# To drop rows with NA
dt <- na.omit(dt)

# Modeling ----------------------------------------------------------------
library(gamlss)

# Exploring the best 4 distributions for ingresos
fits <- fitDist(y=dt$ingresos, type="realplus")
fits$fits

# From the last output we identify that LOGNO, IG, BCCG and GG
# are the four best distributions for "ingresos" marginally.


# LOGNO model -------------------------------------------------------------

# horizonte será una fórmula que contiene la estructura más
# compleja que permitimos para modelar un parámetro
horizonte <- formula(~edad + est_civ + reg + edu)

# Modelo base
m1_logno <- gamlss(ingresos ~ 1, family=LOGNO, data=dt)

# Modelo final con seleccion de variables automático
m2_logno <- stepGAICAll.B(m1_logno,
                          scope=list(lower= ~ 1, upper=horizonte),
                          sigma.scope=list(lower= ~ 1, upper=horizonte)
                          )

summary(m2_logno)
plot(m2_logno)
Rsq(m2_logno)



