
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

# From the last output we identify that BCTo, GB2, exGAUS and GG 
# are the four best distributions for "ingresos" marginally.


# LOGNO model -------------------------------------------------------------

# horizonte será una fórmula que contiene la estructura más
# compleja que permitimos para modelar un parámetro
# BCTo model -------------------------------------------------------------

# horizonte será una fórmula que contiene la estructura más
# compleja que permitimos para modelar un parámetro
horizonte <- formula(~edad + pension + 
                       profesion + est_civ + meses_trabajados + numero_personas
                        + edu)

# Modelo base BCTo
m1_BCTo <- gamlss(ingresos ~ 1, family=BCTo, data=dt)

# Modelo final con seleccion de variables automático
m2_BCTo <- stepGAICAll.B(m1_BCTo,
                          scope=list(lower= ~ 1, upper=horizonte),
                          sigma.scope=list(lower= ~ 1, upper=horizonte)
                          )

summary(m2_BCTo)
plot(m2_BCTo)
Rsq(m2_BCTo)

# GB2 model -------------------------------------------------------------
# Modelo base 
m1_GB2 <- gamlss(ingresos ~ 1, family=GB2, data=dt)

# Modelo final con seleccion de variables automático
m2_GB2 <- stepGAICAll.B(m1_GB2,
                          scope=list(lower= ~ 1, upper=horizonte),
                          sigma.scope=list(lower= ~ 1, upper=horizonte)
)

summary(m2_GB2)
Rsq(m2_GB2)

# exGAUS model -------------------------------------------------------------

# Modelo base exGAUS
m1_exGAUS <- gamlss(ingresos ~ 1, family=exGAUS, data=dt)

# Modelo final con seleccion de variables automático
m2_exGAUS <- stepGAICAll.B(m1_exGAUS,
                       scope=list(lower= ~ 1, upper=horizonte),
                       sigma.scope=list(lower= ~ 1, upper=horizonte)
)
 
summary(m2_exGAUS)
Rsq(m2_exGAUS)

# GG model -------------------------------------------------------------

# Modelo base GG
m1_GG <- gamlss(ingresos ~ 1, family=GG, data=dt)

# Modelo final con seleccion de variables automático
m2_GG <- stepGAICAll.B(m1_GG,
                        scope=list(lower= ~ 1, upper=horizonte),
                        sigma.scope=list(lower= ~ 1, upper=horizonte)
)

summary(m2_BCCG)
Rsq(m2_BCCG)
