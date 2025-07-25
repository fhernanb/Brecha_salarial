
# Loading the modified data -----------------------------------------------
datos <- readRDS(file="Data/datos")

# Filtering ---------------------------------------------------------------
library(dplyr)

# Filtering by "Desarrolladores de software" or 2512
datos |> filter(oficio == 2512) -> dt

# To drop rows with NA
dt <- na.omit(dt)

# To eliminate 0 from ingresos
ind <- dt$ingresos > 0
dt <- dt[ind, ]

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
horizonte <- formula(~ sexo + edad + est_civ + reg + edu)

# Modelo base BCTo
m1_LOGNO <- gamlss(ingresos ~ 1, family=LOGNO, data=dt)

# Modelo final con seleccion de variables automático
m2_LOGNO <- NULL
m2_LOGNO <- stepGAICAll.B(m1_LOGNO,
                          scope=list(lower= ~ 1, upper=horizonte),
                          sigma.scope=list(lower= ~ 1, upper=horizonte)
                          )

summary(m2_LOGNO)
Rsq(m2_LOGNO)
plot(m2_LOGNO)

# IG model --------------------------------------------------------------
# Modelo base 
m1_IG <- gamlss(ingresos ~ 1, family=IG, data=dt)

# Modelo final con seleccion de variables automático
m2_IG <- stepGAICAll.B(m1_IG,
                       scope=list(lower= ~ 1, upper=horizonte),
                       sigma.scope=list(lower= ~ 1, upper=horizonte)
)

summary(m2_IG)
Rsq(m2_IG)

# GG model ------------------------------------------------------------
# Modelo base 
m1_GG <- gamlss(ingresos ~ 1, family=GG, data=dt,
                control=gamlss.control(n.cyc=5000, trace=FALSE))

# Modelo final con seleccion de variables automático
m2_GG <- stepGAICAll.B(m1_GG,
                       scope=list(lower= ~ 1, upper=horizonte),
                       sigma.scope=list(lower= ~ 1, upper=horizonte),
                       nu.scope=list(lower= ~ 1, upper=horizonte)
)

summary(m2_GG)
Rsq(m2_GG)




# NO model ------------------------------------------------------------

horizonte <- formula(~sexo + edad + experiencia + meses_trabajados)

m1_NO <- gamlss(ingresos ~ 1, family=NO, data=dt)

# Modelo final con seleccion de variables automático
m2_NO <- stepGAICAll.B(m1_NO,
                       scope=list(lower= ~ 1, upper=horizonte),
                       sigma.scope=list(lower= ~ 1, upper=horizonte)
)

summary(m2_NO)
Rsq(m2_NO)

mod1 <- gamlss(ingresos ~ edad + meses_trabajados,
               sigma.fo= ~ edad,
               family=NO, data=dt)

summary(mod1)
Rsq(mod1)
plot(mod1)
