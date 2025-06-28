
# Loading the modified data -----------------------------------------------
datos <- readRDS(file="Data/datos")

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

# Exploring the best 4 distributions for ingresos
fits <- fitDist(y=dt$ingresos, type="realplus")
fits$fits

# From the last output we identify that LOGNO, IG, BCCG and GG
# are the four best distributions for "ingresos" marginally.


mod0 <- gamlss(ingresos ~ edad + estado_civil + experiencia + tipo_empleo, 
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


