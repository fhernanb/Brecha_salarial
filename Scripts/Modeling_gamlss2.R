
# Loading the modified data -----------------------------------------------
datos <- readRDS(file="Data/datos")

# Filtering ---------------------------------------------------------------
library(dplyr)

# To select the variables
datos |> select(ingresos, 
                sexo, edu, est_civ,
                edad, experiencia,
                profesion) -> dt

# To drop rows with NA
dt <- na.omit(dt)

# To eliminate 0 from ingresos
ind <- dt$ingresos > 0
dt <- dt[ind, ]


# Para ver las profesiones
table(dt$profesion)

# Ing Civil ---------------------------------------------------------------

subdat <- filter(dt, profesion=="Ingenieros civiles")

# Para organizar los niveles de las variables cuali
table(subdat$edu)
subdat$edu <- as.factor(subdat$edu)
subdat$edu <- relevel(subdat$edu, ref="Universitaria")
table(subdat$edu)

table(subdat$est_civ)
subdat$est_civ<- as.factor(subdat$est_civ)
subdat$est_civ <- relevel(subdat$est_civ, ref="Soltero")
table(subdat$est_civ)

# Para eliminar 10% de las observaciones extremas
loc_outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.05, na.rm = TRUE)
  q3 <- quantile(x, 0.95, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  ind <- x >= lower & x <= upper
  ind
}

ind <- loc_outliers_iqr(subdat$ingresos)
subdat <- subdat[ind, ]

# Graficos para incluir en los slides
library(ggplot2)

p0 <- ggplot(subdat, aes(x=ingresos)) + 
  geom_density() + 
  xlab("Ingreso [millones de pesos]") +
  ylab("Densidad")

p0

p1 <- ggplot(subdat, aes(x=sexo, y=ingresos)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Sexo") +
  ylab("Ingreso [millones de pesos]")

p1

# Save the plot
ggsave("densi_ingreso_ing_civ.png", plot=p0, width=6, height=4, dpi=300)
ggsave("ingreso_sexo_ing_civ.png", plot=p1, width=6, height=4, dpi=300)

# Modeling ----------------------------------------------------------------

# Para instalar gamlss2 visitar el link
# https://gamlss-dev.github.io/gamlss2/

library(gamlss)
library(gamlss2)

# Entrenando el modelo

# Formula para NO, GA e IG
f <- ingresos ~ edad + experiencia + sexo + edu + est_civ | .

# Formula para EXP
f <- ingresos ~ edad + experiencia + sexo + edu + est_civ

mod <- gamlss2(f, data=subdat, family=NO, K=2)

# Para ver la tabla resumen del modelo
summary(mod)

# Para ver analisis de residuales junto
plot(mod)

# Para ver analisis de residuales por separado
plot(mod, which = "hist-resid")
plot(mod, which = "qq-resid")
plot(mod, which = "wp-resid")
plot(mod, which = "scatter-resid")

# Dos metricas importantes. 
# Entre mayor Rsq mejor
# Entre menor AIC mejor
Rsq(mod)
AIC(mod)

# Para obtener la correlacion entre y y y_hat (prediccion)
new <- subdat
pre <- predict(mod, type="parameter", newdata=new)

# Cuidado al calcular la correlacion, leer los mensajes de abajo
cor(pre[, 1], new$ingresos) # Para NO, GA y IG
cor(pre, new$ingresos)      # Para EXP solamente

plot(x=pre[, 1], y=new$ingresos,
     xlim=c(0, 10), ylim=c(0, 10))

abline(a=0, b=1, lty="dashed", col="red", lwd=2)


# xxxxxxx ----------------------------------------------------------------

# Jorge Ivan, por favor repita lo anterior para economía.
# La idea es hacer lo mismo.

subdat <- filter(dt, profesion=="Economistas")

# Para organizar los niveles de las variables cuali
table(subdat$edu)
subdat$edu <- as.factor(subdat$edu)
subdat$edu <- relevel(subdat$edu, ref="Universitaria")
table(subdat$edu)

table(subdat$est_civ)
subdat$est_civ<- as.factor(subdat$est_civ)
subdat$est_civ <- relevel(subdat$est_civ, ref="Soltero")
table(subdat$est_civ)

# Para eliminar 10% de las observaciones extremas
loc_outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.05, na.rm = TRUE)
  q3 <- quantile(x, 0.95, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  ind <- x >= lower & x <= upper
  ind
}

ind <- loc_outliers_iqr(subdat$ingresos)
subdat <- subdat[ind, ]

# Graficos para incluir en los slides
library(ggplot2)

p0 <- ggplot(subdat, aes(x=ingresos)) + 
  geom_density() + 
  xlab("Ingreso [millones de pesos]") +
  ylab("Densidad")

p0

p1 <- ggplot(subdat, aes(x=sexo, y=ingresos)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Sexo") +
  ylab("Ingreso [millones de pesos]")

p1

# Save the plot
ggsave("densi_ingreso_ing_civ.png", plot=p0, width=6, height=4, dpi=300)
ggsave("ingreso_sexo_ing_civ.png", plot=p1, width=6, height=4, dpi=300)

# Modeling ----------------------------------------------------------------

# Para instalar gamlss2 visitar el link
# https://gamlss-dev.github.io/gamlss2/

library(gamlss)
library(gamlss2)

# Entrenando el modelo

# Formula para NO, GA e IG
f <- ingresos ~ edad + experiencia + sexo + edu + est_civ | .

# Formula para EXP
f <- ingresos ~ edad + experiencia + sexo + edu + est_civ

mod <- gamlss2(f, data=subdat, family=NO, K=2)

# Para ver la tabla resumen del modelo
summary(mod)

# Para ver analisis de residuales junto
plot(mod)

# Para ver analisis de residuales por separado
plot(mod, which = "hist-resid")
plot(mod, which = "qq-resid")
plot(mod, which = "wp-resid")
plot(mod, which = "scatter-resid")

# Dos metricas importantes. 
# Entre mayor Rsq mejor
# Entre menor AIC mejor
Rsq(mod)
AIC(mod)

# Para obtener la correlacion entre y y y_hat (prediccion)
new <- subdat
pre <- predict(mod, type="parameter", newdata=new)

# Cuidado al calcular la correlacion, leer los mensajes de abajo
cor(pre[, 1], new$ingresos) # Para NO, GA y IG
cor(pre, new$ingresos)      # Para EXP solamente

plot(x=pre[, 1], y=new$ingresos,
     xlim=c(0, 10), ylim=c(0, 10))

abline(a=0, b=1, lty="dashed", col="red", lwd=2)


