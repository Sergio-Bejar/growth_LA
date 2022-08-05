## Modelo Basico... falta graficar los effectos de "vdem_pol"
library(ggplot2)
library(sandwich)
library(lmtest)
dataLA <- read.csv(here::here("data", "LA_Growth.csv"))
dataLA <- rio::import(here::here("data", "BaseGrowth.xlsx"))


#### Modelo ####

dataLA$polarization <- ((5/6.297)*(dataLA$vdem_pol))-((5/6.297)*(-3.304))

fixed.plm <- plm::plm(vol_growth ~ lag(vol_growth) + tyh_seats + I(tyh_seats^2)+
                              lib_dem_index +dist_mag_mean + election + 
                              log(vol_inf) + spending + trade, data = dataLA, 
                      index=c("code_country", "year"), model="within", effect="individual")
summary(fixed.plm)

Fe.model.fitted <- cbind(fixed.plm$model, 
                          resid=fixed.plm$residuals, 
                          fitted=plm:::fitted_exp.plm(fixed.plm))


ggplot(Fe.model.fitted, aes(x=fitted, y=resid)) +
        geom_point(color="gray40") +
        geom_smooth(method = "lm", col = "blue",formula = y~poly(x,2),se=F)+
        theme_minimal()

# El gráfico tiene forma de embudo, muestra problemas de heterocedasticidad

ggplot(Fe.model.fitted, aes(x=polarization, y=resid)) +
        geom_point(color="gray40") +
        geom_smooth(method = "lm", col = "blue",formula = y~poly(x,2),se=F)+
        theme_minimal()


# Breusch-Pagan Test
lmtest::bptest(fixed.plm) # El modelo tiene problemas de heterocedasticidad

# Estimación con errores robustos
coeftest(fixed.plm, vcov.=vcovHC(fixed.plm, type="HC1")) # Pierde significación pol^2

X <- summary(fixed.plm, vcov = function(x) vcovHC(x, method = "arellano"))

Fe.model.fittedX <- cbind(X$model, 
                         resid=X$residuals, 
                         fitted=plm:::fitted_exp.plm(X))

ggplot(Fe.model.fittedX, aes(x=fitted, y=resid)) +
        geom_point(color="gray40") +
        geom_smooth(method = "lm", col = "blue",formula = y~poly(x,2),se=F)+
        theme_minimal()

