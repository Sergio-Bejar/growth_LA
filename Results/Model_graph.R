## Modelo Basico... falta graficar los effectos de "vdem_pol"
library(ggplot2)
library(sandwich)
library(lmtest)
dataLA <- read.csv(here::here("data", "LA_Growth.csv"))

#### Modelo ####
dataLA$polarization <- ((5/6.297)*(dataLA$vdem_pol))-((5/6.297)*(-3.304))

fixed.plm <- plm::plm(vol_growth~ lag(vol_growth) + polarization + I(polarization^2) + 
                              lib_dem_index +dist_mag_mean + election + domesticcredit +
                              vol_inf + spending + cbi_g, data = dataLA, 
                      index=c("code_country", "year"), model="within")
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



#### Modelo 2 ####
fixed.plm2 <- plm::plm(log(vol_growth) ~ lag(log(vol_growth)) + polarization + I(polarization^2) + 
                               lib_dem_index +dist_mag_mean + election + domesticcredit +
                               vol_inf + spending + cbi_g, data = dataLA, 
                       index=c("code_country", "year"), model="within", effect = "individual")

lmtest::bptest(fixed.plm2)
summary(fixed.plm2)
coeftest(fixed.plm2, vcov.=vcovHC(fixed.plm2, type="HC1")) 


hist(fixed.plm$residuals)


Fe.model.fitted2 <- cbind(fixed.plm2$model, 
                          resid=fixed.plm2$residuals, 
                          fitted=plm:::fitted_exp.plm(fixed.plm2))



ggplot(Fe.model.fitted2, aes(x=fitted, y=resid)) +
        geom_point(color="gray40") +
        geom_smooth(method = "lm", col = "blue",formula = y~poly(x,2),se=F)+
        theme_minimal()


ggplot(Fe.model.fitted2, aes(x=polarization, y=resid)) +
        geom_point(color="gray40") +
        geom_smooth(method = "lm", col = "blue",formula = y~poly(x,2),se=F)+
        theme_minimal()

qqnorm(Fe.model.fitted2$resid)
qqline(Fe.model.fitted2$resid)




# Sin venezuela (outlier)
dataLAX <- dataLA[dataLA$country!="Venezuela",]

fixed.plm3 <- plm::plm(vol_growth ~ lag(vol_growth) + polarization + I(polarization^2) + 
                               lib_dem_index +dist_mag_mean + election + domesticcredit +
                               inflation + spending + cbi_g, data = dataLAX, 
                       index=c("code_country"), model="within", effect = "twoways")

lmtest::bptest(fixed.plm3)
summary(fixed.plm3)
coeftest(fixed.plm3, vcov.=vcovHC(fixed.plm3, type="HC1")) 

Fe.model.fitted3 <- cbind(fixed.plm3$model, 
                          resid=fixed.plm3$residuals, 
                          fitted=plm:::fitted_exp.plm(fixed.plm3))

ggplot(Fe.model.fitted3, aes(x=fitted, y=resid)) +
        geom_point(color="gray40") +
        geom_smooth(method = "lm", col = "blue",formula = y~poly(x,2),se=F)+
        theme_minimal()


