library(plm)
library(ggplot2)

dataLA <- rio::import(here::here("data", "BaseGrowth.xlsx"))


fixed.plm <- plm(log(vol_gdppc) ~ lag(vol_gdppc) + tyh_seats + I(tyh_seats^2) + 
                         vol_spending  + vol_inf + FCF_VarPorc + FDI_VarPorc+
                         cbi_g + lib_dem_index + dist_mag_mean, 
                 data = dataLA, index=c("code_country", "year"), 
                 model="within", effects ="twoways")


summary(fixed.plm)
lmtest::bptest(fixed.plm) # Breusch-Pagan Test

# Estimación con errores robustos a la heterocedasticidad
fixed.plm <- summary(fixed.plm, vcov = function(x) vcovHC(x, method = "arellano"))
fixed.plm

Fe.model.fitted <- cbind(fixed.plm$model, 
                         resid=fixed.plm$residuals, 
                         fitted=plm:::fitted_exp.plm(fixed.plm))


ggplot(Fe.model.fitted, aes(x=fitted, y=resid)) +
        geom_point(color="gray40") +
        geom_smooth(method = "lm", col = "blue",formula = y~poly(x,2),se=F)+
        theme_minimal()



# Gráfico
jpeg(filename = "Figures/Graph.jpg", 
     width = 1900, height = 1900, res = 300)
ggplot(Fe.model.fitted, aes(x=fitted, y=resid)) +
        geom_point(color="gray40") +
        geom_smooth(method = "lm", col = "blue",formula = y~poly(x,2),se=F)+
        theme_minimal()
dev.off()









