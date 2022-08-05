library(plm)
library(ggplot2)

# Datos
dataLA <- rio::import(here::here("data", "BaseGrowth.xlsx"))
dataLA[7:ncol(dataLA)] <- sapply(dataLA[7:ncol(dataLA)], as.numeric)


# Modelos con efectos fijos y aleatorios
fixed.plm <- plm(log(vol_gdppc) ~ tyh_seats + I(tyh_seats^2) + FCF_VarPorc +
                         lag(vol_gdppc) + election + log_inflation + vol_spending +
                         Trade_openness_VarPorc  +lib_dem_index  + dist_mag_mean + Enph_Schmidt, 
                 data = dataLA, index=c("code_country", "year"), 
                 model="within", effects ="twoways")

random.plm <- plm(log(vol_gdppc) ~ tyh_seats + I(tyh_seats^2) + FCF_VarPorc +
                          lag(vol_gdppc) + election + log_inflation + vol_spending +
                          Trade_openness_VarPorc  +lib_dem_index  + dist_mag_mean + Enph_Schmidt, 
                 data = dataLA, index=c("code_country", "year"), 
                 model="random", effects ="twoways")



summary(fixed.plm)
summary(random.plm)


### Tests ####
# VIF test
car::vif(lm(vol_gdppc ~ tyh_seats + I(tyh_seats^2) + FCF_VarPorc +
                    lag(vol_gdppc) + election + log_inflation + vol_spending +
                    Trade_openness_VarPorc  +lib_dem_index  + dist_mag_mean + Enph_Schmidt, data=dataLA))

lmtest::bptest(fixed.plm) # Breusch-Pagan Test
plm::pdwtest(fixed.plm) # Durbin-Watson Test

# Hausman test (h0= random model is preferred)
plm::phtest(fixed.plm,random.plm,  vcov = function(x) vcovHC(x, method = "arellano"))


# Estimación con errores robustos a la heterocedasticidad
fixed.plm <- summary(fixed.plm, vcov = function(x) vcovHC(x, method = "arellano"))
fixed.plm

Fe.model.fitted <- cbind(fixed.plm$model, 
                         resid=fixed.plm$residuals, 
                         fitted=plm:::fitted_exp.plm(fixed.plm))


# Gráficos

Gr <- ggplot(Fe.model.fitted, aes(x=fitted, y=resid)) +
        geom_point(color="gray40",size=0.5) +
        geom_smooth(method = "lm", col = "blue",formula = y~poly(x,2),se=F)+
        labs(title = "") +
        xlab("Fitted Values")+
        ylab("Residuals")+
        theme_minimal() +
        theme(plot.margin = margin(10,30,10,10),
              axis.text.x = element_text(angle = -0, vjust = 0, hjust = 0, size = 6),
              axis.text.y = element_text(size = 6),
              text=element_text(size=10, family="Arial"))


Gi <- ggplot(Fe.model.fitted, aes(x=tyh_seats, y=fitted)) +
        geom_point(color="gray40", size=0.5) +
        geom_smooth(method = "lm", col = "blue",formula = y~poly(x,2),se=F)+
        labs(title = "") +
        xlab("Taylor y Herman's polarization index")+
        ylab("Residuals")+
        theme_minimal() +
        theme(plot.margin = margin(10,30,10,10),
              axis.text.x = element_text(angle = -0, vjust = 0, hjust = 0, size = 6),
              axis.text.y = element_text(size = 6),
              text=element_text(size=10, family="Arial"))


jpeg(filename = "Figures/GraphResiduals.jpg", width = 1900, height = 1900, res = 300)
Gr
dev.off()

jpeg(filename = "Figures/GraphIndex.jpg", width = 1900, height = 1900, res = 300)
Gi
dev.off()









