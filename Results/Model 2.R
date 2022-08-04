dataLA <- rio::import(here::here("data", "BaseGrowth.xlsx"))



fixed.plm <- plm(vol_gdppc ~ lag(vol_gdppc) + tyh_seats + I(tyh_seats^2) + 
                         lib_dem_index + vol_spending + dist_mag_mean + 
                         vol_inf + election + domesticcredit + cbi_g, 
                 data = dataLA, index=c("code_country", "year"), 
                 model="within", effects ="twoways")

summary(fixed.plm)


lmtest::bptest(fixed.plm) 


Fe.model.fitted <- cbind(fixed.plm$model, 
                         resid=fixed.plm$residuals, 
                         fitted=plm:::fitted_exp.plm(fixed.plm))


ggplot(Fe.model.fitted, aes(x=fitted, y=resid)) +
        geom_point(color="gray40") +
        geom_smooth(method = "lm", col = "blue",formula = y~poly(x,2),se=F)+
        theme_minimal()

