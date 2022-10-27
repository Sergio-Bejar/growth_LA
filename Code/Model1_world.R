Base <- rio::import(here::here("data", "GVOL_Dataset.csv"))

library(ggplot2)
library(plm)

Data <- Base
Data <- Data[Data$Year>1970,]
#Data <- Data[Data$Democratic==1,]

Data$Pol_polarization <- scales::rescale(Data$Pol_polarization, c(1,10))
Data <- unique(Data, by = c("Country", "Year"))
lagpad <- function(x, k) {
        if (k>0) {
                return (c(rep(NA, k), x)[1 : length(x)] );
        }
        else {
                return (c(x[(-k+1) : length(x)], rep(NA, -k)));
        }
}
Data$GDPpcVarlagged <- lagpad(Data$GDPpcVar,1)


fixed.plm <- plm(log(GDPpcVar) ~ Pol_polarization + I(Pol_polarization^2) +
                         Inflation + KA_open + GFCF_growth + GDPpclag +
                         Party_Inst + Polyarchy_score,
                 data = Data, index=c("Country", "Year"), 
                 model="within", effects ="twoways")


summary(fixed.plm)
lmtest::bptest(fixed.plm)


Fe.model.fitted <- cbind(fixed.plm$model, 
                         resid=fixed.plm$residuals, 
                         fitted=plm:::fitted_exp.plm(fixed.plm))



Gr <- ggplot(Fe.model.fitted, aes(x=fitted, y=resid)) +
        geom_point(color="gray40",size=0.5) +
        geom_smooth(method = "lm", col = "blue",formula = y~poly(x,2),se=F)+
        labs(title = "") +
        xlab("Fitted Values")+
        ylab("Residuals")+
        theme_minimal() +
        theme(plot.margin = margin(10,30,10,10),
              axis.text.x = element_text(angle = -0, vjust = 0, hjust = 0, size = 6),
              axis.text.y = element_text(size = 10),
              text=element_text(size=14, family="Arial"))
Gr
