library(ggplot2)

# Datos
dataLA <- rio::import(here::here("data", "BaseGrowth.xlsx"))
dataLA[7:ncol(dataLA)] <- sapply(dataLA[7:ncol(dataLA)], as.numeric)



ggplot(data=dataLA, aes(x=year, y=vol_growth, fill=country)) +
        facet_wrap("country") +
        geom_col() +
        ylim(0,20)+
        theme(plot.margin = margin(10,30,10,10),
              axis.text.x = element_text(angle = -0, vjust = 0, hjust = 0, size = 6),
              axis.text.y = element_text(size = 6),
              legend.position = "none",
              text=element_text(size=10, family="Cambria"))




ggplot(data=dataLA, aes(x=year, y=vol_growth, fill=country)) +
        geom_bar(position = "dodge", stat = "identity") +
        ylim(0,20)+
        guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
        scale_x_continuous(breaks = pretty(dataLA$year, n = 10))+
        theme(plot.margin = margin(10,30,10,10),
              axis.text.x = element_text(angle = -0, vjust = 0, hjust = 0, size = 6),
              axis.text.y = element_text(size = 6),
              legend.position = "bottom",
              text=element_text(size=10, family="Cambria"))



dataG <- doBy::summary_by(dataLA, vol_growth~iso3c, FUN=c(mean, sd), na.rm=T)


Gr <- ggplot(data=dataG, aes(x=iso3c, y=vol_growth.mean)) +
        geom_bar(position = "dodge", stat = "identity", fill="#E69F00") + 
        xlab("")+
        ylab("vol_growth (mean)")+
        geom_errorbar(aes(ymin=vol_growth.mean-vol_growth.sd, ymax=vol_growth.mean+vol_growth.sd), width=.1)
        theme_minimal()+
                theme(plot.margin = margin(10,30,10,10),
                      axis.text.x = element_text(angle = -30, vjust = 2, hjust = 1, size = 6),
                      text=element_text(size=10, family="Cambria")))

        
jpeg(filename = "Figures/vol_growthMEAN.jpg", width = 2200, height = 1200, res = 300)
Gr
dev.off()
        
        