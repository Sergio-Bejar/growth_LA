

library(WDI)
WDI::WDIsearch("gdp per capita")

countries <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", 
               "Ecuador", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua",
               "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela, RB")

Prv <- WDI::WDI(indicator ="NE.GDI.FPRV.CN", start = 1991, end=2019 )
Tot <- WDI::WDI(indicator ="NE.GDI.FTOT.KD", start = 1991, end=2019 )

Tot <- Tot[Tot$country %in% countries,]

gdppc <- WDI::WDI(indicator ="NY.GDP.PCAP.KD", start = 1980, end=2019)
gdppc <- gdppc[gdppc$country %in% countries,]




rio::export(Tot, "Formación de capital.xlsx")

rio::export(gdppc, "GDPpc_WDI.xlsx")


        