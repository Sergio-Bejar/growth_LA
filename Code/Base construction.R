# Data extraction

#V-DEM

VDEMbase <- vdemdata::vdem

names <- c("Country", "ISO3c", "Year", "Pol_polarization", "Soc_polarization", "Democratic", "Inst_Demo",
           "Lib_dem_score", "Polyarchy_score", "District_mag", "Party_Inst")

VDEM <- with(VDEMbase, cbind.data.frame(country_name, country_text_id, year,
                         v2cacamps, v2smpolsoc, e_boix_regime, e_democ, 
                         v2x_libdem, v2x_polyarchy, v2elloeldm, v2xps_party ))
names(VDEM) <- names

# WDI

names2 <- c("Country", "Year",
            "GDPpc", "GDP_growth", "GDPpc_growth", 
            "Inflation", "Gini", "Unemployment", 
            "Trade_GDP", "GFCF_constUSD", 
            "GFCF_GDP", "GFCF_growth", "Gob_expend_const",
            "Gob_expend_growth", "Gob_expend_GDP", 
            "ISO3c", "Region", "Income")

WDIbase <- WDI::WDI(indicator =c("NY.GDP.PCAP.KD", "NY.GDP.MKTP.KD.ZG", "5.51.01.10.gdp",
                                 "FP.CPI.TOTL.ZG", "SI.POV.GINI", "SL.UEM.TOTL.NE.ZS",
                                 "NE.TRD.GNFS.ZS",  "NE.GDI.FTOT.KD",
                                 "NE.GDI.FTOT.ZS", "NE.GDI.FTOT.KD.ZG", "NE.CON.GOVT.KD",
                                 "NE.CON.GOVT.KD.ZG","NE.CON.GOVT.ZS"), extra = T)


WDI <- WDIbase[, -c(1, 4, 5, 21, 22, 23, 25)]
names(WDI) <- names2

lagpad <- function(x, k) {
        if (k>0) {
                return (c(rep(NA, k), x)[1 : length(x)] );
        }
        else {
                return (c(x[(-k+1) : length(x)], rep(NA, -k)));
        }
}

WDI <- dplyr::arrange(WDI, Country, Year)
WDI <- WDI |> dplyr::mutate(GDPpclag = lagpad(GDPpc,1)) # El año 1960 debe eliminarse
WDI$GDPpcVar <- 100*((WDI$GDPpc - WDI$GDPpclag)/(WDI$GDPpclag))

#Eliminto las entradas que no pueden ser contadas para la variación
WDI <- WDI[!(WDI$Year == 1960),]
WDI <- WDI[!(WDI$Year == 2004 & WDI$Country == "Turkey"),]

##KA openness
KAop <- rio::import(here::here("data", "kaopen_2020.dta"))

KAop <- KAop[,-1]
names(KAop) <- c("ISO3c", "Country", "Year", "KAopen", "KA_open")

# Join

Base <- dplyr::full_join(VDEM, WDI)
Base <- dplyr::full_join(Base, KAop)

rio::export(Base,here::here("data","GVOL_Dataset.csv"))
