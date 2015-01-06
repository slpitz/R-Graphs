library(ggplot2)
library(grid)
library(gridExtra)
nmmaps<-read.csv("automated.csv", as.is=T)
head(nmmaps)
#g<-ggplot(nmmaps, aes(Julian, CH4))+geom_point(color="firebrick")
autoch4<-nmmaps[nmmaps$CH4flag=="Y",]
g<-ggplot(autoch4, aes(Julian, CH4, color=factor(Chamber)))+geom_point()
g<-g+ggtitle('Stem Methane Flux')
g<-g+labs(x="Julian Date", y=expression(paste("Stem Methane Flux ( ", mu ~ g/m^2 , "h )")), title="Stem Methane Flux")
g

autoco2<-nmmaps[nmmaps$CO2flag=="Y",]
c<-ggplot(nmmaps, aes(Julian, CO2, color=factor(Chamber)))+geom_point()
#c<-c+ggtitle('Stem Respiration')
c<-c+labs(x="Julian Date", y=expression(paste("Stem Respiration ( CO2", mu ~ g/m^2 , "h )")), title="Stem Respiration")
c
d<-grid.arrange(g, c, nrow=2, main = "Stem Gas Fluxes")
ggsave("plot.pdf", width=4, height=4)

autofacet<-read.csv("automatedFacet.csv", as.is=T)
gf<-ggplot(autofacet, aes(Julian, Flux, color=factor(Chamber)))+geom_point()
gf
gf + facet_grid(Gas ~ ., scales="free_y")
gf<-gf+labs(x="Julian Date", y=expression(paste("Gas Fluxes (", mu ~ g/m^2 , "h )")), title="Stem Gas Fluxes")
