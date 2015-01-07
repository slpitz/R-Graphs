#R script to create
#Author: Scott Pitz
# Started: 2015-01-05
library(ggplot2)
library(grid)
library(gridExtra)
nmmaps<-read.csv("automated.csv", as.is=T) #load files
head(nmmaps) #show beginning of file
#g<-ggplot(nmmaps, aes(Julian, CH4))+geom_point(color="firebrick")

#CH4 only graph
autoch4<-nmmaps[nmmaps$CH4flag=="Y",] # drop rows where flux R2 <0.80
g<-ggplot(autoch4, aes(Julian, CH4, color=factor(Chamber)))+geom_point()
g<-g+ggtitle('Stem Methane Flux')
g<-g+labs(x="Julian Date", y=expression(paste("Stem Methane Flux ( ", mu ~ g/m^2 , "h )")), title="Stem Methane Flux")
g

#CO2 only graph
autoco2<-nmmaps[nmmaps$CO2flag=="Y",]
c<-ggplot(nmmaps, aes(Julian, CO2, color=factor(Chamber)))+geom_point()
#c<-c+ggtitle('Stem Respiration')
c<-c+labs(x="Julian Date", y=expression(paste("Stem Respiration ( CO2", mu ~ g/m^2 , "h )")), title="Stem Respiration")
c

#use grid.arrange to stack graphs, doesnt line up x-axis
#d<-grid.arrange(g, c, nrow=2, main = "Stem Gas Fluxes")
#ggsave("plot.pdf", width=4, height=4)

#rearrange data so all fluxes in one column
autofacet<-read.csv("automatedFacet.csv", as.is=T)
gf<-ggplot(autofacet, aes(Julian, Flux, color=factor(Chamber)))+geom_point()
#gf+theme(legend.title = element_text(colour="chocolate", size=16, face="bold"))+
  #scale_color_discrete(name="This color is\ncalled chocolate!?")
gf<-gf+labs(x="Julian Date", y=expression(paste("Gas Fluxes (", mu ~ g/m^2 , "h )")), title="Stem Gas Fluxes")
gf + facet_grid(Gas ~ ., scales="free_y") #scales="free_y" allows each graph's y-axis to scale independently

#transect Data
transect<-read.csv("AllData.csv", as.is=T) #load files
head(transect) #show beginning of file
transect$DateF<-as.Date(transect$DateF)
all<-ggplot(transect, aes(DateF, Methane, color=factor(Type)))+geom_point()
all

#2014 data
data2014<-transect[transect$Year=="2014",]
plot2014<-ggplot(data2014, aes(DateF, Methane, color=factor(Type)))+geom_point()
plot2014<-plot2014+labs(x="Date", y=expression(paste("Gas Fluxes (", mu ~ g/m^2 , "h )")), title="Stem Gas Fluxes")
plot2014<-plot2014+theme(legend.title = element_text(colour="chocolate", size=16, face="bold"))+scale_color_discrete(name="Flux Type")
plot2014
 