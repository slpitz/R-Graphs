#R script to create graphs for transect data
#Author: Scott Pitz
# Started: 2015-01-05
library(ggplot2)
library(grid)
library(gridExtra)
library(plyr)

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
#facet_grid separates data into tiles
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
transect["Eco"] <- NA # That creates the new column filled with "NA"
head(transect)
#if(transect$TreeID=="10127") transect$Eco=="mid"
transect$Eco[transect$TreeID==10127] <- "mid"
transect$Eco[transect$TreeID==10206] <- "wetland"
transect$Eco[transect$TreeID==10210] <- "wetland"
transect$Eco[transect$TreeID==10212] <- "wetland"
transect$Eco[transect$TreeID==10214] <- "wetland"
transect$Eco[transect$TreeID==10315] <- "wetland"
transect$Eco[transect$TreeID==10316] <- "wetland"
transect$Eco[transect$TreeID==10319] <- "wetland"

transect$Eco[transect$TreeID==12000] <- "mid"
transect$Eco[transect$TreeID==12001] <- "upland"
transect$Eco[transect$TreeID==12002] <- "upland"
transect$Eco[transect$TreeID==12003] <- "upland"
transect$Eco[transect$TreeID==12004] <- "mid"
transect$Eco[transect$TreeID==12005] <- "mid"
transect$Eco[transect$TreeID==12006] <- "upland"
transect$Eco[transect$TreeID==12007] <- "mid"
transect$Eco[transect$TreeID==12008] <- "upland"
transect$Eco[transect$TreeID==12009] <- "mid"
transect$Eco[transect$TreeID==12010] <- "upland"
transect$Eco[transect$TreeID==12011] <- "upland"
transect$Eco[transect$TreeID==12012] <- "upland"
transect$Eco[transect$TreeID==12013] <- "upland"
transect$Eco[transect$TreeID==12014] <- "upland"
transect$Eco[transect$TreeID==12015] <- "upland"
transect$Eco[transect$TreeID==12016] <- "upland"
transect$Eco[transect$TreeID==12017] <- "upland"
transect$Eco[transect$TreeID==12018] <- "upland"
transect$Eco[transect$TreeID==12019] <- "upland"
transect$Eco[transect$TreeID==12020] <- "upland"
transect$Eco[transect$TreeID==12021] <- "upland"

#change rounds
transect$Round[transect$DateF=="2014-05-22"] <- 7
transect$Round[transect$DateF=="2014-05-23"] <- 7
transect$Round[transect$DateF=="2014-06-20"] <- 8
transect$Round[transect$DateF=="2014-07-08"] <- 9
transect$Round[transect$DateF=="2014-07-10"] <- 9
transect$Round[transect$DateF=="2014-08-20"] <- 10
transect$Round[transect$DateF=="2014-08-21"] <- 10
transect$Round[transect$DateF=="2014-08-28"] <- 10
transect$Round[transect$DateF=="2014-08-29"] <- 10
transect$Round[transect$DateF=="2014-08-30"] <- 10
transect$Round[transect$DateF=="2014-09-17"] <- 11
transect$Round[transect$DateF=="2014-09-18"] <- 11
transect$Round[transect$DateF=="2014-09-19"] <- 11
transect$Round[transect$DateF=="2014-09-22"] <- 11

all<-ggplot(transect, aes(DateF, Methane, color=factor(Type)))+geom_point()
all

#2014 data
data2014<-transect[transect$Year=="2014",]
plot2014<-ggplot(data2014, aes(DateF, Methane, color=factor(Type)))+geom_point()
plot2014<-plot2014+labs(x="Date", y=expression(paste("Gas Fluxes (", mu ~ g/m^2 , "h )")), title="Stem Gas Fluxes")
plot2014<-plot2014+theme(legend.title = element_text(colour="chocolate", size=16, face="bold"))+scale_color_discrete(name="Flux Type")
plot2014

#2014 data - Moisture plot- All Data
moistureplot<-ggplot(data2014, aes(SoilMoisture1, Methane, color=factor(Type)))+geom_point()
moistureplot<-moistureplot+labs(x="Soil Moisture (VMC%)", y=expression(paste("Gas Fluxes (", mu ~ g/m^2 , "h )")), title="Stem Gas Fluxes")
moistureplot<-moistureplot+theme(legend.title = element_text(colour="chocolate", size=16, face="bold"))+scale_color_discrete(name="Flux Type")
moistureplot

#2014 data - Soil Moisture plot
soildata2014<-data2014[data2014$Type=="soil",]
soilmoistureplot<-ggplot(soildata2014, aes(SoilMoisture1, Methane, color=factor(Type)))+geom_point()
soilmoistureplot<-soilmoistureplot+labs(x="Soil Moisture (VMC%)", y=expression(paste("Soil Methane Flux (", mu ~ g/m^2 , "h )")), title="Soil Gas Fluxes")
soilmoistureplot<-soilmoistureplot+theme(legend.title = element_text(colour="chocolate", size=16, face="bold"))+scale_color_discrete(name="Flux Type")
soilmoistureplot

#2014 data - Tree Moisture plot
treedata2014<-data2014[data2014$Type=="tree",]
treemoistureplot<-ggplot(treedata2014, aes(SoilMoisture1, Methane, color=factor(Type)))+geom_point()
treemoistureplot<-treemoistureplot+labs(x="Soil Moisture (VMC%)", y=expression(paste("2014-Stem Methane Flux (", mu ~ g/m^2 , "h )")), title="Stem Gas Fluxes")
treemoistureplot<-treemoistureplot+theme(legend.title = element_text(colour="chocolate", size=16, face="bold"))+scale_color_discrete(name="Flux Type")
treemoistureplot

#transform data
transect$logMoist<-log10(transect$SoilMoisture1)
transect$logMethaneFlux<-transect$Methane
transect$logMethaneFlux<-log10(transect$logMethaneFlux)
#transect$logMethaneFlux<-transect$logMethaneFlux-3
data2014<-transect[transect$Year=="2014",]

#2014 data - Tree log Moisture plot
treedata2014<-data2014[data2014$Type=="tree",]
treelogmoisture<-ggplot(treedata2014, aes(logMoist, Methane))+geom_point()
treelogmoisture<-treelogmoisture+labs(x="log (Soil Moisture (VMC%))", y=expression(paste("Stem Methane Flux (", mu ~ g/m^2 , "h )")), title="2014 - Stem Gas Flux v. log(VMC)")
treelogmoisture

#2014 data - log Tree log Moisture plot
transect$logMoist<-log10(transect$SoilMoisture1)
treedata2014<-data2014[data2014$Type=="tree",]
logtreelogmoisture<-ggplot(treedata2014, aes(logMoist, logMethaneFlux))+geom_point()
logtreelogmoisture<-logtreelogmoisture+labs(x="log (Soil Moisture (VMC%))", y=expression(paste("log(Stem Methane Flux (", mu ~ g/m^2 , "h ))")), title="2014 - log(Stem Gas Flux) v. log(VMC)")
logtreelogmoisture

#2014 upland data
data2014<-transect[transect$Year=="2014",]
upplot<-data2014[data2014$Eco=="upland",]
up<-ggplot(upplot, aes(DateF, Methane, color=factor(Type)))+geom_point()
up<-up+labs(x="Date", y=expression(paste("Gas Fluxes (", mu ~ g/m^2 , "h )")), title="2014 - Stem Gas Fluxes - Upland All RSQ")
up<-up+theme(legend.title = element_text(colour="chocolate", size=16, face="bold"))+scale_color_discrete(name="Flux Type")
up

# Run the functions length, mean, and sd on the value of "flux" for each group, 
# broken down by type, round
MethaneSum <- ddply(data2014, c("Type", "Round", "Eco"), summarise,
               N    = length(Methane),
               mean = mean(Methane),
               sd   = sd(Methane),
               se   = sd / sqrt(N) )
MethaneSum

DateSum <- ddply(data2014, c("Type", "Round", "Eco"), summarise,
                    N    = length(DateF),
                    mean = mean(DateF),
                    sd   = sd(DateF),
                    se   = sd / sqrt(N) )
DateSum

# merge two data frames by Round and Eco
total <- merge(MethaneSum, DateSum, by=c("Round","Eco","Type"))
total

#Plot summaries by "ecosystem" data
sup<-ggplot(total, aes(mean.y, mean.x, color=factor(Type)))+geom_point(aes(shape=Eco),size = 4)+geom_errorbar(aes(ymin=mean.x-se.x, ymax=mean.x+se.x), width=.1)
  #+geom_point(aes(shape=Eco),size = 4)
sup<-sup+labs(x="Date", y=expression(paste("Gas Fluxes (", mu ~ g/m^2 , "h )")), title="2014 - Stem Gas Fluxes - All Ecosystems - All RSQ")
sup<-sup+theme(legend.title = element_text(colour="chocolate", size=16, face="bold"))+scale_color_discrete(name="Flux Type")
sup

#Plot summaries by "ecosystem" data
uptotal<-total[total$Eco=="upland",]
supup<-ggplot(uptotal, aes(mean.y, mean.x, color=factor(Type)))+geom_point(aes(shape=Eco),size = 4)+geom_errorbar(aes(ymin=mean.x-se.x, ymax=mean.x+se.x), width=.1)
#+geom_point(aes(shape=Eco),size = 4)
supup<-supup+labs(x="Date", y=expression(paste("Gas Fluxes (", mu ~ g/m^2 , "h )")), title="2014 - Stem Gas Fluxes - Upland All RSQ")
supup<-supup+theme(legend.title = element_text(colour="chocolate", size=16, face="bold"))+scale_color_discrete(name="Flux Type")
supup

