#### Amphora Code ####

#### Preamble ####

library(readxl) 
library(ggplot2)
amphora <- read_excel("~/amphora.xlsx")
View(amphora)

### Number of vessels by period ###

amphora <- read_excel("~/amphora.xlsx")
amphora <- amphora[amphora$period != "Unknown", ]
amphora$period <- factor(amphora$period,levels = c("Late Republic", "1st-2nd Cent. AD", "3rd-5th Cent. AD"))
ggplot(amphora, aes(x=period)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Number of Amphora by Period", x="Period", y="Number of Amphora") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


###################################################################

#### The Late Republic ####

### Vessel Forms by Provenance ###

amphora <- read_excel("~/amphora.xlsx")
laterepublic <- subset(amphora, period=="Late Republic")
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
laterepublic2 <- laterepublic %>%
  group_by(period,origin,type) %>%
  summarise(n=n()) %>%
  group_by(period) %>% 
  mutate(perc=100*n/sum(n))
View(laterepublic2)
ggplot(laterepublic2, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="Late Republic Percentage per Vessel", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

adriatic <- subset(laterepublic2, origin=="Adriatic")
ggplot(adriatic, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="Late Republic Adriatic Vessels", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

eastern <- subset(laterepublic2, origin=="Eastern")
ggplot(eastern, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="Late Republic Eastern Vessels", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

iberian <- subset(laterepublic2, origin=="Iberian")
ggplot(iberian, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="Late Republic Iberian Vessels", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

tyrrhenian <- subset(laterepublic2, origin=="Tyrrhenian")
ggplot(tyrrhenian, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="Late Republic Tyrrhenian Vessels", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

african <- subset(laterepublic2, origin=="North Africa")
ggplot(african, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="Late Republic North African Vessels", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))



### Total Provenance for period ###

amphora <- read_excel("~/amphora.xlsx")
laterepublic <- subset(amphora, period=="Late Republic")
amphora <- amphora[amphora$origin != "Unknown", ]
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
laterepublic <- laterepublic[laterepublic$site != "Acqui Terme", ]
laterepublic <- laterepublic[laterepublic$site != "Alba", ]
laterepublic <- laterepublic[laterepublic$site != "Altinum", ]
laterepublic <- laterepublic[laterepublic$site != "Ariminum", ]
laterepublic <- laterepublic[laterepublic$site != "Augusta Bagiennorum", ]
laterepublic <- laterepublic[laterepublic$site != "Brescia", ]
laterepublic <- laterepublic[laterepublic$site != "Como", ]
laterepublic <- laterepublic[laterepublic$site != "Este", ]
laterepublic <- laterepublic[laterepublic$site != "Industria", ]
laterepublic <- laterepublic[laterepublic$site != "Novara", ]
laterepublic <- laterepublic[laterepublic$site != "Oderzo", ]
laterepublic <- laterepublic[laterepublic$site != "Porto Maurizio", ]
laterepublic <- laterepublic[laterepublic$site != "Reggio Emilia", ]
laterepublic <- laterepublic[laterepublic$site != "Trento", ]
laterepublic <- laterepublic[laterepublic$site != "Vercelli", ]
laterepublic <- laterepublic[laterepublic$origin != "Unknown", ]
laterepublic2 <- laterepublic %>% 
  group_by(material,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(laterepublic2)
ggplot(laterepublic2, aes(x = factor(material), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +  labs(title="Provenance of Amphorae during the Late Republic",x = "", y = NULL, fill = "Provenance") +  theme_minimal(base_size = 14) + scale_fill_manual(values = c("#CDC08C", "#02401B", "#A2A475", "#81A88D", "#972D15"))

### Provenance by Site ###

amphora <- read_excel("~/amphora.xlsx")
laterepublic <- subset(amphora, period=="Late Republic")
amphora <- amphora[amphora$origin != "Unknown", ]
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
laterepublic <- laterepublic[laterepublic$site != "Acqui Terme", ]
laterepublic <- laterepublic[laterepublic$site != "Alba", ]
laterepublic <- laterepublic[laterepublic$site != "Altinum", ]
laterepublic <- laterepublic[laterepublic$site != "Ariminum", ]
laterepublic <- laterepublic[laterepublic$site != "Augusta Bagiennorum", ]
laterepublic <- laterepublic[laterepublic$site != "Brescia", ]
laterepublic <- laterepublic[laterepublic$site != "Como", ]
laterepublic <- laterepublic[laterepublic$site != "Este", ]
laterepublic <- laterepublic[laterepublic$site != "Industria", ]
laterepublic <- laterepublic[laterepublic$site != "Novara", ]
laterepublic <- laterepublic[laterepublic$site != "Oderzo", ]
laterepublic <- laterepublic[laterepublic$site != "Porto Maurizio", ]
laterepublic <- laterepublic[laterepublic$site != "Reggio Emilia", ]
laterepublic <- laterepublic[laterepublic$site != "Trento", ]
laterepublic <- laterepublic[laterepublic$site != "Vercelli", ]
laterepublic <- laterepublic[laterepublic$origin != "Unknown", ]
laterepublic2 <- laterepublic %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(laterepublic2)
laterepublic2$site <- factor(laterepublic2$site,levels = c("Aquileia", "Padua", "Vicenza", "Forli", "Modena", "Verona", "Calvatone", "Cremona", "Milan", "Como", "Ivrea", "Luna"))
ggplot(laterepublic2, aes(x = factor(site), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(title="Late Republic Amphora Provenance by Site",x = "", y = NULL, fill = "Origin") +  theme_minimal(base_size = 14) + scale_fill_manual(values = c("#CDC08C", "#02401B", "#A2A475", "#81A88D", "#972D15")) + theme(axis.text.x = element_text(angle = 90))

### Hierarchical Clustering (no ports) ###

library(readxl) 
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
amphora <- read_excel("~/amphora.xlsx")
amphora <- amphora[amphora$origin != "Unknown", ]
laterepublic <- subset(amphora, period=="Late Republic")
laterepublic <- laterepublic[laterepublic$site != "Aquileia", ]
laterepublic <- laterepublic[laterepublic$site != "Luna", ]
laterepublic <- laterepublic[laterepublic$site != "Acqui Terme", ]
laterepublic <- laterepublic[laterepublic$site != "Alba", ]
laterepublic <- laterepublic[laterepublic$site != "Altinum", ]
laterepublic <- laterepublic[laterepublic$site != "Ariminum", ]
laterepublic <- laterepublic[laterepublic$site != "Augusta Bagiennorum", ]
laterepublic <- laterepublic[laterepublic$site != "Brescia", ]
laterepublic <- laterepublic[laterepublic$site != "Como", ]
laterepublic <- laterepublic[laterepublic$site != "Este", ]
laterepublic <- laterepublic[laterepublic$site != "Industria", ]
laterepublic <- laterepublic[laterepublic$site != "Novara", ]
laterepublic <- laterepublic[laterepublic$site != "Oderzo", ]
laterepublic <- laterepublic[laterepublic$site != "Porto Maurizio", ]
laterepublic <- laterepublic[laterepublic$site != "Reggio Emilia", ]
laterepublic <- laterepublic[laterepublic$site != "Trento", ]
laterepublic <- laterepublic[laterepublic$site != "Vercelli", ]
laterepublic2 <- laterepublic %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
laterepublicMatrix <- acast(laterepublic2, site~origin, value.var="perc", fill=0)
View(laterepublicMatrix)
amphorapercrep <- as.data.frame(laterepublicMatrix)
library(ggdendro)
View(amphorapercrep)
library(ggplot2)
library(ggdendro)
measures <- amphorapercrep[,1:5]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
amphorapercrepHClust <- hclust(distMeasures, method="average")
ggdendrogram(amphorapercrepHClust, rotate=T) + labs(title="Hierarchical Clustering in the Late Republic based on Amphora Provenance", x="", y="Distance between assemblages") + theme_minimal()

### Hierarchical Clustering Ligurian Port ###

library(readxl) 
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
amphora <- read_excel("~/amphora.xlsx")
amphora <- amphora[amphora$origin != "Unknown", ]
laterepublic <- subset(amphora, period=="Late Republic")
laterepublic <- laterepublic[laterepublic$site != "Aquileia", ]
laterepublic <- laterepublic[laterepublic$site != "Acqui Terme", ]
laterepublic <- laterepublic[laterepublic$site != "Alba", ]
laterepublic <- laterepublic[laterepublic$site != "Altinum", ]
laterepublic <- laterepublic[laterepublic$site != "Ariminum", ]
laterepublic <- laterepublic[laterepublic$site != "Augusta Bagiennorum", ]
laterepublic <- laterepublic[laterepublic$site != "Brescia", ]
laterepublic <- laterepublic[laterepublic$site != "Como", ]
laterepublic <- laterepublic[laterepublic$site != "Este", ]
laterepublic <- laterepublic[laterepublic$site != "Industria", ]
laterepublic <- laterepublic[laterepublic$site != "Novara", ]
laterepublic <- laterepublic[laterepublic$site != "Oderzo", ]
laterepublic <- laterepublic[laterepublic$site != "Porto Maurizio", ]
laterepublic <- laterepublic[laterepublic$site != "Reggio Emilia", ]
laterepublic <- laterepublic[laterepublic$site != "Trento", ]
laterepublic <- laterepublic[laterepublic$site != "Vercelli", ]
laterepublic2 <- laterepublic %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
laterepublicMatrix <- acast(laterepublic2, site~origin, value.var="perc", fill=0)
View(laterepublicMatrix)
amphorapercrep <- as.data.frame(laterepublicMatrix)
library(ggdendro)
View(amphorapercrep)
library(ggplot2)
library(ggdendro)
measures <- amphorapercrep[,1:5]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
amphorapercrepHClust <- hclust(distMeasures, method="average")
ggdendrogram(amphorapercrepHClust, rotate=T) + labs(title="Hierarchical Clustering in the Late Republic based on Amphora Provenance with Ligurian port", x="", y="Distance between assemblages") + theme_minimal()

### Hierarchical Clustering with Adriatic ports ###

library(readxl) 
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
amphora <- read_excel("~/amphora.xlsx")
amphora <- amphora[amphora$origin != "Unknown", ]
laterepublic <- subset(amphora, period=="Late Republic")
laterepublic <- laterepublic[laterepublic$site != "Luna", ]
laterepublic <- laterepublic[laterepublic$site != "Acqui Terme", ]
laterepublic <- laterepublic[laterepublic$site != "Alba", ]
laterepublic <- laterepublic[laterepublic$site != "Altinum", ]
laterepublic <- laterepublic[laterepublic$site != "Ariminum", ]
laterepublic <- laterepublic[laterepublic$site != "Augusta Bagiennorum", ]
laterepublic <- laterepublic[laterepublic$site != "Brescia", ]
laterepublic <- laterepublic[laterepublic$site != "Como", ]
laterepublic <- laterepublic[laterepublic$site != "Este", ]
laterepublic <- laterepublic[laterepublic$site != "Industria", ]
laterepublic <- laterepublic[laterepublic$site != "Novara", ]
laterepublic <- laterepublic[laterepublic$site != "Oderzo", ]
laterepublic <- laterepublic[laterepublic$site != "Porto Maurizio", ]
laterepublic <- laterepublic[laterepublic$site != "Reggio Emilia", ]
laterepublic <- laterepublic[laterepublic$site != "Trento", ]
laterepublic <- laterepublic[laterepublic$site != "Vercelli", ]
laterepublic2 <- laterepublic %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
laterepublicMatrix <- acast(laterepublic2, site~origin, value.var="perc", fill=0)
View(laterepublicMatrix)
amphorapercrep <- as.data.frame(laterepublicMatrix)
library(ggdendro)
View(amphorapercrep)
library(ggplot2)
library(ggdendro)
measures <- amphorapercrep[,1:5]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
amphorapercrepHClust <- hclust(distMeasures, method="average")
ggdendrogram(amphorapercrepHClust, rotate=T) + labs(title="Hierarchical Clustering in the Late Republic based on Amphora Provenance with Adriatic port", x="", y="Distance between assemblages") + theme_minimal()

### Simpson's Index ###

amphora <- read_excel("~/amphora.xlsx")
amphora <- amphora[amphora$type != "Unknown", ]
amphora <- amphora[amphora$type != "Adriatic", ]
amphora <- amphora[amphora$type != "North African", ]
amphora <- amphora[amphora$type != "Eastern", ]
amphora <- amphora[amphora$type != "Gallic", ]
amphora <- amphora[amphora$type != "Tyrrhenian", ]
amphora <- amphora[amphora$type != "Iberian", ]
laterepublic <- subset(amphora, period=="Late Republic")
laterepublic <- laterepublic[laterepublic$site != "Acqui Terme", ]
laterepublic <- laterepublic[laterepublic$site != "Alba", ]
laterepublic <- laterepublic[laterepublic$site != "Altinum", ]
laterepublic <- laterepublic[laterepublic$site != "Ariminum", ]
laterepublic <- laterepublic[laterepublic$site != "Augusta Bagiennorum", ]
laterepublic <- laterepublic[laterepublic$site != "Brescia", ]
laterepublic <- laterepublic[laterepublic$site != "Como", ]
laterepublic <- laterepublic[laterepublic$site != "Este", ]
laterepublic <- laterepublic[laterepublic$site != "Industria", ]
laterepublic <- laterepublic[laterepublic$site != "Novara", ]
laterepublic <- laterepublic[laterepublic$site != "Oderzo", ]
laterepublic <- laterepublic[laterepublic$site != "Porto Maurizio", ]
laterepublic <- laterepublic[laterepublic$site != "Reggio Emilia", ]
laterepublic <- laterepublic[laterepublic$site != "Vercelli", ]
laterepublicMatrix <- acast(laterepublic, site~type, value.var="type", fill=0)
View(laterepublicMatrix)
library(vegan)
diversity(laterepublicMatrix, index="simpson")

### Morisita-Horn Overalp Coefficient ###

amphora <- read_excel("~/amphora.xlsx")
amphora <- amphora[amphora$type != "Unknown", ]
amphora <- amphora[amphora$type != "Adriatic", ]
amphora <- amphora[amphora$type != "North African", ]
amphora <- amphora[amphora$type != "Eastern", ]
amphora <- amphora[amphora$type != "Gallic", ]
amphora <- amphora[amphora$type != "Tyrrhenian", ]
amphora <- amphora[amphora$type != "Iberian", ]
laterepublic <- subset(amphora, period=="Late Republic")
laterepublic <- laterepublic[laterepublic$site != "Acqui Terme", ]
laterepublic <- laterepublic[laterepublic$site != "Alba", ]
laterepublic <- laterepublic[laterepublic$site != "Altinum", ]
laterepublic <- laterepublic[laterepublic$site != "Ariminum", ]
laterepublic <- laterepublic[laterepublic$site != "Augusta Bagiennorum", ]
laterepublic <- laterepublic[laterepublic$site != "Brescia", ]
laterepublic <- laterepublic[laterepublic$site != "Como", ]
laterepublic <- laterepublic[laterepublic$site != "Este", ]
laterepublic <- laterepublic[laterepublic$site != "Industria", ]
laterepublic <- laterepublic[laterepublic$site != "Novara", ]
laterepublic <- laterepublic[laterepublic$site != "Oderzo", ]
laterepublic <- laterepublic[laterepublic$site != "Porto Maurizio", ]
laterepublic <- laterepublic[laterepublic$site != "Reggio Emilia", ]
laterepublic <- laterepublic[laterepublic$site != "Trento", ]
laterepublic <- laterepublic[laterepublic$site != "Vercelli", ]
library(reshape2)
laterepublicMatrix <- acast(laterepublic, site~type, value.var="type", fill=0)
View(laterepublicMatrix)
library(vegan)
repDistance <- vegdist(laterepublicMatrix, method="horn")
repDistance
repDistanceMatrix <- as.matrix(repDistance)
rep <- melt(repDistanceMatrix, varnames=c("site1", "site2"), value.name="distance")
library(ggplot2)
ggplot(rep, aes(x=site1, y=site2, fill=distance, label=round(distance, 2))) + geom_raster() + labs(title="Morisita-Horn Overlap Coefficient", x = NULL, y = NULL, fill = "Distance") + geom_raster() + geom_text(color="white")
morisitaHClust <- hclust(repDistance, method="average")
library(ggdendro)
ggdendrogram(morisitaHClust, rotate=T) + labs(title="Vessel Similarity Between Sites Using Morisita-Horn Overlap Coefficient Late Republic", x="", y="Percentage Dissimilarity between assemblages") + theme_minimal()

#### Late Republic Stamps ####

library(readxl)
library(ggplot2)
stamps <- read_excel("~/stamps.xlsx")
republican <- subset(stamps, date=="Late Republic")

### Stamped Vessels ###

ggplot(republican, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Stamped Amphora Forms from the Late Republic", x="Type", y="Number of Amphora") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Morisita-Horn Overlap Coefficient ###

library(reshape2)
republicMatrix <- acast(republican, site~code, value.var="code", fill=0)
View(republicMatrix)
library(vegan)
repDistance <- vegdist(republicMatrix, method="horn")
repDistance
repDistanceMatrix <- as.matrix(impDistance)
rep <- melt(repDistanceMatrix, varnames=c("site1", "site2"), value.name="distance")
library(ggplot2)
ggplot(rep, aes(x=site1, y=site2, fill=distance, label=round(distance, 2))) + geom_raster() + labs(title="Morisita-Horn Overlap Coefficient", x = NULL, y = NULL, fill = "Distance") + geom_raster() + geom_text(color="white")
morisitaHClust <- hclust(repDistance, method="average")
library(ggdendro)
ggdendrogram(morisitaHClust, rotate=T) + labs(title="Stamp Similarity Between Sites Using Morisita-Horn Overlap Coefficient during the Late Republic", x="", y="Distance between assemblages") + theme_minimal()


###################################################################

#### The First-Second Centuries AD ####

### Vessel Forms by Provenance ###

amphora <- read_excel("~/amphora.xlsx")
imperial <- subset(amphora, period=="1st-2nd Cent. AD")
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
imperial2 <- imperial %>%
  group_by(period,origin,type) %>%
  summarise(n=n()) %>%
  group_by(period) %>% 
  mutate(perc=100*n/sum(n))
View(imperial2)
ggplot(imperial2, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="1st-2nd Cent. AD Percentage per Vessel", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

adriatic <- subset(imperial2, origin=="Adriatic")
ggplot(adriatic, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="First-Second Centuries Adriatic Vessels", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

eastern <- subset(imperial2, origin=="Eastern")
ggplot(eastern, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="First-Second Centuries Eastern Vessels", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

gallic <- subset(imperial2, origin=="Gallic")
ggplot(gallic, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="First-Second Centuries Gallic Vessels", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

iberian <- subset(imperial2, origin=="Iberian")
ggplot(iberian, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="First-Second Centuries Iberian Vessels", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

tyrrhenian <- subset(imperial2, origin=="Tyrrhenian")
ggplot(tyrrhenian, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="First-Second Centuries Tyrrhenian Vessels", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

african <- subset(imperial2, origin=="North Africa")
ggplot(african, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="First-Second Centuries North African Vessels", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

unknown <- subset(imperial2, origin=="Unknown")
ggplot(unknown, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="First-Second Centuries Unknown Vessels", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Total Provenance for period ###

amphora <- read_excel("~/amphora.xlsx")
imperial <- subset(amphora, period=="1st-2nd Cent. AD")
imperial <- imperial[imperial$origin != "Unknown", ]
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
imperial2 <- imperial %>% 
  group_by(material,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(imperial2)
ggplot(imperial2, aes(x = factor(material), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +  labs(title="Provenance of Amphorae during the first-second centuries AD",x = "", y = NULL, fill = "Provenance") +  theme_minimal(base_size = 14) + scale_fill_manual(values = c("#CDC08C", "#02401B", "#9C964A", "#A2A475", "#81A88D", "#972D15"))

### Provenance by Site ###

amphora <- read_excel("~/amphora.xlsx")
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
imperial <- subset(amphora, period=="1st-2nd Cent. AD")
imperial <- imperial[imperial$origin != "Unknown", ]
imperial2 <- imperial %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(imperial2)
imperial2$site <- factor(imperial2$site,levels = c("Ariminum", "Ravenna", "Altinum", "Aquileia", "Oderzo", "Padua", "Vicenza", "Este", "Forli", "Bologna", "Reggio Emilia", "Modena", "Verona", "Trento", "Calvatone", "Cremona", "Brescia", "Cividate Camuno", "Milan", "Como", "Novara", "Vercelli", "Industria", "Ivrea", "Alba", "Augusta Bagiennorum", "Acqui Terme", "Libarna", "Luna", "Porto Maurizio"))
ggplot(imperial2, aes(x = factor(site), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(title="First-Second Centuries AD Amphora Provenance by Site",x = "", y = NULL, fill = "Origin") +  theme_minimal(base_size = 14)  + scale_fill_manual(values = c("#CDC08C", "#02401B", "#9C964A", "#A2A475", "#81A88D", "#972D15")) + theme(axis.text.x = element_text(angle = 90))

### Hierarchical Clustering (no ports) ###

library(readxl) 
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
amphora <- read_excel("~/amphora.xlsx")
imperial <- subset(amphora, period=="1st-2nd Cent. AD")
imperial <- imperial[imperial$site != "Altinum", ]
imperial <- imperial[imperial$site != "Aquileia", ]
imperial <- imperial[imperial$site != "Ariminum", ]
imperial <- imperial[imperial$site != "Luna", ]
imperial <- imperial[imperial$site != "Porto Maurizio", ]
imperial <- imperial[imperial$site != "Ravenna", ]
imperial <- imperial[imperial$origin != "Unknown", ]
imperial2 <- imperial %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
imperialMatrix <- acast(imperial2, site~origin, value.var="perc", fill=0)
View(imperialMatrix)
amphorapercimp <- as.data.frame(imperialMatrix)
library(ggdendro)
View(amphorapercimp)
library(ggplot2)
library(ggdendro)
measures <- amphorapercimp[,1:6]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
amphorapercimpHClust <- hclust(distMeasures, method="average")
ggdendrogram(amphorapercimpHClust, rotate=T) + labs(title="Hierarchical Clustering in the first-second Centuries AD based on Amphora Provenance", x="", y="Distance between assemblages") + theme_minimal()

### Hierarchical Clustering (Adriatic ports) ###

library(readxl) 
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
amphora <- read_excel("~/amphora.xlsx")
imperial <- subset(amphora, period=="1st-2nd Cent. AD")
imperial <- imperial[imperial$site != "Luna", ]
imperial <- imperial[imperial$site != "Porto Maurizio", ]
imperial <- imperial[imperial$origin != "Unknown", ]
imperial2 <- imperial %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
imperialMatrix <- acast(imperial2, site~origin, value.var="perc", fill=0)
View(imperialMatrix)
amphorapercimp <- as.data.frame(imperialMatrix)
library(ggdendro)
View(amphorapercimp)
library(ggplot2)
library(ggdendro)
measures <- amphorapercimp[,1:6]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
amphorapercimpHClust <- hclust(distMeasures, method="average")
ggdendrogram(amphorapercimpHClust, rotate=T) + labs(title="Hierarchical Clustering in the first-second Centuries AD based on Amphora Provenance with Adriatic ports", x="", y="Distance between assemblages") + theme_minimal()

### Hierarchical Clustering (with Ligurian ports) ###

library(readxl) 
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
amphora <- read_excel("~/amphora.xlsx")
imperial <- subset(amphora, period=="1st-2nd Cent. AD")
imperial <- imperial[imperial$site != "Altinum", ]
imperial <- imperial[imperial$site != "Aquileia", ]
imperial <- imperial[imperial$site != "Ariminum", ]
imperial <- imperial[imperial$site != "Ravenna", ]
imperial <- imperial[imperial$origin != "Unknown", ]
imperial2 <- imperial %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
imperialMatrix <- acast(imperial2, site~origin, value.var="perc", fill=0)
View(imperialMatrix)
amphorapercimp <- as.data.frame(imperialMatrix)
library(ggdendro)
View(amphorapercimp)
library(ggplot2)
library(ggdendro)
measures <- amphorapercimp[,1:6]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
amphorapercimpHClust <- hclust(distMeasures, method="average")
ggdendrogram(amphorapercimpHClust, rotate=T) + labs(title="Hierarchical Clustering in the first-second Centuries AD based on Amphora Provenance with Ligurian ports", x="", y="Distance between assemblages") + theme_minimal()

### Simpson's Index ###

amphora <- read_excel("~/amphora.xlsx")
amphora <- amphora[amphora$type != "Unknown", ]
amphora <- amphora[amphora$type != "Adriatic", ]
amphora <- amphora[amphora$type != "North African", ]
amphora <- amphora[amphora$type != "Eastern", ]
amphora <- amphora[amphora$type != "Gallic", ]
amphora <- amphora[amphora$type != "Tyrrhenian", ]
amphora <- amphora[amphora$type != "Iberian", ]
imperial <- subset(amphora, period=="1st-2nd Cent. AD")
library(reshape2)
imperialMatrix <- acast(imperial, site~type, value.var="type", fill=0)
View(imperialMatrix)
library(vegan)
diversity(imperialMatrix, index="simpson")

### Morisita-Horn Overlap Coefficient ###

amphora <- read_excel("~/amphora.xlsx")
amphora <- amphora[amphora$type != "Unknown", ]
amphora <- amphora[amphora$type != "Adriatic", ]
amphora <- amphora[amphora$type != "North African", ]
amphora <- amphora[amphora$type != "Eastern", ]
amphora <- amphora[amphora$type != "Gallic", ]
amphora <- amphora[amphora$type != "Tyrrhenian", ]
amphora <- amphora[amphora$type != "Iberian", ]
imperial <- subset(amphora, period=="1st-2nd Cent. AD")
library(reshape2)
imperialMatrix <- acast(imperial, site~type, value.var="type", fill=0)
View(imperialMatrix)
library(vegan)
impDistance <- vegdist(imperialMatrix, method="horn")
impDistance
impDistanceMatrix <- as.matrix(impDistance)
imp <- melt(impDistanceMatrix, varnames=c("site1", "site2"), value.name="distance")
library(ggplot2)
ggplot(imp, aes(x=site1, y=site2, fill=distance, label=round(distance, 2))) + geom_raster() + labs(title="Morisita-Horn Overlap Coefficient", x = NULL, y = NULL, fill = "Distance") + geom_raster() + geom_text(color="white")
morisitaHClust <- hclust(impDistance, method="average")
library(ggdendro)
ggdendrogram(morisitaHClust, rotate=T) + labs(title="Vessel Similarity Between Sites Using Morisita-Horn Overlap Coefficient First-Second Centuries AD", x="", y="Distance between assemblages") + theme_minimal()

#### First-Second Centuries AD Stamps ####

library(readxl)
library(ggplot2)
stamps <- read_excel("~/stamps.xlsx")
imperial <- subset(stamps, date=="1st-2nd Cent. AD")

### Stamped Vessels ###

imperial <- subset(stamps, date=="1st-2nd Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Stamped Amphora Forms from the First-Second Centuries AD", x="Type", y="Number of Amphora") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Morisita-Horn Overlap Coefficient ###

library(reshape2)
imperialMatrix <- acast(imperial, site~code, value.var="code", fill=0)
View(imperialMatrix)
library(vegan)
impDistance <- vegdist(imperialMatrix, method="horn")
impDistance
impDistanceMatrix <- as.matrix(impDistance)
imp <- melt(impDistanceMatrix, varnames=c("site1", "site2"), value.name="distance")
library(dplyr)
library(ggplot2)
library(scales)
qn = quantile(imp$distance, c(0.01, 0.99), na.rm = TRUE)
qn01 <- rescale(c(qn, range(imp$distance)))
imp$site2 <- with(imp,factor(site2,levels = rev(sort(unique(site2)))))
ggplot(imp, aes(x=site1, y=site2, fill=distance, label=round(distance,2))) + geom_raster()+ geom_text(col="grey20")  + theme_bw() + scale_fill_gradientn(colours=c("indianred2", "steelblue2", "cornsilk"), values = c(0, seq(qn01[1], qn01[2], length.out = 18), 1.01), breaks=c(0.5,0.6,0.7,0.8,0.9,0.999), labels=c(0.5,0.6,0.7,0.8,0.9,1), limits=c(0.5, 1), na.value="transparent") + theme(panel.border=element_blank(), legend.position = "right", legend.direction="vertical", legend.title=element_blank(), legend.key.width=unit(0.02, "npc"), legend.key.height=unit(0.1, "npc"), axis.ticks.y=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_text(angle=45, hjust=0)) + xlab("") + ylab("") + scale_x_discrete(position="top")
morisitaHClust <- hclust(impDistance, method="average")
library(ggdendro)
ggdendrogram(morisitaHClust, rotate=T) + labs(title="Stamp Similarity Between Sites Using Morisita-Horn Overlap Coefficient during the First-Second Centuries AD", x="", y="Distance between assemblages") + theme_minimal()

###################################################################

#### The Third-Fifth Centuries AD ####

### Vessel Forms by Provenance ###

amphora <- read_excel("~/amphora.xlsx")
lateantique <- subset(amphora, period=="3rd-5th Cent. AD")
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
lateantique2 <- lateantique %>%
  group_by(period,origin,type) %>%
  summarise(n=n()) %>%
  group_by(period) %>% 
  mutate(perc=100*n/sum(n))
View(lateantique2)
ggplot(lateantique2, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="Third-Fifth Centuries AD Percentage per Vessel", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

eastern <- subset(lateantique2, origin=="Eastern")
ggplot(eastern, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="Third-Fifth Centuries Eastern Vessels", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

iberian <- subset(lateantique2, origin=="Iberian")
ggplot(iberian, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="Third-Fifth Centuries Iberian Vessels", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

african <- subset(lateantique2, origin=="North Africa")
ggplot(african, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="Third-Fifth Centuries North African Vessels", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

tyrrhenian <- subset(lateantique2, origin=="Tyrrhenian")
ggplot(tyrrhenian, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="Third-Fifth Centuries Tyrrhenian Vessels", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

unknown <- subset(lateantique2, origin=="Unknown")
ggplot(unknown, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + facet_wrap(~origin, scales="free_x") + geom_text(vjust=-1) + labs(title="Third-Fifth Centuries Unknown Vessels", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Total Provenance for period ###

amphora <- read_excel("~/amphora.xlsx")
lateantique <- subset(amphora, period=="3rd-5th Cent. AD")
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
lateantique <- lateantique[lateantique$site != "Acqui Terme", ]
lateantique <- lateantique[lateantique$site != "Ariminum", ]
lateantique <- lateantique[lateantique$site != "Augusta Bagiennorum", ]
lateantique <- lateantique[lateantique$site != "Calvatone", ]
lateantique <- lateantique[lateantique$site != "Cividate Camuno", ]
lateantique <- lateantique[lateantique$site != "Como", ]
lateantique <- lateantique[lateantique$site != "Cremona", ]
lateantique <- lateantique[lateantique$site != "Forli", ]
lateantique <- lateantique[lateantique$site != "Industria", ]
lateantique <- lateantique[lateantique$site != "Porto Maurizio", ]
lateantique <- lateantique[lateantique$site != "Vercelli", ]
lateantique <- lateantique[lateantique$origin != "Unknown", ]
lateantique2 <- lateantique %>% 
  group_by(material,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(lateantique2)
ggplot(lateantique2, aes(x = factor(material), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +  labs(title="Provenance of Amphorae during the Third-Fifth Centuries AD",x = "", y = NULL, fill = "Provenance") +  theme_minimal(base_size = 14) + scale_fill_manual(values = c("#02401B", "#A2A475", "#81A88D", "#972D15"))


### Provenance by site ###

amphora <- read_excel("~/amphora.xlsx")
lateantique <- subset(amphora, period=="3rd-5th Cent. AD")
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
lateantique <- lateantique[lateantique$site != "Acqui Terme", ]
lateantique <- lateantique[lateantique$site != "Ariminum", ]
lateantique <- lateantique[lateantique$site != "Augusta Bagiennorum", ]
lateantique <- lateantique[lateantique$site != "Calvatone", ]
lateantique <- lateantique[lateantique$site != "Cividate Camuno", ]
lateantique <- lateantique[lateantique$site != "Como", ]
lateantique <- lateantique[lateantique$site != "Cremona", ]
lateantique <- lateantique[lateantique$site != "Forli", ]
lateantique <- lateantique[lateantique$site != "Industria", ]
lateantique <- lateantique[lateantique$site != "Porto Maurizio", ]
lateantique <- lateantique[lateantique$site != "Vercelli", ]
lateantique <- lateantique[lateantique$origin != "Unknown", ]
lateantique2 <- lateantique %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(lateantique2)
lateantique2$site <- factor(lateantique2$site,levels = c("Aquileia", "Altinum", "Verona", "Trento", "Brescia", "Milan", "Ivrea"))
ggplot(lateantique2, aes(x = factor(site), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(title="Third-Fifth Centuries AD Amphora Provenance by Site",x = "", y = NULL, fill = "Origin") +  theme_minimal(base_size = 14)  + scale_fill_manual(values = c("#02401B", "#A2A475", "#81A88D", "#972D15")) + theme(axis.text.x = element_text(angle = 90))


### Hierarchical Clustering (no ports) ###

library(readxl) 
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
amphora <- read_excel("~/amphora.xlsx")
lateantique <- subset(amphora, period=="3rd-5th Cent. AD")
lateantique <- lateantique[lateantique$site != "Altinum", ]
lateantique <- lateantique[lateantique$site != "Aquileia", ]
lateantique <- lateantique[lateantique$site != "Ariminum", ]
lateantique <- lateantique[lateantique$site != "Luna", ]
lateantique <- lateantique[lateantique$site != "Porto Maurizio", ]
lateantique <- lateantique[lateantique$site != "Ravenna", ]
lateantique <- lateantique[lateantique$site != "Acqui Terme", ]
lateantique <- lateantique[lateantique$site != "Ariminum", ]
lateantique <- lateantique[lateantique$site != "Augusta Bagiennorum", ]
lateantique <- lateantique[lateantique$site != "Calvatone", ]
lateantique <- lateantique[lateantique$site != "Cividate Camuno", ]
lateantique <- lateantique[lateantique$site != "Como", ]
lateantique <- lateantique[lateantique$site != "Cremona", ]
lateantique <- lateantique[lateantique$site != "Forli", ]
lateantique <- lateantique[lateantique$site != "Industria", ]
lateantique <- lateantique[lateantique$site != "Porto Maurizio", ]
lateantique <- lateantique[lateantique$site != "Vercelli", ]
lateantique2 <- lateantique %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
lateMatrix <- acast(lateantique2, site~origin, value.var="perc", fill=0)
View(lateMatrix)
amphoraperclate <- as.data.frame(lateMatrix)
library(ggdendro)
View(amphoraperlate)
library(ggplot2)
library(ggdendro)
measures <- amphoraperclate[,1:4]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
amphoraperclateHClust <- hclust(distMeasures, method="average")
ggdendrogram(amphoraperclateHClust, rotate=T) + labs(title="Hierarchical Clustering in the Third-Fifth Centuries AD based on Amphora Provenance", x="", y="Distance between assemblages") + theme_minimal()


### Hierarchical Clustering (Adriatic ports) ###

library(readxl) 
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
amphora <- read_excel("~/amphora.xlsx")
lateantique <- subset(amphora, period=="3rd-5th Cent. AD")
lateantique <- lateantique[lateantique$site != "Luna", ]
lateantique <- lateantique[lateantique$site != "Porto Maurizio", ]
lateantique <- lateantique[lateantique$site != "Acqui Terme", ]
lateantique <- lateantique[lateantique$site != "Ariminum", ]
lateantique <- lateantique[lateantique$site != "Augusta Bagiennorum", ]
lateantique <- lateantique[lateantique$site != "Calvatone", ]
lateantique <- lateantique[lateantique$site != "Cividate Camuno", ]
lateantique <- lateantique[lateantique$site != "Como", ]
lateantique <- lateantique[lateantique$site != "Cremona", ]
lateantique <- lateantique[lateantique$site != "Forli", ]
lateantique <- lateantique[lateantique$site != "Industria", ]
lateantique <- lateantique[lateantique$site != "Porto Maurizio", ]
lateantique <- lateantique[lateantique$site != "Vercelli", ]
lateantique2 <- lateantique %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
lateMatrix <- acast(lateantique2, site~origin, value.var="perc", fill=0)
View(lateMatrix)
amphoraperclate <- as.data.frame(lateMatrix)
library(ggdendro)
View(amphoraperlate)
library(ggplot2)
library(ggdendro)
measures <- amphoraperclate[,1:4]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
amphoraperclateHClust <- hclust(distMeasures, method="average")
ggdendrogram(amphoraperclateHClust, rotate=T) + labs(title="Hierarchical Clustering in the Third-Fifth Centuries AD based on Amphora Provenance with Adriatic Ports", x="", y="Distance between assemblages") + theme_minimal()

### Simpson's Index ###

amphora <- read_excel("~/amphora.xlsx")
amphora <- amphora[amphora$type != "Unknown", ]
amphora <- amphora[amphora$type != "Adriatic", ]
amphora <- amphora[amphora$type != "North African", ]
amphora <- amphora[amphora$type != "Eastern", ]
amphora <- amphora[amphora$type != "Gallic", ]
amphora <- amphora[amphora$type != "Tyrrhenian", ]
amphora <- amphora[amphora$type != "Iberian", ]
lateantique <- subset(amphora, period=="3rd-5th Cent. AD")

lateantique <- lateantique[lateantique$site != "Acqui Terme", ]
lateantique <- lateantique[lateantique$site != "Ariminum", ]
lateantique <- lateantique[lateantique$site != "Augusta Bagiennorum", ]
lateantique <- lateantique[lateantique$site != "Calvatone", ]
lateantique <- lateantique[lateantique$site != "Cividate Camuno", ]
lateantique <- lateantique[lateantique$site != "Como", ]
lateantique <- lateantique[lateantique$site != "Cremona", ]
lateantique <- lateantique[lateantique$site != "Forli", ]
lateantique <- lateantique[lateantique$site != "Industria", ]
lateantique <- lateantique[lateantique$site != "Porto Maurizio", ]
lateantique <- lateantique[lateantique$site != "Vercelli", ]
library(reshape2)
lateMatrix <- acast(lateantique, site~type, value.var="type", fill=0)
View(lateMatrix)
library(vegan)
diversity(lateMatrix, index="simpson")

### Morisita-Horn Overlap Coefficient ###

amphora <- read_excel("~/amphora.xlsx")
amphora <- amphora[amphora$type != "Unknown", ]
amphora <- amphora[amphora$type != "Adriatic", ]
amphora <- amphora[amphora$type != "North African", ]
amphora <- amphora[amphora$type != "Eastern", ]
amphora <- amphora[amphora$type != "Gallic", ]
amphora <- amphora[amphora$type != "Tyrrhenian", ]
amphora <- amphora[amphora$type != "Iberian", ]
lateantique <- subset(amphora, period=="3rd-5th Cent. AD")

lateantique <- lateantique[lateantique$site != "Acqui Terme", ]
lateantique <- lateantique[lateantique$site != "Ariminum", ]
lateantique <- lateantique[lateantique$site != "Augusta Bagiennorum", ]
lateantique <- lateantique[lateantique$site != "Calvatone", ]
lateantique <- lateantique[lateantique$site != "Cividate Camuno", ]
lateantique <- lateantique[lateantique$site != "Como", ]
lateantique <- lateantique[lateantique$site != "Cremona", ]
lateantique <- lateantique[lateantique$site != "Forli", ]
lateantique <- lateantique[lateantique$site != "Industria", ]
lateantique <- lateantique[lateantique$site != "Porto Maurizio", ]
lateantique <- lateantique[lateantique$site != "Vercelli", ]
library(reshape2)
lateMatrix <- acast(lateantique, site~type, value.var="type", fill=0)
View(lateMatrix)
library(vegan)
lateDistance <- vegdist(lateMatrix, method="horn")
lateDistance
lateDistanceMatrix <- as.matrix(lateDistance)
late <- melt(lateDistanceMatrix, varnames=c("site1", "site2"), value.name="distance")
library(ggplot2)
ggplot(late, aes(x=site1, y=site2, fill=distance, label=round(distance, 2))) + geom_raster() + labs(title="Morisita-Horn Overlap Coefficient", x = NULL, y = NULL, fill = "Distance") + geom_raster() + geom_text(color="white")
morisitaHClust <- hclust(lateDistance, method="average")
library(ggdendro)
ggdendrogram(morisitaHClust, rotate=T) + labs(title="Vessel Similarity Between Sites Using Morisita-Horn Overlap Coefficient during the Third-Fifth Centuries AD", x="", y="Distance between assemblages") + theme_minimal()


#### Amphora Contents ####

### Sherds ###

library(readxl) 
amphora <- read_excel("~/amphora.xlsx")
View(amphora)
amphora <- amphora[amphora$period != "Unknown", ]
amphora <- amphora[amphora$origin != "Unknown", ]
amphora <- amphora[amphora$contents != "Unknown", ]
amphora <- amphora[amphora$capacity != "Unknown", ]
library(ggplot2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)

laterepublic <- subset(amphora, period=="Late Republic")
imperial <- subset(amphora, period=="1st-2nd Cent. AD")
lateantique <- subset(amphora, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=contents)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Late Republic Amphora Contents", x="Contents", y="Number of Amphora") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=contents)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="First-Second Centuries AD Amphora Contents", x="Contents", y="Number of Amphora") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=contents)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Third-Fifth Centuries AD Amphora Contents", x="Contents", y="Number of Amphora") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))



### Volume ###

library(readxl)
totalvolume <- read_excel("~/PhD/Database/All Amphora/totalvolumerep.xlsx")
View(amphoracontentrep)
library(ggplot2)
ggplot(totalvolume, aes(x=Contents, y=Republic)) + geom_bar(stat = "identity") + geom_text(stat='identity', aes(label=Republic), vjust=-1)+  labs(title="Late Republic. Total Contents by Volume",x = "Contents", y = "Volume (litres)") +  theme_minimal(base_size = 14)
ggplot(totalvolume, aes(x=Contents, y=Imperial)) + geom_bar(stat = "identity") + geom_text(stat='identity', aes(label=Imperial), vjust=-1)+  labs(title="First-Second Centuries AD Total Contents by Volume",x = "Contents", y = "Volume (litres)") +  theme_minimal(base_size = 14)
ggplot(totalvolume, aes(x=Contents, y=Late)) + geom_bar(stat = "identity") + geom_text(stat='identity', aes(label=Late), vjust=-1)+  labs(title="Third-Fifth Centuries AD Total Contents by Volume",x = "Contents", y = "Volume (litres)") +  theme_minimal(base_size = 14)


######################################################################

### Vessels by site and period ####

library(readxl) 
library(ggplot2)
amphora <- read_excel("~/amphora.xlsx")
View(amphora)
amphora <- amphora[amphora$period != "Unknown", ]
amphora <- amphora[amphora$origin != "Unknown", ]

### Acqui Terme ###

acquiterme <- subset(amphora, site=="Acqui Terme")
laterepublic <- subset(acquiterme, period=="Late Republic")
imperial <- subset(acquiterme, period=="1st-2nd Cent. AD")
lateantique <- subset(acquiterme, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Acqui Terme Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) 
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Acqui Terme First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Acqui Terme Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Alba ###

alba <- subset(amphora, site=="Alba")
laterepublic <- subset(alba, period=="Late Republic")
imperial <- subset(alba, period=="1st-2nd Cent. AD")
lateantique <- subset(alba, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Alba Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) 
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Alba First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Alba Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Altinum ###

altinum <- subset(amphora, site=="Altinum")
laterepublic <- subset(altinum, period=="Late Republic")
imperial <- subset(altinum, period=="1st-2nd Cent. AD")
lateantique <- subset(altinum, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Altinum Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) 
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Altinum First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Altinum Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Aquileia ###

aquileia <- subset(amphora, site=="Aquileia")
laterepublic <- subset(aquileia, period=="Late Republic")
imperial <- subset(aquileia, period=="1st-2nd Cent. AD")
lateantique <- subset(aquileia, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Aquileia Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) 
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Aquileia First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Aquileia Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Ariminum ###

ariminum <- subset(amphora, site=="Ariminum")
laterepublic <- subset(ariminum, period=="Late Republic")
imperial <- subset(ariminum, period=="1st-2nd Cent. AD")
lateantique <- subset(ariminum, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ariminum Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ariminum First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ariminum Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Augusta Bagiennorum ###

augusta <- subset(amphora, site=="Augusta Bagiennorum")
laterepublic <- subset(augusta, period=="Late Republic")
imperial <- subset(augusta, period=="1st-2nd Cent. AD")
lateantique <- subset(augusta, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Augusta Bagiennorum Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Augusta Bagiennorum First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Augusta Bagiennorum Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Bologna ###

bologna <- subset(amphora, site=="Bologna")
laterepublic <- subset(bologna, period=="Late Republic")
imperial <- subset(bologna, period=="1st-2nd Cent. AD")
lateantique <- subset(bologna, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Bologna Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Bologna First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Bologna Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Brescia ###

brescia <- subset(amphora, site=="Brescia")
laterepublic <- subset(brescia, period=="Late Republic")
imperial <- subset(brescia, period=="1st-2nd Cent. AD")
lateantique <- subset(brescia, period=="3rd-5th Cent. AD")
  ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Brescia Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Brescia First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Brescia Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Calvatone ###

calvatone <- subset(amphora, site=="Calvatone")
laterepublic <- subset(calvatone, period=="Late Republic")
imperial <- subset(calvatone, period=="1st-2nd Cent. AD")
lateantique <- subset(calvatone, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Calvatone Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Calvatone First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Calvatone Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Cividate Camuno ###

camuno <- subset(amphora, site=="Cividate Camuno")
laterepublic <- subset(camuno, period=="Late Republic")
imperial <- subset(camuno, period=="1st-2nd Cent. AD")
lateantique <- subset(camuno, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cividate Camuno Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cividate Camuno First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cividate Camuno Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Como ###

como <- subset(amphora, site=="Como")
laterepublic <- subset(como, period=="Late Republic")
imperial <- subset(como, period=="1st-2nd Cent. AD")
lateantique <- subset(como, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Como Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Como First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Como Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Cremona ###

cremona <- subset(amphora, site=="Cremona")
laterepublic <- subset(cremona, period=="Late Republic")
imperial <- subset(cremona, period=="1st-2nd Cent. AD")
lateantique <- subset(cremona, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cremona Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cremona First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cremona Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Este ###

este <- subset(amphora, site=="Este")
laterepublic <- subset(este, period=="Late Republic")
imperial <- subset(este, period=="1st-2nd Cent. AD")
lateantique <- subset(este, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Este Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Este First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Este Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Forli ###

forli <- subset(amphora, site=="Forli")
laterepublic <- subset(forli, period=="Late Republic")
imperial <- subset(forli, period=="1st-2nd Cent. AD")
lateantique <- subset(forli, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Forli Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Forli First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Forli Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Industria ###

industria <- subset(amphora, site=="Industria")
laterepublic <- subset(industria, period=="Late Republic")
imperial <- subset(industria, period=="1st-2nd Cent. AD")
lateantique <- subset(industria, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Industria Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Industria First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Industria Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Ivrea ###

ivrea <- subset(amphora, site=="Ivrea")
laterepublic <- subset(ivrea, period=="Late Republic")
imperial <- subset(ivrea, period=="1st-2nd Cent. AD")
lateantique <- subset(ivrea, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ivrea Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ivrea First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ivrea Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Libarna ###

libarna <- subset(amphora, site=="Libarna")
laterepublic <- subset(libarna, period=="Late Republic")
imperial <- subset(libarna, period=="1st-2nd Cent. AD")
lateantique <- subset(libarna, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Libarna Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Libarna First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Libarna Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Luna ###

luna <- subset(amphora, site=="Luna")
laterepublic <- subset(luna, period=="Late Republic")
imperial <- subset(luna, period=="1st-2nd Cent. AD")
lateantique <- subset(luna, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Luna Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Luna First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Luna Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Milan ###

milan <- subset(amphora, site=="Milan")
laterepublic <- subset(milan, period=="Late Republic")
imperial <- subset(milan, period=="1st-2nd Cent. AD")
lateantique <- subset(milan, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Milan Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Milan First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Milan Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Modena ###

modena <- subset(amphora, site=="Modena")
laterepublic <- subset(modena, period=="Late Republic")
imperial <- subset(modena, period=="1st-2nd Cent. AD")
lateantique <- subset(modena, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Modena Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Modena First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Modena Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Novara ###

novara <- subset(amphora, site=="Novara")
laterepublic <- subset(novara, period=="Late Republic")
imperial <- subset(novara, period=="1st-2nd Cent. AD")
lateantique <- subset(novara, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Novara Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Novara First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Novara Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Oderzo ###

oderzo <- subset(amphora, site=="Oderzo")
laterepublic <- subset(oderzo, period=="Late Republic")
imperial <- subset(oderzo, period=="1st-2nd Cent. AD")
lateantique <- subset(oderzo, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Oderzo Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Oderzo First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Oderzo Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Padua ###

padua <- subset(amphora, site=="Padua")
laterepublic <- subset(padua, period=="Late Republic")
imperial <- subset(padua, period=="1st-2nd Cent. AD")
lateantique <- subset(padua, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Padua Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Padua First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Padua Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Porto Maurizio ###

porto <- subset(amphora, site=="Porto Maurizio")
laterepublic <- subset(porto, period=="Late Republic")
imperial <- subset(porto, period=="1st-2nd Cent. AD")
lateantique <- subset(porto, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title=" Porto Maurizio Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title=" Porto Maurizio First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title=" Porto Maurizio Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Ravenna ###

ravenna <- subset(amphora, site=="Ravenna")
laterepublic <- subset(ravenna, period=="Late Republic")
imperial <- subset(ravenna, period=="1st-2nd Cent. AD")
lateantique <- subset(ravenna, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title=" Ravenna Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title=" Ravenna First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title=" Ravenna Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Reggio Emilia ###

reggio <- subset(amphora, site=="Reggio Emilia")
laterepublic <- subset(reggio, period=="Late Republic")
imperial <- subset(reggio, period=="1st-2nd Cent. AD")
lateantique <- subset(reggio, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Reggio Emilia Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Reggio Emilia First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Reggio Emilia Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Trento ###

trento <- subset(amphora, site=="Trento")
laterepublic <- subset(trento, period=="Late Republic")
imperial <- subset(trento, period=="1st-2nd Cent. AD")
lateantique <- subset(trento, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Trento Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Trento First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Trento Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Verona ###

verona <- subset(amphora, site=="Verona")
laterepublic <- subset(verona, period=="Late Republic")
imperial <- subset(verona, period=="1st-2nd Cent. AD")
lateantique <- subset(verona, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Verona Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Verona First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Verona Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Vercelli ###

vercelli <- subset(amphora, site=="Vercelli")
laterepublic <- subset(vercelli, period=="Late Republic")
imperial <- subset(vercelli, period=="1st-2nd Cent. AD")
lateantique <- subset(vercelli, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Vercelli Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Vercelli First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Vercelli Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Vicenza ###

vicenza <- subset(amphora, site=="Vicenza")
laterepublic <- subset(vicenza, period=="Late Republic")
imperial <- subset(vicenza, period=="1st-2nd Cent. AD")
lateantique <- subset(vicenza, period=="3rd-5th Cent. AD")
ggplot(laterepublic, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Vicenza Republican Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Vicenza First-Second Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Vicenza Third-Fifth Centuries AD Amphora", x="Type", y="Number of Sherds") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))










