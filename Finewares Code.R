#### Fineware Coding ####

### Preamble ####

library(readxl) 
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
View(finewares)

#### Long Dates ####

### Number of vessels by long-date ###

finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$longdate != "Unknown", ]
finewares$shortdate <- factor(finewares$longdate,levels = c("1st-2nd Cent. AD", "3rd-5th Cent. AD"))
ggplot(finewares, aes(x=longdate)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Number of Finewares by Period", x="Period", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


#### First-Second Centuries ####

### Vessel Percentages by Provenance ###

library(readxl) 
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
finewares2 <- finewares %>% 
  group_by(longdate,type,origin) %>%
  summarise(n=n()) %>% 
  group_by(longdate) %>% 
  mutate(perc=100*n/sum(n))
View(finewares2)
imperial <- subset(finewares2, longdate=="1st-2nd Cent. AD")
ggplot(imperial, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="First-Second Centuries AD Percentage per Vessel", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")

central <- subset(imperial, origin=="Central Italic")
ggplot(central, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="First-Second Centuries AD Percentage per Vessel with a Central Italic Provenance", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")

eastern <- subset(imperial, origin=="Eastern")
ggplot(eastern, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="First-Second Centuries AD Percentage per Vessel with an Eastern Provenance", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")

gallic <- subset(imperial, origin=="Gallic")
ggplot(gallic, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="First-Second Centuries AD Percentage per Vessel with a Gallic Provenance", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")

northern <- subset(imperial, origin=="Northern Italic")
ggplot(northern, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="First-Second Centuries AD Percentage per Vessel with a Northern Italic Provenance", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")

african <- subset(imperial, origin=="African")
ggplot(african, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="First-Second Centuries AD Percentage per Vessel with a North African Provenance", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")


### Total Provenances for Period ###

finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
finewares <- finewares[finewares$longdate != "Unknown", ]
finewares2 <- finewares %>% 
  group_by(longdate,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(finewares2)
imperial <- subset(finewares2, longdate=="1st-2nd Cent. AD")
ggplot(imperial, aes(x = factor(longdate), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +  labs(title="Provenance of First-Second Centuries AD Finewares",x = "", y = NULL, fill = "Vessel Type") +  theme_minimal(base_size = 14) + scale_fill_manual(values = c("#02401B", "#9C964A", "#A2A475", "#81A88D", "#972D15"))

### Provenance by Site ###

library(readxl) 
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
finewares <- finewares[finewares$longdate != "Unknown", ]
imperial <- subset(finewares, longdate=="1st-2nd Cent. AD")
imperial2 <- imperial %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(imperial2)
imperial2$site <- factor(imperial2$site,levels = c("Ariminum", "Ravenna", "Altinum", "Aquileia", "Julia Concordia", "Padua", "Forli", "Adria", "Bologna", "Modena", "Verona", "Trento", "Calvatone", "Cremona", "Brescia", "Cividate Camuno", "Milan", "Como", "Novara", "Vercelli", "Ivrea", "Turin", "Chieri", "Tortona", "Alba", "Augusta Bagiennorum", "Acqui Terme", "Luna"))
ggplot(imperial2, aes(x = factor(site), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(title="First-Second Centuries AD Fineware Origins by Site",x = "", y = NULL, fill = "Origin") +  theme_minimal(base_size = 14) + scale_fill_manual(values = c("#02401B", "#9C964A", "#A2A475", "#81A88D", "#972D15")) + theme(axis.text.x = element_text(angle = 90))

### Hierarchical Clustering no Ports ###

library(readxl) 
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$longdate != "Unknown", ]
finewares <- finewares[finewares$origin != "Unknown", ]
imperial <- subset(finewares, longdate=="1st-2nd Cent. AD")
imperial <- imperial[imperial$site != "Aquileia", ]
imperial <- imperial[imperial$site != "Ariminum", ]
imperial <- imperial[imperial$site != "Altinum", ]
imperial <- imperial[imperial$site != "Ravenna", ]
imperial <- imperial[imperial$site != "Luna", ]
imperial2 <- imperial %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
imperialMatrix <- acast(imperial2, site~origin, value.var="perc", fill=0)
View(imperialMatrix)
finewarepercimp <- as.data.frame(imperialMatrix)
library(ggdendro)
View(finewarepercimp)
library(ggplot2)
library(ggdendro)
measures <- finewarepercimp[,1:5]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
finewarepercimpHClust <- hclust(distMeasures, method="average")
ggdendrogram(finewarepercimpHClust, rotate=T) + labs(title="Hierarchical Clustering in the first-second Centuries AD based on Fineware Provenance", x="", y="Distance between assemblages") + theme_minimal()


### Hierarchical Clustering Adriatic Ports ###

library(readxl) 
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$longdate != "Unknown", ]
finewares <- finewares[finewares$origin != "Unknown", ]
imperial <- subset(finewares, longdate=="1st-2nd Cent. AD")
imperial <- imperial[imperial$site != "Luna", ]
imperial2 <- imperial %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
imperialMatrix <- acast(imperial2, site~origin, value.var="perc", fill=0)
View(imperialMatrix)
finewarepercimp <- as.data.frame(imperialMatrix)
library(ggdendro)
View(finewarepercimp)
library(ggplot2)
library(ggdendro)
measures <- finewarepercimp[,1:5]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
finewarepercimpHClust <- hclust(distMeasures, method="average")
ggdendrogram(finewarepercimpHClust, rotate=T) + labs(title="Hierarchical Clustering in the First-Second Centuries AD based on Fineware Provenance with Adriatic Ports", x="", y="Distance between assemblages") + theme_minimal()

### Hierarchical Clustering Ligurian Port ###

library(readxl) 
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$longdate != "Unknown", ]
finewares <- finewares[finewares$origin != "Unknown", ]
imperial <- subset(finewares, longdate=="1st-2nd Cent. AD")
imperial <- imperial[imperial$site != "Aquileia", ]
imperial <- imperial[imperial$site != "Ariminum", ]
imperial <- imperial[imperial$site != "Altinum", ]
imperial <- imperial[imperial$site != "Ravenna", ]
imperial2 <- imperial %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
imperialMatrix <- acast(imperial2, site~origin, value.var="perc", fill=0)
View(imperialMatrix)
finewarepercimp <- as.data.frame(imperialMatrix)
library(ggdendro)
View(finewarepercimp)
library(ggplot2)
library(ggdendro)
measures <- finewarepercimp[,1:5]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
finewarepercimpHClust <- hclust(distMeasures, method="average")
ggdendrogram(finewarepercimpHClust, rotate=T) + labs(title="Hierarchical Clustering in the First-Second Centuries AD based on Fineware Provenance with Ligurian Port", x="", y="Distance between assemblages") + theme_minimal()

### Diversity ####

### Simpson's Index ###

library(readxl)
finewares <- read_excel("~/finewares.xlsx")
imperial <- subset(finewares, longdate=="1st-2nd Cent. AD")
imperial <- imperial[imperial$type != "Unknown", ]
finewares <- finewares[finewares$type != "Adriatic", ]
finewares <- finewares[finewares$type != "African", ]
finewares <- finewares[finewares$type != "Central Italic", ]
finewares <- finewares[finewares$type != "Eastern", ]
finewares <- finewares[finewares$type != "Gallic", ]
finewares <- finewares[finewares$type != "Northern Italic", ]
library(reshape2)
imperialMatrix <- acast(imperial, site~type, value.var="type", fill=0)
View(imperialMatrix)
library(vegan)
diversity(imperialMatrix, index="simpson")

### Morisita-Horn Overlap Coefficient ###

library(readxl)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$site != "Turin", ]
finewares <- finewares[finewares$type != "Unknown", ]
finewares <- finewares[finewares$type != "Adriatic", ]
finewares <- finewares[finewares$type != "African", ]
finewares <- finewares[finewares$type != "Central Italic", ]
finewares <- finewares[finewares$type != "Eastern", ]
finewares <- finewares[finewares$type != "Gallic", ]
finewares <- finewares[finewares$type != "Northern Italic", ]
imperial <- subset(finewares, longdate=="1st-2nd Cent. AD")
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


#### Third-Fifth Centuries AD ####

### Vessel Percentages by Provenance ###

library(readxl) 
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
finewares2 <- finewares %>% 
  group_by(longdate,type,origin) %>%
  summarise(n=n()) %>% 
  group_by(longdate) %>% 
  mutate(perc=100*n/sum(n))
View(finewares2)
late <- subset(finewares2, longdate=="3rd-5th Cent. AD")
ggplot(late, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="Third-Fifth Centuries AD Percentage per Vessel", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")

adriatic <- subset(late, origin=="Adriatic")
ggplot(central, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="Third-Fifth Centuries AD Percentage per Vessel with an Adriatic Provenance", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")

eastern <- subset(late, origin=="Eastern")
ggplot(eastern, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="Third-Fifth Centuries AD Percentage per Vessel with an Eastern Provenance", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")

gallic <- subset(late, origin=="Gallic")
ggplot(gallic, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="Third-Fifth Centuries AD Percentage per Vessel with a Gallic Provenance", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")

northern <- subset(late, origin=="Northern Italic")
ggplot(northern, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="Third-Fifth Centuries AD Percentage per Vessel with a Northern Italic Provenance", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")

african <- subset(late, origin=="African")
ggplot(african, aes(x=type, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="Third-Fifth Centuries AD Percentage per Vessel with a North African Provenance", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")


### Total Provenances for Period ###

finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
finewares <- finewares[finewares$longdate != "Unknown", ]
finewares2 <- finewares %>% 
  group_by(longdate,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(finewares2)
late <- subset(finewares2, longdate=="3rd-5th Cent. AD")
ggplot(late, aes(x = factor(longdate), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +  labs(title="Provenance of Third-Fifth Centuries AD Finewares",x = "", y = NULL, fill = "Vessel Type") +  theme_minimal(base_size = 14) + scale_fill_manual(values = c("#CDC08C", "#02401B", "#A2A475", "#81A88D", "#972D15"))

### Provenance by Site ###

library(readxl) 
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
late <- subset(finewares, longdate=="3rd-5th Cent. AD")
late <- late[late$site != "Acqui Terme", ]
late <- late[late$site != "Augusta Bagiennorum", ]
late <- late[late$site != "Calvatone", ]
late <- late[late$site != "Cividate Camuno", ]
late <- late[late$site != "Cremona", ]
late <- late[late$site != "Como", ]
late <- late[late$site != "Julia Concordia", ]
late <- late[late$site != "Trento", ]
late2 <- late %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(late2)
late2$site <- factor(late2$site,levels = c("Altinum", "Aquileia", "Ariminum", "Forli", "Verona", "Brescia", "Milan", "Alba", "Acqui Terme"))
ggplot(late2, aes(x = factor(site), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(title="Third-Fifth Centuries AD Fineware Origins by Site",x = "", y = NULL, fill = "Origin") +  theme_minimal(base_size = 14)  + scale_fill_manual(values = c("#CDC08C", "#02401B", "#A2A475", "#81A88D", "#972D15")) + theme(axis.text.x = element_text(angle = 90))

### Hierarchical Clustering no Ports ####

library(readxl) 
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
late <- subset(finewares, longdate=="3rd-5th Cent. AD")
late <- late[late$site != "Altinum", ]
late <- late[late$site != "Ariminum", ]
late <- late[late$site != "Aquileia", ]
late <- late[late$site != "Acqui Terme", ]
late <- late[late$site != "Augusta Bagiennorum", ]
late <- late[late$site != "Calvatone", ]
late <- late[late$site != "Cividate Camuno", ]
late <- late[late$site != "Cremona", ]
late <- late[late$site != "Como", ]
late <- late[late$site != "Julia Concordia", ]
late <- late[late$site != "Trento", ]
late2 <- late %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
lateMatrix <- acast(late2, site~origin, value.var="perc", fill=0)
View(lateMatrix)
finewareperclate <- as.data.frame(lateMatrix)
library(ggdendro)
View(finewareperlate)
library(ggplot2)
library(ggdendro)
measures <- finewareperclate[,1:5]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
finewareperclateHClust <- hclust(distMeasures, method="average")
ggdendrogram(finewareperclateHClust, rotate=T) + labs(title="Hierarchical Clustering in the Third-Fifth Centuries AD based on Fineware Provenance", x="", y="Distance between assemblages") + theme_minimal()

### Hierarchical Clustering Adriatic Ports ####

library(readxl) 
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
late <- subset(finewares, longdate=="3rd-5th Cent. AD")
late <- late[late$site != "Acqui Terme", ]
late <- late[late$site != "Augusta Bagiennorum", ]
late <- late[late$site != "Calvatone", ]
late <- late[late$site != "Cividate Camuno", ]
late <- late[late$site != "Cremona", ]
late <- late[late$site != "Como", ]
late <- late[late$site != "Julia Concordia", ]
late <- late[late$site != "Trento", ]
late2 <- late %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
lateMatrix <- acast(late2, site~origin, value.var="perc", fill=0)
View(lateMatrix)
finewareperclate <- as.data.frame(lateMatrix)
library(ggdendro)
View(finewareperlate)
library(ggplot2)
library(ggdendro)
measures <- finewareperclate[,1:5]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
finewareperclateHClust <- hclust(distMeasures, method="average")
ggdendrogram(finewareperclateHClust, rotate=T) + labs(title="Hierarchical Clustering in the Third-Fifth Centuries AD based on Fineware Provenance with Adriatic Ports", x="", y="Distance between assemblages") + theme_minimal()

### Diversity ####

### Simpsons Index ###

library(readxl)
finewares <- read_excel("~/finewares.xlsx")
late <- subset(finewares, longdate=="3rd-5th Cent. AD")
late <- late[late$type != "Unknown", ]
finewares <- finewares[finewares$type != "Adriatic", ]
finewares <- finewares[finewares$type != "African", ]
finewares <- finewares[finewares$type != "Central Italic", ]
finewares <- finewares[finewares$type != "Eastern", ]
finewares <- finewares[finewares$type != "Gallic", ]
finewares <- finewares[finewares$type != "Northern Italic", ]
late <- late[late$site != "Acqui Terme", ]
late <- late[late$site != "Augusta Bagiennorum", ]
late <- late[late$site != "Calvatone", ]
late <- late[late$site != "Cividate Camuno", ]
late <- late[late$site != "Cremona", ]
late <- late[late$site != "Como", ]
late <- late[late$site != "Julia Concordia", ]
late <- late[late$site != "Trento", ]
library(reshape2)
lateMatrix <- acast(late, site~type, value.var="type", fill=0)
View(lateMatrix)
library(vegan)
diversity(lateMatrix, index="simpson")

### Morisita Horn Overlap Coefficient ###

library(readxl)
latevessels <- read_excel("~/latevessels.xlsx")
latevessels <- latevessels[latevessels$site != "Augusta Bagiennorum", ]
latevessels <- latevessels[latevessels$site != "Calvatone", ]
latevessels <- latevessels[latevessels$site != "Cividate Camuno", ]
latevessels <- latevessels[latevessels$site != "Cremona", ]
latevessels <- latevessels[latevessels$site != "Como", ]
latevessels <- latevessels[latevessels$site != "Julia Concordia", ]
latevessels <- latevessels[latevessels$site != "Trento", ]
finewares <- finewares[finewares$type != "Adriatic", ]
finewares <- finewares[finewares$type != "African", ]
finewares <- finewares[finewares$type != "Central Italic", ]
finewares <- finewares[finewares$type != "Eastern", ]
finewares <- finewares[finewares$type != "Gallic", ]
finewares <- finewares[finewares$type != "Northern Italic", ]
library(reshape2)
lateMatrix <- acast(latevessels, site~vessel, value.var="values", fill=0)
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
ggdendrogram(morisitaHClust, rotate=T) + labs(title="Vessel Similarity Between Sites Using Morisita-Horn Overlap Coefficient Third-Fifth Centuries AD", x="", y="Distance between assemblages") + theme_minimal()


###################################################################################################################################

### Gallic Workshops ####

library(readxl) 
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
View(finewares)
finewares <- finewares[finewares$longdate != "Unknown", ]

gallic <- subset(finewares, origin=="Gallic")
gallic$origin2 <- factor(gallic$origin2,levels = c("South Gallic", "Central Gallic", "Unknown"))
ggplot(gallic, aes(x=origin2)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Gallic Sigillata Origins", x="Location", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### ARS Productions ####

### ARS Imperial ###

library(readxl) 
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
View(finewares)
finewares <- finewares[finewares$longdate != "Unknown", ]

african <- subset(finewares, origin=="African")
african <- subset(african, longdate=="1st-2nd Cent. AD")
ggplot(african, aes(x=production)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="ARS Production Types First-Second Centuries AD", x="Production", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### ARS Late ###

library(readxl) 
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
View(finewares)
finewares <- finewares[finewares$longdate != "Unknown", ]
finewares <- finewares[finewares$production != "Unknown", ]
finewares <- finewares[finewares$production != "NULL", ]
african <- subset(finewares, origin=="African")
african <- subset(african, longdate=="3rd-5th Cent. AD")
ggplot(african, aes(x=production)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="ARS Production Types Third-Fifth Centuries AD", x="Production", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

##############################################################################################################################################

#### Short Dates ####

### Number of Vessels by Short Date ####

finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$shortdate != "Unknown", ]
finewares$shortdate <- factor(finewares$shortdate,levels = c("Augustan", "1st Cent. AD", "2nd Cent. AD", "3rd Cent. AD", "4th Cent. AD", "5th Cent. AD"))
ggplot(finewares, aes(x=shortdate)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Number of Finewares by Century", x="Period", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

#### Augustan Period ####

### Total Provenances for Period ###

library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(ggplot2)
library(readxl) 
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
finewares <- finewares[finewares$shortdate != "Unknown", ]
finewares2 <- finewares %>% 
  group_by(shortdate,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(finewares2)
augustan <- subset(finewares2, shortdate=="Augustan")
ggplot(augustan, aes(x = factor(shortdate), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +  labs(title="Provenance of Augustan Finewares",x = "", y = NULL, fill = "Vessel Type") +  theme_minimal(base_size = 14) + scale_fill_manual(values = c("#9C964A", "#A2A475", "#81A88D", "#972D15"))


### Provenance by Site ###

library(readxl) 
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
View(finewares)
finewares <- finewares[finewares$shortdate != "Unknown", ]
finewares <- finewares[finewares$origin != "Unknown", ]
finewares <- finewares[finewares$site != "Acqui Terme", ]
finewares <- finewares[finewares$site != "Alba", ]
finewares <- finewares[finewares$site != "Aquileia", ]
finewares <- finewares[finewares$site != "Ariminum", ]
finewares <- finewares[finewares$site != "Augusta Bagiennorum", ]
finewares <- finewares[finewares$site != "Bologna", ]
finewares <- finewares[finewares$site != "Calvatone", ]
finewares <- finewares[finewares$site != "Cremona", ]
finewares <- finewares[finewares$site != "Padua", ]
finewares <- finewares[finewares$site != "Ravenna", ]
finewares <- finewares[finewares$site != "Tortona", ]
augustan <- subset(finewares, shortdate=="Augustan")
augustan2 <- augustan %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(augustan2)
augustan2$site <- factor(augustan2$site,levels = c("Altinum", "Julia Concordia", "Adria", "Trento", "Modena", "Brescia", "Milan", "Como", "Ivrea", "Luna"))
ggplot(augustan2, aes(x = factor(site), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(title="Augustan Fineware Origins by Site",x = "", y = NULL, fill = "Origin") +  theme_minimal(base_size = 14)  + scale_fill_manual(values = c("#9C964A", "#A2A475", "#81A88D", "#972D15")) + theme(axis.text.x = element_text(angle = 90))

### Hierarchical Clustering, no ports ###

library(readxl)
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
augustan <- subset(finewares, shortdate=="Augustan")
augustan <- augustan[augustan$site != "Ariminum", ]
augustan <- augustan[augustan$site != "Altinum", ]
augustan <- augustan[augustan$site != "Aquileia", ]
augustan <- augustan[augustan$site != "Ravenna", ]
augustan <- augustan[augustan$site != "Luna", ]
augustan <- augustan[augustan$site != "Acqui Terme", ]
augustan <- augustan[augustan$site != "Alba", ]
augustan <- augustan[augustan$site != "Aquileia", ]
augustan <- augustan[augustan$site != "Ariminum", ]
augustan <- augustan[augustan$site != "Augusta Bagiennorum", ]
augustan <- augustan[augustan$site != "Bologna", ]
augustan <- augustan[augustan$site != "Calvatone", ]
augustan <- augustan[augustan$site != "Cremona", ]
augustan <- augustan[augustan$site != "Padua", ]
augustan <- augustan[augustan$site != "Ravenna", ]
augustan <- augustan[augustan$site != "Tortona", ]
augustan2 <- augustan %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
augustanMatrix <- acast(augustan2, site~origin, value.var="perc", fill=0)
View(augustanMatrix)
augustanpercs <- as.data.frame(augustanMatrix)
library(ggdendro)
View(augustanpercs)
library(ggplot2)
library(ggdendro)
measures <- augustanpercs[,1:4]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
augustanpercsHClust <- hclust(distMeasures, method="average")
ggdendrogram(augustanpercsHClust, rotate=T) + labs(title="Hierarchical Clustering in the Augustan Period based on Fineware Provenance", x="", y="Distance between assemblages") + theme_minimal()

### Hierarchical Clustering, with Adriatic port ###

library(readxl)
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
augustan <- subset(finewares, shortdate=="Augustan")
augustan <- augustan[augustan$site != "Acqui Terme", ]
augustan <- augustan[augustan$site != "Luna", ]
augustan <- augustan[augustan$site != "Alba", ]
augustan <- augustan[augustan$site != "Aquileia", ]
augustan <- augustan[augustan$site != "Ariminum", ]
augustan <- augustan[augustan$site != "Augusta Bagiennorum", ]
augustan <- augustan[augustan$site != "Bologna", ]
augustan <- augustan[augustan$site != "Calvatone", ]
augustan <- augustan[augustan$site != "Cremona", ]
augustan <- augustan[augustan$site != "Padua", ]
augustan <- augustan[augustan$site != "Ravenna", ]
augustan <- augustan[augustan$site != "Tortona", ]
augustan2 <- augustan %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
augustanMatrix <- acast(augustan2, site~origin, value.var="perc", fill=0)
View(augustanMatrix)
augustanpercs <- as.data.frame(augustanMatrix)
library(ggdendro)
View(augustanpercs)
library(ggplot2)
library(ggdendro)
measures <- augustanpercs[,1:4]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
augustanpercsHClust <- hclust(distMeasures, method="average")
ggdendrogram(augustanpercsHClust, rotate=T) + labs(title="Hierarchical Clustering in the Augustan Period with Adriatic Port", x="", y="Distance between assemblages") + theme_minimal()

### Hierarchical Clustering, with Ligurian port ###

library(readxl)
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
augustan <- subset(finewares, shortdate=="Augustan")
augustan <- augustan[augustan$site != "Acqui Terme", ]
augustan <- augustan[augustan$site != "Altinum", ]
augustan <- augustan[augustan$site != "Alba", ]
augustan <- augustan[augustan$site != "Aquileia", ]
augustan <- augustan[augustan$site != "Ariminum", ]
augustan <- augustan[augustan$site != "Augusta Bagiennorum", ]
augustan <- augustan[augustan$site != "Bologna", ]
augustan <- augustan[augustan$site != "Calvatone", ]
augustan <- augustan[augustan$site != "Cremona", ]
augustan <- augustan[augustan$site != "Padua", ]
augustan <- augustan[augustan$site != "Ravenna", ]
augustan <- augustan[augustan$site != "Tortona", ]
augustan2 <- augustan %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
augustanMatrix <- acast(augustan2, site~origin, value.var="perc", fill=0)
View(augustanMatrix)
augustanpercs <- as.data.frame(augustanMatrix)
library(ggdendro)
View(augustanpercs)
library(ggplot2)
library(ggdendro)
measures <- augustanpercs[,1:4]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
augustanpercsHClust <- hclust(distMeasures, method="average")
ggdendrogram(augustanpercsHClust, rotate=T) + labs(title="Hierarchical Clustering in the Augustan Period with Ligurian Port", x="", y="Distance between assemblages") + theme_minimal()

### Simpsons Index ###

library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(vegan)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$type != "Unknown", ]
finewares <- finewares[finewares$type != "Adriatic", ]
finewares <- finewares[finewares$type != "African", ]
finewares <- finewares[finewares$type != "Central Italic", ]
finewares <- finewares[finewares$type != "Eastern", ]
finewares <- finewares[finewares$type != "Gallic", ]
finewares <- finewares[finewares$type != "Northern Italic", ]
augustan <- subset(finewares, shortdate=="Augustan")
augustan <- augustan[augustan$site != "Acqui Terme", ]
augustan <- augustan[augustan$site != "Alba", ]
augustan <- augustan[augustan$site != "Aquileia", ]
augustan <- augustan[augustan$site != "Ariminum", ]
augustan <- augustan[augustan$site != "Augusta Bagiennorum", ]
augustan <- augustan[augustan$site != "Bologna", ]
augustan <- augustan[augustan$site != "Calvatone", ]
augustan <- augustan[augustan$site != "Cremona", ]
augustan <- augustan[augustan$site != "Padua", ]
augustan <- augustan[augustan$site != "Ravenna", ]
augustan <- augustan[augustan$site != "Tortona", ]
augustanMatrix <- acast(augustan, site~type, value.var="type", fill=0)
View(augustanMatrix)
diversity(augustanMatrix, index="simpson")

### Morisita-Horn Overlap Coefficient ###

library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(vegan)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$type != "Unknown", ]
finewares <- finewares[finewares$type != "Adriatic", ]
finewares <- finewares[finewares$type != "African", ]
finewares <- finewares[finewares$type != "Central Italic", ]
finewares <- finewares[finewares$type != "Eastern", ]
finewares <- finewares[finewares$type != "Gallic", ]
finewares <- finewares[finewares$type != "Northern Italic", ]
augustan <- subset(finewares, shortdate=="Augustan")
augustan <- augustan[augustan$site != "Acqui Terme", ]
augustan <- augustan[augustan$site != "Alba", ]
augustan <- augustan[augustan$site != "Aquileia", ]
augustan <- augustan[augustan$site != "Ariminum", ]
augustan <- augustan[augustan$site != "Augusta Bagiennorum", ]
augustan <- augustan[augustan$site != "Bologna", ]
augustan <- augustan[augustan$site != "Calvatone", ]
augustan <- augustan[augustan$site != "Cremona", ]
augustan <- augustan[augustan$site != "Padua", ]
augustan <- augustan[augustan$site != "Ravenna", ]
augustan <- augustan[augustan$site != "Tortona", ]
augustanMatrix <- acast(augustan, site~type, value.var="type", fill=0)
View(augustanMatrix)
augDistance <- vegdist(augustanMatrix, method="horn")
augDistance
augDistanceMatrix <- as.matrix(repDistance)
aug <- melt(augDistanceMatrix, varnames=c("site1", "site2"), value.name="distance")
library(ggplot2)
ggplot(aug, aes(x=site1, y=site2, fill=distance, label=round(distance, 2))) + geom_raster() + labs(title="Morisita-Horn Overlap Coefficient", x = NULL, y = NULL, fill = "Distance") + geom_raster() + geom_text(color="white")
morisitaHClust <- hclust(augDistance, method="average")
library(ggdendro)
ggdendrogram(morisitaHClust, rotate=T) + labs(title="Augustan Period Vessel Similarity Between Sites Using Morisita-Horn Overlap Coefficient", x="", y="Distance between assemblages") + theme_minimal()



#### First Cent. AD ####

### Total Provenances for Period ###

library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(ggplot2)
finewares <- read_excel("~/PhD/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
finewares <- finewares[finewares$shortdate != "Unknown", ]
finewares2 <- finewares %>% 
  group_by(shortdate,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(finewares2)
first <- subset(finewares2, shortdate=="1st Cent. AD")
ggplot(first, aes(x = factor(shortdate), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +  labs(title="Provenance of First Century AD Finewares",x = "", y = NULL, fill = "Vessel Type") +  theme_minimal(base_size = 14) + scale_fill_manual(values = c("#02401B", "#9C964A", "#A2A475", "#81A88D", "#972D15"))


### Provenance by Site ###

library(readxl) 
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
View(finewares)
finewares <- finewares[finewares$shortdate != "Unknown", ]
finewares <- finewares[finewares$origin != "Unknown", ]
finewares <- finewares[finewares$site != "Acqui Terme", ]
first <- subset(finewares, shortdate=="1st Cent. AD")
first2 <- first %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(first2)
first2$site <- factor(first2$site,levels = c("Ariminum", "Ravenna", "Altinum", "Aquileia", "Julia Concordia", "Padua", "Forli", "Adria", "Bologna", "Modena", "Verona", "Trento", "Calvatone", "Cremona", "Brescia", "Milan", "Como", "Novara", "Vercelli", "Ivrea", "Turin", "Chieri", "Tortona", "Alba", "Augusta Bagiennorum", "Luna"))
ggplot(first2, aes(x = factor(site), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(title="First Century AD Fineware Origins by Site",x = "", y = NULL, fill = "Origin") +  theme_minimal(base_size = 14)  + scale_fill_manual(values = c("#02401B", "#9C964A", "#A2A475", "#81A88D", "#972D15")) + theme(axis.text.x = element_text(angle = 90))

### Hierarchical Clustering, no ports ###

library(readxl)
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
first <- subset(finewares, shortdate=="1st Cent. AD")
first <- first[first$site != "Ariminum", ]
first <- first[first$site != "Altinum", ]
first <- first[first$site != "Aquileia", ]
first <- first[first$site != "Ravenna", ]
first <- first[first$site != "Luna", ]
first <- first[first$site != "Acqui Terme", ]
first2 <- first %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
firstMatrix <- acast(first2, site~origin, value.var="perc", fill=0)
View(firstMatrix)
firstpercs <- as.data.frame(firstMatrix)
library(ggdendro)
View(firstpercs)
library(ggplot2)
library(ggdendro)
measures <- firstpercs[,1:5]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
firstpercsHClust <- hclust(distMeasures, method="average")
ggdendrogram(firstpercsHClust, rotate=T) + labs(title="Hierarchical Clustering in the First Century AD Based on Fineware Provenance", x="", y="Distance between assemblages") + theme_minimal()

### Hierarchical Clustering, with Adriatic port ###

library(readxl)
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
first <- subset(finewares, shortdate=="1st Cent. AD")
first <- first[first$site != "Luna", ]
first <- first[first$site != "Acqui Terme", ]
first2 <- first %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
firstMatrix <- acast(first2, site~origin, value.var="perc", fill=0)
View(firstMatrix)
firstpercs <- as.data.frame(firstMatrix)
library(ggdendro)
View(firstpercs)
library(ggplot2)
library(ggdendro)
measures <- firstpercs[,1:5]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
firstpercsHClust <- hclust(distMeasures, method="average")
ggdendrogram(firstpercsHClust, rotate=T) + labs(title="Hierarchical Clustering in the First Century AD Based on Fineware Provenance with Adriatic Ports", x="", y="Distance between assemblages") + theme_minimal()

### Hierarchical Clustering, with Ligurian port ###

library(readxl)
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
first <- subset(finewares, shortdate=="1st Cent. AD")
first <- first[first$site != "Ariminum", ]
first <- first[first$site != "Altinum", ]
first <- first[first$site != "Aquileia", ]
first <- first[first$site != "Ravenna", ]
first <- first[first$site != "Acqui Terme", ]
first2 <- first %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
firstMatrix <- acast(first2, site~origin, value.var="perc", fill=0)
View(firstMatrix)
firstpercs <- as.data.frame(firstMatrix)
library(ggdendro)
View(firstpercs)
library(ggplot2)
library(ggdendro)
measures <- firstpercs[,1:5]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
firstpercsHClust <- hclust(distMeasures, method="average")
ggdendrogram(firstpercsHClust, rotate=T) + labs(title="Hierarchical Clustering in the First Century AD Based on Fineware Provenance with Ligurian Port", x="", y="Distance between assemblages") + theme_minimal()

### Simpsons Index ###

library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(vegan)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$type != "Unknown", ]
finewares <- finewares[finewares$type != "Adriatic", ]
finewares <- finewares[finewares$type != "African", ]
finewares <- finewares[finewares$type != "Central Italic", ]
finewares <- finewares[finewares$type != "Eastern", ]
finewares <- finewares[finewares$type != "Gallic", ]
finewares <- finewares[finewares$type != "Northern Italic", ]
first <- subset(finewares, shortdate=="1st Cent. AD")
first <- first[first$site != "Acqui Terme", ]
first <- first[first$site != "Turin", ]
firstMatrix <- acast(first, site~type, value.var="type", fill=0)
View(firstMatrix)
diversity(firstMatrix, index="simpson")


### Morisita-Horn Overlap Coefficient ###

library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(vegan)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$type != "Unknown", ]
finewares <- finewares[finewares$type != "Adriatic", ]
finewares <- finewares[finewares$type != "African", ]
finewares <- finewares[finewares$type != "Central Italic", ]
finewares <- finewares[finewares$type != "Eastern", ]
finewares <- finewares[finewares$type != "Gallic", ]
finewares <- finewares[finewares$type != "Northern Italic", ]
first <- subset(finewares, shortdate=="1st Cent. AD")
first <- first[first$site != "Acqui Terme", ]
firstMatrix <- acast(first, site~type, value.var="type", fill=0)
View(firstMatrix)
firstDistance <- vegdist(firstMatrix, method="horn")
firstDistance
firstDistanceMatrix <- as.matrix(firstDistance)
first <- melt(firstDistanceMatrix, varnames=c("site1", "site2"), value.name="distance")
library(ggplot2)
ggplot(first, aes(x=site1, y=site2, fill=distance, label=round(distance, 2))) + geom_raster() + labs(title="Morisita-Horn Overlap Coefficient", x = NULL, y = NULL, fill = "Distance") + geom_raster() + geom_text(color="white")
morisitaHClust <- hclust(firstDistance, method="average")
library(ggdendro)
ggdendrogram(morisitaHClust, rotate=T) + labs(title="First Century AD Vessel Similarity Between Sites Using Morisita-Horn Overlap Coefficient", x="", y="Distance between assemblages") + theme_minimal()



#### Second Cent. AD ####

### Total Provenances for Period ###

library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
finewares <- finewares[finewares$shortdate != "Unknown", ]
finewares2 <- finewares %>% 
  group_by(shortdate,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(finewares2)
second <- subset(finewares2, shortdate=="2nd Cent. AD")
ggplot(second, aes(x = factor(shortdate), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +  labs(title="Provenance of Second Century AD Finewares",x = "", y = NULL, fill = "Vessel Type") +  theme_minimal(base_size = 14) + scale_fill_manual(values = c("#02401B", "#A2A475", "#81A88D", "#972D15"))


### Provenance by Site ###

library(readxl) 
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
View(finewares)
finewares <- finewares[finewares$shortdate != "Unknown", ]
finewares <- finewares[finewares$origin != "Unknown", ]
finewares <- finewares[finewares$site != "Acqui Terme", ]
finewares <- finewares[finewares$site != "Ariminum", ]
finewares <- finewares[finewares$site != "Brescia", ]
finewares <- finewares[finewares$site != "Calvatone", ]
finewares <- finewares[finewares$site != "Forli", ]
finewares <- finewares[finewares$site != "Ivrea", ]
finewares <- finewares[finewares$site != "Julia Concordia", ]
finewares <- finewares[finewares$site != "Padua", ]
finewares <- finewares[finewares$site != "Ravenna", ]
finewares <- finewares[finewares$site != "Trento", ]
library(wesanderson)
second <- subset(finewares, shortdate=="2nd Cent. AD")
second2 <- second %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(second2)
second2$site <- factor(second2$site,levels = c("Altinum", "Aquileia", "Verona", "Cremona", "Milan", "Como", "Chieri", "Alba"))
ggplot(second2, aes(x = factor(site), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(title="Second Century AD Fineware Origins by Site",x = "", y = NULL, fill = "Origin") +  theme_minimal(base_size = 14)  + scale_fill_manual(values = c("#02401B", "#A2A475", "#81A88D", "#972D15")) + theme(axis.text.x = element_text(angle = 90))

### Hierarchical Clustering, no ports ###

library(readxl)
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
second <- subset(finewares, shortdate=="2nd Cent. AD")
second <- second[second$site != "Ariminum", ]
second <- second[second$site != "Altinum", ]
second <- second[second$site != "Aquileia", ]
second <- second[second$site != "Ravenna", ]
second <- second[second$site != "Luna", ]
second <- second[second$site != "Acqui Terme", ]
second <- second[second$site != "Ariminum", ]
second <- second[second$site != "Brescia", ]
second <- second[second$site != "Calvatone", ]
second <- second[second$site != "Forli", ]
second <- second[second$site != "Ivrea", ]
second <- second[second$site != "Julia Concordia", ]
second <- second[second$site != "Padua", ]
second <- second[second$site != "Ravenna", ]
second <- second[second$site != "Trento", ]
second2 <- second %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
secondMatrix <- acast(second2, site~origin, value.var="perc", fill=0)
View(secondMatrix)
secondpercs <- as.data.frame(secondMatrix)
library(ggdendro)
View(secondpercs)
library(ggplot2)
library(ggdendro)
measures <- secondpercs[,1:3]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
secondpercsHClust <- hclust(distMeasures, method="average")
ggdendrogram(secondpercsHClust, rotate=T) + labs(title="Hierarchical Clustering in the Second Century AD Based on Fineware Provenance", x="", y="Distance between assemblages") + theme_minimal()

### Hierarchical Clustering, with Adriatic ports ###

library(readxl)
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
second <- subset(finewares, shortdate=="2nd Cent. AD")
second <- second[second$site != "Luna", ]
second <- second[second$site != "Acqui Terme", ]
second <- second[second$site != "Ariminum", ]
second <- second[second$site != "Brescia", ]
second <- second[second$site != "Calvatone", ]
second <- second[second$site != "Forli", ]
second <- second[second$site != "Ivrea", ]
second <- second[second$site != "Julia Concordia", ]
second <- second[second$site != "Padua", ]
second <- second[second$site != "Ravenna", ]
second <- second[second$site != "Trento", ]
second2 <- second %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
secondMatrix <- acast(second2, site~origin, value.var="perc", fill=0)
View(secondMatrix)
secondpercs <- as.data.frame(secondMatrix)
library(ggdendro)
View(secondpercs)
library(ggplot2)
library(ggdendro)
measures <- secondpercs[,1:4]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
secondpercsHClust <- hclust(distMeasures, method="average")
ggdendrogram(secondpercsHClust, rotate=T) + labs(title="Hierarchical Clustering in the Second Century AD Based on Fineware Provenance with Adriatic Ports", x="", y="Distance between assemblages") + theme_minimal()

### Simpsons Index ###

library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(vegan)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$type != "Unknown", ]
finewares <- finewares[finewares$type != "Adriatic", ]
finewares <- finewares[finewares$type != "African", ]
finewares <- finewares[finewares$type != "Central Italic", ]
finewares <- finewares[finewares$type != "Eastern", ]
finewares <- finewares[finewares$type != "Gallic", ]
finewares <- finewares[finewares$type != "Northern Italic", ]
second <- subset(finewares, shortdate=="2nd Cent. AD")
second <- second[second$site != "Acqui Terme", ]
second <- second[second$site != "Ariminum", ]
second <- second[second$site != "Brescia", ]
second <- second[second$site != "Calvatone", ]
second <- second[second$site != "Forli", ]
second <- second[second$site != "Ivrea", ]
second <- second[second$site != "Julia Concordia", ]
second <- second[second$site != "Padua", ]
second <- second[second$site != "Ravenna", ]
second <- second[second$site != "Trento", ]
secondMatrix <- acast(second, site~type, value.var="type", fill=0)
View(secondMatrix)
diversity(secondMatrix, index="simpson")


### Morisita-Horn Overlap Coefficient ###

library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(vegan)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$type != "Unknown", ]
finewares <- finewares[finewares$type != "Adriatic", ]
finewares <- finewares[finewares$type != "African", ]
finewares <- finewares[finewares$type != "Central Italic", ]
finewares <- finewares[finewares$type != "Eastern", ]
finewares <- finewares[finewares$type != "Gallic", ]
finewares <- finewares[finewares$type != "Northern Italic", ]
second <- subset(finewares, shortdate=="2nd Cent. AD")
second <- second[second$site != "Acqui Terme", ]
second <- second[second$site != "Ariminum", ]
second <- second[second$site != "Brescia", ]
second <- second[second$site != "Calvatone", ]
second <- second[second$site != "Forli", ]
second <- second[second$site != "Ivrea", ]
second <- second[second$site != "Julia Concordia", ]
second <- second[second$site != "Padua", ]
second <- second[second$site != "Ravenna", ]
second <- second[second$site != "Trento", ]
secondMatrix <- acast(second, site~type, value.var="type", fill=0)
View(secondMatrix)
secondDistance <- vegdist(secondMatrix, method="horn")
secondDistance
secondDistanceMatrix <- as.matrix(secondDistance)
second <- melt(secondDistanceMatrix, varnames=c("site1", "site2"), value.name="distance")
library(ggplot2)
ggplot(second, aes(x=site1, y=site2, fill=distance, label=round(distance, 2))) + geom_raster() + labs(title="Morisita-Horn Overlap Coefficient", x = NULL, y = NULL, fill = "Distance") + geom_raster() + geom_text(color="white")
morisitaHClust <- hclust(secondDistance, method="average")
library(ggdendro)
ggdendrogram(morisitaHClust, rotate=T) + labs(title="Second Century AD Vessel Similarity Between Sites Using Morisita-Horn Overlap Coefficient", x="", y="Distance between assemblages") + theme_minimal()


#### Third Cent. AD ####

### Total Provenances for Period ###

library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
finewares <- finewares[finewares$shortdate != "Unknown", ]
finewares2 <- finewares %>% 
  group_by(shortdate,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(finewares2)
third <- subset(finewares2, shortdate=="3rd Cent. AD")
ggplot(third, aes(x = factor(shortdate), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +  labs(title="Provenance of Third Century AD Finewares",x = "", y = NULL, fill = "Vessel Type") +  theme_minimal(base_size = 14) + scale_fill_manual(values = c("#CDC08C", "#02401B", "#A2A475", "#81A88D", "#972D15"))


### Provenance by Site ###

library(readxl) 
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
View(finewares)
finewares <- finewares[finewares$shortdate != "Unknown", ]
finewares <- finewares[finewares$origin != "Unknown", ]
third <- subset(finewares, shortdate=="3rd Cent. AD")
third <- third[third$site != "Acqui Terme", ]
third <- third[third$site != "Augusta Bagiennorum", ]
third <- third[third$site != "Calvatone", ]
third <- third[third$site != "Cremona", ]
third <- third[third$site != "Julia Concordia", ]
third <- third[third$site != "Trento", ]
third2 <- third %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(third2)
third2$site <- factor(third2$site,levels = c("Ariminum", "Altinum", "Aquileia", "Forli", "Verona", "Brescia", "Milan",  "Alba"))
ggplot(third2, aes(x = factor(site), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(title="Third Century AD Fineware Origins by Site",x = "", y = NULL, fill = "Origin") +  theme_minimal(base_size = 14)  + scale_fill_manual(values = c("#CDC08C", "#02401B", "#A2A475", "#81A88D", "#972D15")) + theme(axis.text.x = element_text(angle = 90))

### Hierarchical Clustering, no ports ###

library(readxl)
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
third <- subset(finewares, shortdate=="3rd Cent. AD")
third <- third[third$site != "Altinum", ]
third <- third[third$site != "Aquileia", ]
third <- third[third$site != "Ariminum", ]
third <- third[third$site != "Acqui Terme", ]
third <- third[third$site != "Augusta Bagiennorum", ]
third <- third[third$site != "Calvatone", ]
third <- third[third$site != "Cremona", ]
third <- third[third$site != "Julia Concordia", ]
third <- third[third$site != "Trento", ]
third2 <- third %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
thirdMatrix <- acast(third2, site~origin, value.var="perc", fill=0)
View(thirdMatrix)
thirdpercs <- as.data.frame(thirdMatrix)
library(ggdendro)
View(thirdpercs)
library(ggplot2)
library(ggdendro)
measures <- thirdpercs[,1:5]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
thirdpercsHClust <- hclust(distMeasures, method="average")
ggdendrogram(thirdpercsHClust, rotate=T) + labs(title="Hierarchical Clustering in the Third Century AD Based on Fineware Provenance", x="", y="Distance between assemblages") + theme_minimal()

### Hierarchical Clustering, with Adriatic ports ###

library(readxl)
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
third <- subset(finewares, shortdate=="3rd Cent. AD")
third <- third[third$site != "Acqui Terme", ]
third <- third[third$site != "Augusta Bagiennorum", ]
third <- third[third$site != "Calvatone", ]
third <- third[third$site != "Cremona", ]
third <- third[third$site != "Julia Concordia", ]
third <- third[third$site != "Trento", ]
third2 <- third %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
thirdMatrix <- acast(third2, site~origin, value.var="perc", fill=0)
View(thirdMatrix)
thirdpercs <- as.data.frame(thirdMatrix)
library(ggdendro)
View(thirdpercs)
library(ggplot2)
library(ggdendro)
measures <- thirdpercs[,1:5]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
thirdpercsHClust <- hclust(distMeasures, method="average")
ggdendrogram(thirdpercsHClust, rotate=T) + labs(title="Hierarchical Clustering in the Third Century AD Based on Fineware Provenance with Adriatic Ports", x="", y="Distance between assemblages") + theme_minimal()

### Simpsons Index ###

library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(vegan)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$type != "Unknown", ]
finewares <- finewares[finewares$type != "Adriatic", ]
finewares <- finewares[finewares$type != "African", ]
finewares <- finewares[finewares$type != "Central Italic", ]
finewares <- finewares[finewares$type != "Eastern", ]
finewares <- finewares[finewares$type != "Gallic", ]
finewares <- finewares[finewares$type != "Northern Italic", ]
third <- subset(finewares, shortdate=="3rd Cent. AD")
third <- third[third$site != "Acqui Terme", ]
third <- third[third$site != "Augusta Bagiennorum", ]
third <- third[third$site != "Calvatone", ]
third <- third[third$site != "Cremona", ]
third <- third[third$site != "Julia Concordia", ]
third <- third[third$site != "Trento", ]
thirdMatrix <- acast(third, site~type, value.var="type", fill=0)
View(thirdMatrix)
diversity(thirdMatrix, index="simpson")


### Morisita-Horn Overlap Coefficient ###

library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(vegan)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$type != "Unknown", ]
finewares <- finewares[finewares$type != "Adriatic", ]
finewares <- finewares[finewares$type != "African", ]
finewares <- finewares[finewares$type != "Central Italic", ]
finewares <- finewares[finewares$type != "Eastern", ]
finewares <- finewares[finewares$type != "Gallic", ]
finewares <- finewares[finewares$type != "Northern Italic", ]
third <- subset(finewares, shortdate=="3rd Cent. AD")
third <- third[third$site != "Acqui Terme", ]
third <- third[third$site != "Augusta Bagiennorum", ]
third <- third[third$site != "Calvatone", ]
third <- third[third$site != "Cremona", ]
third <- third[third$site != "Julia Concordia", ]
third <- third[third$site != "Trento", ]
thirdMatrix <- acast(third, site~type, value.var="type", fill=0)
View(thirdMatrix)
thirdDistance <- vegdist(thirdMatrix, method="horn")
thirdDistance
thirdDistanceMatrix <- as.matrix(thirdDistance)
third <- melt(thirdDistanceMatrix, varnames=c("site1", "site2"), value.name="distance")
library(ggplot2)
ggplot(third, aes(x=site1, y=site2, fill=distance, label=round(distance, 2))) + geom_raster() + labs(title="Morisita-Horn Overlap Coefficient", x = NULL, y = NULL, fill = "Distance") + geom_raster() + geom_text(color="white")
morisitaHClust <- hclust(thirdDistance, method="average")
library(ggdendro)
ggdendrogram(morisitaHClust, rotate=T) + labs(title="Third Century AD Vessel Similarity Between Sites Using Morisita-Horn Overlap Coefficient", x="", y="Distance between assemblages") + theme_minimal()


#### Fourth Cent. AD ####

### Total Provenances for Period ###

library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
finewares <- finewares[finewares$shortdate != "Unknown", ]
finewares2 <- finewares %>% 
  group_by(shortdate,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(finewares2)
fourth <- subset(finewares2, shortdate=="4th Cent. AD")
ggplot(fourth, aes(x = factor(shortdate), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +  labs(title="Provenance of Fourth Century AD Finewares",x = "", y = NULL, fill = "Vessel Type") +  theme_minimal(base_size = 14) + scale_fill_manual(values = c("#CDC08C", "#02401B", "#972D15"))


### Provenance by Site ###

library(readxl) 
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
View(finewares)
finewares <- finewares[finewares$shortdate != "Unknown", ]
finewares <- finewares[finewares$origin != "Unknown", ]
fourth <- subset(finewares, shortdate=="4th Cent. AD")
fourth <- fourth[fourth$site != "Acqui Terme", ]
fourth <- fourth[fourth$site != "Calvatone", ]
fourth <- fourth[fourth$site != "Como", ]
fourth <- fourth[fourth$site != "Forli", ]
fourth <- fourth[fourth$site != "Trento", ]
fourth2 <- fourth %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(fourth2)
fourth2$site <- factor(fourth2$site,levels = c("Ariminum", "Altinum", "Aquileia", "Verona", "Brescia", "Milan",  "Alba"))
ggplot(fourth2, aes(x = factor(site), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(title="Fourth Century AD Fineware Origins by Site",x = "", y = NULL, fill = "Origin") +  theme_minimal(base_size = 14) + scale_fill_manual(values = c("#CDC08C", "#02401B", "#972D15")) + theme(axis.text.x = element_text(angle = 90))

### Hierarchical Clustering, no ports ###

library(readxl)
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
fourth <- subset(finewares, shortdate=="4th Cent. AD")
fourth <- fourth[fourth$site != "Altinum", ]
fourth <- fourth[fourth$site != "Aquileia", ]
fourth <- fourth[fourth$site != "Ariminum", ]
fourth <- fourth[fourth$site != "Acqui Terme", ]
fourth <- fourth[fourth$site != "Calvatone", ]
fourth <- fourth[fourth$site != "Como", ]
fourth <- fourth[fourth$site != "Forli", ]
fourth <- fourth[fourth$site != "Trento", ]
fourth2 <- fourth %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
fourthMatrix <- acast(fourth2, site~origin, value.var="perc", fill=0)
View(fourthMatrix)
fourthpercs <- as.data.frame(fourthMatrix)
library(ggdendro)
View(fourthpercs)
library(ggplot2)
library(ggdendro)
measures <- fourthpercs[,1:2]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
fourthpercsHClust <- hclust(distMeasures, method="average")
ggdendrogram(fourthpercsHClust, rotate=T) + labs(title="Hierarchical Clustering in the Fourth Century AD Based on Fineware Provenance", x="", y="Distance between assemblages") + theme_minimal()

### Hierarchical Clustering, with Adriatic ports ###

library(readxl)
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
fourth <- subset(finewares, shortdate=="4th Cent. AD")
fourth <- fourth[fourth$site != "Acqui Terme", ]
fourth <- fourth[fourth$site != "Calvatone", ]
fourth <- fourth[fourth$site != "Como", ]
fourth <- fourth[fourth$site != "Forli", ]
fourth <- fourth[fourth$site != "Trento", ]
fourth2 <- fourth %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
fourthMatrix <- acast(fourth2, site~origin, value.var="perc", fill=0)
View(fourthMatrix)
fourthpercs <- as.data.frame(fourthMatrix)
library(ggdendro)
View(fourthpercs)
library(ggplot2)
library(ggdendro)
measures <- fourthpercs[,1:3]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
fourthpercsHClust <- hclust(distMeasures, method="average")
ggdendrogram(fourthpercsHClust, rotate=T) + labs(title="Hierarchical Clustering in the Fourth Century AD Based on Fineware Provenance with Adriatic Ports", x="", y="Distance between assemblages") + theme_minimal()

### Simpsons Index ###

library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(vegan)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$type != "Unknown", ]
finewares <- finewares[finewares$type != "Adriatic", ]
finewares <- finewares[finewares$type != "African", ]
finewares <- finewares[finewares$type != "Central Italic", ]
finewares <- finewares[finewares$type != "Eastern", ]
finewares <- finewares[finewares$type != "Gallic", ]
finewares <- finewares[finewares$type != "Northern Italic", ]
fourth <- subset(finewares, shortdate=="4th Cent. AD")
fourth <- fourth[fourth$site != "Acqui Terme", ]
fourth <- fourth[fourth$site != "Calvatone", ]
fourth <- fourth[fourth$site != "Como", ]
fourth <- fourth[fourth$site != "Forli", ]
fourth <- fourth[fourth$site != "Trento", ]
fourthMatrix <- acast(fourth, site~type, value.var="type", fill=0)
View(fourthMatrix)
diversity(fourthMatrix, index="simpson")


### Morisita-Horn Overlap Coefficient ###

library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(vegan)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$type != "Unknown", ]
finewares <- finewares[finewares$type != "Adriatic", ]
finewares <- finewares[finewares$type != "African", ]
finewares <- finewares[finewares$type != "Central Italic", ]
finewares <- finewares[finewares$type != "Eastern", ]
finewares <- finewares[finewares$type != "Gallic", ]
finewares <- finewares[finewares$type != "Northern Italic", ]
fourth <- subset(finewares, shortdate=="4th Cent. AD")
fourth <- fourth[fourth$site != "Acqui Terme", ]
fourth <- fourth[fourth$site != "Calvatone", ]
fourth <- fourth[fourth$site != "Como", ]
fourth <- fourth[fourth$site != "Forli", ]
fourth <- fourth[fourth$site != "Trento", ]
fourthMatrix <- acast(fourth, site~type, value.var="type", fill=0)
View(fourthMatrix)
fourthDistance <- vegdist(fourthMatrix, method="horn")
fourthDistance
fourthDistanceMatrix <- as.matrix(fourthDistance)
fourth <- melt(fourthDistanceMatrix, varnames=c("site1", "site2"), value.name="distance")
library(ggplot2)
ggplot(fourth, aes(x=site1, y=site2, fill=distance, label=round(distance, 2))) + geom_raster() + labs(title="Morisita-Horn Overlap Coefficient", x = NULL, y = NULL, fill = "Distance") + geom_raster() + geom_text(color="white")
morisitaHClust <- hclust(fourthDistance, method="average")
library(ggdendro)
ggdendrogram(morisitaHClust, rotate=T) + labs(title="Fourth Century AD Vessel Similarity Between Sites Using Morisita-Horn Overlap Coefficient", x="", y="Distance between assemblages") + theme_minimal()


#### Fifth Cent. AD ####

### Total Provenances for Period ###

library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
finewares <- finewares[finewares$shortdate != "Unknown", ]
fifth <- subset(finewares, shortdate=="5th Cent. AD")
fifth <- fifth[fifth$site != "Acqui Terme", ]
fifth <- fifth[fifth$site != "Alba", ]
fifth <- fifth[fifth$site != "Altinum", ]
fifth <- fifth[fifth$site != "Aquileia", ]
fifth <- fifth[fifth$site != "Ariminum", ]
fifth <- fifth[fifth$site != "Trento", ]
fifth2 <- fifth %>% 
  group_by(shortdate,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(fifth2)
ggplot(fifth2, aes(x = factor(shortdate), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +  labs(title="Provenance of Fifth Century AD Finewares",x = "", y = NULL, fill = "Vessel Type") +  theme_minimal(base_size = 14) + scale_fill_manual(values = c("#CDC08C", "#02401B", "#972D15"))


### Provenance by Site ###

library(readxl) 
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")
View(finewares)
finewares <- finewares[finewares$shortdate != "Unknown", ]
finewares <- finewares[finewares$origin != "Unknown", ]
fifth <- subset(finewares, shortdate=="5th Cent. AD")
fifth <- fifth[fifth$site != "Acqui Terme", ]
fifth <- fifth[fifth$site != "Alba", ]
fifth <- fifth[fifth$site != "Altinum", ]
fifth <- fifth[fifth$site != "Aquileia", ]
fifth <- fifth[fifth$site != "Ariminum", ]
fifth <- fifth[fifth$site != "Trento", ]
fifth2 <- fifth %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(fifth2)
fifth2$site <- factor(fifth2$site,levels = c("Verona", "Brescia", "Milan"))
ggplot(fifth2, aes(x = factor(site), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(title="Fifth Century AD Fineware Origins by Site",x = "", y = NULL, fill = "Origin") +  theme_minimal(base_size = 14)  + scale_fill_manual(values = c("#CDC08C", "#02401B", "#972D15")) + theme(axis.text.x = element_text(angle = 90))

### Hierarchical Clustering, no ports ###

library(readxl)
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$origin != "Unknown", ]
fifth <- subset(finewares, shortdate=="5th Cent. AD")
fifth <- fifth[fifth$site != "Acqui Terme", ]
fifth <- fifth[fifth$site != "Alba", ]
fifth <- fifth[fifth$site != "Altinum", ]
fifth <- fifth[fifth$site != "Aquileia", ]
fifth <- fifth[fifth$site != "Ariminum", ]
fifth <- fifth[fifth$site != "Trento", ]
fifth2 <- fifth %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
fifthMatrix <- acast(fifth2, site~origin, value.var="perc", fill=0)
View(fifthMatrix)
fifthpercs <- as.data.frame(fifthMatrix)
library(ggdendro)
View(fifthpercs)
library(ggplot2)
library(ggdendro)
measures <- fifthpercs[,1]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
fifthpercsHClust <- hclust(distMeasures, method="average")
ggdendrogram(fifthpercsHClust, rotate=T) + labs(title="Hierarchical Clustering in the Fifth Century AD Based on Fineware Provenance", x="", y="Distance between assemblages") + theme_minimal()

### Simpsons Index ###

library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(vegan)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$type != "Unknown", ]
finewares <- finewares[finewares$type != "Adriatic", ]
finewares <- finewares[finewares$type != "African", ]
finewares <- finewares[finewares$type != "Central Italic", ]
finewares <- finewares[finewares$type != "Eastern", ]
finewares <- finewares[finewares$type != "Gallic", ]
finewares <- finewares[finewares$type != "Northern Italic", ]
fifth <- subset(finewares, shortdate=="5th Cent. AD")
fifth <- fifth[fifth$site != "Acqui Terme", ]
fifth <- fifth[fifth$site != "Alba", ]
fifth <- fifth[fifth$site != "Altinum", ]
fifth <- fifth[fifth$site != "Aquileia", ]
fifth <- fifth[fifth$site != "Ariminum", ]
fifth <- fifth[fifth$site != "Trento", ]
fifthMatrix <- acast(fifth, site~type, value.var="type", fill=0)
View(fifthMatrix)
diversity(fifthMatrix, index="simpson")


### Morisita-Horn Overlap Coefficient ###

library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(vegan)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$type != "Unknown", ]
finewares <- finewares[finewares$type != "Adriatic", ]
finewares <- finewares[finewares$type != "African", ]
finewares <- finewares[finewares$type != "Central Italic", ]
finewares <- finewares[finewares$type != "Eastern", ]
finewares <- finewares[finewares$type != "Gallic", ]
finewares <- finewares[finewares$type != "Northern Italic", ]
fifth <- subset(finewares, shortdate=="5th Cent. AD")
fifth <- fifth[fifth$site != "Acqui Terme", ]
fifth <- fifth[fifth$site != "Alba", ]
fifth <- fifth[fifth$site != "Altinum", ]
fifth <- fifth[fifth$site != "Aquileia", ]
fifth <- fifth[fifth$site != "Ariminum", ]
fifth <- fifth[fifth$site != "Trento", ]
fifthMatrix <- acast(fifth, site~type, value.var="type", fill=0)
View(fifthMatrix)
fifthDistance <- vegdist(fifthMatrix, method="horn")
fifthDistance
fifthDistanceMatrix <- as.matrix(fourthDistance)
fifth <- melt(fifthDistanceMatrix, varnames=c("site1", "site2"), value.name="distance")
library(ggplot2)
ggplot(fifth, aes(x=site1, y=site2, fill=distance, label=round(distance, 2))) + geom_raster() + labs(title="Morisita-Horn Overlap Coefficient", x = NULL, y = NULL, fill = "Distance") + geom_raster() + geom_text(color="white")
morisitaHClust <- hclust(fifthDistance, method="average")
library(ggdendro)
ggdendrogram(morisitaHClust, rotate=T) + labs(title="Fifth Century AD Vessel Similarity Between Sites Using Morisita-Horn Overlap Coefficient", x="", y="Distance between assemblages") + theme_minimal()

##################################################################################################################################################


#### Stamps ####

### Stamp Diversity Morista Horn ####
library(readxl)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$workshop != "Unknown", ]
finewares <- finewares[finewares$workshop != "NULL", ]
finewares <- finewares[finewares$site != "Ariminum", ]
finewares <- finewares[finewares$site != "Altinum", ]
finewares <- finewares[finewares$site != "Bologna", ]
finewares <- finewares[finewares$site != "Calvatone", ]
finewares <- finewares[finewares$site != "Como", ]
finewares <- finewares[finewares$site != "Trento", ]
finewares <- finewares[finewares$site != "Ravenna", ]
finewares <- finewares[finewares$site != "Acqui Terme", ]
library(reshape2)
imperialMatrix <- acast(finewares, site~workshop, value.var="workshop", fill=0)
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
ggdendrogram(morisitaHClust, rotate=T) + labs(title="Stamp Similarity Between Sites Using Morisita-Horn Overlap Coefficient", x="", y="Distance between assemblages") + theme_minimal()

### Stamps by Provenance Morisita-Horn ####

### Northern Italic Stamps ###

library(readxl)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$workshop != "Unknown", ]
finewares <- finewares[finewares$workshop != "NULL", ]
finewares <- finewares[finewares$site != "Ariminum", ]
finewares <- finewares[finewares$site != "Altinum", ]
finewares <- finewares[finewares$site != "Bologna", ]
finewares <- finewares[finewares$site != "Calvatone", ]
finewares <- finewares[finewares$site != "Como", ]
finewares <- finewares[finewares$site != "Trento", ]
finewares <- finewares[finewares$site != "Ravenna", ]
finewares <- finewares[finewares$site != "Acqui Terme", ]
finewares <- finewares[finewares$origin2 != "Central Italic", ]
finewares <- finewares[finewares$origin2 != "Central Gallic", ]
finewares <- finewares[finewares$origin2 != "South Gallic", ]
finewares <- finewares[finewares$origin2 != "Arretine", ]
finewares <- finewares[finewares$origin2 != "Eastern", ]
finewares <- finewares[finewares$origin2 != "Unknown", ]
library(reshape2)
imperialMatrix <- acast(finewares, site~workshop, value.var="workshop", fill=0)
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
ggdendrogram(morisitaHClust, rotate=T) + labs(title="Northern Italic Stamp Similarity Between Sites Using Morisita-Horn Overlap Coefficient", y="Distance between assemblages") + theme_minimal()

### All Central Italic Stamps ###

library(readxl)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$workshop != "Unknown", ]
finewares <- finewares[finewares$workshop != "NULL", ]
finewares <- finewares[finewares$site != "Ariminum", ]
finewares <- finewares[finewares$site != "Altinum", ]
finewares <- finewares[finewares$site != "Bologna", ]
finewares <- finewares[finewares$site != "Calvatone", ]
finewares <- finewares[finewares$site != "Como", ]
finewares <- finewares[finewares$site != "Trento", ]
finewares <- finewares[finewares$site != "Ravenna", ]
finewares <- finewares[finewares$site != "Acqui Terme", ]
finewares <- finewares[finewares$origin2 != "Northern Italic", ]
finewares <- finewares[finewares$origin2 != "Central Gallic", ]
finewares <- finewares[finewares$origin2 != "South Gallic", ]
finewares <- finewares[finewares$origin2 != "Eastern", ]
finewares <- finewares[finewares$origin2 != "Unknown", ]
library(reshape2)
imperialMatrix <- acast(finewares, site~workshop, value.var="workshop", fill=0)
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
ggdendrogram(morisitaHClust, rotate=T) + labs(title="Arretine and Central Italic Stamp Similarity Between Sites Using Morisita-Horn Overlap Coefficient", x="", y="Distance between assemblages") + theme_minimal()

### Arretine Stamps ###

library(readxl)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$workshop != "Unknown", ]
finewares <- finewares[finewares$workshop != "NULL", ]
finewares <- finewares[finewares$site != "Ariminum", ]
finewares <- finewares[finewares$site != "Altinum", ]
finewares <- finewares[finewares$site != "Bologna", ]
finewares <- finewares[finewares$site != "Calvatone", ]
finewares <- finewares[finewares$site != "Como", ]
finewares <- finewares[finewares$site != "Trento", ]
finewares <- finewares[finewares$site != "Ravenna", ]
finewares <- finewares[finewares$site != "Acqui Terme", ]
finewares <- finewares[finewares$origin2 != "Northern Italic", ]
finewares <- finewares[finewares$origin2 != "Central Gallic", ]
finewares <- finewares[finewares$origin2 != "South Gallic", ]
finewares <- finewares[finewares$origin2 != "Eastern", ]
finewares <- finewares[finewares$origin2 != "Unknown", ]
finewares <- finewares[finewares$origin2 != "Central Italic", ]
library(reshape2)
imperialMatrix <- acast(finewares, site~workshop, value.var="workshop", fill=0)
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
ggdendrogram(morisitaHClust, rotate=T) + labs(title="Arretine Stamp Similarity Between Sites Using Morisita-Horn Overlap Coefficient", x="", y="Distance between assemblages") + theme_minimal()


### Central Italic Stamps ###

library(readxl)
finewares <- read_excel("~/finewares.xlsx")
finewares <- finewares[finewares$workshop != "Unknown", ]
finewares <- finewares[finewares$workshop != "NULL", ]
finewares <- finewares[finewares$site != "Ariminum", ]
finewares <- finewares[finewares$site != "Altinum", ]
finewares <- finewares[finewares$site != "Bologna", ]
finewares <- finewares[finewares$site != "Calvatone", ]
finewares <- finewares[finewares$site != "Como", ]
finewares <- finewares[finewares$site != "Trento", ]
finewares <- finewares[finewares$site != "Ravenna", ]
finewares <- finewares[finewares$site != "Acqui Terme", ]
finewares <- finewares[finewares$origin2 != "Northern Italic", ]
finewares <- finewares[finewares$origin2 != "Central Gallic", ]
finewares <- finewares[finewares$origin2 != "South Gallic", ]
finewares <- finewares[finewares$origin2 != "Eastern", ]
finewares <- finewares[finewares$origin2 != "Unknown", ]
finewares <- finewares[finewares$origin2 != "Arretine", ]
library(reshape2)
imperialMatrix <- acast(finewares, site~workshop, value.var="workshop", fill=0)
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
ggdendrogram(morisitaHClust, rotate=T) + labs(title="Non-Arretine Stamp Similarity Between Sites Using Morisita-Horn Overlap Coefficient", x="", y="Distance between assemblages") + theme_minimal()


#######################################################################################################################


### Vessels by Site and Period ####

library(readxl) 
library(ggplot2)
finewares <- read_excel("~/finewares.xlsx")

### Acqui Terme ###

acqui <- subset(finewares, site=="Acqui Terme")
imperial <- subset(acqui, longdate=="1st-2nd Cent. AD")
lateantique <- subset(acqui, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Acqui Terme First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Acqui Terme Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

acqui <- subset(finewares, site=="Acqui Terme")
augustan <- subset(acqui, shortdate=="Augustan")
first <- subset(acqui, shortdate=="1st Cent. AD")
second <- subset(acqui, shortdate=="2nd Cent. AD")
third <- subset(acqui, shortdate=="3rd Cent. AD")
fourth <- subset(acqui, shortdate=="4th Cent. AD")
fifth <- subset(acqui, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Acqui Terme Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Acqui Terme First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Acqui Terme Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Acqui Terme Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Acqui Terme Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Acqui Terme Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Adria ###

adria <- subset(finewares, site=="Adria")
imperial <- subset(adria, longdate=="1st-2nd Cent. AD")
lateantique <- subset(adria, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Adria First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Adria Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

adria <- subset(finewares, site=="Adria")
augustan <- subset(adria, shortdate=="Augustan")
first <- subset(adria, shortdate=="1st Cent. AD")
second <- subset(adria, shortdate=="2nd Cent. AD")
third <- subset(adria, shortdate=="3rd Cent. AD")
fourth <- subset(adria, shortdate=="4th Cent. AD")
fifth <- subset(adria, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Adria Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Adria First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Adria Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Adria Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Adria Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Adria Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Alba ###

alba <- subset(finewares, site=="Alba")
imperial <- subset(alba, longdate=="1st-2nd Cent. AD")
lateantique <- subset(alba, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Alba First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Alba Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

alba <- subset(finewares, site=="Alba")
augustan <- subset(alba, shortdate=="Augustan")
first <- subset(alba, shortdate=="1st Cent. AD")
second <- subset(alba, shortdate=="2nd Cent. AD")
third <- subset(alba, shortdate=="3rd Cent. AD")
fourth <- subset(alba, shortdate=="4th Cent. AD")
fifth <- subset(alba, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Alba Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Alba First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Alba Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Alba Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Alba Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Alba Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Altinum ###

altinum <- subset(finewares, site=="Altinum")
imperial <- subset(altinum, longdate=="1st-2nd Cent. AD")
lateantique <- subset(altinum, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Altinum First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Altinum Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

altinum <- subset(finewares, site=="Altinum")
augustan <- subset(altinum, shortdate=="Augustan")
first <- subset(altinum, shortdate=="1st Cent. AD")
second <- subset(altinum, shortdate=="2nd Cent. AD")
third <- subset(altinum, shortdate=="3rd Cent. AD")
fourth <- subset(altinum, shortdate=="4th Cent. AD")
fifth <- subset(altinum, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Altinum Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Altinum First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Altinum Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Altinum Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Altinum Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Altinum Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Aquileia ###

aquileia <- subset(finewares, site=="Aquileia")
imperial <- subset(aquileia, longdate=="1st-2nd Cent. AD")
lateantique <- subset(aquileia, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Aquileia First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Aquileia Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

aquileia <- subset(finewares, site=="Aquileia")
augustan <- subset(aquileia, shortdate=="Augustan")
first <- subset(aquileia, shortdate=="1st Cent. AD")
second <- subset(aquileia, shortdate=="2nd Cent. AD")
third <- subset(aquileia, shortdate=="3rd Cent. AD")
fourth <- subset(aquileia, shortdate=="4th Cent. AD")
fifth <- subset(aquileia, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Aquileia Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Aquileia First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Aquileia Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Aquileia Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Aquileia Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Aquileia Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Ariminum ###

rimini <- subset(finewares, site=="Ariminum")
imperial <- subset(rimini, longdate=="1st-2nd Cent. AD")
lateantique <- subset(rimini, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ariminum First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ariminum Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

rimini <- subset(finewares, site=="Ariminum")
augustan <- subset(rimini, shortdate=="Augustan")
first <- subset(rimini, shortdate=="1st Cent. AD")
second <- subset(rimini, shortdate=="2nd Cent. AD")
third <- subset(rimini, shortdate=="3rd Cent. AD")
fourth <- subset(rimini, shortdate=="4th Cent. AD")
fifth <- subset(rimini, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ariminum Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ariminum First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ariminum Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ariminum Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ariminum Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ariminum Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Augusta Bagiennorum ###

augusta <- subset(finewares, site=="Augusta Bagiennorum")
imperial <- subset(augusta, longdate=="1st-2nd Cent. AD")
lateantique <- subset(augusta, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Augusta Bagiennorum First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Augusta Bagiennorum Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

augusta <- subset(finewares, site=="Augusta Bagiennorum")
augustan <- subset(augusta, shortdate=="Augustan")
first <- subset(augusta, shortdate=="1st Cent. AD")
second <- subset(augusta, shortdate=="2nd Cent. AD")
third <- subset(augusta, shortdate=="3rd Cent. AD")
fourth <- subset(augusta, shortdate=="4th Cent. AD")
fifth <- subset(augusta, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Augusta Bagiennorum Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Augusta Bagiennorum First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Augusta Bagiennorum Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Augusta Bagiennorum Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Augusta Bagiennorum Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Augusta Bagiennorum Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Bologna ###

bologna <- subset(finewares, site=="Bologna")
imperial <- subset(bologna, longdate=="1st-2nd Cent. AD")
lateantique <- subset(bologna, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Bologna First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Bologna Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

bologna <- subset(finewares, site=="Bologna")
augustan <- subset(bologna, shortdate=="Augustan")
first <- subset(bologna, shortdate=="1st Cent. AD")
second <- subset(bologna, shortdate=="2nd Cent. AD")
third <- subset(bologna, shortdate=="3rd Cent. AD")
fourth <- subset(bologna, shortdate=="4th Cent. AD")
fifth <- subset(bologna, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Bologna Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Bologna First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Bologna Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Bologna Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Bologna Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Bologna Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Brescia ###

brescia <- subset(finewares, site=="Brescia")
imperial <- subset(brescia, longdate=="1st-2nd Cent. AD")
lateantique <- subset(brescia, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Brescia First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Brescia Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

brescia <- subset(finewares, site=="Brescia")
augustan <- subset(brescia, shortdate=="Augustan")
first <- subset(brescia, shortdate=="1st Cent. AD")
second <- subset(brescia, shortdate=="2nd Cent. AD")
third <- subset(brescia, shortdate=="3rd Cent. AD")
fourth <- subset(brescia, shortdate=="4th Cent. AD")
fifth <- subset(brescia, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Brescia Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Brescia First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Brescia Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Brescia Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Brescia Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Brescia Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Calvatone ###

calvatone <- subset(finewares, site=="Calvatone")
imperial <- subset(calvatone, longdate=="1st-2nd Cent. AD")
lateantique <- subset(calvatone, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Calvatone First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Calvatone Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

calvatone <- subset(finewares, site=="Calvatone")
augustan <- subset(calvatone, shortdate=="Augustan")
first <- subset(calvatone, shortdate=="1st Cent. AD")
second <- subset(calvatone, shortdate=="2nd Cent. AD")
third <- subset(calvatone, shortdate=="3rd Cent. AD")
fourth <- subset(calvatone, shortdate=="4th Cent. AD")
fifth <- subset(calvatone, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Calvatone Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Calvatone First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Calvatone Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Calvatone Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Calvatone Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Calvatone Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Chieri ###

chieri <- subset(finewares, site=="Chieri")
imperial <- subset(chieri, longdate=="1st-2nd Cent. AD")
lateantique <- subset(chieri, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Chieri First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Chieri Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

chieri <- subset(finewares, site=="Chieri")
augustan <- subset(chieri, shortdate=="Augustan")
first <- subset(chieri, shortdate=="1st Cent. AD")
second <- subset(chieri, shortdate=="2nd Cent. AD")
third <- subset(chieri, shortdate=="3rd Cent. AD")
fourth <- subset(chieri, shortdate=="4th Cent. AD")
fifth <- subset(chieri, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Chieri Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Chieri First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Chieri Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Chieri Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Chieri Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Chieri Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Cividate Camuno ###

cividate <- subset(finewares, site=="Cividate Camuno")
imperial <- subset(cividate, longdate=="1st-2nd Cent. AD")
lateantique <- subset(cividate, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cividate Camuno First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cividate Camuno Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

cividate <- subset(finewares, site=="Cividate Camuno")
augustan <- subset(cividate, shortdate=="Augustan")
first <- subset(cividate, shortdate=="1st Cent. AD")
second <- subset(cividate, shortdate=="2nd Cent. AD")
third <- subset(cividate, shortdate=="3rd Cent. AD")
fourth <- subset(cividate, shortdate=="4th Cent. AD")
fifth <- subset(cividate, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cividate Camuno Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cividate Camuno First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cividate Camuno Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cividate Camuno Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cividate Camuno Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cividate Camuno Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Como ###

como <- subset(finewares, site=="Como")
imperial <- subset(como, longdate=="1st-2nd Cent. AD")
lateantique <- subset(como, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Como First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Como Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

como <- subset(finewares, site=="Como")
augustan <- subset(como, shortdate=="Augustan")
first <- subset(como, shortdate=="1st Cent. AD")
second <- subset(como, shortdate=="2nd Cent. AD")
third <- subset(como, shortdate=="3rd Cent. AD")
fourth <- subset(como, shortdate=="4th Cent. AD")
fifth <- subset(como, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Como Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Como First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Como Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Como Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Como Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Como Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Cremona ###

cremona <- subset(finewares, site=="Cremona")
imperial <- subset(cremona, longdate=="1st-2nd Cent. AD")
lateantique <- subset(cremona, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cremona First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cremona Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

cremona <- subset(finewares, site=="Cremona")
augustan <- subset(cremona, shortdate=="Augustan")
first <- subset(cremona, shortdate=="1st Cent. AD")
second <- subset(cremona, shortdate=="2nd Cent. AD")
third <- subset(cremona, shortdate=="3rd Cent. AD")
fourth <- subset(cremona, shortdate=="4th Cent. AD")
fifth <- subset(cremona, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cremona Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cremona First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cremona Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cremona Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cremona Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Cremona Ffith Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Forli ###

forli <- subset(finewares, site=="Forli")
imperial <- subset(forli, longdate=="1st-2nd Cent. AD")
lateantique <- subset(forli, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Forli First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Forli Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

forli <- subset(finewares, site=="Forli")
augustan <- subset(forli, shortdate=="Augustan")
first <- subset(forli, shortdate=="1st Cent. AD")
second <- subset(forli, shortdate=="2nd Cent. AD")
third <- subset(forli, shortdate=="3rd Cent. AD")
fourth <- subset(forli, shortdate=="4th Cent. AD")
fifth <- subset(forli, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Forli Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Forli First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Forli Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Forli Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Forli Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Forli Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Ivrea ###

ivrea <- subset(finewares, site=="Ivrea")
imperial <- subset(ivrea, longdate=="1st-2nd Cent. AD")
lateantique <- subset(ivrea, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ivrea First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ivrea Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

ivrea <- subset(finewares, site=="Ivrea")
augustan <- subset(ivrea, shortdate=="Augustan")
first <- subset(ivrea, shortdate=="1st Cent. AD")
second <- subset(ivrea, shortdate=="2nd Cent. AD")
third <- subset(ivrea, shortdate=="3rd Cent. AD")
fourth <- subset(ivrea, shortdate=="4th Cent. AD")
fifth <- subset(ivrea, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ivrea Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ivrea First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ivrea Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ivrea Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ivrea Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ivrea Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Julia Concordia ###

concordia <- subset(finewares, site=="Julia Concordia")
imperial <- subset(concordia, longdate=="1st-2nd Cent. AD")
lateantique <- subset(concordia, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Julia Concordia First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Julia Concordia Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

concordia <- subset(finewares, site=="Julia Concordia")
augustan <- subset(concordia, shortdate=="Augustan")
first <- subset(concordia, shortdate=="1st Cent. AD")
second <- subset(concordia, shortdate=="2nd Cent. AD")
third <- subset(concordia, shortdate=="3rd Cent. AD")
fourth <- subset(concordia, shortdate=="4th Cent. AD")
fifth <- subset(concordia, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Julia Concordia Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Julia Concordia First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Julia Concordia Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Julia Concordia Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Julia Concordia Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Julia Concordia Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Luna ###

luna <- subset(finewares, site=="Luna")
imperial <- subset(luna, longdate=="1st-2nd Cent. AD")
lateantique <- subset(luna, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Luna First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Luna Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

luna <- subset(finewares, site=="Luna")
augustan <- subset(luna, shortdate=="Augustan")
first <- subset(luna, shortdate=="1st Cent. AD")
second <- subset(luna, shortdate=="2nd Cent. AD")
third <- subset(luna, shortdate=="3rd Cent. AD")
fourth <- subset(luna, shortdate=="4th Cent. AD")
fifth <- subset(luna, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Luna Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Luna First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Luna Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Luna Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Luna Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Luna Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Milan ###

milan <- subset(finewares, site=="Milan")
imperial <- subset(milan, longdate=="1st-2nd Cent. AD")
lateantique <- subset(milan, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Milan First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Milan Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

milan <- subset(finewares, site=="Milan")
augustan <- subset(milan, shortdate=="Augustan")
first <- subset(milan, shortdate=="1st Cent. AD")
second <- subset(milan, shortdate=="2nd Cent. AD")
third <- subset(milan, shortdate=="3rd Cent. AD")
fourth <- subset(milan, shortdate=="4th Cent. AD")
fifth <- subset(milan, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Milan Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Milan First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Milan Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Milan Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Milan Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Milan Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Modena ###

modena <- subset(finewares, site=="Modena")
imperial <- subset(modena, longdate=="1st-2nd Cent. AD")
lateantique <- subset(modena, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Modena First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Modena Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

modena <- subset(finewares, site=="Modena")
augustan <- subset(modena, shortdate=="Augustan")
first <- subset(modena, shortdate=="1st Cent. AD")
second <- subset(modena, shortdate=="2nd Cent. AD")
third <- subset(modena, shortdate=="3rd Cent. AD")
fourth <- subset(modena, shortdate=="4th Cent. AD")
fifth <- subset(modena, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Modena Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Modena First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Modena Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Modena Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Modena Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Modena Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Padua ###

padua <- subset(finewares, site=="Padua")
imperial <- subset(padua, longdate=="1st-2nd Cent. AD")
lateantique <- subset(padua, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Padua First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Padua Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

padua <- subset(finewares, site=="Padua")
augustan <- subset(padua, shortdate=="Augustan")
first <- subset(padua, shortdate=="1st Cent. AD")
second <- subset(padua, shortdate=="2nd Cent. AD")
third <- subset(padua, shortdate=="3rd Cent. AD")
fourth <- subset(padua, shortdate=="4th Cent. AD")
fifth <- subset(padua, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Padua Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Padua First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Padua Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Padua Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Padua Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Padua Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Ravenna ###

ravenna <- subset(finewares, site=="Ravenna")
imperial <- subset(ravenna, longdate=="1st-2nd Cent. AD")
lateantique <- subset(ravenna, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ravenna First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ravenna Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

ravenna <- subset(finewares, site=="Ravenna")
augustan <- subset(ravenna, shortdate=="Augustan")
first <- subset(ravenna, shortdate=="1st Cent. AD")
second <- subset(ravenna, shortdate=="2nd Cent. AD")
third <- subset(ravenna, shortdate=="3rd Cent. AD")
fourth <- subset(ravenna, shortdate=="4th Cent. AD")
fifth <- subset(ravena, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ravenna Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ravenna First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ravenna Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ravenna Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ravenna Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ravenna Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Tortona ###

tortona <- subset(finewares, site=="Tortona")
imperial <- subset(tortona, longdate=="1st-2nd Cent. AD")
lateantique <- subset(tortona, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Tortona First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Tortona Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

tortona <- subset(finewares, site=="Tortona")
augustan <- subset(tortona, shortdate=="Augustan")
first <- subset(tortona, shortdate=="1st Cent. AD")
second <- subset(tortona, shortdate=="2nd Cent. AD")
third <- subset(tortona, shortdate=="3rd Cent. AD")
fourth <- subset(tortona, shortdate=="4th Cent. AD")
fifth <- subset(tortona, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Tortona Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Tortona First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Tortona Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Tortona Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Tortona Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Tortona Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Trento ###

trento <- subset(finewares, site=="Trento")
imperial <- subset(trento, longdate=="1st-2nd Cent. AD")
lateantique <- subset(trento, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Trento First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Trento Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

trento <- subset(finewares, site=="Trento")
augustan <- subset(trento, shortdate=="Augustan")
first <- subset(trento, shortdate=="1st Cent. AD")
second <- subset(trento, shortdate=="2nd Cent. AD")
third <- subset(trento, shortdate=="3rd Cent. AD")
fourth <- subset(trento, shortdate=="4th Cent. AD")
fifth <- subset(trento, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Trento Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Trento First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Trento Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Trento Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Trento Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Trento Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Turin ###

turin <- subset(finewares, site=="Turin")
imperial <- subset(turin, longdate=="1st-2nd Cent. AD")
lateantique <- subset(turin, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Turin First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Turin Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

turin <- subset(finewares, site=="Turin")
augustan <- subset(turin, shortdate=="Augustan")
first <- subset(turin, shortdate=="1st Cent. AD")
second <- subset(turin, shortdate=="2nd Cent. AD")
third <- subset(turin, shortdate=="3rd Cent. AD")
fourth <- subset(turin, shortdate=="4th Cent. AD")
fifth <- subset(turin, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Turin Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Turin First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Turin Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Turin Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Turin Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Turin Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Verona ###

verona <- subset(finewares, site=="Verona")
imperial <- subset(verona, longdate=="1st-2nd Cent. AD")
lateantique <- subset(verona, longdate=="3rd-5th Cent. AD")
ggplot(imperial, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Verona First-Second Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(lateantique, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Verona Third-Fifth Centuries AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

verona <- subset(finewares, site=="Verona")
augustan <- subset(verona, shortdate=="Augustan")
first <- subset(verona, shortdate=="1st Cent. AD")
second <- subset(verona, shortdate=="2nd Cent. AD")
third <- subset(verona, shortdate=="3rd Cent. AD")
fourth <- subset(verona, shortdate=="4th Cent. AD")
fifth <- subset(verona, shortdate=="5th Cent. AD")
ggplot(augustan, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Verona Augustan Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(first, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Verona First Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(second, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Verona Second Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(third, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Verona Third Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fourth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Verona Fourth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
ggplot(fifth, aes(x=type)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Verona Fifth Century AD Finewares", x="Type", y="Number of Finewares") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))














