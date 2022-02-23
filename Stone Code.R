### Stone Coding ###

### Preamble ###

library(readxl) 
library(ggplot2)
stone <- read_excel("~/stone.xlsx")
View(stone)

### Stone Percentages ####

stone <- read_excel("~/stone.xlsx")
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(wesanderson)
stone <- stone[stone$origin != "Unknown", ]
stone2 <- stone %>% 
  group_by(material,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(stone2)
ggplot(stone2, aes(x = factor(material), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +  labs(title="Provenance of Stone and Marble",x = "", y = NULL, fill = "Provenance") +  theme_minimal(base_size = 14) + scale_fill_manual(values = wes_palette("Cavalcanti1", 7, type="continuous"))


### Stone Percentages with Unknowns ####

stone <- read_excel("~/stone.xlsx")
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(wesanderson)
stone2 <- stone %>% 
  group_by(material,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(stone2)
ggplot(stone2, aes(x = factor(material), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +  labs(title="Provenance of Stone and Marble",x = "", y = NULL, fill = "Provenance") +  theme_minimal(base_size = 14) + scale_fill_manual(values = wes_palette("Cavalcanti1", 7, type="continuous"))

### Stone Percentages by colour ####

stone <- read_excel("~/stone.xlsx")
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(wesanderson)
stone <- stone[stone$colour != "Unknown", ]
stone2 <- stone %>% 
  group_by(colour,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(stone2)
ggplot(stone2, aes(x = factor(colour), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +  labs(title="Provenance of Stone and Marble",x = "", y = NULL, fill = "Provenance") +  theme_minimal(base_size = 14) + scale_fill_manual(values = wes_palette("Cavalcanti1", 8, type="continuous"))


### Lithotypes by Provenance ####

stone <- read_excel("~/stone.xlsx")
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(wesanderson)
stone2 <- stone %>% 
  group_by(material,lithotype,origin) %>%
  summarise(n=n()) %>% 
  group_by(material) %>% 
  mutate(perc=100*n/sum(n))
View(stone2)
ggplot(stone2, aes(x=lithotype, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="Percentages of Lithotypes Grouped by Origin", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")

alpine <- subset(stone2, origin=="Alpine")
ggplot(alpine, aes(x=lithotype, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="Percentages of Alpine Lithotypes", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")

asia <- subset(stone2, origin=="Asia Minor")
ggplot(asia, aes(x=lithotype, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="Percentages of Asia Minor Lithotypes", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")

central <- subset(stone2, origin=="Central Italy")
ggplot(central, aes(x=lithotype, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="Percentages of Central Italian Lithotypes", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")

african <- subset(stone2, origin=="Egypt and North Africa")
ggplot(african, aes(x=lithotype, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="Percentages of North African Lithotypes", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")

gallic <- subset(stone2, origin=="Gaul")
ggplot(gallic, aes(x=lithotype, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="Percentages of Gallic Lithotypes", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")

greek <- subset(stone2, origin=="Greece and the Aegean")
ggplot(greek, aes(x=lithotype, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="Percentages of Greek Lithotypes", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")

unknown <- subset(stone2, origin=="Unknown")
ggplot(unknown, aes(x=lithotype, y=perc, label=round(perc,2))) + geom_bar(stat="identity") + geom_text(vjust=-1) + labs(title="Percentages of Unknown Lithotypes", x="Type", y="Percent") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~origin, scales="free_x")

### Provenance by Site ####

library(readxl) 
library(ggplot2)
stone <- read_excel("~/stone.xlsx")
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(wesanderson)
stone <- stone[stone$origin != "Unknown", ]
stone2 <- stone %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(stone2)
stone2$site <- factor(stone2$site,levels = c("Ravenna", "Aquileia", "Altinum", "Forli", "Verona", "Brescia", "Milan", "Vercelli", "Como", "Tortona", "Alessandria", "Augusta Bagiennorum", "Aosta"))
ggplot(stone2, aes(x = factor(site), y = perc, fill = factor(origin))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(title="Stone and Marble Provenance by Site",x = "", y = NULL, fill = "Origin") +  theme_minimal(base_size = 14)  + scale_fill_manual(values = wes_palette("Cavalcanti1", 8, type="continuous")) + theme(axis.text.x = element_text(angle = 90))



### Provenance by Site White and Polychrome ####

library(readxl) 
library(ggplot2)
stone <- read_excel("~/stone.xlsx")
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(wesanderson)
stone <- stone[stone$colour != "Unknown", ]
stone2 <- stone %>% 
  group_by(site,colour) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
View(stone2)
stone2$site <- factor(stone2$site,levels = c("Ravenna", "Aquileia", "Altinum", "Forli", "Verona", "Brescia", "Milan", "Vercelli", "Como", "Tortona", "Alessandria", "Augusta Bagiennorum", "Aosta"))
ggplot(stone2, aes(x = factor(site), y = perc, fill = factor(colour))) + geom_bar(stat="identity", width = 0.7) + scale_y_continuous(breaks = brks, labels = scales::percent(brks)) + labs(title="Breakdown of Stone and Marble Colours by Site",x = "", y = NULL, fill = "Origin") +  theme_minimal(base_size = 14)  + scale_fill_manual(values = wes_palette("Cavalcanti1", 5, type="continuous")) + theme(axis.text.x = element_text(angle = 90))


### Hierarchical Clustering ####

library(readxl) 
library(ggplot2)
library(reshape2)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)
library(dplyr)
library(wesanderson)
stone <- read_excel("~/stone.xlsx")
stone <- stone[stone$origin != "Unknown", ]
stone2 <- stone %>% 
  group_by(site,origin) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
stoneMatrix <- acast(stone2, site~origin, value.var="perc", fill=0)
View(stoneMatrix)
stoneMatrix <- as.data.frame(stoneMatrix)
library(ggdendro)
measures <- stoneMatrix[,1:6]
pcs <- prcomp(measures)
pcs
plot(pcs)
distMeasures <- dist(measures)
stoneHClust <- hclust(distMeasures, method="average")
ggdendrogram(stoneHClust, rotate=T) + labs(title="Hierarchical Clustering based on Lithotype Provenance", x="", y="Distance between assemblages") + theme_minimal()

### Simpsons Index ####

library(readxl) 
library(ggplot2)
stone <- read_excel("~/stone.xlsx")
stone <- stone[stone$lithotype != "Unknown", ]
stone <- stone[stone$lithotype != "White Marble", ]
library(reshape2)
stoneMatrix <- acast(stone, site~lithotype, value.var="lithotype", fill=0)
View(stoneMatrix)
library(vegan)
diversity(stoneMatrix, index="simpson")


### Morisita-Horn Overlap Coefficient ####

stone <- read_excel("~/stone.xlsx")
stone <- stone[stone$lithotype != "Unknown", ]
stone <- stone[stone$lithotype != "White Marble", ]
library(reshape2)
stoneMatrix <- acast(stone, site~lithotype, value.var="lithotype", fill=0)
View(stoneMatrix)
library(vegan)
impDistance <- vegdist(stoneMatrix, method="horn")
impDistance
impDistanceMatrix <- as.matrix(impDistance)
imp <- melt(impDistanceMatrix, varnames=c("site1", "site2"), value.name="distance")
library(ggplot2)
ggplot(imp, aes(x=site1, y=site2, fill=distance, label=round(distance, 2))) + geom_raster() + labs(title="Morisita-Horn Overlap Coefficient", x = NULL, y = NULL, fill = "Distance") + geom_raster() + geom_text(color="white")
morisitaHClust <- hclust(impDistance, method="average")
library(ggdendro)
ggdendrogram(morisitaHClust, rotate=T) + labs(title="Lithotype Similarity Between Sites Using Morisita-Horn Overlap Coefficient", x="", y="Distance between assemblages") + theme_minimal()

### Pavia Sarcophagi ####

pavia <- read_excel("~/pavia.xlsx")
ggplot(pavia, aes(x=origin)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Pavia Sarcophagi Origins", x="Origin", y="Number of Examples") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))


### Breakdown of Lithotypes by Site ####

stone <- read_excel("~/stone.xlsx")

### Alessandria ###

alessandria <- subset(stone, site=="Alessandria")
ggplot(alessandria, aes(x=lithotype)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Alessandria Lithotypes", x="Type", y="Number of Fragments") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Altinum ###

altinum <- subset(stone, site=="Altinum")
ggplot(altinum, aes(x=lithotype)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Altinum Lithotypes", x="Type", y="Number of Fragments") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Aosta ###

aosta <- subset(stone, site=="Aosta")
ggplot(aosta, aes(x=lithotype)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Aosta Lithotypes", x="Type", y="Number of Fragments") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Aquileia ###

aquileia <- subset(stone, site=="Aquileia")
ggplot(aquileia, aes(x=lithotype)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Aquileia Lithotypes", x="Type", y="Number of Fragments") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Augusta Bagiennorum ###

augusta <- subset(stone, site=="Augusta Bagiennorum")
ggplot(augusta, aes(x=lithotype)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Augusta Bagiennorum Lithotypes", x="Type", y="Number of Fragments") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Brescia ###

brescia <- subset(stone, site=="Brescia")
ggplot(brescia, aes(x=lithotype)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Brescia Lithotypes", x="Type", y="Number of Fragments") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Como ###

como <- subset(stone, site=="Como")
ggplot(como, aes(x=lithotype)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Como Lithotypes", x="Type", y="Number of Fragments") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Forli ###

forli <- subset(stone, site=="Forli")
ggplot(forli, aes(x=lithotype)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Forli Lithotypes", x="Type", y="Number of Fragments") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Milan ###

milan <- subset(stone, site=="Milan")
ggplot(milan, aes(x=lithotype)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Milan Lithotypes", x="Type", y="Number of Fragments") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Ravenna ###

ravenna <- subset(stone, site=="Ravenna")
ggplot(ravenna, aes(x=lithotype)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Ravenna Lithotypes", x="Type", y="Number of Fragments") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Tortona ###

tortona <- subset(stone, site=="Tortona")
ggplot(tortona, aes(x=lithotype)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Tortona Lithotypes", x="Type", y="Number of Fragments") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Vercelli ###

vercelli <- subset(stone, site=="Vercelli")
ggplot(vercelli, aes(x=lithotype)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Vercelli Lithotypes", x="Type", y="Number of Fragments") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))

### Verona ###

verona <- subset(stone, site=="Verona")
ggplot(verona, aes(x=lithotype)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title="Verona Lithotypes", x="Type", y="Number of Fragments") + theme_minimal() + theme(axis.text.x = element_text(angle = 90))



