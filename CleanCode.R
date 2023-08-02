library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)


####Data Processing#######

setwd('E:\\ResearchProject\\Aminul\\Dengu WASH')

Wash <- read.csv("WASH.csv")


options(scipen = 999) ## To disable scientific notation

#Number of dengue fever infections, 2019#########################
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log

Wash$Denglog <- log10(Wash$DengueCase+1)

worldgovt <- dplyr::select(Wash, region = ï..Country.Name, Denglog = Denglog, "CC" =  Country.Code)
head(worldgovt)

 diff <- setdiff(world$region, worldgovt$region)

## Clean the dataset accordingly
 worldgovt <- worldgovt %>%
   mutate(region = recode(str_trim(region), "United States" = "USA",
                          "United Kingdom" = "UK",
                          "Korea (Rep.)" = "South Korea",
                          "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                          "Congo (Rep.)" = "Republic of Congo")) %>%
   ## Editing the "North Korea" entry is a little trickier for some reason
   mutate(region = case_when((CC == "PRK") ~ "North Korea",
                             TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$Dengue <- as.numeric(as.character(worldgovt$Denglog))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worlddeng <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Dengue)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("Dengue Cases by country (Log)") +
  plain

a <- worlddeng




#Unsafe water, sanitation and hygiene services deaths#########################
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log

Wash$unwashd <- Wash$UNWASHD

worldgovt <- dplyr::select(Wash, region = ï..Country.Name, unwashd = unwashd, "CC" =  Country.Code)
head(worldgovt)

diff <- setdiff(world$region, worldgovt$region)

## Clean the dataset accordingly
worldgovt <- worldgovt %>%
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Korea (Rep.)" = "South Korea",
                         "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                         "Congo (Rep.)" = "Republic of Congo")) %>%
  ## Editing the "North Korea" entry is a little trickier for some reason
  mutate(region = case_when((CC == "PRK") ~ "North Korea",
                            TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$Deaths <- as.numeric(as.character(worldgovt$unwashd))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldunwashd <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Deaths)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("Unsafe water, sanitation and hygiene services deaths (%)") +
  plain

b <- worldunwashd




#Investment in water and sanitation with private participation (current US$)#########################
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

Wash$POPUSESAFEDWlog <- log10(Wash$POPUSESAFEDW+1)

worldgovt <- dplyr::select(Wash, region = ï..Country.Name, POPUSESAFEDW = POPUSESAFEDWlog, "CC" =  Country.Code)
head(worldgovt)

diff <- setdiff(world$region, worldgovt$region)

## Clean the dataset accordingly
worldgovt <- worldgovt %>%
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Korea (Rep.)" = "South Korea",
                         "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                         "Congo (Rep.)" = "Republic of Congo")) %>%
  ## Editing the "North Korea" entry is a little trickier for some reason
  mutate(region = case_when((CC == "PRK") ~ "North Korea",
                            TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$US_Dollar <- as.numeric(as.character(worldgovt$POPUSESAFEDW))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldPOPUSESAFEDW <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = US_Dollar)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("Investment in water and sanitation with private participation (current US$)") +
  plain

c <- worldPOPUSESAFEDW





#Water and sanitation development assistance as part of official government spending plans#########################
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

worldgovt <- dplyr::select(Wash, region = ï..Country.Name, WASHDEV = WASHDEV, "CC" =  Country.Code)
head(worldgovt)

diff <- setdiff(world$region, worldgovt$region)

## Clean the dataset accordingly
worldgovt <- worldgovt %>%
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Korea (Rep.)" = "South Korea",
                         "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                         "Congo (Rep.)" = "Republic of Congo")) %>%
  ## Editing the "North Korea" entry is a little trickier for some reason
  mutate(region = case_when((CC == "PRK") ~ "North Korea",
                            TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$US_Dollar <- as.numeric(as.character(worldgovt$WASHDEV))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldPOPUSESAFEDW <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = US_Dollar)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("People using at least basic sanitation services (% of population)") +
  plain

d <- worldPOPUSESAFEDW


#People using safely managed sanitation services (% of population)#########################
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log

worldgovt <- dplyr::select(Wash, region = ï..Country.Name, POPUSESAFEW = POPUSESAFEW, "CC" =  Country.Code)
head(worldgovt)

diff <- setdiff(world$region, worldgovt$region)

## Clean the dataset accordingly
worldgovt <- worldgovt %>%
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Korea (Rep.)" = "South Korea",
                         "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                         "Congo (Rep.)" = "Republic of Congo")) %>%
  ## Editing the "North Korea" entry is a little trickier for some reason
  mutate(region = case_when((CC == "PRK") ~ "North Korea",
                            TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$Population <- as.numeric(as.character(worldgovt$POPUSESAFEW))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldPopulation <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Population)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("People using safely managed sanitation services (% of population)") +
  plain

e <- worldPopulation


#People using at least basic sanitation services (% of population)#########################
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

worldgovt <- dplyr::select(Wash, region = ï..Country.Name, BASSAN = BASSAN, "CC" =  Country.Code)
head(worldgovt)

diff <- setdiff(world$region, worldgovt$region)

## Clean the dataset accordingly
worldgovt <- worldgovt %>%
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Korea (Rep.)" = "South Korea",
                         "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                         "Congo (Rep.)" = "Republic of Congo")) %>%
  ## Editing the "North Korea" entry is a little trickier for some reason
  mutate(region = case_when((CC == "PRK") ~ "North Korea",
                            TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$Population <- as.numeric(as.character(worldgovt$BASSAN))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldPopulation <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = BASSAN)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("People using at least basic sanitation services (% of population)") +
  plain

f <- worldPopulation


#People using safely managed sanitation services (% of population)#########################
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

worldgovt <- dplyr::select(Wash, region = ï..Country.Name, POPUSESAFES = POPUSESAFES, "CC" =  Country.Code)
head(worldgovt)

diff <- setdiff(world$region, worldgovt$region)

## Clean the dataset accordingly
worldgovt <- worldgovt %>%
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Korea (Rep.)" = "South Korea",
                         "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                         "Congo (Rep.)" = "Republic of Congo")) %>%
  ## Editing the "North Korea" entry is a little trickier for some reason
  mutate(region = case_when((CC == "PRK") ~ "North Korea",
                            TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$Population <- as.numeric(as.character(worldgovt$POPUSESAFES))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldPopulation <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Population)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("People using safely managed sanitation services (% of population)") +
  plain

g <- worldPopulation


#Safely treated domestic wastewater flows (% of population)#########################
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log

worldgovt <- dplyr::select(Wash, region = ï..Country.Name, SAFETWASH = SAFETWASH, "CC" =  Country.Code)
head(worldgovt)

diff <- setdiff(world$region, worldgovt$region)

## Clean the dataset accordingly
worldgovt <- worldgovt %>%
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Korea (Rep.)" = "South Korea",
                         "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                         "Congo (Rep.)" = "Republic of Congo")) %>%
  ## Editing the "North Korea" entry is a little trickier for some reason
  mutate(region = case_when((CC == "PRK") ~ "North Korea",
                            TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$Population <- as.numeric(as.character(worldgovt$SAFETWASH))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldPopulation <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Population)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("Safely treated domestic wastewater flows (% of population)") +
  plain

h <- worldPopulation

#People with basic handwashing facilities including soap and water (% of population)#########################
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log

worldgovt <- dplyr::select(Wash, region = ï..Country.Name, BASHAN = BASHAN, "CC" =  Country.Code)
head(worldgovt)

diff <- setdiff(world$region, worldgovt$region)

## Clean the dataset accordingly
worldgovt <- worldgovt %>%
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Korea (Rep.)" = "South Korea",
                         "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                         "Congo (Rep.)" = "Republic of Congo")) %>%
  ## Editing the "North Korea" entry is a little trickier for some reason
  mutate(region = case_when((CC == "PRK") ~ "North Korea",
                            TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$Population <- as.numeric(as.character(worldgovt$BASHAN))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldPopulation <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Population)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("People with basic handwashing facilities including soap and water (% of population)") +
  plain

i <- worldPopulation



tiff("DCWASH.tiff", units="in", width=30, height=20, res=300)
gridExtra::grid.arrange(a,b,c,d,e,f,g,h,i)
dev.off()



#####Top 20 CFR bar plot After##########

#filter
data <- Wash[with(Wash,order(-Wash$DengueCase)),]

p<-ggplot(data=data[1:10,], aes(x=reorder(ï..Country.Name,DengueCase), y=DengueCase)) +
  geom_bar(stat="identity", fill = '#003366', color = '#add8e6')+coord_flip()+
  labs(title="Top 10 dengue fever infections countries", y="", x = "Top ten countries")
p <- p + theme(plot.title = element_text(color = "black", size = 10))
p

data <- Wash[with(Wash,order(-Wash$UNWASHD)),]

p1<-ggplot(data=data[1:10,], aes(x=reorder(ï..Country.Name,UNWASHD), y=UNWASHD)) +
  geom_bar(stat="identity", fill = '#003366', color = '#add8e6')+coord_flip()+
  labs(title="Top 10 Unsafe water, sanitation, and hygiene services deaths", y="", x = "Top ten countries")
p1 <- p1 + theme(plot.title = element_text(color = "black", size = 10))
p1

data <- Wash[with(Wash,order(-Wash$POPUSESAFEDW)),]

p2<-ggplot(data=data[1:10,], aes(x=reorder(ï..Country.Name,POPUSESAFEDW), y=POPUSESAFEDW)) +
  geom_bar(stat="identity", fill = '#003366', color = '#add8e6')+coord_flip()+
  labs(title="Top 10 Investment in water and sanitation with private participation (current US$)", y="", x = "Top ten countries")
p2 <- p2 + theme(plot.title = element_text(color = "black", size = 10))
p2

data <- Wash[with(Wash,order(-Wash$WASHDEV)),]

p3<-ggplot(data=data[1:10,], aes(x=reorder(ï..Country.Name,WASHDEV), y=WASHDEV)) +
  geom_bar(stat="identity", fill = '#003366', color = '#add8e6')+coord_flip()+
  labs(title="Top 10 Water and sanitation development assistance as part of official government spending plans", y="", x = "Top ten countries")
p3 <- p3 + theme(plot.title = element_text(color = "black", size = 10))
p3

data <- Wash[with(Wash,order(-Wash$POPUSESAFEW)),]

p4<-ggplot(data=data[1:10,], aes(x=reorder(ï..Country.Name,POPUSESAFEW), y=POPUSESAFEW)) +
  geom_bar(stat="identity", fill = '#003366', color = '#add8e6')+coord_flip()+
  labs(title="Top 10 Water and sanitation development assistance as part of official government spending plans", y="R", x = "Top ten countries")
p4 <- p4 + theme(plot.title = element_text(color = "black", size = 10))
p4

data <- Wash[with(Wash,order(-Wash$BASSAN)),]

p5<-ggplot(data=data[1:10,], aes(x=reorder(ï..Country.Name,BASSAN), y=BASSAN)) +
  geom_bar(stat="identity", fill = '#003366', color = '#add8e6')+coord_flip()+
  labs(title="Top 10 Water and sanitation development assistance as part of official government spending plans", y="", x = "Top ten countries")
p5 <- p5 + theme(plot.title = element_text(color = "black", size = 10))
p5

data <- Wash[with(Wash,order(-Wash$POPUSESAFES)),]

p6<-ggplot(data=data[1:10,], aes(x=reorder(ï..Country.Name,POPUSESAFES), y=POPUSESAFES)) +
  geom_bar(stat="identity", fill = '#003366', color = '#add8e6')+coord_flip()+
  labs(title="Top 10 Water and sanitation development assistance as part of official government spending plans", y="", x = "Top ten countries")
p6 <- p6 + theme(plot.title = element_text(color = "black", size = 10))
p6

data <- Wash[with(Wash,order(-Wash$SAFETWASH)),]

p7<-ggplot(data=data[1:10,], aes(x=reorder(ï..Country.Name,SAFETWASH), y=SAFETWASH)) +
  geom_bar(stat="identity", fill = '#003366', color = '#add8e6')+coord_flip()+
  labs(title="Top 10 Water and sanitation development assistance as part of official government spending plans", y="", x = "Top ten countries")
p7 <- p7 + theme(plot.title = element_text(color = "black", size = 10))
p7

data <- Wash[with(Wash,order(-Wash$BASHAN)),]

p8<-ggplot(data=data[1:10,], aes(x=reorder(ï..Country.Name,BASHAN), y=BASHAN)) +
  geom_bar(stat="identity", fill = '#003366', color = '#add8e6')+coord_flip()+
  labs(title="Top 10 Water and sanitation development assistance as part of official government spending plans", y="", x = "Top ten countries")
p8 <- p8 + theme(plot.title = element_text(color = "black", size = 10))
p8

tiff("Bar.tiff", units="in", width=30, height=20, res=300)
gridExtra::grid.arrange(p,p1,p2,p3,p4,p5,p6,p7,p8)
dev.off()

#Model

## To disable scientific notation
options(scipen = 999)
library(car)

Wash[is.na(Wash)] <- 0

#regression model 
model <- lm(scale(DengueCase) ~  scale(UNWASHD) + scale(POPUSESAFEDW) + scale(WASHDEV) + scale(POPUSESAFEW) +
              scale(BASSAN)  + scale(POPUSESAFES) + scale(SAFETWASH) + scale(BASHAN)  , data = Wash)

# + scale(POPUSESAFES)
# + scale(SAFETWASH)

model
#Model summary
summary(model)
round(model$coefficients,2)

#Confidence interval of IRR
round(confint(model),2)

Washcor <- Wash[,3:11]

library(mgcv)
library(GGally)
library(mgcv)
library(visreg)

pm <- ggpairs(Washcor)

pm + theme(strip.text.x = element_text(size = 10),
           strip.text.y = element_text(size = 10))
