#####PREPARATION####


#Libraries
library(tidyverse)
library(tmap)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(rworldmap)

#Datasets
d <- read_csv("data_ecosystem.csv") #dp = DataframePlatforms
dp <- read_csv("data_platforms.csv") #dp = DataframePlatforms
dw <- read_csv("data_websites.csv") #dw = DataframeWebsites
iso <- read_csv("ISO3166.csv")


####GEOGRAPHICAL DATA####

##GEOGRAPHICAL ORIGIN OF TRAFFIC EXCLUSIVELY FROM UNIQUE VISITORS OF LABOUR PLATFORMS

#Calculate the number of unique visitors minus the bounce rate percentage
dp <- mutate(dp, sw_ptraffic_t=sw_ptraffic_s-((sw_ptraffic_bounce*sw_ptraffic_s)/100))
#Selection of geographical variables
px <- dp$sw_pgeo1 %>% strsplit(",") %>% unlist()
py <- dp$sw_pgeo2 %>% strsplit(",") %>% lapply(as.numeric) 
#Calculate the number of unique visitors from each country
py <- lapply(seq_along(py), function(i) (unlist(dp$sw_ptraffic_t[i])*unlist(py[i]))/100) %>% unlist()
#Simplify the numbers
py <- round(py, digits=0)
#Create new dataframe from the results
pz <- bind_cols(px,py)
colnames(pz) <- c("country","value")
pz <- pz %>% group_by(country) %>% summarize_all(sum)  %>% arrange(desc(value))
#Transform ISO-3 codes to country names
for (i in 1:NROW(iso)){pz$country <- sub(iso[i,3], iso[i,1], pz$country)}
#Create percentage column
pz <- pz %>% mutate(percentage=round((value/sum(value))*100, digits=2))


##GEOGRAPHICAL ORIGIN OF TRAFFIC EXCLUSIVELY FROM UNIQUE VISITORS OF COMPANY WEBSITES

dp <- mutate(dp, sw_ctraffic_t=sw_ctraffic_s-((sw_ctraffic_bounce*sw_ctraffic_s)/100))
cx <- dp$sw_cgeo1 %>% strsplit(",") %>% unlist()
cy <- dp$sw_cgeo2 %>% strsplit(",") %>% lapply(as.numeric) 
cy <- lapply(seq_along(cy), function(i) (unlist(dp$sw_ctraffic_t[i])*unlist(cy[i]))/100) %>% unlist()
cy <- round(cy, digits=0)
cz <- bind_cols(cx,cy)
colnames(cz) <- c("country","value")
cz <- cz %>% group_by(country) %>% summarize_all(sum)  %>% arrange(desc(value))
for (i in 1:NROW(iso)){
  cz$country <- sub(iso[i,3], iso[i,1], cz$country) }


##GEOGRAPHICAL ORIGIN OF TRAFFIC FROM UNIQUE VISITORS OF PLATFORMS INCLUDING REQUESTERS AND WORKERS

dw <- mutate(dw, sw_ctraffic_t=sw_ctraffic_s-((sw_ctraffic_bounce*sw_ctraffic_s)/100))
x <- dw$sw_cgeo1 %>% strsplit(",") %>% unlist()
y <- dw$sw_cgeo2 %>% strsplit(",") %>% lapply(as.numeric) 
y <- lapply(seq_along(y), function(i) (unlist(dw$sw_ctraffic_t[i])*unlist(y[i]))/100) %>% unlist()
y <- round(y, digits=0)
z <- bind_cols(x,y)
colnames(z) <- c("country","value")
z <- z %>% group_by(country) %>% summarize_all(sum)  %>% arrange(desc(value))
for (i in 1:NROW(iso)){
  z$country <- sub(iso[i,3], iso[i,1], z$country) }

##MAPPING THE TRAFFIC ORIGIN OF THESE WEBISTES

mapDevice('x11')
map <- joinCountryData2Map(pz, joinCode="NAME", nameJoinColumn="country")

mapf <- mapCountryData(map, nameColumnToPlot="percentage", catMethod="fixedWidth", colourPalette="heat")

mapf

write_csv(pz, "output.csv")
