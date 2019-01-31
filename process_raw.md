---
title: 'State of the Ecosystem: Indicator Processing & Visualization'
author: Sean Hardison
output_format: html_document
---

## Introduction
The purpose of this file is to document State of the Ecosystem (SOE) indicator data processing. To update existing data sets, set the `save_clean` parameter in the Rmarkdown set-up chunk to `TRUE`. Raw data for these indicators are available in the file directory `extdata`, and libraries required to process indicator data sets are shown below.


```r
knitr::opts_chunk$set(echo = TRUE,                                     fig.align='center')

#Required libraries
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(here)
library(zoo)
library(kableExtra)
library(sf)
library(rgdal)
library(raster)
library(sp)
library(gridExtra)

#Data directories
raw.dir <- here("inst","extdata") #raw data directory
clean.dir <- here("data") #output directory for cleaned data
gis.dir <- here("inst","extdata","gis")
sample.dir <- here("inst","extdata","sample")

#CRS
crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#Write output to file
save_clean <- FALSE

#Execute spatial processing (must be TRUE to write clean data to file). If FALSE, will load sample data from file for plotting example
spatial_processing <- FALSE
```


## Data sets

### Surface winds {.tabset .tabset-fade}

These data are sourced from the [NCEP North American Regional Reanalysis (NARR)](https://www.esrl.noaa.gov/psd/data/gridded/data.narr.monolevel.html), extending from January 1979 to September 2018. 

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Variables in "NCEP NARR surface wind; TKE; HLCY, monthly, 1979-2018, V1.csv"</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:left;"> Name </th>
   <th style="text-align:left;"> Units </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Wind speed </td>
   <td style="text-align:left;"> uwnd </td>
   <td style="text-align:left;width: 5cm; "> m sec^-1^ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wind direction </td>
   <td style="text-align:left;"> vwnd </td>
   <td style="text-align:left;width: 5cm; "> ° </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Turbulent kinetic energy </td>
   <td style="text-align:left;"> tke </td>
   <td style="text-align:left;width: 5cm; "> J kg^-1^ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Storm relative helicity </td>
   <td style="text-align:left;"> hlcy </td>
   <td style="text-align:left;width: 5cm; "> m^2^sec^-2^ </td>
  </tr>
</tbody>
</table>


Variables included in these data are surface wind speed and direction (*uwnd* and *vwnd* respectively), surface turbulent kinetic energy (TKE), and storm relative helicity (HLCY). An indicator for total wind speed is calculated below as
$$\textrm{TWS} = \sqrt{u^2 + v^2}$$. Data are visualized seasonally (Fall = October, November, December; Winter = January, February, March; Spring = April, May, June; Summer = July, August, September).

**Filename**: NCEP NARR surface wind; TKE; HLCY, monthly, 1979-2018, V1.csv  
**Contributor**: Vincent Saba, (vincent.saba@noaa.gov)

#### Processing


```r
# Read in raw data
d <- read.csv(file.path(raw.dir,"NCEP NARR surface wind; TKE; HLCY, monthly, 1979-2018, V1.csv"))

# Processing steps for all wind speed data
wind_clean1 <- d  %>% gather(., Var, Value, GB.uwnd:MAB.tke) %>% #convert wide to long
  dplyr::rename(Time = Month.Year) %>% #rename time variable
  separate(Var, c("EPU","Var"),"\\.") %>% #separate out EPU from variable names
  mutate(Time = as.yearmon(as.Date(.$Time,Time, format = "%d-%b-%y"), "%m/%Y"), #Convert to date format
         Units = plyr::mapvalues(Var, from = unique(Var), to = c(rep("J/kg",2),"m^2/sec^2","J/kg")), #add units
         Time, season = plyr::mapvalues(month(Time), from = seq(1,12,1), #Get season
                                         to = c(rep("winter",3),
                                                rep("spring",3),
                                                rep("summer",3),
                                                rep("fall",3)))) 

# Calculate total wind speed from u and v components
total_wind_speed <- wind_clean1 %>% 
  filter(Var == "uwnd"| Var == "vwnd") %>% #select variables
  spread(., Var, Value) %>% #convert to wide for calculating tws
  mutate(`total wind speed` = sqrt(uwnd^2 + vwnd^2)) %>%  #tws
  dplyr::select(-uwnd, -vwnd) %>% #start processing back to SOE format
  gather(.,Var, Value, `total wind speed`) #convert to long

wind_clean <- rbind(wind_clean1, total_wind_speed)
wind_clean <- wind_clean %>% unite(., Var, c(Var, season), sep = " ") #merge season into Var column

if (save_clean){
save(wind_clean, file =
       file.path(clean.dir, "wind_clean.Rdata"))
}
```

#### MAB Total Wind Speed


```r
mab <- wind_clean %>%
  filter(str_detect(Var, "total wind speed"), EPU == "MAB") %>% #filter
  mutate(Var = word(Var, -1)) %>% 
  mutate(Time = year(Time)) %>% #find year
  group_by(Time, Var) %>% #group by season
  dplyr::summarise(Value = mean(Value)) %>% #find mean
  ggplot()+ #plot
  geom_line(aes(x = Time, y = Value))+
  facet_wrap(.~Var, nrow = 2)+
  ylab("Wind Speed (m/s)") +
  ggtitle("MAB Total Wind Speed") +
  theme_bw()+
  theme(strip.background = element_blank()) 

mab
```

<!–– img src="figure/wind_speed_vis-1.png" title="plot of chunk wind_speed_vis" alt="plot of chunk wind_speed_vis" style="display: block; margin: auto;" / -->

#### GB Total Wind Speed


```r
gb <- wind_clean %>%
  filter(str_detect(Var, "total wind speed"), EPU == "GB") %>% #filter
  mutate(Var = word(Var, -1)) %>% 
  mutate(Time = year(Time)) %>% #find year
  group_by(Time, Var) %>% #group by season
  dplyr::summarise(Value = mean(Value)) %>% #find mean
  
  ggplot()+ #plot
  geom_line(aes(x = Time, y = Value))+
  facet_wrap(.~Var, nrow = 2)+
  ylab("Wind Speed (m/s)") +
  ggtitle("GB Total Wind Speed") +
  theme_bw()+
  theme(strip.background = element_blank()) 

gb
```

<!–– img src="figure/GB tws-1.png" title="plot of chunk GB tws" alt="plot of chunk GB tws" style="display: block; margin: auto;" / -->

#### GOM Total Wind Speed


```r
gom <- wind_clean %>%
  filter(str_detect(Var, "total wind speed"), EPU == "GOM") %>% #filter
  mutate(Var = word(Var, -1)) %>% 
  mutate(Time = year(Time)) %>% #find year
  group_by(Time, Var) %>% #group by season
  dplyr::summarise(Value = mean(Value)) %>% #find mean
  
  ggplot()+ #plot
  geom_line(aes(x = Time, y = Value))+
  facet_wrap(.~Var, nrow = 2)+
  ylab("Wind Speed (m/s)") +
  ggtitle("GOM Total Wind Speed") +
  theme_bw()+
  theme(strip.background = element_blank()) 

gom
```

<!–– img src="figure/GOM tws-1.png" title="plot of chunk GOM tws" alt="plot of chunk GOM tws" style="display: block; margin: auto;" / -->

#### MAB Helicity


```r
mab <- wind_clean %>%
  filter(str_detect(Var, "hcly"), EPU == "MAB") %>% #filter
  mutate(Var = word(Var, -1)) %>% 
  mutate(Time = year(Time)) %>% #find year
  group_by(Time, Var) %>% #group by season
  dplyr::summarise(Value = mean(Value)) %>% #find mean
  ggplot()+ #plot
  geom_line(aes(x = Time, y = Value))+
  facet_wrap(.~Var, nrow = 2)+
  ylab(expression("Relative Helicity (m"^2*" sec"^-2*")")) +
  ggtitle("MAB Storm Relative Helicity ") +
  theme_bw()+
  theme(strip.background = element_blank()) 

mab
```

<!–– img src="figure/MAB hel-1.png" title="plot of chunk MAB hel" alt="plot of chunk MAB hel" style="display: block; margin: auto;" / -->

#### GB Helicity


```r
gb <- wind_clean %>%
  filter(str_detect(Var, "hcly"), EPU == "GB") %>% #filter
  mutate(Var = word(Var, -1)) %>% 
  mutate(Time = year(Time)) %>% #find year
  group_by(Time, Var) %>% #group by season
  dplyr::summarise(Value = mean(Value)) %>% #find mean
  ggplot()+ #plot
  geom_line(aes(x = Time, y = Value))+
  facet_wrap(.~Var, nrow = 2)+
  ylab(expression("Relative Helicity (m"^2*" sec"^-2*")")) +
  ggtitle("GB Storm Relative Helicity ") +
  theme_bw()+
  theme(strip.background = element_blank()) 

gb
```

<!–– img src="figure/GB hel-1.png" title="plot of chunk GB hel" alt="plot of chunk GB hel" style="display: block; margin: auto;" / -->

#### GOM Helicity


```r
gom <- wind_clean %>%
  filter(str_detect(Var, "hcly"), EPU == "GOM") %>% #filter
  mutate(Var = word(Var, -1)) %>% 
  mutate(Time = year(Time)) %>% #find year
  group_by(Time, Var) %>% #group by season
  dplyr::summarise(Value = mean(Value)) %>% #find mean
  ggplot()+ #plot
  geom_line(aes(x = Time, y = Value))+
  facet_wrap(.~Var, nrow = 2)+
  ylab(expression("Relative Helicity (m"^2*" sec"^-2*")")) +
  ggtitle("GOM Storm Relative Helicity ") +
  theme_bw()+
  theme(strip.background = element_blank()) 

gom
```

<!–– img src="figure/GOM hel-1.png" title="plot of chunk GOM hel" alt="plot of chunk GOM hel" style="display: block; margin: auto;" / -->

#### MAB TKE


```r
mab <- wind_clean %>%
  filter(str_detect(Var, "tke"), EPU == "MAB") %>% #filter
  mutate(Var = word(Var, -1)) %>% 
  mutate(Time = year(Time)) %>% #find year
  group_by(Time, Var) %>% #group by season
  dplyr::summarise(Value = mean(Value)) %>% #find mean
  ggplot()+ #plot
  geom_line(aes(x = Time, y = Value))+
  facet_wrap(.~Var, nrow = 2)+
  ylab(expression("Total Kinetic Energy (J kg"^-1*")")) +
  ggtitle("MAB Total Kinetic Energy ") +
  theme_bw()+
  theme(strip.background = element_blank()) 

mab
```

<!–– img src="figure/MAB tke-1.png" title="plot of chunk MAB tke" alt="plot of chunk MAB tke" style="display: block; margin: auto;" / -->

#### GB TKE


```r
gb <- wind_clean %>%
  filter(str_detect(Var, "tke"), EPU == "GB") %>% #filter
  mutate(Var = word(Var, -1)) %>% 
  mutate(Time = year(Time)) %>% #find year
  group_by(Time, Var) %>% #group by season
  dplyr::summarise(Value = mean(Value)) %>% #find mean
  ggplot()+ #plot
  geom_line(aes(x = Time, y = Value))+
  facet_wrap(.~Var, nrow = 2)+
  ylab(expression("Total Kinetic Energy (J kg"^-1*")")) +
  ggtitle("GB Total Kinetic Energy ") +
  theme_bw()+
  theme(strip.background = element_blank()) 

gb
```

<!–– img src="figure/GB tke-1.png" title="plot of chunk GB tke" alt="plot of chunk GB tke" style="display: block; margin: auto;" / -->


#### GOM TKE


```r
gom <- wind_clean %>%
  filter(str_detect(Var, "tke"), EPU == "GOM") %>% #filter
  mutate(Var = word(Var, -1)) %>%
  mutate(Time = year(Time)) %>% #find year
  group_by(Time, Var) %>% #group by season
  dplyr::summarise(Value = mean(Value)) %>% #find mean
  ggplot()+ #plot
  geom_line(aes(x = Time, y = Value))+
  facet_wrap(.~Var, nrow = 2)+
  ylab(expression("Total Kinetic Energy (J kg"^-1*")")) +
  ggtitle("GOM Total Kinetic Energy ") +
  theme_bw()+
  theme(strip.background = element_blank())

gom
```

<!–– img src="figure/GOM tke-1.png" title="plot of chunk GOM tke" alt="plot of chunk GOM tke" style="display: block; margin: auto;" / -->

### Slopewater proportions {.tabset .tabset-fade}

Slopewater proportions give the percent total of water type observed in the deep Northeast Channel (150-200 m depth). 

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Variables in "slopewater_proportions.csv"</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:left;"> Names </th>
   <th style="text-align:left;"> Units </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Warm Slope Water proportion </td>
   <td style="text-align:left;width: 5cm; "> WSW </td>
   <td style="text-align:left;"> unitless </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Labrador Shelf Slope Water proportion </td>
   <td style="text-align:left;width: 5cm; "> LSLW </td>
   <td style="text-align:left;"> unitless </td>
  </tr>
</tbody>
</table>

Raw data fields correspond to year, water mass flavor (WSW = Warm Slope Water, LSLW = Labrador Slope Water), and proportion of total expressed as a percentage. 

**Filename**: slopewater_proportions.csv  
**Contributor**: Paula Fratantoni (paula.fratantoni@noaa.gov)

#### Processing


```r
d <- read.csv(file.path(raw.dir,"slopewater_proportions.csv"))

slopewater <- d %>%
  dplyr::rename(Time = year, Var = water.mass.flavor, Value = prop) %>% 
  mutate(EPU = "all", Units = "unitless", Var2 = "proportion ne channel") %>% 
  unite(.,Var,c(Var,Var2), sep = " ")
  
if (save_clean){
save(slopewater, file =
       file.path(clean.dir, "slopewater_proportions.Rdata"))
}
```

#### Visualization


```r
slopewater %>% 
  mutate(Var, Var = plyr::mapvalues(Var, from = c("WSW proportion ne channel",
                                                  "LSLW proportion ne channel"),
                                    to = c("WSW","LSLW"))) %>% 
  dplyr::rename(Flavor  = Var) %>% 
ggplot() +
  geom_line(aes(x = Time, y = Value, color = Flavor))+
  geom_point(aes(x = Time, y = Value, color = Flavor)) +
  ylab("Percent of Total Slopewater") +
  ggtitle("Slopewater Proportions in NE Channel")+
  theme_bw()+
  theme(strip.background = element_blank())
```

<!–– img src="figure/slopewater_vis-1.png" title="plot of chunk slopewater_vis" alt="plot of chunk slopewater_vis" style="display: block; margin: auto;" / -->


### Ocean temperature anomaly {.tabset .tabset-fade}

These data include *in situ* regional time series of both surface and bottom water temperature anomalies on the Northeast Continental Shelf. Raw data is split into four files by EPU (SS, GOM, GB, and MAB).

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Variables in "Eco{EPU}_core_Ttopbot.csv"</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:left;"> Names </th>
   <th style="text-align:left;"> Units </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> SST anomaly </td>
   <td style="text-align:left;"> Tsfc_anom </td>
   <td style="text-align:left;width: 5cm; "> ?C </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Reference SST (1981-2010) </td>
   <td style="text-align:left;"> Tsfc_ref </td>
   <td style="text-align:left;width: 5cm; "> ?C </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bottom temp. anomaly </td>
   <td style="text-align:left;"> Tbot_anom </td>
   <td style="text-align:left;width: 5cm; "> ?C </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Reference BT (1981-2010) </td>
   <td style="text-align:left;"> Tbot_ref </td>
   <td style="text-align:left;width: 5cm; "> ?C </td>
  </tr>
</tbody>
</table>

**Filenames**: EcoSS_core_Ttopbot.csv, EcoGoM_core_Ttopbot.csv, EcoGB_core_Ttopbot.csv, EcoMAB_core_Ttopbot.csv  
**Contributor**: Paula Fratantoni (paula.fratantoni@noaa.gov)

#### Processing


```r
ss <- read.csv(file.path(raw.dir,"EcoSS_core_Ttopbot.csv")) %>% mutate(EPU = "SS")
gom <- read.csv(file.path(raw.dir,"EcoGoM_core_Ttopbot.csv")) %>% mutate(EPU = "GOM")
gb <- read.csv(file.path(raw.dir,"EcoGB_core_Ttopbot.csv")) %>% mutate(EPU = "GB")
mab <- read.csv(file.path(raw.dir,"EcoMAB_core_Ttopbot.csv")) %>% mutate(EPU = "MAB")

ocean_temp_insitu <- rbind(ss, gom, gb, mab) %>% #bind all
  dplyr::rename(Time = decimal.year, Var = variable.name, Value = temperature) %>% #rename
  mutate(Units = "degreesC", Time = as.Date(format(date_decimal(Time), "%Y-%b-%d"), "%Y-%b-%d"),
         Var, Var = plyr::mapvalues(Var, from = c("Tsfc_anom",
                             "Tsfc_ref",
                             "Tbot_anom",
                             "Tbot_ref"),
                             to = c("sst anomaly in situ",
                                "reference sst in situ (1981-2010)",
                                "bottom temp anomaly in situ",
                                "reference bt in situ (1981-2010)"))) #Rename variables

if (save_clean){
save(ocean_temp_insitu, file =
       file.path(clean.dir, "ocean_temp_insitu.Rdata"))
}
```

#### Visualization

```r
one <- ocean_temp_insitu %>%
  filter(Var == "sst anomaly in situ") %>% 
  group_by(Time = year(Time), EPU) %>% 
  dplyr::summarise(Value = mean(Value)) %>% 
ggplot2::ggplot() +
  geom_line(aes(x = Time, y = Value)) +
  geom_point(aes(x = Time, y = Value), size = 1) +
  facet_grid(.~EPU) +
  ylab(expression("Temp. Anomaly ("*degree*"C)")) +
  ggtitle("SST") +
  theme_bw()+
  theme(strip.background = element_blank())

two <- ocean_temp_insitu %>%
 filter(Var == "bottom temp anomaly in situ") %>%
 group_by(Time = year(Time), EPU) %>%
 dplyr::summarise(Value = mean(Value)) %>% 
ggplot2::ggplot() +
  geom_line(aes(x = Time, y = Value)) +
  geom_point(aes(x = Time, y = Value), size = 1) +
  facet_grid(.~EPU) +
  ylab(expression("Temp. Anomaly ("*degree*"C)")) +
  ggtitle("Bottom temperature") +
  theme_bw()+
  theme(strip.background = element_blank())

grid.arrange(one, two)
```

<!–– img src="figure/ocean_temp_vis-1.png" title="plot of chunk ocean_temp_vis" alt="plot of chunk ocean_temp_vis" style="display: block; margin: auto;" / -->


### Ocean salinity anomaly {.tabset .tabset-fade}

These data include *in situ* regional time series of both surface and bottom salinity anomalies on the Northeast Continental Shelf. Raw data is split into four files by EPU (SS, GOM, GB, and MAB).

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Variables in "Eco{EPU}_core_Stopbot.csv"</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:left;"> Names </th>
   <th style="text-align:left;"> Units </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Surface salinity anomaly </td>
   <td style="text-align:left;width: 5cm; "> Ssfc_anom </td>
   <td style="text-align:left;"> PSU </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Reference surface salinity (1981-2010) </td>
   <td style="text-align:left;width: 5cm; "> Ssfc_ref </td>
   <td style="text-align:left;"> PSU </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bottom salinity anomaly </td>
   <td style="text-align:left;width: 5cm; "> Sbot_anom </td>
   <td style="text-align:left;"> PSU </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Reference bottom salinity (1981-2010) </td>
   <td style="text-align:left;width: 5cm; "> Sbot_ref </td>
   <td style="text-align:left;"> PSU </td>
  </tr>
</tbody>
</table>

**Filenames**: EcoSS_core_Stopbot.csv, EcoGoM_core_Stopbot.csv, EcoGB_core_Stopbot.csv, EcoMAB_core_Stopbot.csv  
**Contributor**: Paula Fratantoni (paula.fratantoni@noaa.gov)

#### Processing


```r
ss <- read.csv(file.path(raw.dir,"EcoSS_core_Stopbot.csv")) %>% mutate(EPU = "SS")
gom <- read.csv(file.path(raw.dir,"EcoGoM_core_Stopbot.csv")) %>% mutate(EPU = "GOM")
gb <- read.csv(file.path(raw.dir,"EcoGB_core_Stopbot.csv")) %>% mutate(EPU = "GB")
mab <- read.csv(file.path(raw.dir,"EcoMAB_core_Stopbot.csv")) %>% mutate(EPU = "MAB")

ocean_sal_insitu <- rbind(ss, gom, gb, mab) %>% #bind all
  dplyr::rename(Time = decimal.year, Var = variable.name, Value = salinity) %>% #rename
  mutate(Units = "PSU", Time = as.Date(format(date_decimal(Time), "%Y-%b-%d"), "%Y-%b-%d"),
         Var, Var = plyr::mapvalues(Var, from = c("Ssfc_anom",
                             "Ssfc_ref",
                             "Sbot_anom",
                             "Sbot_ref"),
                     to = c("surface salinity anomaly in situ",
                        "reference surface salinity in situ (1981-2010)",
                        "bottom salinity anomaly in situ",
                        "reference bottom salinity in situ (1981-2010)")))
if (save_clean){
save(ocean_sal_insitu, file =
       file.path(clean.dir, "ocean_sal_insitu.Rdata"))
}

#surface salinity

#bottom salinity
```

#### Visualization


```r
one <- ocean_sal_insitu %>%
  filter(Var == "surface salinity anomaly in situ") %>% 
  group_by(Time = year(Time), EPU) %>% 
  dplyr::summarise(Value = mean(Value)) %>% 
ggplot2::ggplot() +
  geom_line(aes(x = Time, y = Value)) +
  geom_point(aes(x = Time, y = Value), size = 1) +
  facet_grid(.~EPU) +
  ylab("Salinity Anomaly (PSU)") +
  ggtitle("Surface salinity") +
  theme_bw()+
  theme(strip.background = element_blank())

two <- ocean_sal_insitu %>%
 filter(Var == "bottom salinity anomaly in situ") %>%
 group_by(Time = year(Time), EPU) %>%
 dplyr::summarise(Value = mean(Value)) %>% 
ggplot2::ggplot() +
  geom_line(aes(x = Time, y = Value)) +
  geom_point(aes(x = Time, y = Value), size = 1) +
  facet_grid(.~EPU) +
  ylab("Salinity Anomaly (PSU)") +
  ggtitle("Bottom salinity") +
  theme_bw()+
  theme(strip.background = element_blank())

grid.arrange(one, two)
```

<!–– img src="figure/salinity_vis-1.png" title="plot of chunk salinity_vis" alt="plot of chunk salinity_vis" style="display: block; margin: auto;" / -->


### Stratification {.tabset .tabset-fade}

These data are time series of average stratification (0-50 m depth) by EPU. 

**Filename**: Strat50.csv
**Contributor**: Paula Fratantoni (paula.fratantoni@noaa.gov)

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Variables in "Strat50.csv"</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:left;"> Names </th>
   <th style="text-align:left;"> Units </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> stratification </td>
   <td style="text-align:left;width: 5cm; "> stratification </td>
   <td style="text-align:left;"> kg m ^-3^ </td>
  </tr>
</tbody>
</table>

#### Processing


```r
strat <- read.csv(file.path(raw.dir, "Strat50.csv"), stringsAsFactors = FALSE)

stratification <- strat %>% 
  dplyr::rename(Time = time, Var = var, Value = stratification) %>% 
  separate(., Var, c("Var","EPU"), sep = "_") %>% 
  mutate(Var = "stratification (0-50 m)",
         Units = "kg m^-3") 

if (save_clean){
  save(stratification, file = file.path(clean.dir, "stratification.Rdata"))
}
```

#### Visualization


```r
ggplot(data =stratification)+
  geom_line(aes(x = Time, y = Value)) +
  geom_point(aes(x = Time, y = Value)) +
  facet_grid(.~EPU) +
  theme_bw() +
  ylab(expression("Stratification (kg m"^-3*")")) +
  theme(strip.background = element_blank())
```

<!–– img src="figure/strat_vis-1.png" title="plot of chunk strat_vis" alt="plot of chunk strat_vis" style="display: block; margin: auto;" / -->


### EcoMon Nutrient Data {.tabset .tabset-fade}

These data include nutrient data sampled on EcoMon cruises between 11/3/2009 - 10/19/2016.

**Filename**: EcoMon Nutrient Data Through June 2018.csv  
**Contributor**: Chris Melrose (chris.melrose@noaa.gov)  

#### Processing


```r
d <- read.csv(file.path(raw.dir,"EcoMon Nutrient Data Through June 2018.csv"), stringsAsFactors = FALSE)

#Create data frame for mapping units to variable names
mapping <- data.frame(Units = as.character(d[1,]),
                      Var = as.character(names(d)))
mapping[mapping$Units == "" | mapping$Units == "NA",]$Units <- NA

#remove row with units
d <- slice(d,-1)

d1 <- d %>% 
  mutate(Time = Date_UTC) %>% #create Time variable
  dplyr::select(-Date_UTC,-Time_UTC) %>% #remove time, date
  gather(., Var, Value, -Latitude, -Longitude, -Time) %>% #turn wide to long while retaining lat/lon
  filter(!is.na(Value)) %>% #remove NA
  left_join(., mapping, by = c("Var")) %>% #join units 
  mutate(Longitude = as.numeric(Longitude),
         Latitude = as.numeric(Latitude),
         Time = mdy(Time)) %>% 
  filter(Latitude > 32, Latitude<50)


#Sanity check
# t1 <- d1[d1$Var == "CTDOXYMOL" ,]$Value
# t <-  d %>% slice(.,-1)
# t <- as.character(t$CTDOXYMOL)
# all(t == t1)

#Read in EPU shapefile
epu <- readOGR(file.path(gis.dir, "EPU_Extended.shp"), verbose = F) 
epu <- as(epu, "sf") #convert to sf object

if(spatial_processing){

  #Test maps
  #All EPUs
  #ggplot() + geom_sf(data = epu)
  
  #Scotian shelf
  # ss <- epu %>% filter(EPU == "SS")
  # ggplot() + geom_sf(data = ss)
  
  #get latitude and longitude for creating SpatialPointsDataFrame
  lat <-  as.numeric(d$Latitude)
  lon <- as.numeric(d$Longitude)
  coords <- data.frame(lon = lon,lat = lat)
  
  #create spdf
  spdf <- SpatialPointsDataFrame(coords = coords, data = coords,
                                 proj4string = CRS(crs))
  #convert to sf
  coords_sf <- st_as_sf(spdf) 
  
  #get intersection for mapping EPUs back to nutrient data
  epu_intersect <- st_intersection(epu, coords_sf)
  #plot(epu_intersect[epu_intersect$EPU == "MAB",])
  
  #Map back to nutrient data frame
  epu_df <- data.frame(Longitude = epu_intersect$lon,
                       Latitude = epu_intersect$lat,
                       EPU = epu_intersect$EPU)
  #join
  NE_LME_nutrients <- d1 %>% 
    left_join(.,epu_df, by = c("Latitude","Longitude"))
  
  #Select data for plotting 
  Nitr <- NE_LME_nutrients %>% filter(Var == "NITRIT.NITRAT")
  
  #Back to SOE format
  NE_LME_nutrients <- NE_LME_nutrients %>%
    dplyr::select(-Latitude, -Longitude) %>% 
    mutate(Value = as.numeric(Value))
  
  if (save_clean){
    save(NE_LME_nutrients,file = file.path(clean.dir, "EcoMon_nutrients.Rdata"))
  }

} else {
  load(file.path(sample.dir,"sample_nutrients.Rdata"))
  load(file.path(clean.dir,"EcoMon_nutrients.Rdata"))
}
```

#### QA


```r
#Confirm transformation
ggplot() + 
  geom_sf(data = epu) +
  geom_point(data = Nitr, aes(x = Longitude, y = Latitude, color = EPU)) +
  ggtitle("Mapping EcoMon Nutrient Data to EPU") +
  theme_bw() 
```

<!–– img src="figure/ecomon_plotting1-1.png" title="plot of chunk ecomon_plotting1" alt="plot of chunk ecomon_plotting1" style="display: block; margin: auto;" / -->


#### Visualization


```r
N <- NE_LME_nutrients %>% 
  filter(Var == "NITRIT.NITRAT", Value > 0, !is.na(EPU)) %>% 
  group_by(EPU, Time = year(Time)) %>% 
  dplyr::summarise(Value = mean(Value, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(x = Time, y = Value, color = EPU)) +
  ylab(expression("Nitrit+Nitrat (micromoles kg"^-1*")"))+
  ggtitle("Nitrit+Nitrat")+
  theme_bw()+
  theme(strip.background = element_blank())+
  guides(colour=FALSE)

P <- NE_LME_nutrients %>% 
  filter(Var == "PHSPHT", Value > 0, !is.na(EPU)) %>% 
  group_by(EPU, Time = year(Time)) %>% 
  dplyr::summarise(Value = mean(Value, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(x = Time, y = Value, color = EPU)) +
  ylab(expression("Phosphate (micromoles kg"^-1*")"))+
  ggtitle("Phosphate")+
  theme_bw()+
  theme(strip.background = element_blank())+
  guides(colour=FALSE)

ammon <- NE_LME_nutrients %>% 
  filter(Var == "AMMONIA", Value > 0, !is.na(EPU)) %>% 
  group_by(EPU, Time = year(Time)) %>% 
  dplyr::summarise(Value = mean(Value, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(x = Time, y = Value, color = EPU)) +
  ylab(expression("Ammonia (micromoles kg"^-1*")"))+
  ggtitle("Ammonia")+
  theme_bw()+
  theme(strip.background = element_blank())+
  theme(strip.background = element_blank(),legend.position = c(0.85, 0.6),
        legend.key = element_rect(color="transparent"),
        legend.background = element_rect(fill="transparent"))

silcat <- NE_LME_nutrients %>% 
  filter(Var == "SILCAT", Value > 0, !is.na(EPU)) %>% 
  group_by(EPU, Time = year(Time)) %>% 
  dplyr::summarise(Value = mean(Value, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(x = Time, y = Value, color = EPU)) +
  ylab(expression("Silicates (micromoles kg"^-1*")"))+
    ggtitle("Silicates")+
  theme_bw()+
  guides(color= FALSE)

cowplot::plot_grid(N, P, ammon , silcat)
```

<!–– img src="figure/N nutrients-1.png" title="plot of chunk N nutrients" alt="plot of chunk N nutrients" style="display: block; margin: auto;" / -->





