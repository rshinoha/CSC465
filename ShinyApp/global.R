library(choroplethrZip)
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)

#setwd("C:/Users/Ryoh/Documents/CSC465/Project/ProjectShiny")

# UI/Server variables
# ===================
viotype <<- list("All Violations"="all",
                 "Pest-Related"="pest",
                 "Food Handling"="handling",
                 "Facility Violations"="facility")
borough <<- list("Bronx"="bronx",
                 "Brooklyn"="kings",
                 "Manhattan"="new york",
                 "Queens"="queens",
                 "Staten Island"="richmond")
year <<- list("2014"="14",
              "2015"="15",
              "2016"="16",
              "2017"="17")

nyc_fips <- c(36005, 36047, 36061, 36081, 36085)
bronx <- 36005
kings <- 36047
newyork <- 36061
queens <- 36081
richmond <- 36085

# Colors, breaks, and lables
# ==========================
# All violations
vbreaks <- c(0,2.6,2.8,3.0,3.2,3.4,3.6,10.0)
vlabels <- c("< 2.6","2.6-2.79","2.8-2.99","3.0-3.19","3.2-3.39","3.4-3.59","> 3.6","NA")
vcol <- c("< 2.6"='#edf8e9',"2.6-2.79"='#c7e9c0',
          "2.8-2.99"='#a1d99b',"3.0-3.19"='#74c476',
          "3.2-3.39"='#41ab5d',"3.4-3.59"='#238b45',
          "> 3.6"='#005a32',"NA"='#A9A9A9')

# Pests
pbreaks <- c(0,0.6,0.8,1.0,1.2,1.4,1.6,3.0)
plabels <- c("< 0.6","0.6-0.79","0.8-0.99","1.0-1.19","1.2-1.39","1.4-1.59","> 1.6","NA")
pcol <- c("< 0.6"='#fee5d9',"0.6-0.79"='#fcbba1',
          "0.8-0.99"='#fc9272',"1.0-1.19"='#fb6a4a',
          "1.2-1.39"='#ef3b2c',"1.4-1.59"='#cb181d',
          "> 1.6"='#99000d',"NA"='#A9A9A9')
  
# Handling
hbreaks <- c(0,0.6,0.8,1.0,1.2,1.4,1.6,3.0)
hlabels <- c("< 0.6","0.6-0.79","0.8-0.99","1.0-1.19","1.2-1.39","1.4-1.59","> 1.6","NA")
hcol <- c("< 0.6"='#eff3ff',"0.6-0.79"='#c6dbef',
          "0.8-0.99"='#9ecae1',"1.0-1.19"='#6baed6',
          "1.2-1.39"='#4292c6',"1.4-1.59"='#2171b5',
          "> 1.6"='#084594',"NA"='#A9A9A9')
  
# Facility
fbreaks <- c(0,1.0,1.2,1.3,1.4,1.5,1.6,3.0)
flabels <- c("< 1.0","1.0-1.19","1.2-1.29","1.3-1.39","1.4-1.49","1.5-1.59","> 1.6","NA")
fcol <- c("< 1.0"='#f2f0f7',"1.0-1.19"='#dadaeb',
          "1.2-1.29"='#bcbddc',"1.3-1.39"='#9e9ac8',
          "1.4-1.49"='#807dba',"1.5-1.59"='#6a51a3',
          "> 1.6"='#4a1486',"NA"='#A9A9A9')

# Legends (because **** zip_choropleth and county_choropleth)
allones <- as.numeric(c(1,1,1,1,1,1,1,1))
vlegend <- cbind(vlabels,allones)
plegend <- cbind(plabels,allones)
hlegend <- cbind(hlabels,allones)
flegend <- cbind(flabels,allones)

vlegend <- data.frame(vlegend)
plegend <- data.frame(plegend)
hlegend <- data.frame(hlegend)
flegend <- data.frame(flegend)

vlegend$vlabels <- factor(vlegend$vlabels,
                          levels=rev(vlabels))
plegend$plabels <- factor(plegend$plabels,
                          levels=rev(plabels))
hlegend$hlabels <- factor(hlegend$hlabels,
                          levels=rev(hlabels))
flegend$flabels <- factor(flegend$flabels,
                          levels=rev(flabels))

# Zip code-based
# ==============
# All violations
v14 <- read.csv("v14.csv")
v15 <- read.csv("v15.csv")
v16 <- read.csv("v16.csv")
v17 <- read.csv("v17.csv")

# Pests
p14 <- read.csv("p14.csv")
p15 <- read.csv("p15.csv")
p16 <- read.csv("p16.csv")
p17 <- read.csv("p17.csv")

# Handling
h14 <- read.csv("h14.csv")
h15 <- read.csv("h15.csv")
h16 <- read.csv("h16.csv")
h17 <- read.csv("h17.csv")

# Facility
f14 <- read.csv("f14.csv")
f15 <- read.csv("f15.csv")
f16 <- read.csv("f16.csv")
f17 <- read.csv("f17.csv")

# Borough-based
# =============
# All volations
bv14 <- read.csv("bv14.csv")
bv15 <- read.csv("bv15.csv")
bv16 <- read.csv("bv16.csv")
bv17 <- read.csv("bv17.csv")

# Pests
bp14 <- read.csv("bp14.csv")
bp15 <- read.csv("bp15.csv")
bp16 <- read.csv("bp16.csv")
bp17 <- read.csv("bp17.csv")

# Handling
bh14 <- read.csv("bh14.csv")
bh15 <- read.csv("bh15.csv")
bh16 <- read.csv("bh16.csv")
bh17 <- read.csv("bh17.csv")

# Facility
bf14 <- read.csv("bf14.csv")
bf15 <- read.csv("bf15.csv")
bf16 <- read.csv("bf16.csv")
bf17 <- read.csv("bf17.csv")

v14$region <- as.character(v14$region)
v15$region <- as.character(v15$region)
v16$region <- as.character(v16$region)
v17$region <- as.character(v17$region)

p14$region <- as.character(p14$region)
p15$region <- as.character(p15$region)
p16$region <- as.character(p16$region)
p17$region <- as.character(p17$region)

h14$region <- as.character(h14$region)
h15$region <- as.character(h15$region)
h16$region <- as.character(h16$region)
h17$region <- as.character(h17$region)

f14$region <- as.character(f14$region)
f15$region <- as.character(f15$region)
f16$region <- as.character(f16$region)
f17$region <- as.character(f17$region)

# Funtion to highlight county: hcounty
hcounty <- function(f){
  data(county.map, package="choroplethrMaps", envir=environment())
  df = county.map[county.map$region %in% f, ]
  geom_polygon(data=df, aes(long, lat, group = group), color = "yellow", fill = NA, size = 1)
}