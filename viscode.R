library(ggplot2)
library(plyr)
library(data.table)
library(devtools)
library(choroplethrZip)
library(choroplethr)
library(choroplethrMaps)
data("county.regions")

setwd("C:/Users/Ryoh/Documents/CSC465/Project")

violation <- read.csv('NYCRestaurantV1.csv')
inspection <- read.csv('NYCRestaurantV2.csv')

# Going to keep only the initial cycle inspections to not dilute violation counts
violation <- violation[violation$INSTYPE=="Cycle Inspection / Initial Inspection",]
inspection <- inspection[inspection$INSTYPE=="Cycle Inspection / Initial Inspection",]

# Forgot to convert previous csv's to date
violation$DATE <- as.Date(violation$DATE, format="%m/%d/%Y")
violation$VIOCODE <- as.character(violation$VIOCODE)
inspection$DATE <- as.Date(inspection$DATE, format="%m/%d/%Y")

# Change zip codes to string
violation$ZIPCODE <- as.character(violation$ZIPCODE)
inspection$ZIPCODE <- as.character(inspection$ZIPCODE)

# Counts number of unique INSPECTIONS per zip code within a year: restzip
restzip <- function(df,yr,other){
  minyr <- as.character(yr-1)
  maxyr <- as.character(yr+1)
  mindate <- paste(minyr,"-12-31",sep="")
  maxdate <- paste(maxyr,"-01-01",sep="")
  if(missing(other)){
    numrest <- unique(df[df$DATE > as.Date(mindate) & df$DATE < as.Date(maxdate),c("CAMIS","ZIPCODE","DATE")])
    return(count(numrest,vars="ZIPCODE"))
  }
  else if(other=="b"){
    numrest <- unique(df[df$DATE > as.Date(mindate) & df$DATE < as.Date(maxdate),c("CAMIS","BORO","DATE")])
    numrest <- count(numrest,vars="BORO")
    numrest$BORO <- as.character(numrest$BORO)
    numrest[numrest$BORO=="MANHATTAN","BORO"] <- "new york"
    numrest[numrest$BORO=="BRONX","BORO"] <- "bronx"
    numrest[numrest$BORO=="BROOKLYN","BORO"] <- "kings"
    numrest[numrest$BORO=="QUEENS","BORO"] <- "queens"
    numrest[numrest$BORO=="STATEN ISLAND","BORO"] <- "richmond"
    return(numrest)
  }
}

# Counts of unique restaurants per zip code for years 2014-2017: r14, r15, r16, r17
r14 <- restzip(inspection, 2014)
r15 <- restzip(inspection, 2015)
r16 <- restzip(inspection, 2016)
r17 <- restzip(inspection, 2017)

# Rename frequency columns to restaurant counts
r14 <- rename(r14,c("freq"="REST"))
r15 <- rename(r15,c("freq"="REST"))
r16 <- rename(r16,c("freq"="REST"))
r17 <- rename(r17,c("freq"="REST"))

# Group violation categories
viocat <- read.csv('viocat.csv')
viocat <- rename(viocat,c("VIOLATION.CODE"="VIOCODE"))
viocat$VIOCODE <- as.character(viocat$VIOCODE)
viomerge <- join(x=violation,y=viocat, by = "VIOCODE")
viomerge <- viomerge[-c(1)]
viomerge <- rename(viomerge,c("CATEGORY"="VIOCAT"))
viomerge$VIOCAT <- as.character(viomerge$VIOCAT)
viomerge$BORO <- as.character(viomerge$BORO)
viomerge[viomerge$BORO=="MANHATTAN","BORO"] <- "new york"
viomerge[viomerge$BORO=="BRONX","BORO"] <- "bronx"
viomerge[viomerge$BORO=="BROOKLYN","BORO"] <- "kings"
viomerge[viomerge$BORO=="QUEENS","BORO"] <- "queens"
viomerge[viomerge$BORO=="STATEN ISLAND","BORO"] <- "richmond"
viomerge[viomerge$ZIPCODE=="11370" & viomerge$BORO=="bronx","ZIPCODE"] <- "10470"

zipboro <- unique(viomerge[,c("ZIPCODE","BORO")])

# Counts number of violations of interest per zip code within a year: viozip
viozip <- function(df,yr,type){
  minyr <- as.character(yr-1)
  maxyr <- as.character(yr+1)
  mindate <- paste(minyr,"-12-31",sep="")
  maxdate <- paste(maxyr,"-01-01",sep="")
  if(missing(type)){
    tempdf <- count(df[df$DATE > as.Date(mindate) &
                         df$DATE < as.Date(maxdate) &
                         df$VIOCAT != "",],
                    vars="ZIPCODE")
  }
  else{
    tempdf <- count(df[df$DATE > as.Date(mindate) &
                         df$DATE < as.Date(maxdate) &
                         df$VIOCAT==type,],
                    vars="ZIPCODE")
  }
  tempdf <- join(x=tempdf,y=zipboro,by="ZIPCODE",type="inner")
  tempdf <- unique(tempdf)
  return(tempdf)
}

# Counts of all violation type per zip code for years 2015, 2016, and 2017: v15, v16, v17
v14 <- viozip(viomerge,2014)
v15 <- viozip(viomerge,2015)
v16 <- viozip(viomerge,2016)
v17 <- viozip(viomerge,2017)

# Counts of pest type violations per zip code for years 2015, 2016. and 2017: p15, p16, p17
p14 <- viozip(viomerge,2014,"pest")
p15 <- viozip(viomerge,2015,"pest")
p16 <- viozip(viomerge,2016,"pest")
p17 <- viozip(viomerge,2017,"pest")

# Counts of food handling type violations per zip code for years 2015, 2016, and 2017: h15, h16, h17
h14 <- viozip(viomerge,2014,"handling")
h15 <- viozip(viomerge,2015,"handling")
h16 <- viozip(viomerge,2016,"handling")
h17 <- viozip(viomerge,2017,"handling")

# Counts of facility type violations per zip code for years 2015, 2016, and 2017: f15, f16, f17
f14 <- viozip(viomerge,2014,"facility")
f15 <- viozip(viomerge,2015,"facility")
f16 <- viozip(viomerge,2016,"facility")
f17 <- viozip(viomerge,2017,"facility")

# Merge restaurant counts to each violation types
v14 <- merge(v14,r14,by="ZIPCODE",all.x=TRUE)
v15 <- merge(v15,r15,by="ZIPCODE",all.x=TRUE)
v16 <- merge(v16,r16,by="ZIPCODE",all.x=TRUE)
v17 <- merge(v17,r17,by="ZIPCODE",all.x=TRUE)

p14 <- merge(p14,r14,by="ZIPCODE",all.x=TRUE)
p15 <- merge(p15,r15,by="ZIPCODE",all.x=TRUE)
p16 <- merge(p16,r16,by="ZIPCODE",all.x=TRUE)
p17 <- merge(p17,r17,by="ZIPCODE",all.x=TRUE)

h14 <- merge(h14,r14,by="ZIPCODE",all.x=TRUE)
h15 <- merge(h15,r15,by="ZIPCODE",all.x=TRUE)
h16 <- merge(h16,r16,by="ZIPCODE",all.x=TRUE)
h17 <- merge(h17,r17,by="ZIPCODE",all.x=TRUE)

f14 <- merge(f14,r14,by="ZIPCODE",all.x=TRUE)
f15 <- merge(f15,r15,by="ZIPCODE",all.x=TRUE)
f16 <- merge(f16,r16,by="ZIPCODE",all.x=TRUE)
f17 <- merge(f17,r17,by="ZIPCODE",all.x=TRUE)

# Calculate violations per restaurant
v14["orival"] <- v14["freq"] / v14["REST"]
v15["orival"] <- v15["freq"] / v15["REST"]
v16["orival"] <- v16["freq"] / v16["REST"]
v17["orival"] <- v17["freq"] / v17["REST"]

p14["orival"] <- p14["freq"] / p14["REST"]
p15["orival"] <- p15["freq"] / p15["REST"]
p16["orival"] <- p16["freq"] / p16["REST"]
p17["orival"] <- p17["freq"] / p17["REST"]

h14["orival"] <- h14["freq"] / h14["REST"]
h15["orival"] <- h15["freq"] / h15["REST"]
h16["orival"] <- h16["freq"] / h16["REST"]
h17["orival"] <- h17["freq"] / h17["REST"]

f14["orival"] <- f14["freq"] / f14["REST"]
f15["orival"] <- f15["freq"] / f15["REST"]
f16["orival"] <- f16["freq"] / f16["REST"]
f17["orival"] <- f17["freq"] / f17["REST"]

# Get summary statistics
# summary(v14$orival)
# summary(v15$orival)
# summary(v16$orival)
# summary(v17$orival)

# summary(p14$orival)
# summary(p15$orival)
# summary(p16$orival)
# summary(p17$orival)
# 
# summary(h14$orival)
# summary(h15$orival)
# summary(h16$orival)
# summary(h17$orival)
#
# summary(f14$orival)
# summary(f15$orival)
# summary(f16$orival)
# summary(f17$orival)

# Rename the zipcodes
v14 <- rename(v14,c("ZIPCODE"="region"))
v15 <- rename(v15,c("ZIPCODE"="region"))
v16 <- rename(v16,c("ZIPCODE"="region"))
v17 <- rename(v17,c("ZIPCODE"="region"))

p14 <- rename(p14,c("ZIPCODE"="region"))
p15 <- rename(p15,c("ZIPCODE"="region"))
p16 <- rename(p16,c("ZIPCODE"="region"))
p17 <- rename(p17,c("ZIPCODE"="region"))

h14 <- rename(h14,c("ZIPCODE"="region"))
h15 <- rename(h15,c("ZIPCODE"="region"))
h16 <- rename(h16,c("ZIPCODE"="region"))
h17 <- rename(h17,c("ZIPCODE"="region"))

f14 <- rename(f14,c("ZIPCODE"="region"))
f15 <- rename(f15,c("ZIPCODE"="region"))
f16 <- rename(f16,c("ZIPCODE"="region"))
f17 <- rename(f17,c("ZIPCODE"="region"))

# Categorize violation rates

# hist(v14$orival,breaks="FD")
# hist(v15$orival,breaks="FD")
# hist(v16$orival,breaks="FD")
# hist(v17$orival,breaks="FD")
# 
# hist(p14$orival,breaks="FD")
# hist(p15$orival,breaks="FD")
# hist(p16$orival,breaks="FD")
# hist(p17$orival,breaks="FD")

# hist(h14$orival,breaks="FD")
# hist(h15$orival,breaks="FD")
# hist(h16$orival,breaks="FD")
# hist(h17$orival,breaks="FD")

# hist(f14$orival,breaks="FD")
# hist(f15$orival,breaks="FD")
# hist(f16$orival,breaks="FD")
# hist(f17$orival,breaks="FD")

nyc_fips = c(36005, 36047, 36061, 36081, 36085)

# Custom color scaling
vbreaks <- c(0,2.6,2.8,3.0,3.2,3.4,3.6,10.0)
vlabels <- c("< 2.6","2.6-2.79","2.8-2.99","3.0-3.19","3.2-3.39","3.4-3.59","> 3.6","NA")
vcol <- c("< 2.6"='#edf8e9',"2.6-2.79"='#c7e9c0',
          "2.8-2.99"='#a1d99b',"3.0-3.19"='#74c476',
          "3.2-3.39"='#41ab5d',"3.4-3.59"='#238b45',
          "> 3.6"='#005a32',"NA"='#A9A9A9')
v14["value"] <- cut(v14[["orival"]],breaks=vbreaks,labels=vlabels[1:7])
v15["value"] <- cut(v15[["orival"]],breaks=vbreaks,labels=vlabels[1:7])
v16["value"] <- cut(v16[["orival"]],breaks=vbreaks,labels=vlabels[1:7])
v17["value"] <- cut(v17[["orival"]],breaks=vbreaks,labels=vlabels[1:7])

# zip_choropleth(v15, county_zoom = nyc_fips, legend="Violation per restaurant")
# zip_choropleth(v16, county_zoom = nyc_fips, legend="Violation per restaurant")
# zip_choropleth(v17, county_zoom = nyc_fips, legend="Violation per restaurant") +
#   scale_fill_manual(breaks=vbreaks,
#                     values=c('#edf8e9','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#005a32'),
#                     na.value="black")

pbreaks <- c(0,0.6,0.8,1.0,1.2,1.4,1.6,3.0)
plabels <- c("< 0.6","0.6-0.79","0.8-0.99","1.0-1.19","1.2-1.39","1.4-1.59","> 1.6","NA")
pcol <- c("< 0.6"='#fee5d9',"0.6-0.79"='#fcbba1',
          "0.8-0.99"='#fc9272',"1.0-1.19"='#fb6a4a',
          "1.2-1.39"='#ef3b2c',"1.4-1.59"='#cb181d',
          "> 1.6"='#99000d',"NA"='#A9A9A9')
p14["value"] <- cut(p14[["orival"]],breaks=pbreaks,labels=plabels[1:7])
p15["value"] <- cut(p15[["orival"]],breaks=pbreaks,labels=plabels[1:7])
p16["value"] <- cut(p16[["orival"]],breaks=pbreaks,labels=plabels[1:7])
p17["value"] <- cut(p17[["orival"]],breaks=pbreaks,labels=plabels[1:7])

# zip_choropleth(p15, county_zoom = nyc_fips, legend="Violation per restaurant")
# zip_choropleth(p16, county_zoom = nyc_fips, legend="Violation per restaurant")
# zip_choropleth(p17, county_zoom = nyc_fips, legend="Violation per restaurant")

hbreaks <- c(0,0.6,0.8,1.0,1.2,1.4,1.6,3.0)
hlabels <- c("< 0.6","0.6-0.79","0.8-0.99","1.0-1.19","1.2-1.39","1.4-1.59","> 1.6","NA")
hcol <- c("< 0.6"='#eff3ff',"0.6-0.79"='#c6dbef',
          "0.8-0.99"='#9ecae1',"1.0-1.19"='#6baed6',
          "1.2-1.39"='#4292c6',"1.4-1.59"='#2171b5',
          "> 1.6"='#084594',"NA"='#A9A9A9')
h14["value"] <- cut(h14[["orival"]],breaks=hbreaks,labels=hlabels[1:7])
h15["value"] <- cut(h15[["orival"]],breaks=hbreaks,labels=hlabels[1:7])
h16["value"] <- cut(h16[["orival"]],breaks=hbreaks,labels=hlabels[1:7])
h17["value"] <- cut(h17[["orival"]],breaks=hbreaks,labels=hlabels[1:7])

# zip_choropleth(h15, county_zoom = nyc_fips, legend="Violation per restaurant")
# zip_choropleth(h16, county_zoom = nyc_fips, legend="Violation per restaurant")
# zip_choropleth(h17, county_zoom = nyc_fips, legend="Violation per restaurant")

fbreaks <- c(0,1.0,1.2,1.3,1.4,1.5,1.6,3.0)
flabels <- c("< 1.0","1.0-1.19","1.2-1.29","1.3-1.39","1.4-1.49","1.5-1.59","> 1.6","NA")
fcol <- c("< 1.0"='#f2f0f7',"1.0-1.19"='#dadaeb',
          "1.2-1.29"='#bcbddc',"1.3-1.39"='#9e9ac8',
          "1.4-1.49"='#807dba',"1.5-1.59"='#6a51a3',
          "> 1.6"='#4a1486',"NA"='#A9A9A9')
f14["value"] <- cut(f14[["orival"]],breaks=fbreaks,labels=flabels[1:7])
f15["value"] <- cut(f15[["orival"]],breaks=fbreaks,labels=flabels[1:7])
f16["value"] <- cut(f16[["orival"]],breaks=fbreaks,labels=flabels[1:7])
f17["value"] <- cut(f17[["orival"]],breaks=fbreaks,labels=flabels[1:7])

# zip_choropleth(f15, county_zoom = nyc_fips, legend="Violation per restaurant")
# zip_choropleth(f16, county_zoom = nyc_fips, legend="Violation per restaurant")
# zip_choropleth(f17, county_zoom = nyc_fips, legend="Violation per restaurant")

# County/borough level choropleths

br14 <- restzip(inspection,2014,"b")
br15 <- restzip(inspection,2015,"b")
br16 <- restzip(inspection,2016,"b")
br17 <- restzip(inspection,2017,"b")

# Create and borough stats: viobor
viobor <- function(df,yr,type){
  minyr <- as.character(yr-1)
  maxyr <- as.character(yr+1)
  mindate <- paste(minyr,"-12-31",sep="")
  maxdate <- paste(maxyr,"-01-01",sep="")
  if(missing(type)){
    tempdf <- count(df[df$DATE > as.Date(mindate) &
                         df$DATE < as.Date(maxdate) &
                         df$VIOCAT != "",],
                    vars="BORO")
  }
  else{
    tempdf <- count(df[df$DATE > as.Date(mindate) &
                         df$DATE < as.Date(maxdate) &
                         df$VIOCAT==type,],
                    vars="BORO")
  }
  if(yr==2014){
    tempdf["orival"] <- tempdf["freq"] / br14["freq"]
  }
  else if(yr==2015){
    tempdf["orival"] <- tempdf["freq"] / br15["freq"]
  }
  else if(yr==2016){
    tempdf["orival"] <- tempdf["freq"] / br16["freq"]
  }
  else if(yr==2017){
    tempdf["orival"] <- tempdf["freq"] / br17["freq"]
  }
  else{print("error")}
  tempdf["region"] <- tempdf["BORO"]
  tempdf[tempdf$region=="bronx","region"] <- 36005
  tempdf[tempdf$region=="new york","region"] <- 36061
  tempdf[tempdf$region=="kings","region"] <- 36047
  tempdf[tempdf$region=="queens","region"] <- 36081
  tempdf[tempdf$region=="richmond","region"] <- 36085
  tempdf$region <- as.numeric(tempdf$region)
  return(tempdf)
}

bv14 <- viobor(viomerge,2014)
bv15 <- viobor(viomerge,2015)
bv16 <- viobor(viomerge,2016)
bv17 <- viobor(viomerge,2017)

bp14 <- viobor(viomerge,2014,"pest")
bp15 <- viobor(viomerge,2015,"pest")
bp16 <- viobor(viomerge,2016,"pest")
bp17 <- viobor(viomerge,2017,"pest")

bh14 <- viobor(viomerge,2014,"handling")
bh15 <- viobor(viomerge,2015,"handling")
bh16 <- viobor(viomerge,2016,"handling")
bh17 <- viobor(viomerge,2017,"handling")

bf14 <- viobor(viomerge,2014,"facility")
bf15 <- viobor(viomerge,2015,"facility")
bf16 <- viobor(viomerge,2016,"facility")
bf17 <- viobor(viomerge,2017,"facility")

# 

bv14["value"] <- cut(bv14[["orival"]],breaks=vbreaks,labels=vlabels[1:7])
bv15["value"] <- cut(bv15[["orival"]],breaks=vbreaks,labels=vlabels[1:7])
bv16["value"] <- cut(bv16[["orival"]],breaks=vbreaks,labels=vlabels[1:7])
bv17["value"] <- cut(bv17[["orival"]],breaks=vbreaks,labels=vlabels[1:7])

bp14["value"] <- cut(bp14[["orival"]],breaks=pbreaks,labels=plabels[1:7])
bp15["value"] <- cut(bp15[["orival"]],breaks=pbreaks,labels=plabels[1:7])
bp16["value"] <- cut(bp16[["orival"]],breaks=pbreaks,labels=plabels[1:7])
bp17["value"] <- cut(bp17[["orival"]],breaks=pbreaks,labels=plabels[1:7])

bh14["value"] <- cut(bh14[["orival"]],breaks=hbreaks,labels=hlabels[1:7])
bh15["value"] <- cut(bh15[["orival"]],breaks=hbreaks,labels=hlabels[1:7])
bh16["value"] <- cut(bh16[["orival"]],breaks=hbreaks,labels=hlabels[1:7])
bh17["value"] <- cut(bh17[["orival"]],breaks=hbreaks,labels=hlabels[1:7])

bf14["value"] <- cut(bf14[["orival"]],breaks=fbreaks,labels=flabels[1:7])
bf15["value"] <- cut(bf15[["orival"]],breaks=fbreaks,labels=flabels[1:7])
bf16["value"] <- cut(bf16[["orival"]],breaks=fbreaks,labels=flabels[1:7])
bf17["value"] <- cut(bf17[["orival"]],breaks=fbreaks,labels=flabels[1:7])

# county_choropleth(bv15,county_zoom = nyc_fips,legend="Violation per restaurant")
# county_choropleth(bv16,county_zoom = nyc_fips,legend="Violation per restaurant")
# county_choropleth(bv17,county_zoom = nyc_fips,legend="Violation per restaurant")
# 
# county_choropleth(bp15,county_zoom = nyc_fips,legend="Violation per restaurant")
# county_choropleth(bp16,county_zoom = nyc_fips,legend="Violation per restaurant")
# county_choropleth(bp17,county_zoom = nyc_fips,legend="Violation per restaurant")
# 
# county_choropleth(bh15,county_zoom = nyc_fips,legend="Violation per restaurant")
# county_choropleth(bh16,county_zoom = nyc_fips,legend="Violation per restaurant")
# county_choropleth(bh17,county_zoom = nyc_fips,legend="Violation per restaurant")
# 
# county_choropleth(bf15,county_zoom = nyc_fips,legend="Violation per restaurant")
# county_choropleth(bf16,county_zoom = nyc_fips,legend="Violation per restaurant")
# county_choropleth(bf17,county_zoom = nyc_fips,legend="Violation per restaurant") +
#   scale_fill_manual(values=c('#edf8e9','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#005a32'))

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

bronx <- 36005
kings <- 36047
newyork <- 36061
queens <- 36081
richmond <- 36085

# Produces choropleth of interest: whichchoro
whichchoro <- function(year,vio,boro){
  # Borough choropleths
  if(missing(boro)){
    if(vio=="all"){
      switch(year,
             "14"=county_choropleth(bv14,county_zoom=nyc_fips) +
               scale_fill_manual(values=vcol,breaks=vbreaks,labels=vlabels),
             "15"=county_choropleth(bv15,county_zoom=nyc_fips) +
               scale_fill_manual(values=vcol,breaks=vbreaks,labels=vlabels),
             "16"=county_choropleth(bv16,county_zoom=nyc_fips) +
               scale_fill_manual(values=vcol,breaks=vbreaks,labels=vlabels),
             "17"=county_choropleth(bv17,county_zoom=nyc_fips) +
               scale_fill_manual(values=vcol,breaks=vbreaks,labels=vlabels))
    }
    else if(vio=="pest"){
      switch(year,
             "14"=county_choropleth(bp14,county_zoom=nyc_fips) +
               scale_fill_manual(values=pcol,breaks=pbreaks,labels=plabels),
             "15"=county_choropleth(bp15,county_zoom=nyc_fips) +
               scale_fill_manual(values=pcol,breaks=pbreaks,labels=plabels),
             "16"=county_choropleth(bp16,county_zoom=nyc_fips) +
               scale_fill_manual(values=pcol,breaks=pbreaks,labels=plabels),
             "17"=county_choropleth(bp17,county_zoom=nyc_fips) +
               scale_fill_manual(values=pcol,breaks=pbreaks,labels=plabels))
    }
    else if(vio=="handling"){
      switch(year,
             "14"=county_choropleth(bh14,county_zoom=nyc_fips) +
               scale_fill_manual(values=hcol,breaks=hbreaks,labels=hlabels),
             "15"=county_choropleth(bh15,county_zoom=nyc_fips) +
               scale_fill_manual(values=hcol,breaks=hbreaks,labels=hlabels),
             "16"=county_choropleth(bh16,county_zoom=nyc_fips) +
               scale_fill_manual(values=hcol,breaks=hbreaks,labels=hlabels),
             "17"=county_choropleth(bh17,county_zoom=nyc_fips) +
               scale_fill_manual(values=hcol,breaks=hbreaks,labels=hlabels))
    }
    else{
      switch(year,
             "14"=county_choropleth(bf14,county_zoom=nyc_fips) +
               scale_fill_manual(values=fcol,breaks=fbreaks,labels=flabels),
             "15"=county_choropleth(bf15,county_zoom=nyc_fips) +
               scale_fill_manual(values=fcol,breaks=fbreaks,labels=flabels),
             "16"=county_choropleth(bf16,county_zoom=nyc_fips) +
               scale_fill_manual(values=fcol,breaks=fbreaks,labels=flabels),
             "17"=county_choropleth(bf17,county_zoom=nyc_fips) +
               scale_fill_manual(values=fcol,breaks=fbreaks,labels=flabels))
    }
  }
  # Zip choropleths of boroughs of choice
  else{
    zipboro <- switch(boro,
                      "bronx"=bronx,
                      "kings"=kings,
                      "new york"=newyork,
                      "queens"=queens,
                      "richmond"=richmond)
    if(vio=="all"){
      switch(year,
             "14"=zip_choropleth(v14[v14$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=vbreaks,values=vcol,na.value="darkgray"),
             "15"=zip_choropleth(v15[v15$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=vbreaks,values=vcol,na.value="darkgray"),
             "16"=zip_choropleth(v16[v16$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=vbreaks,values=vcol,na.value="darkgray"),
             "17"=zip_choropleth(v17[v17$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=vbreaks,values=vcol,na.value="darkgray"))
    }
    else if(vio=="pest"){
      switch(year,
             "14"=zip_choropleth(p14[p14$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=pbreaks,values=pcol,na.value="darkgray"),
             "15"=zip_choropleth(p15[p15$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=pbreaks,values=pcol,na.value="darkgray"),
             "16"=zip_choropleth(p16[p16$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=pbreaks,values=pcol,na.value="darkgray"),
             "17"=zip_choropleth(p17[p17$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=pbreaks,values=pcol,na.value="darkgray"))
    }
    else if(vio=="handling"){
      switch(year,
             "14"=zip_choropleth(h14[h14$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=hbreaks,values=hcol,na.value="darkgray"),
             "15"=zip_choropleth(h15[h15$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=hbreaks,values=hcol,na.value="darkgray"),
             "16"=zip_choropleth(h16[h16$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=hbreaks,values=hcol,na.value="darkgray"),
             "17"=zip_choropleth(h17[h17$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=hbreaks,values=hcol,na.value="darkgray"))
    }
    else{
      switch(year,
             "14"=zip_choropleth(f14[f14$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=fbreaks,values=fcol,na.value="darkgray"),
             "15"=zip_choropleth(f15[f15$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=fbreaks,values=fcol,na.value="darkgray"),
             "16"=zip_choropleth(f16[f16$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=fbreaks,values=fcol,na.value="darkgray"),
             "17"=zip_choropleth(f17[f17$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=fbreaks,values=fcol,na.value="darkgray"))
    }
  }
}

whichlegend <- function(vio){
  # Manually create legends, because choroplethr is being a pain
  if(vio=="all"){
    ggplot(vlegend,aes(x=vlabels,y=allones,fill=vlabels)) + geom_bar(stat='identity') +
      coord_flip() + #ggtitle("Violations per Restaurant") +
      scale_fill_manual(values=vcol) +
      theme(legend.position = "none",
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.x=element_blank(),
            panel.background=element_rect(color="white",fill="white"))
  }
  else if(vio=="pest"){
    ggplot(plegend,aes(x=plabels,y=allones,fill=plabels)) + geom_bar(stat='identity') +
      coord_flip() + #ggtitle("Violations per Restaurant") +
      scale_fill_manual(values=pcol) +
      theme(legend.position = "none",
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.x=element_blank(),
            panel.background=element_rect(color="white",fill="white"))
  }
  else if(vio=="handling"){
    ggplot(hlegend,aes(x=hlabels,y=allones,fill=hlabels)) + geom_bar(stat='identity') +
      coord_flip() + #ggtitle("Violations per Restaurant") +
      scale_fill_manual(values=hcol) +
      theme(legend.position = "none",
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.x=element_blank(),
            panel.background=element_rect(color="white",fill="white"))
  }
  else{
    ggplot(flegend,aes(x=flabels,y=allones,fill=flabels)) + geom_bar(stat='identity') +
      coord_flip() + #ggtitle("Violations per Restaurant") +
      scale_fill_manual(values=fcol) +
      theme(legend.position = "none",
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.x=element_blank(),
            panel.background=element_rect(color="white",fill="white"))
  }
}

chorolist <- function(type,boro){
  if(missing(boro)){
    for(i in 1:4){
      title <- paste(type,as.character(i+13),sep="")
      title <- paste(title,".png")
      print(whichchoro(as.character(i+13),type))
      # dev.copy(png, title)
      # dev.off()
    }
  }
  else{
    for(i in 1:4){
      title <- substr(boro,1,1)
      title <- paste(title,type,sep="")
      title <- paste(title,as.character(i+13),sep="")
      print(whichchoro(as.character(i+13),type,boro))
      # dev.copy(png, title)
      # dev.off()
    }
  }
}

whichlegend("all")
whichlegend("pest")
whichlegend("handling")
whichlegend("facility")

chorolist("all")
chorolist("pest")
chorolist("handling")
chorolist("facility")

chorolist("all","bronx")
chorolist("pest","bronx")
chorolist("handling","bronx")
chorolist("facility","bronx")

chorolist("all","kings")
chorolist("pest","kings")
chorolist("handling","kings")
chorolist("facility","kings")

chorolist("all","new york")
chorolist("pest","new york")
chorolist("handling","new york")
chorolist("facility","new york")

chorolist("all","queens")
chorolist("pest","queens")
chorolist("handling","queens")
chorolist("facility","queens")

chorolist("all","richmond")
chorolist("pest","richmond")
chorolist("handling","richmond")
chorolist("facility","richmond")

# zip_choropleth(v14,county_zoom=nyc_fips) +
#   scale_fill_manual(breaks=vbreaks,values=vcol,na.value="black")
# zip_choropleth(v15,county_zoom=nyc_fips) +
#   scale_fill_manual(breaks=vbreaks,values=vcol,na.value="black")
# zip_choropleth(v16,county_zoom=nyc_fips) +
#   scale_fill_manual(breaks=vbreaks,values=vcol,na.value="black")
# zip_choropleth(v17,county_zoom=nyc_fips) +
#   scale_fill_manual(breaks=vbreaks,values=vcol,na.value="black")
# 
# zip_choropleth(p14,county_zoom=nyc_fips) +
#   scale_fill_manual(breaks=pbreaks,values=pcol,na.value="black")
# zip_choropleth(p15,county_zoom=nyc_fips) +
#   scale_fill_manual(breaks=pbreaks,values=pcol,na.value="black")
# zip_choropleth(p16,county_zoom=nyc_fips) +
#   scale_fill_manual(breaks=pbreaks,values=pcol,na.value="black")
# zip_choropleth(p17,county_zoom=nyc_fips) +
#   scale_fill_manual(breaks=pbreaks,values=pcol,na.value="black")
# 
# zip_choropleth(h14,county_zoom=nyc_fips) +
#   scale_fill_manual(breaks=hbreaks,values=hcol,na.value="black")
# zip_choropleth(h15,county_zoom=nyc_fips) +
#   scale_fill_manual(breaks=hbreaks,values=hcol,na.value="black")
# zip_choropleth(h16,county_zoom=nyc_fips) +
#   scale_fill_manual(breaks=hbreaks,values=hcol,na.value="black")
# zip_choropleth(h17,county_zoom=nyc_fips) +
#   scale_fill_manual(breaks=hbreaks,values=hcol,na.value="black")
# 
# zip_choropleth(f14,county_zoom=nyc_fips) +
#   scale_fill_manual(breaks=fbreaks,values=fcol,na.value="black")
# zip_choropleth(f15,county_zoom=nyc_fips) +
#   scale_fill_manual(breaks=fbreaks,values=fcol,na.value="black")
# zip_choropleth(f16,county_zoom=nyc_fips) +
#   scale_fill_manual(breaks=fbreaks,values=fcol,na.value="black")
# zip_choropleth(f17,county_zoom=nyc_fips) +
#   scale_fill_manual(breaks=fbreaks,values=fcol,na.value="black")
# 
# whichchoro("14","all")
# whichchoro("15","all")
# whichchoro("16","all")
# whichchoro("17","all")
# whichlegend("all")
# 
# whichchoro("14","pest")
# whichchoro("15","pest")
# whichchoro("16","pest")
# whichchoro("17","pest")
# whichlegend("pest")
# 
# whichchoro("14","handling")
# whichchoro("15","handling")
# whichchoro("16","handling")
# whichchoro("17","handling")
# whichlegend("handling")
# 
# whichchoro("14","facility")
# whichchoro("15","facility")
# whichchoro("16","facility")
# whichchoro("17","facility")
# whichlegend("facility")
# 
# whichchoro("14","all","new york")
# whichchoro("15","all","new york")
# whichchoro("16","all","new york")
# whichchoro("17","all","new york")
# 
# whichchoro("14","pest","new york")
# whichchoro("15","pest","new york")
# whichchoro("16","pest","new york")
# whichchoro("17","pest","new york")
# 
# whichchoro("14","handling","new york")
# whichchoro("15","handling","new york")
# whichchoro("16","handling","new york")
# whichchoro("17","handling","new york")
# 
# whichchoro("14","facility","new york")
# whichchoro("15","facility","new york")
# whichchoro("16","facility","new york")
# whichchoro("17","facility","new york")

# Export df's for shiny
write.csv(v14,"v14.csv")
write.csv(v15,"v15.csv")
write.csv(v16,"v16.csv")
write.csv(v17,"v17.csv")

write.csv(p14,"p14.csv")
write.csv(p15,"p15.csv")
write.csv(p16,"p16.csv")
write.csv(p17,"p17.csv")

write.csv(h14,"h14.csv")
write.csv(h15,"h15.csv")
write.csv(h16,"h16.csv")
write.csv(h17,"h17.csv")

write.csv(f14,"f14.csv")
write.csv(f15,"f15.csv")
write.csv(f16,"f16.csv")
write.csv(f17,"f17.csv")

write.csv(bv14,"bv14.csv")
write.csv(bv15,"bv15.csv")
write.csv(bv16,"bv16.csv")
write.csv(bv17,"bv17.csv")

write.csv(bp14,"bp14.csv")
write.csv(bp15,"bp15.csv")
write.csv(bp16,"bp16.csv")
write.csv(bp17,"bp17.csv")

write.csv(bh14,"bh14.csv")
write.csv(bh15,"bh15.csv")
write.csv(bh16,"bh16.csv")
write.csv(bh17,"bh17.csv")

write.csv(bf14,"bf14.csv")
write.csv(bf15,"bf15.csv")
write.csv(bf16,"bf16.csv")
write.csv(bf17,"bf17.csv")


# Copy/paste previous choropleths

rest <- unique(inspection[,c("CAMIS","ZIPCODE","BORO")])

# Creating choropleth of zip codes
# df of the number of restaurants per zip code: restzip
restzip <- count(rest,"ZIPCODE")
restzip <- rename(restzip, c("ZIPCODE"="region","freq"="value"))
restzip$region <- as.character(restzip$region)
# Counties/boroughs of New York: nyc_flips
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
zip_choropleth(restzip, county_zoom = nyc_fips,
               title = "Number of restaurants per NYC zip code", legend = "Count")
