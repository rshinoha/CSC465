library(choroplethrZip)
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)

setwd("C:/Users/Ryoh/Documents/CSC465/Project/ProjectShinyV2")

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
vbreaks <- c(0,2.5,3.0,3.5,4.0,4.5,5.0,10.0)
vlabels <- c("< 2.5","2.5-2.99","3.0-3.49","3.5-3.99","4.0-4.49","4.5-4.99","> 5.0","NA")
vcol <- c("< 2.5"='#edf8e9',"2.5-2.99"='#c7e9c0',
          "3.0-3.49"='#a1d99b',"3.5-3.99"='#74c476',
          "4.0-4.49"='#41ab5d',"4.5-4.99"='#238b45',
          "> 5.0"='#005a32',"NA"='#000000')

# Pests
pbreaks <- c(0,0.6,0.8,0.9,1.0,1.5,2.0,3.0)
plabels <- c("< 0.6","0.6-0.79","0.8-0.89","0.9-0.99","1.0-1.49","1.5-1.99","> 2.0","NA")
pcol <- c("< 0.6"='#fee5d9',"0.6-0.79"='#fcbba1',
          "0.8-0.89"='#fc9272',"0.9-0.99"='#fb6a4a',
          "1.0-1.49"='#ef3b2c',"1.5-1.99"='#cb181d',
          "> 2.0"='#99000d',"NA"='#000000')

# Handling
hbreaks <- c(0,0.8,0.9,1.0,1.25,1.5,2.0,3.0)
hlabels <- c("< 0.8","0.8-0.89","0.9-0.99","1.0-1.24","1.25-1.49","1.5-1.99","> 2.0","NA")
hcol <- c("< 0.8"='#eff3ff',"0.8-0.89"='#c6dbef',
          "0.9-0.99"='#9ecae1',"1.0-1.24"='#6baed6',
          "1.25-1.49"='#4292c6',"1.5-1.99"='#2171b5',
          "> 2.0"='#084594',"NA"='#000000')

# Facility
fbreaks <- c(0,1.0,1.3,1.4,1.5,1.75,2.0,3.0)
flabels <- c("< 1.0","1.0-1.29","1.3-1.39","1.4-1.49","1.5-1.74","1.75-1.99","> 2.0","NA")
fcol <- c("< 1.0"='#f2f0f7',"1.0-1.29"='#dadaeb',
          "1.3-1.39"='#bcbddc',"1.4-1.49"='#9e9ac8',
          "1.5-1.74"='#807dba',"1.75-1.99"='#6a51a3',
          "> 2.0"='#4a1486',"NA"='#000000')

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
                          levels=rev(c("< 2.5","2.5-2.99","3.0-3.49","3.5-3.99",
                                       "4.0-4.49","4.5-4.99","> 5.0","NA")))
plegend$plabels <- factor(plegend$plabels,
                          levels=rev(c("< 0.6","0.6-0.79","0.8-0.89","0.9-0.99",
                                       "1.0-1.49","1.5-1.99","> 2.0","NA")))
hlegend$hlabels <- factor(hlegend$hlabels,
                          levels=rev(c("< 0.8","0.8-0.89","0.9-0.99","1.0-1.24",
                                       "1.25-1.49","1.5-1.99","> 2.0","NA")))
flegend$flabels <- factor(flegend$flabels,
                          levels=rev(c("< 1.0","1.0-1.29","1.3-1.39","1.4-1.49",
                                       "1.5-1.74","1.75-1.99","> 2.0","NA")))

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
               scale_fill_manual(breaks=vbreaks,values=vcol,na.value="black"),
             "15"=zip_choropleth(v15[v15$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=vbreaks,values=vcol,na.value="black"),
             "16"=zip_choropleth(v16[v16$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=vbreaks,values=vcol,na.value="black"),
             "17"=zip_choropleth(v17[v17$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=vbreaks,values=vcol,na.value="black"))
    }
    else if(vio=="pest"){
      switch(year,
             "14"=zip_choropleth(p14[p14$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=pbreaks,values=pcol,na.value="black"),
             "15"=zip_choropleth(p15[p15$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=pbreaks,values=pcol,na.value="black"),
             "16"=zip_choropleth(p16[p16$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=pbreaks,values=pcol,na.value="black"),
             "17"=zip_choropleth(p17[p17$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=pbreaks,values=pcol,na.value="black"))
    }
    else if(vio=="handling"){
      switch(year,
             "14"=zip_choropleth(h14[h14$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=hbreaks,values=hcol,na.value="black"),
             "15"=zip_choropleth(h15[h15$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=hbreaks,values=hcol,na.value="black"),
             "16"=zip_choropleth(h16[h16$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=hbreaks,values=hcol,na.value="black"),
             "17"=zip_choropleth(h17[h17$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=hbreaks,values=hcol,na.value="black"))
    }
    else{
      switch(year,
             "14"=zip_choropleth(f14[f14$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=fbreaks,values=fcol,na.value="black"),
             "15"=zip_choropleth(f15[f15$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=fbreaks,values=fcol,na.value="black"),
             "16"=zip_choropleth(f16[f16$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=fbreaks,values=fcol,na.value="black"),
             "17"=zip_choropleth(f17[f17$BORO==boro,],county_zoom=c(zipboro)) +
               scale_fill_manual(breaks=fbreaks,values=fcol,na.value="black"))
    }
  }
}

chorolist <- function(type,boro){
  l <- list()
  if(missing(boro)){
    for(i in 1:4){
      l[[i]] <- whichchoro(as.character(i+13),type)
    }
  }
  else{
    for(i in 1:4){
      l[[i]] <- whichchoro(as.character(i+13),type,boro)
    }
  }
  return(l)
}

# Lists to put NYC choropleths
cball <- list()
cbpest <- list()
cbhandling <- list()
cbfacility <- list()

# Lists to put Bronx choropleths
ball <- list()
bpest <- list()
bhandling <- list()
bfacility <- list()

# Lists to put Kings choropleths
kall <- list()
kpest <- list()
khandling <- list()
kfacility <- list()

# Lists to put New York choropleths
nall <- list()
npest <- list()
nhandling <- list()
nfacility <- list()

# Lists to put Queens choropleths
qall <- list()
qpest <- list()
qhandling <- list()
qfacility <- list()

# Lists to put Richmond choropleths
rall <- list()
rpest <- list()
rhandling <- list()
rfacility <- list()

cball <- chorolist("all")
cbpest <- chorolist("pest")
cbhandling <- chorolist("handling")
cbfacility <- chorolist("facility")

ball <- chorolist("all","bronx")
bpest <- chorolist("pest","bronx")
bhandling <- chorolist("handling","bronx")
bfacility <- chorolist("facility","bronx")

kall <- chorolist("all","kings")
kpest <- chorolist("pest","kings")
khandling <- chorolist("handling","kings")
kfacility <- chorolist("facility","kings")

nall <- chorolist("all","new york")
npest <- chorolist("pest","new york")
nhandling <- chorolist("handling","new york")
nfacility <- chorolist("facility","new york")

qall <- chorolist("all","queens")
qpest <- chorolist("pest","queens")
qhandling <- chorolist("handling","queens")
qfacility <- chorolist("facility","queens")

rall <- chorolist("all","richmond")
rpest <- chorolist("pest","richmond")
rhandling <- chorolist("handling","richmond")
rfacility <- chorolist("facility","richmond")