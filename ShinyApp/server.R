function(input,output,session){
  boro <- reactive({
    if(input$violation=="all"){
      switch(input$year,
             "14"=bv14,
             "15"=bv15,
             "16"=bv16,
             "17"=bv17)
    }
    else if(input$violation=="pest"){
      switch(input$year,
             "14"=bp14,
             "15"=bp15,
             "16"=bp16,
             "17"=bp17)
    }
    else if(input$violation=="handling"){
      switch(input$year,
             "14"=bh14,
             "15"=bh15,
             "16"=bh16,
             "17"=bh17)
    }
    else if(input$violation=="facility"){
      switch(input$year,
             "14"=bf14,
             "15"=bf15,
             "16"=bf16,
             "17"=bf17)
    }
    else{
      stop("Bad input")
    }
  })
  zip <- reactive({
    if(input$violation=="all"){
      switch(input$year,
             "14"=v14[v14$BORO==input$borough,],
             "15"=v15[v15$BORO==input$borough,],
             "16"=v16[v16$BORO==input$borough,],
             "17"=v17[v17$BORO==input$borough,])
    }
    else if(input$violation=="pest"){
      switch(input$year,
             "14"=p14[p14$BORO==input$borough,],
             "15"=p15[p15$BORO==input$borough,],
             "16"=p16[p16$BORO==input$borough,],
             "17"=p17[p17$BORO==input$borough,])
    }
    else if(input$violation=="handling"){
      switch(input$year,
             "14"=h14[h14$BORO==input$borough,],
             "15"=h15[h15$BORO==input$borough,],
             "16"=h16[h16$BORO==input$borough,],
             "17"=h17[h17$BORO==input$borough,])
    }
    else if(input$violation=="facility"){
      switch(input$year,
             "14"=f14[f14$BORO==input$borough,],
             "15"=f15[f15$BORO==input$borough,],
             "16"=f16[f16$BORO==input$borough,],
             "17"=f17[f17$BORO==input$borough,])
    }
    else{
      stop("Bad input")
    }
  })
  zipboro <- reactive({
    switch(input$borough,
           "bronx"=bronx,
           "kings"=kings,
           "new york"=newyork,
           "queens"=queens,
           "richmond"=richmond)
  })
  zipall <- reactive({
    switch(input$boroz,
           "bronx"=bronx,
           "kings"=kings,
           "new york"=newyork,
           "queens"=queens,
           "richmond"=richmond)
  })
  output$choroBoro <- renderPlot({
    if(input$violation=="all"){
      county_choropleth(boro(),county_zoom=nyc_fips) +
        hcounty(zipboro()) +
        scale_fill_manual(values=vcol,breaks=vbreaks,labels=vlabels)
    }
    else if(input$violation=="pest"){
      county_choropleth(boro(),county_zoom=nyc_fips) +
        hcounty(zipboro()) +
        scale_fill_manual(values=pcol,breaks=pbreaks,labels=plabels)
    }
    else if(input$violation=="handling"){
      county_choropleth(boro(),county_zoom=nyc_fips) +
        hcounty(zipboro()) +
        scale_fill_manual(values=hcol,breaks=hbreaks,labels=hlabels)
    }
    else{
      county_choropleth(boro(),county_zoom=nyc_fips) +
        hcounty(zipboro()) +
        scale_fill_manual(values=fcol,breaks=fbreaks,labels=flabels)
    }
  })
  output$choroZip <- renderPlot({
    if(input$violation=="all"){
      zip_choropleth(zip(),county_zoom=c(zipboro())) +
        scale_fill_manual(breaks=vbreaks,values=vcol,na.value="darkgray")
    }
    else if(input$violation=="pest"){
      zip_choropleth(zip(),county_zoom=c(zipboro())) +
        scale_fill_manual(breaks=pbreaks,values=pcol,na.value="darkgray")
    }
    else if(input$violation=="handling"){
      zip_choropleth(zip(),county_zoom=c(zipboro())) +
        scale_fill_manual(breaks=hbreaks,values=hcol,na.value="darkgray")
    }
    else{
      zip_choropleth(zip(),county_zoom=c(zipboro())) +
        scale_fill_manual(breaks=fbreaks,values=fcol,na.value="darkgray")
    }
  })
  output$Boro14 <- renderPlot({
    if(input$viob=="all"){
      county_choropleth(bv14,county_zoom=nyc_fips) +
        scale_fill_manual(values=vcol,breaks=vbreaks,labels=vlabels)
    }
    else if(input$viob=="pest"){
      county_choropleth(bp14,county_zoom=nyc_fips) +
        scale_fill_manual(values=pcol,breaks=pbreaks,labels=plabels)
    }
    else if(input$viob=="handling"){
      county_choropleth(bh14,county_zoom=nyc_fips) +
        scale_fill_manual(values=hcol,breaks=hbreaks,labels=hlabels)
    }
    else{
      county_choropleth(bf14,county_zoom=nyc_fips) +
        scale_fill_manual(values=fcol,breaks=fbreaks,labels=flabels)
    }
  })
  output$Boro15 <- renderPlot({
    if(input$viob=="all"){
      county_choropleth(bv15,county_zoom=nyc_fips) +
        scale_fill_manual(values=vcol,breaks=vbreaks,labels=vlabels)
    }
    else if(input$viob=="pest"){
      county_choropleth(bp15,county_zoom=nyc_fips) +
        scale_fill_manual(values=pcol,breaks=pbreaks,labels=plabels)
    }
    else if(input$viob=="handling"){
      county_choropleth(bh15,county_zoom=nyc_fips) +
        scale_fill_manual(values=hcol,breaks=hbreaks,labels=hlabels)
    }
    else{
      county_choropleth(bf15,county_zoom=nyc_fips) +
        scale_fill_manual(values=fcol,breaks=fbreaks,labels=flabels)
    }
  })
  output$Boro16 <- renderPlot({
    if(input$viob=="all"){
      county_choropleth(bv16,county_zoom=nyc_fips) +
        scale_fill_manual(values=vcol,breaks=vbreaks,labels=vlabels)
    }
    else if(input$viob=="pest"){
      county_choropleth(bp16,county_zoom=nyc_fips) +
        scale_fill_manual(values=pcol,breaks=pbreaks,labels=plabels)
    }
    else if(input$viob=="handling"){
      county_choropleth(bh16,county_zoom=nyc_fips) +
        scale_fill_manual(values=hcol,breaks=hbreaks,labels=hlabels)
    }
    else{
      county_choropleth(bf16,county_zoom=nyc_fips) +
        scale_fill_manual(values=fcol,breaks=fbreaks,labels=flabels)
    }
  })
  output$Boro17 <- renderPlot({
    if(input$viob=="all"){
      county_choropleth(bv17,county_zoom=nyc_fips) +
        scale_fill_manual(values=vcol,breaks=vbreaks,labels=vlabels)
    }
    else if(input$viob=="pest"){
      county_choropleth(bp17,county_zoom=nyc_fips) +
        scale_fill_manual(values=pcol,breaks=pbreaks,labels=plabels)
    }
    else if(input$viob=="handling"){
      county_choropleth(bh17,county_zoom=nyc_fips) +
        scale_fill_manual(values=hcol,breaks=hbreaks,labels=hlabels)
    }
    else{
      county_choropleth(bf17,county_zoom=nyc_fips) +
        scale_fill_manual(values=fcol,breaks=fbreaks,labels=flabels)
    }
  })
  output$Zip14 <- renderPlot({
    if(input$vioz=="all"){
      zip_choropleth(v14[v14$BORO==input$boroz,],county_zoom=c(zipall())) +
        scale_fill_manual(breaks=vbreaks,values=vcol,na.value="darkgray")
    }
    else if(input$vioz=="pest"){
      zip_choropleth(p14[p14$BORO==input$boroz,],county_zoom=c(zipall())) +
        scale_fill_manual(breaks=pbreaks,values=pcol,na.value="darkgray")
    }
    else if(input$vioz=="handling"){
      zip_choropleth(h14[h14$BORO==input$boroz,],county_zoom=c(zipall())) +
        scale_fill_manual(breaks=hbreaks,values=hcol,na.value="darkgray")
    }
    else{
      zip_choropleth(f14[f14$BORO==input$boroz,],county_zoom=c(zipall())) +
        scale_fill_manual(breaks=fbreaks,values=fcol,na.value="darkgray")
    }
  })
  output$Zip15 <- renderPlot({
    if(input$vioz=="all"){
      zip_choropleth(v15[v15$BORO==input$boroz,],county_zoom=c(zipall())) +
        scale_fill_manual(breaks=vbreaks,values=vcol,na.value="darkgray")
    }
    else if(input$vioz=="pest"){
      zip_choropleth(p15[p15$BORO==input$boroz,],county_zoom=c(zipall())) +
        scale_fill_manual(breaks=pbreaks,values=pcol,na.value="darkgray")
    }
    else if(input$vioz=="handling"){
      zip_choropleth(h15[h15$BORO==input$boroz,],county_zoom=c(zipall())) +
        scale_fill_manual(breaks=hbreaks,values=hcol,na.value="darkgray")
    }
    else{
      zip_choropleth(f15[f15$BORO==input$boroz,],county_zoom=c(zipall())) +
        scale_fill_manual(breaks=fbreaks,values=fcol,na.value="darkgray")
    }
  })
  output$Zip16 <- renderPlot({
    if(input$vioz=="all"){
      zip_choropleth(v16[v16$BORO==input$boroz,],county_zoom=c(zipall())) +
        scale_fill_manual(breaks=vbreaks,values=vcol,na.value="darkgray")
    }
    else if(input$vioz=="pest"){
      zip_choropleth(p16[p16$BORO==input$boroz,],county_zoom=c(zipall())) +
        scale_fill_manual(breaks=pbreaks,values=pcol,na.value="darkgray")
    }
    else if(input$vioz=="handling"){
      zip_choropleth(h16[h16$BORO==input$boroz,],county_zoom=c(zipall())) +
        scale_fill_manual(breaks=hbreaks,values=hcol,na.value="darkgray")
    }
    else{
      zip_choropleth(f16[f16$BORO==input$boroz,],county_zoom=c(zipall())) +
        scale_fill_manual(breaks=fbreaks,values=fcol,na.value="darkgray")
    }
  })
  output$Zip17 <- renderPlot({
    if(input$vioz=="all"){
      zip_choropleth(v17[v17$BORO==input$boroz,],county_zoom=c(zipall())) +
        scale_fill_manual(breaks=vbreaks,values=vcol,na.value="darkgray")
    }
    else if(input$vioz=="pest"){
      zip_choropleth(p17[p17$BORO==input$boroz,],county_zoom=c(zipall())) +
        scale_fill_manual(breaks=pbreaks,values=pcol,na.value="darkgray")
    }
    else if(input$vioz=="handling"){
      zip_choropleth(h17[h17$BORO==input$boroz,],county_zoom=c(zipall())) +
        scale_fill_manual(breaks=hbreaks,values=hcol,na.value="darkgray")
    }
    else{
      zip_choropleth(f17[f17$BORO==input$boroz,],county_zoom=c(zipall())) +
        scale_fill_manual(breaks=fbreaks,values=fcol,na.value="darkgray")
    }
  })
  output$legend <- renderPlot({
    if(input$violation=="all"){
      ggplot(vlegend,aes(x=vlabels,y=allones,fill=vlabels)) + geom_bar(stat='identity') +
        coord_flip() +
        scale_fill_manual(values=vcol) +
        theme(legend.position = "none",
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.x=element_blank(),
              panel.background=element_rect(color="white",fill="white"))
    }
    else if(input$violation=="pest"){
      ggplot(plegend,aes(x=plabels,y=allones,fill=plabels)) + geom_bar(stat='identity') +
        coord_flip() +
        scale_fill_manual(values=pcol) +
        theme(legend.position = "none",
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.x=element_blank(),
              panel.background=element_rect(color="white",fill="white"))
    }
    else if(input$violation=="handling"){
      ggplot(hlegend,aes(x=hlabels,y=allones,fill=hlabels)) + geom_bar(stat='identity') +
        coord_flip() +
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
        coord_flip() +
        scale_fill_manual(values=fcol) +
        theme(legend.position = "none",
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.x=element_blank(),
              panel.background=element_rect(color="white",fill="white"))
    }
  })
  output$legendb <- renderPlot({
    if(input$viob=="all"){
      ggplot(vlegend,aes(x=vlabels,y=allones,fill=vlabels)) + geom_bar(stat='identity') +
        coord_flip() +
        scale_fill_manual(values=vcol) +
        theme(legend.position = "none",
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.x=element_blank(),
              panel.background=element_rect(color="white",fill="white"))
    }
    else if(input$viob=="pest"){
      ggplot(plegend,aes(x=plabels,y=allones,fill=plabels)) + geom_bar(stat='identity') +
        coord_flip() +
        scale_fill_manual(values=pcol) +
        theme(legend.position = "none",
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.x=element_blank(),
              panel.background=element_rect(color="white",fill="white"))
    }
    else if(input$viob=="handling"){
      ggplot(hlegend,aes(x=hlabels,y=allones,fill=hlabels)) + geom_bar(stat='identity') +
        coord_flip() +
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
        coord_flip() +
        scale_fill_manual(values=fcol) +
        theme(legend.position = "none",
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.x=element_blank(),
              panel.background=element_rect(color="white",fill="white"))
    }
  })
  output$legendz <- renderPlot({
    if(input$vioz=="all"){
      ggplot(vlegend,aes(x=vlabels,y=allones,fill=vlabels)) + geom_bar(stat='identity') +
        coord_flip() +
        scale_fill_manual(values=vcol) +
        theme(legend.position = "none",
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.x=element_blank(),
              panel.background=element_rect(color="white",fill="white"))
    }
    else if(input$vioz=="pest"){
      ggplot(plegend,aes(x=plabels,y=allones,fill=plabels)) + geom_bar(stat='identity') +
        coord_flip() +
        scale_fill_manual(values=pcol) +
        theme(legend.position = "none",
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.x=element_blank(),
              panel.background=element_rect(color="white",fill="white"))
    }
    else if(input$vioz=="handling"){
      ggplot(hlegend,aes(x=hlabels,y=allones,fill=hlabels)) + geom_bar(stat='identity') +
        coord_flip() +
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
        coord_flip() +
        scale_fill_manual(values=fcol) +
        theme(legend.position = "none",
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.x=element_blank(),
              panel.background=element_rect(color="white",fill="white"))
    }
  })
}