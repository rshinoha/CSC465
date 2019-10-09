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
  output$choroBoro <- renderPlot({
    if(input$violation=="all"){
      county_choropleth(boro(),county_zoom=nyc_fips) +
        scale_fill_manual(values=vcol,breaks=vbreaks,labels=vlabels)
    }
    else if(input$violation=="pest"){
      county_choropleth(boro(),county_zoom=nyc_fips) +
        scale_fill_manual(values=pcol,breaks=pbreaks,labels=plabels)
    }
    else if(input$violation=="handling"){
      county_choropleth(boro(),county_zoom=nyc_fips) +
        scale_fill_manual(values=hcol,breaks=hbreaks,labels=hlabels)
    }
    else{
      county_choropleth(boro(),county_zoom=nyc_fips) +
        scale_fill_manual(values=fcol,breaks=fbreaks,labels=flabels)
    }
  })
  output$choroZip <- renderPlot({
    if(input$violation=="all"){
      zip_choropleth(zip(),county_zoom=c(zipboro())) +
        scale_fill_manual(breaks=vbreaks,values=vcol,na.value="black")
    }
    else if(input$violation=="pest"){
      zip_choropleth(zip(),county_zoom=c(zipboro())) +
        scale_fill_manual(breaks=pbreaks,values=pcol,na.value="black")
    }
    else if(input$violation=="handling"){
      zip_choropleth(zip(),county_zoom=c(zipboro())) +
        scale_fill_manual(breaks=hbreaks,values=hcol,na.value="black")
    }
    else{
      zip_choropleth(zip(),county_zoom=c(zipboro())) +
        scale_fill_manual(breaks=fbreaks,values=fcol,na.value="black")
    }
  })
  output$legend <- renderPlot({
    if(input$violation=="all"){
      ggplot(vlegend,aes(x=vlabels,y=allones,fill=vlabels)) + geom_bar(stat='identity') +
        coord_flip() +
        scale_fill_manual(values=c("< 2.5"='#edf8e9',"2.5-2.99"='#c7e9c0',
                                   "3.0-3.49"='#a1d99b',"3.5-3.99"='#74c476',
                                   "4.0-4.49"='#41ab5d',"4.5-4.99"='#238b45',
                                   "> 5.0"='#005a32',"NA"='#000000')) +
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
        scale_fill_manual(values=c("< 0.6"='#fee5d9',"0.6-0.79"='#fcbba1',
                                   "0.8-0.89"='#fc9272',"0.9-0.99"='#fb6a4a',
                                   "1.0-1.49"='#ef3b2c',"1.5-1.99"='#cb181d',
                                   "> 2.0"='#99000d',"NA"='#000000')) +
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
        scale_fill_manual(values=c("< 0.8"='#eff3ff',"0.8-0.89"='#c6dbef',
                                   "0.9-0.99"='#9ecae1',"1.0-1.24"='#6baed6',
                                   "1.25-1.49"='#4292c6',"1.5-1.99"='#2171b5',
                                   "> 2.0"='#084594',"NA"='#000000')) +
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
        scale_fill_manual(values=c("< 1.0"='#f2f0f7',"1.0-1.29"='#dadaeb',
                                   "1.3-1.39"='#bcbddc',"1.4-1.49"='#9e9ac8',
                                   "1.5-1.74"='#807dba',"1.75-1.99"='#6a51a3',
                                   "> 2.0"='#4a1486',"NA"='#000000')) +
        theme(legend.position = "none",
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.x=element_blank(),
              panel.background=element_rect(color="white",fill="white"))
    }
  })
}
