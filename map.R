---
title: "Heat map"
output: html_document
---



```{r,results='asis',warning=FALSE}
suppressPackageStartupMessages(library(googleVis))
op=options(gvis.plot.tag='chart')

load("/Users/YY/Downloads/newdata1.Rdata")

#Location

GEO = data.frame(newdata$STATUS,newdata$LCA_CASE_EMPLOYER_STATE)
GEO$recode =  newdata$STATUS == "CERTIFIED" | newdata$STATUS == "CERTIFIED-WITHDRAWN"

GEO1_certi =aggregate(GEO$recode, by=list(Category=GEO$newdata.LCA_CASE_EMPLOYER_STATE),
                FUN=sum)
colnames(GEO1_certi)=c("State","H1B")
# GEO2 =aggregate(GEO$newdata.LCA_CASE_EMPLOYER_STATE,
#                 by=list(Category=GEO$newdata.LCA_CASE_EMPLOYER_STATE),
#                 FUN=NROW)
# GEO3 = data.frame("Category"=GEO1_certi[,1],"Certified"=GEO1_certi[,2],"Total"=GEO2[,2])
# GEO3 = GEO3[-1,]
# GEO3$prop = GEO3$Certified/GEO3$Total

H1B_heat = gvisGeoChart(GEO1_certi[-1,], locationvar='State', colorvar = 'H1B',
        options=list(title="Heated map of H1B visa obtainment",
                     titlevar="Title",region='US',projection="kavrayskiy-vii",numvar="H1B",
                       displayMode="regions", resolution="provinces",
                         colorAxis="{colors:['yellow','red']}",width=650, height=400))

T <- gvisTable(GEO1_certi[-1,], 
               options=list(width=270, height=400))

GT <- gvisMerge(H1B_heat,T, horizontal=TRUE) 
plot(GT)
```