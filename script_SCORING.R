library(sf)
tract<-read_sf("census_tract_cb.shp")
head(tract)
colnames(tract)

tract$num_doc
tract$ScoreHSGED <- ((tract$num_hs + tract$num_ged) * 25)
tract$ScoreAsPro <- ((tract$num_as + tract$num_pro) * 50)
tract$ScoreBach <- (tract$num_bac*75)
tract$ScoreGrad <- ((tract$num_mas + tract$num_doc) * 100)

tract$TractEduLevelScore<- (tract$ScoreHSGED+tract$ScoreAsPro+tract$ScoreBach+tract$ScoreGrad)/(tract$edctn_l)
tract$TractEduLevelScore
class(tract)
colnames(tract)

st_write(tract, "tract_cb_eduscore.shp")
#tract6<-read_sf("tract_cb_eduscore.shp")
#########
#Scoring Income
library(tidyverse)
library(scales)
tract
colnames(tract)
class(tract$income)
maxincome<-max(tract$income,na.rm=TRUE)
minincome<-min(tract$income,na.rm=TRUE)

tract$incomescore<-
  ifelse(tract$income>=53675,
         (rescale(tract$income, to=c(75,100), from=c(53657,250001))),
         rescale(tract$income, to=c(0,75),from=c(0,53656)))

tract$incomescoreRE <-
  ifelse(tract$income>=53675,
         (rescale(tract$income, to=c(50,100), from=c(53657,250001))),
         rescale(tract$income, to=c(0,50),from=c(0,53656)))

tract$incomescore<-
 ifelse(tract$income>=53675,
         (rescale(tract$income, to=c(75,100), from=c(53657,100000))),
        rescale(tract$income, to=c(0,75),from=c(0,53656)))

#### some plotting 
#### adding plotting with ed-scoring file KM already saved into data

tract_cb_eduscore <- read_sf('data/tract_cb_eduscore.shp')

ggplot(data = tract_cb_eduscore, aes(geometry=geometry, fill = TrctELS))+
  geom_sf(size = .01)+
  scale_fill_gradient2(low = "red", mid = "yellow1", high = "darkgreen", 
                       midpoint = 50, name = "Education Score")

#### re running the income scoring so i(DT) have the values 
tract <- read_sf("data/census_tract_cb.shp")

maxincome<-max(tract$income,na.rm=TRUE)
minincome<-min(tract$income,na.rm=TRUE)

tract$incomescore<-
  ifelse(tract$income>=53675,
         (rescale(tract$income, to=c(75,100), from=c(53657,250001))),
         rescale(tract$income, to=c(0,75),from=c(0,53656)))

st_write(tract, 'data/ScoringOutputs/tract_cb_incomescoring.shp')

ggplot(data = tract, aes(geometry=geometry, fill = incomescore))+
  geom_sf(size = .01)+
  scale_fill_gradient2(low = "red", mid = "yellow1", high = "darkgreen", 
                       midpoint = 50, name = "Income Score")
ggplot(data = tract, aes(geometry=geometry, fill = incomescoreRE))+
  geom_sf(size = .01)+
  scale_fill_gradient2(low = "red", mid = "yellow1", high = "darkgreen", 
                       midpoint = 50, name = "Income Score RE")
