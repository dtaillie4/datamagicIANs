library(sf)
tract<-read_sf("tract_data_new.shp")
head(tract)
colnames(tract)

tract$num_doc
tract$ScoreHSGED <- ((tract$num_hs + tract$num_ged) * 25)
tract$ScoreAsPro <- ((tract$num_as + tract$num_pro) * 50)
tract$ScoreBach <- (tract$num_bac*75)
tract$ScoreGrad <- ((tract$num_mas + tract$num_doc) * 100)

tract$TractEduLevelScore<- (tract$ScoreHSGED+tract$ScoreAsPro+tract$ScoreBach+tract$ScoreGrad)/(tract$edctn_l)
tract$TractEduLevelScore
tract$TractEduLevelScore
