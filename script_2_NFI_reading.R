library(FrenchNFIfindeR)
library(data.table)
library(stringr)
library(lubridate)
library(sf)
library(ggplot2)
library(broman)
#### get pairing functions ####
source(file.path("Functions.R"))
#### get the whole NFI #### 
get_NFI() # type 1

NFI_flora<-readRDS(file.path("harmonized_flora","harmonized_NFI_survey_2005_2022.RData"))

# line to remove duplicated species, run depending on your question and if you need trees, canopy trees, or understory trees etc...
NFI_flora<-NFI_flora[duplicated==FALSE,]

## script from article_themo_beta_part
colnames(NFI_plot_info)[1]<-"campagne"


#### Spatial data
# sylvoecoregion
SER_sf<-read_sf(file.path("spatial_Data"),layer="ser_l93")


#### loading climate data extracted from the digitalis v2b french model
NFI_climate<-readRDS(file.path("Data","Data_NFI_2005_2021","climatic_database_NFI_2021.RData"))
NFI_climate[,campagne:=NULL]
NFI_climate[,greco:=NULL]


##loading precise elevation data
precise_elevation<-fread(file.path("env_data","altitudes_Ifn_2005_2021.txt"))
colnames(precise_elevation)<-c("idp","alti")
precise_elevation[,idp:=as.numeric(idp)]

## we compute the canopy cover of a plot
canopy_cover<-NFI_cover[strate=="R",.(canopy_cover=sum(tcl),canopy_cover_abs=sum(tca)),by=idp]

#### management and basal area computation
NFI_plot_info[,gest_struct:=sfo]
# need library broman
NFI_plot_info[,tmp:=switchv(as.character(sver),"0",`NA`="0",`0`="0",`X`="0",`2`="1",`3`="4",`4`="2",`5`="3",`6`="1")]
NFI_plot_info$gest_struct[NFI_plot_info$campagne>2013]<-NFI_plot_info$tmp[NFI_plot_info$campagne>2013]
NFI_plot_info[,gest_struct:=switchv(as.character(gest_struct),`NA`=NA,`0`="debois",`1`="FR",`2`="FIR",`3`="TSF",`4`="T")]
NFI_plot_info[,gest_struct:=as.character(gest_struct)]
# Imputation aux arbres simplifi?s de la valeur moyenne des mesures des arbres de la m?me placette, essences, et classe de taille
NFI_tree[, dimess := cut(c13, breaks = c(0, 70.5, 117.5, 164.5, 1000), labels = c("PB", "BM", "GB", "TGB"), right = FALSE)]
NFI_tree[, htot := ifelse(is.na(htot), mean(htot, na.rm = TRUE), htot), by = c("idp", "espar", "dimess")]
NFI_tree[, ir5 := ifelse(is.na(ir5), mean(ir5, na.rm = TRUE), ir5), by = c("idp", "espar", "dimess")]

### we keep the living and standing trees only
NFI_tree<-NFI_tree[veget=="0",]

NFI_tree[,campagne_2 := floor(idp/100000)+2005]

### basal area tree level
NFI_tree[,gFinal:=c13*c13*w/(4*pi)]
NFI_tree[,gInitial:=pi/10000*(c13/(2*pi)-ir5/10)^2*w]

### basal area
NFI_dendro <- NFI_tree[,.(basal_area=sum(gFinal), Ntot=sum(w)), by = idp]


####   ####


NFI_plot_info[,ident:=as.character(idp)]
NFI_plot_info[,ser:=as.character(ser)]
NFI_plot_info[,greco:= substr(ser,1,1)]
# we start by merging various datasets with the data.table 'NFI_plot_info' containing all NFI plots ID (idp) and
# basic informations such as coordinates 
NFI_plot_info<-merge(NFI_plot_info,NFI_climate,all.x=T,by="idp")
NFI_plot_info<-merge(NFI_plot_info,NFI_dendro,all.x=T,by="idp")
NFI_plot_info<-merge(NFI_plot_info,canopy_cover,all.x=T,by="idp")
NFI_plot_info<-merge(NFI_plot_info,precise_elevation,all.x=T,by="idp")

NFI_plot_info[,elevation_blurred:=elevation]
NFI_plot_info[,elevation:=ifelse(is.na(alti),elevation,alti)]
NFI_plot_info[,alti:=NULL]

## the NFI_ecologie allows us to know the date when the floristic survey was performed
NFI_plot_info<-merge(NFI_plot_info,NFI_ecology,all.x=T,by="idp")
NFI_plot_info[,dateeco:=as.character(dateeco)]
NFI_plot_info[,dateeco:=ymd(dateeco)]
## we create a boolean variable: the survey was done during the vegetation season ?
NFI_plot_info[,survey_during_vege_season:=ifelse(month(dateeco)%in%c(4:9),1,0)]

####  Thermal, soil optimum data and habitat preferences

# sp_indicator_value<-fread(file.path("Data","climplant_names_trait_V1.2.csv"))
# colnames(sp_indicator_value)[colnames(sp_indicator_value)=="YearMeanMean"]<-"topt_climplant"
# 
# 
# sp_indicator_value[,niche_breadth:=YearMean95-YearMean05]
# sp_indicator_value[,niche_breadth_n:=(YearMean95-YearMean05)/topt_climplant]
# 
# 
# climplant_full<-fread(file.path("Data","MeanTempYearClimPlantV1_2.csv"))
# sp_tmp<-climplant_full$V1
# climplant_full<-data.table(t(climplant_full[,2:ncol(climplant_full)]))
# colnames(climplant_full)<-sp_tmp
# melt_climplant_full<-melt(climplant_full)
# colnames(melt_climplant_full)<-c("species_name","topt_climplant")
# rm(sp_tmp)
# 
# NFI_flora_traits<-NFI_flora[idp%in%NFI_plot_info$idp & !is.na(species_name),]
# 
# NFI_flora_traits<-merge(NFI_flora_traits,sp_indicator_value[,c("lb_nom_final","topt_climplant","YearMeanMedian","YearMean95","YearMean05","YearMaxMean",  
#                                                                "niche_breadth","niche_breadth_n",
#                                                                "Area","indFor_Freq","indFor_Chytry","N_ellenberg","R_ellenberg","L_Ellenberg" ,
#                                                                "vi_pH" ,"vi_CN", "azote","topt_picq" , "topt" , "pHopt" , "CNopt"  , "Nopt" , "STopt","Li")],by.x="species_name",by.y="lb_nom_final",all.x=T)
# 
# # aggregating the survey to have a Community Inferred Temperature at the plot level (community)
# cit_climplant<-NFI_flora_traits[tree!=1 ,## removal of the tree species 
#                                 .(cit_climplant=mean(topt_climplant,na.rm=T),
#                                   median_climplant=mean(YearMeanMedian,na.rm=T),
#                                   cit_tmax_climplant=mean(YearMaxMean,na.rm=T),
#                                   cit_climplant_05=mean(YearMean05,na.rm=T),
#                                   cit_climplant_95=mean(YearMean95,na.rm=T),
#                                   cit_ecoplant=mean(topt,na.rm=T),
#                                   cit_ecoplant_picq=mean(topt_picq,na.rm=T),
#                                   n_sp_climplant=sum(!is.na(topt_climplant)),
#                                   mean_area=mean(Area,na.rm=T),
#                                   mean_niche_breadth=mean(niche_breadth,na.rm=T),
#                                   mean_niche_breadth_n=mean(niche_breadth_n,na.rm=T),
#                                   freq_for_mean=mean(indFor_Freq,na.rm=T),
#                                   indfor_Chytry=mean(indFor_Chytry,na.rm=T),
#                                   mean_azote=mean(azote,na.rm=T),
#                                   mean_N=mean(Nopt,na.rm=T),
#                                   mean_R=mean(R_ellenberg,na.rm=T),
#                                   mean_pH=mean(pHopt,na.rm=T),
#                                   mean_CN=mean(vi_CN,na.rm=T),
#                                   mean_L=mean(Li,na.rm=T)),
#                                 by=idp]
# 
# 
# rm(NFI_flora_traits)
# 
# ## include the bio indicated data, survey scale
# NFI_plot_info<-merge(NFI_plot_info,cit_climplant,by="idp",all.x=T)
# head(NFI_flora)
# 

## set the time period
NFI_plot_info[,cycle:=ifelse(campagne%in%c(2005,2006,2007,2008,2009),"first",ifelse(campagne%in%c(2010,2011,2012,2013,2014),"first_half","second"))]
NFI_plot_info[,period:=ifelse(campagne%in%c(2005,2006,2007,2008,2009,2010,2011,2012),"past",ifelse(campagne%in%c(2012,2013,2014),NA,"recent"))]

NFI_flora
NFI_flora[,duplicated := duplicated(cd_ref),by=idp]
count_sp<-NFI_flora[duplicated==FALSE,.N,by=idp]


## here insert selections
NFI_plot_info<-NFI_plot_info[idp%in% count_sp[N>=5,idp] ,]# some plots lacks any species with a thermal optimum


#NFI_plot_info<-NFI_plot_info[!is.na(cit_climplant),]# some plots lacks any species with a thermal optimum
NFI_plot_info<-NFI_plot_info[gest_struct!="debois",]## the nfi classify deforested plots, we remove them
#NFI_plot_info<-NFI_plot_info[n_sp_climplant>=5,]
NFI_plot_info_sf<-st_as_sf(NFI_plot_info,coords=c("xl","yl"),crs=st_crs(2154))


library(raster)
topo<-raster("S:/BD_SIG/topo/bdalti50/mnt50_fill_L93.tif")
NFI_plot_info$alti_mnt<-extract(topo,NFI_plot_info_sf)
rm(topo)

NFI_plot_info[,alti:=ifelse(is.na(alti),alti_mnt,alti)]

NFI_plot_info_sf<-st_as_sf(NFI_plot_info,coords=c("xl","yl"),crs=st_crs(2154))

