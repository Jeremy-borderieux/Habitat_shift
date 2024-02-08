# you have to run this script once a year
# years 2022-2023 are already installed

#### downloading from the NFI website ####

remotes::install_github("Jeremy-borderieux/FrenchNFIfindeR")
library(FrenchNFIfindeR)

## get the whole NFI
get_NFI()

## harmonize the taxonomy to taxref V13 and includes trees in the floristic surveys 

#rtation with all cdref and overstory trees ####


library(data.table)
library(stringr)
library(lubridate)
library(sf)
library(ggplot2)

## inserer le ficher flore recent 202x de l'ifn


flore_new_nfi<-data.table(read.table(unz(file.path("NFI_data","raw_data","data_NFI.zip"), "FLORE.csv"),  sep = ";", header = T, stringsAsFactors = F))

flore_new_nfi[,X:=NULL]
flore_new_nfi[,CANOPY:="non"]
flore_new_nfi[,SOURCE:="Donnees_floristique"]

final_tax_ref_pm<-fread(file.path("taxo_info","TaxRef_v13_PM.csv"))

espar_cd_ref<-fread(file.path("taxo_info","espar-cdref13.csv"))
colnames(espar_cd_ref)<-c("espar","lib_espar","cd_ref","lib_cdref")
espar_cd_ref[,espar:=ifelse(nchar(espar)==1,paste0("0",espar),espar)]


flore_new_nfi[CAMPAGNE%in%c(2005,2006) & CD_REF%in% espar_cd_ref$cd_ref,CANOPY:="surement"]


arbres<-data.table(read.table(unz(file.path("NFI_data","raw_data","data_NFI.zip"), "ARBRE.csv"),  sep = ";", header = T, stringsAsFactors = F))
arbres_unique<-arbres[,.N,by=.(CAMPAGNE,IDP,ESPAR)]
arbres_unique<-merge(arbres_unique,espar_cd_ref,by.x="ESPAR",by.y="espar")
arbres_unique[,ABOND:=1]
arbres_unique[,CD_REF:=cd_ref]
arbres_unique[,CANOPY:="oui"]
arbres_unique[,SOURCE:="Donnees_arbres"]

arbres_unique<-arbres_unique[,c("CAMPAGNE","IDP","CD_REF","ABOND","CANOPY","SOURCE")]


plot_with_arbre<-arbres_unique[,unique(IDP)]
plot_without_arbre<-flore_new_nfi[!IDP %in%  unique(arbres_unique$IDP),unique(IDP)]


###merging arbre
flore_new_nfi<-rbind(flore_new_nfi,arbres_unique)

##merging cover when there is no arbre 
NFI_cover<-data.table(read.table(unz(file.path("NFI_data","raw_data","data_NFI.zip"), "COUVERT.csv"),  sep = ";", header = T, stringsAsFactors = F))
# NFI_cover<-data.table(read.table(unz(file.path("Data","Data_NFI_2005_2021","export_dataifn_2005_2021.zip"),"COUVERT.csv"),sep=";",header=T))
# NR_trees<-NFI_cover[strate=="NR",]
NR_trees<-NFI_cover
NR_trees[,CANOPY:=ifelse(STRATE=="NR","non","oui")]

NR_trees[,STRATE:=NULL]
NR_trees[,TCA:=NULL]
NR_trees[,TCL:=NULL]
NR_trees[,P1525:=NULL]
NR_trees[,P7ARES:=NULL]
NR_trees[,X:=NULL]


NR_trees<-merge(NR_trees,espar_cd_ref,by.x="ESPAR_C",by.y="espar")
NR_trees[,CD_REF:=cd_ref]
NR_trees[,ABOND:=1]



R_trees<-NR_trees[IDP %in% plot_without_arbre & CANOPY=="oui",]
R_trees[,SOURCE:= "Donnees_couvert_R_25m"]

R_trees<-R_trees[,c("CAMPAGNE","IDP","CD_REF","ABOND","CANOPY","SOURCE")]

flore_new_nfi<-rbind(flore_new_nfi,R_trees)

NR_trees<-NR_trees[CANOPY=="non",]
NR_trees[,SOURCE:= "Donnees_couvert_NR_15m"]
NR_trees<-NR_trees[,c("CAMPAGNE","IDP","CD_REF","ABOND","CANOPY","SOURCE")]

flore_new_nfi<-rbind(flore_new_nfi,NR_trees)


flore_new_nfi<-flore_new_nfi[order(IDP),,]


flore_new_nfi[,cd_ref_bis:=final_tax_ref_pm$cd_ref_bis[match(flore_new_nfi$CD_REF,final_tax_ref_pm$CD_NOM)]]
flore_new_nfi[,cd_taxsup_si_ssp:=final_tax_ref_pm$cd_taxsup_espece[match(flore_new_nfi$CD_REF,final_tax_ref_pm$CD_NOM)]]
flore_new_nfi[,cd_ref_final:=ifelse(is.na(cd_taxsup_si_ssp),cd_ref_bis,cd_taxsup_si_ssp)]

flore_new_nfi[,nom_final:=final_tax_ref_pm$LB_NOM[match(flore_new_nfi$cd_ref_final,final_tax_ref_pm$CD_NOM)]  ]

flore_new_nfi[,nom_final_2:=final_tax_ref_pm$LB_NOM[match(flore_new_nfi$CD_REF,final_tax_ref_pm$CD_NOM)]  ]


#flore_new_nfi[,tree:=ifelse(str_extract_all(nom_final,"^[:alpha:]+",T)%in%traitarbres$Genre,1,0)]

flore_new_nfi[,duplicated := duplicated(CD_REF),by=IDP ]
colnames(flore_new_nfi)[1:6]<-c("campagne","idp","cd_ref","abund","canopy","source")

write.table(flore_new_nfi,file.path("harmonized_flora","harmonized_NFI_survey_2005_2022.csv"),sep=";",row.names = T)
saveRDS(flore_new_nfi,file.path("harmonized_flora","harmonized_NFI_survey_2005_2022.RData"))

rm(list = ls(all.names = TRUE))
gc()


## removing duplicated trees 
flore_new_nfi<-flore_new_nfi[duplicated==FALSE,]

flore_new_nfi_export<-rbind(flore_new_nfi[tree==1 & duplicated==FALSE,],
                            flore_new_nfi[tree==0,])





