#### Geographical pairing and plot selection ####
library(foreach)
library(doParallel)


col_of_interest<-c("campagne","alti","canopy_cover","basal_area","Ntot","xl","yl")


NFI_plot_info_sf$idp<-as.numeric(NFI_plot_info_sf$idp)
NFI_plot_info_sf$xl<-st_coordinates(NFI_plot_info_sf)[,1]
NFI_plot_info_sf$yl<-st_coordinates(NFI_plot_info_sf)[,2]

clust_pairs<-makeCluster(3) # approx computation time with 3 cores = 6 minutes
registerDoParallel(clust_pairs)
pairs_ser<-foreach(ser_current=unique(NFI_plot_info$greco),.combine = rbind,.multicombine = T,.packages = c("data.table","foreach","sf"))%dopar%{
  
  # within each ecoregions we define the matrix of distance between each plots
  idp_in_ser<-NFI_plot_info[greco==ser_current,idp]
  print(ser_current)
  
  pair_distances<-get_distances(NFI_plot_info_sf[NFI_plot_info_sf$greco==ser_current,],NFI_plot_info_sf[NFI_plot_info_sf$greco==ser_current,]$period,"recent","past",5)
  pair_distances[,idp1:=as.numeric(idp1)]
  pair_distances[,idp2:=as.numeric(idp2)]
  
  # then we select pairs of plots optimally, we maximize the number of pairs with these contains:
  # the plots should be less than 1.9km apart, and have been surveyed 9 or 11 years appart 
  # the plots should not have an elevation difference of more than 100m
  
  pairs<-get_pair_optimal(NFI_plot_info_sf[,],pair_distances[idp1 %in%idp_in_ser & idp2 %in% idp_in_ser ,],distance_tresh=1.91,
                          differences_to_compute=col_of_interest,
                          differences_tresh_min = c(9,-100,rep(0,5)),
                          differences_tresh_max = c(11,100,rep(0,5)))
  pairs[,greco:=ser_current]
  pairs
}


stopCluster(clust_pairs)



pairs_ser[,pair_id_ser:=paste0(greco,"_",pair_id)]
#pairs_ser[,greco:=str_extract(ser,"[:alpha:]")] # large ecoregions GRECO
summary(pairs_ser)

pairs_ser[,.N,by=greco]
