#### Geographical pairing ####

## helper function to quickly get a data.table containing the distances between each pairs of plots (SF object) in a maximum distance threshold
## ifn_sf an sf object, the ID of the points should be named "idp
## name of the column of the factor used to create two classes of plots for pairing, (clust_level_1,clust_level_2)
get_distances<-function(ifn_sf,clust_factor,clust_level_1,clust_level_2,distance_tresh=6){
  
  coord_clust_1<-ifn_sf[clust_factor==clust_level_1,]
  coord_clust_2<-ifn_sf[clust_factor==clust_level_2,]
  coord_clust_1<-coord_clust_1[,"idp"]
  coord_clust_2<-coord_clust_2[,"idp"]
  
  ## the function st _distance return a matrix of distances, transformed into a data.table
  dist_mat<-st_distance(coord_clust_2,coord_clust_1,tolerance=5000)
  units(dist_mat)<-"km"
  colnames(dist_mat)<-coord_clust_1$idp
  rownames(dist_mat)<-coord_clust_2$idp
  
  all_pair<-expand.grid(coord_clust_2$idp,coord_clust_1$idp,stringsAsFactors = FALSE)
  
  all_pair<-data.table(all_pair)
  all_pair[,dist_pair:=as.numeric(unlist(dist_mat))]
  all_pair_2_res<-all_pair[dist_pair<distance_tresh,]
  
  names(all_pair_2_res)<-c("idp2","idp1","dist_pair")
  
  all_pair_2_res<-all_pair_2_res[order(dist_pair),]
  all_pair_2_res<-all_pair_2_res[,c(2,1,3)]
  
  return(all_pair_2_res)
}

delete_dt_row <- function(DT, del.idxs) { ## useful function to fasten get_pair_optimal()
  # delete rows instead of re-writing the data.table into memory
  keep.idxs <- setdiff(DT[, .I], del.idxs)
  cols = names(DT)
  DT.subset <- data.table(DT[[1]][keep.idxs])
  setnames(DT.subset, cols[1])
  for (col in cols[2:length(cols)]) {
    DT.subset[, (col) := DT[[col]][keep.idxs]]
    DT[, (col) := NULL]
  }
  return(DT.subset)
}


## this function uses the same SF object sf_ifn, as well as the distance data.table created by get_distances (dist table)
## the difference of the variable we want to compute can be specified with valid column names in differences_to_compute
## differences_tresh_min and differences_tresh_max are used to discards (before the final selection of pairs) potential pairs with a larger difference of one a the variable
## if set to 0, no differences checks are performed. For the following analysis, the differences cheched are elevation (less than 100m difference) and year of the survey (must be 10 or 9 years appart)
get_pair_optimal<-function(sf_ifn,dist_table,distance_tresh=5,differences_to_compute=NULL,
                           differences_tresh_min=if(is.null(differences_to_compute)) NULL else rep(0,length(differences_to_compute)),
                           differences_tresh_max=if(is.null(differences_to_compute)) NULL else rep(0,length(differences_to_compute)),
                           ident_is_num=T){
  
  dist_table<-dist_table[dist_pair<=distance_tresh,]# remove plots too far for each other:5km
  
  ## data preprocessing to get differences betwenn covariable
  dist_table[,pair_id:=1:nrow(dist_table)]
  subsest_idp1<-merge(dist_table[,c("idp1","pair_id")],as.data.table(sf_ifn),by.x="idp1",by.y="idp")
  subsest_idp2<-merge(dist_table[,c("idp2","pair_id")],as.data.table(sf_ifn),by.x="idp2",by.y="idp")
  subsest_idp1<-subsest_idp1[order(pair_id),]
  subsest_idp2<-subsest_idp2[order(pair_id),]
  
  ## check that the differences to compute and test are correctly provided
  if(length(differences_to_compute)!=length(differences_tresh_min | length(differences_to_compute)!=length(differences_tresh_max)))stop("Length of columns to compute differences is different to the number of difference tresholds")
  ## compute the differences
  for(col in differences_to_compute)dist_table[,paste0(col,"_dif")]<-subsest_idp1[[col]]-subsest_idp2[[col]]
  
  ## check for a differences criterion, for  this analysis: elevation and year of survey threshold only
  foreach(col = differences_to_compute ,trsh_min = differences_tresh_min,trsh_max=differences_tresh_max)%do%{
    new_col<-paste0(col,"_dif")
    if(trsh_min!=0)dist_table<-dist_table[abs(dist_table[[new_col]])>= trsh_min,]
    if(trsh_max!=0)dist_table<-dist_table[abs(dist_table[[new_col]])<= trsh_max,]
    
  }
  
  
  ## this empty data.frame will keep the selected pairs (pairs), and mimic the structure of dist_table
  kept_pair<-data.frame(matrix(NA,nrow = nrow(dist_table),ncol=ncol(dist_table)))
  
  colnames(kept_pair)<-colnames(dist_table)
  
  incr<-1
  while(nrow(dist_table)!=0){
    
    cat(paste("->",incr))
    
    # we use the function table to count the number od time each plot_id (called idp) are found, as it is also the number 
    # of neighbor it has
    nb_neig_idp1_dt<-as.data.table(table(dist_table$idp1))
    colnames(nb_neig_idp1_dt)<-c("idp1","nb_neig1")
    nb_neig_idp2_dt<-as.data.table(table(dist_table$idp2))
    colnames(nb_neig_idp2_dt)<-c("idp2","nb_neig2")
    
    # we match the number of neigboh with dist_table, that containt all the potential pairs
    dist_table[,nb_neig1:=nb_neig_idp1_dt[match(dist_table$idp1,idp1),2]]
    dist_table[,nb_neig2:=nb_neig_idp2_dt[match(dist_table$idp2,idp2),2]]
    
    ## this line check the minimum number of neighbor a plot in the pair has
    dist_table[,priority_low_neig:=pmin(nb_neig1,nb_neig2)]
    ## this line check the maximum number of neighbor a plot in the pair has
    dist_table[,priority_max_neig:=pmax(nb_neig1,nb_neig2)]
    
    # priority is given to the pair containing a plot with the lower minimal neighbor count
    ## then to the pair with the lower maximal count, then to the closest pair
    setorder(dist_table,priority_low_neig,priority_max_neig,dist_pair)
    
    # fast way to remove those columns now that the pairs have been prioritize
    dist_table[,nb_neig1:=NULL]
    dist_table[,nb_neig2:=NULL]
    dist_table[,priority_low_neig:=NULL]  
    dist_table[,priority_max_neig:=NULL]
    
    # the first row of dist_table is the selected one
    kept_pair[incr,]<-as.data.frame(dist_table[1,])
    
    incr<-incr+1
    kept_idp1<-dist_table[1,idp1]
    kept_idp2<-dist_table[1,idp2]
    
    ## we remove every potential pair with a plot that has just been selected
    dist_table<-delete_dt_row(dist_table,(1:nrow(dist_table))[dist_table[,idp1==kept_idp1]])
    dist_table<-delete_dt_row(dist_table,(1:nrow(dist_table))[dist_table[,idp2==kept_idp2]])
    
    
    
  }
  
  colnames(kept_pair)<-colnames(dist_table)
  
  kept_pair<-data.table(kept_pair)
  kept_pair<-kept_pair[!is.na(idp1),] ## remove the empty rows, kept_pair was too large by design
  kept_pair<-kept_pair[order(dist_pair),]
  kept_pair[,pair_id:=1:nrow(kept_pair)]
  
  return(kept_pair)
}


get_centroid_pairs<-function(sf_2rows){
res<-st_as_sf( st_centroid(st_cast(st_combine(sf_2rows),"LINESTRING")),crs=st_crs(2154))
res$pair_id<-sf_2rows$pair_id[1]
return(res)}

#### data transformation ####

##  function to create a [site_id,species] matrix of absence-presence from the flora survey
create_table_sp<-function(survey,id_names="idp"){
  
  table_survey<-table(survey[,get(id_names)],survey$species_name)
  table_survey<-as.data.frame.matrix(table_survey)
  
  di<-dim(table_survey)
  sp<-colnames(table_survey)
  id<-rownames(table_survey)
  
  table_survey<-as.matrix(table_survey)
  table_survey<-as.numeric(table_survey)
  table_survey<-matrix(table_survey,nrow=di[1],ncol =di[2] )
  table_survey<-as.data.frame(table_survey)
  
  rownames(table_survey)<-id
  colnames(table_survey)<-sp
  
  return(table_survey)
}
