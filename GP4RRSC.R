#################### Importation des librairies a utiliser  ##########
library(dtwclust)
library(xts)#resutl1<- xts(dataset)
library(dtw)     
library(dplyr)
library(imputeTS)
library(ggplot2)
library(labeling)
library(factoextra)
library(hash)
#library(arules)

#################### Reorganisation des differents elements dans leurs clusters correspondants  #############
constructClusters<-function(dataset, number_cluster){
  clustList<-list()
  dataset_2 <- as.ts(dataset)
  ##################################  Clusterisation  la fonction dtwclust::tscluster() ##################
  cat(nrow(dataset_2), "\n")
  pc_dtw_result <- tsclust(dataset_2, number_cluster, type = "partitional", seed = 8L,
                           distance = "dtw_basic", centroid = "dba",
                           norm = "L2", window.size = 20L)
  
  
  #################### Recuperation des differents clusters  #############
  for (i in 1:3) {
    clusters<- pc_dtw_result@cluster
  }
  
  # clustList contiendra les  listes de differents clusters 
  for (i in 1: pc_dtw_result@k) {
    
    clustList[[i]] <-which(clusters==i)
    
  }
  
  return(clustList)
}

#################### recuperation des Datasets Segmentes  ###########################
constructDonnee_Segmentees<-function(dataset_3, cl) {
  Donnee_Segmente<-list()
  dataset_3 <- na.omit(dataset)
  for (j in 1: length(cl) ) {
    Donnee_Segmente[[j]] <- dataset_3[cl[[j]], ]
  }
  return(Donnee_Segmente )
}

########################### Extraction des motifs graduels saisonsiers croissants dans chaque clustlist[[i]] ##############

#################### ----- IMPLEMENT GRAPGT METHOD ----- ####################
####### FUNCTION TRANSFORM DATA INTO SPECIFIC FORMAT THAT ALGORITHM WE USE ####### 
transformDataSet <- function(data){
  data_mat <- as.matrix(data)
  vec_date <- vector()
  vec_product <- vector()
  vec_quantity <- vector()
  #vec_date <- data_mat[1,num_date] 
  #vec_product <- data_mat[1,num_product]
  #vec_quantity <- data_mat[1,num_quantity]
  
  # Recupere la liste des dates
  cat("----- Extract list of date------ \n")
  vec_date <- unique(data_mat[,num_date])
  
  # Recupere la liste des produits
  cat("----- Extract list of products------ \n")
  vec_product <-unique(data_mat[,num_product])
  
  # Sort vector product
  mat <-matrix(0, nrow = length(vec_date), ncol = (length(vec_product) +1))
  
  cat("----- End of sort vector product ------ \n")
  lenvec <- length(vec_date)
  
  for (i in 1:length(vec_date)) {
    mat[i,1] <- vec_date[i]
    #cat("ITERATION ", i, "/", lenvec, "\n")
    d <- data_mat[data_mat[,num_date]==vec_date[i],]
	if(length(d) > ncol(data)){
		for (j in 1:length(vec_product)) {
		  mat[i,j+1] <- sum(as.numeric(d[d[,num_product] == vec_product[j], num_quantity]))
		}
	}
  }
  
  lst <- list()
  lst[[1]] <- mat
  lst[[2]] <- vec_product
  return(lst)
}
transformDataSet1 <- function(data, num_date, num_product, num_quantity){
  data_mat <- as.matrix(data)
  vec_date <- vector()
  vec_product <- vector()
  vec_quantity <- vector()
  vec_date <- data_mat[1,num_date] 
  vec_product <- data_mat[1,num_product]
  vec_quantity <- data_mat[1,num_quantity]
  
  # Recupere la liste des dates
  cat("----- Extract list of date------ \n")
  vec_date <- unique(data_mat[,num_date])
  
  
  # Recupere la liste des produits
  cat("----- Extract list of products------ \n")
  vec_product <-unique(data_mat[,num_product])
  
  # Sort vector product
  mat <-matrix(0, nrow = length(vec_date), ncol = (length(vec_product) +1))
  
  cat("----- End of sort vector product ------ \n")
  for (i in seq_along(vec_date)) {
  cat(i, "\n")
    mat[i,1] <- vec_date[i]
    data_mat_reduce <- data_mat[data_mat[,num_date]==vec_date[i],]
    
    if(is.null(ncol(data_mat_reduce))){
      #Transformation en matrice de ligne
      vec_aux <- as.matrix(data_mat[which(data_mat[,num_date]== vec_date[i]), ])
      data_mat_reduce <- matrix("",nrow = 1, ncol = nrow(vec_aux))
      for (k in 1:nrow(vec_aux))
        data_mat_reduce[1,k] <- vec_aux[k,1]  
    }
    
    for (j in seq_along(vec_product)) {
      data_mat_reduce2 <- data_mat_reduce[data_mat_reduce[,num_product]==vec_product[j],]
      if(is.null(nrow(data_mat_reduce2))){
        vec_aux <- as.matrix(data_mat_reduce[which(data_mat_reduce[,num_product] == vec_product[j]), ])
        data_mat_reduce2 <- matrix("", nrow = 1, ncol = nrow(vec_aux))
        for (k in 1:nrow(vec_aux))
          data_mat_reduce2[1,k] <- vec_aux[k,1]
      }
      
      mat[i,j+1] <- sum(as.numeric(data_mat_reduce2[,num_quantity]))
    } 
  }
  lst <- list()
  lst[[1]] <- mat
  lst[[2]] <- vec_product
  return(lst)
}

###### FUNCTION TO COMPUTE RELEVANCE FOR EACH USER #########
compute_Relevance <- function(data){
  data_mat <- as.matrix(data)
  #vec_date <- vector()
  vec_product <- vector()
  vec_quantity <- vector()
  vec_user <- vector()
  
  #vec_date <- data_mat[1,num_date] 
  #vec_product <- data_mat[1,num_product]
  #vec_quantity <- data_mat[1,num_quantity]
  #vec_user <- data_mat[1,num_user]
  
  # Récupère la liste des utilisateurs
  cat("----- Extract list of user------ \n")
  vec_user <-unique(data_mat[,num_user])
  
  
  # Recupere la liste des produits
  cat("----- Extract list of products------ \n")
  vec_product <-unique(data_mat[,num_product])
  
  
  # Init resulting matrix
  mat <-matrix(0, nrow = length(vec_user), ncol = (length(vec_product) +1))
  i <- 1
  for (eltUser in vec_user) {
    mat[i,1] <- eltUser
    data_mat_reduce <- data_mat[data_mat[,num_user]==eltUser,]
    
    if(is.null(ncol(data_mat_reduce))){
      #Transformation en matrice de ligne
      vec_aux <- as.matrix(data_mat[which(data_mat[,num_user]== eltUser), ])
      data_mat_reduce <- matrix("",nrow = 1, ncol = nrow(vec_aux))
      k <- 1
	  for (eltAux in vec_aux){ 
        data_mat_reduce[1,k] <- eltAux[1]  
		k <- k + 1
	  }
    }
    j <- 1
    for (eltProd in vec_product) {
      data_mat_reduce2 <- data_mat_reduce[data_mat_reduce[,num_product]==eltProd,]
      if(is.null(nrow(data_mat_reduce2))){
        vec_aux <- as.matrix(data_mat_reduce[which(data_mat_reduce[,num_product] == eltProd), ])
        data_mat_reduce2 <- matrix("", nrow = 1, ncol = nrow(vec_aux))
        k <- 1
		for (eltAux in vec_aux){
          data_mat_reduce2[1,k] <- eltAux[1]
		  k <- k + 1
		}
      }
      
      mat[i,j+1] <- sum(as.numeric(data_mat_reduce2[,num_quantity]))
	  j <- j + 1
    } 
	i <- i + 1
  }
  
  lst <- list()
  lst[[1]] <- mat
  lst[[2]] <- vec_product
  return(lst)
}

####### FUNCTION GRAPE ###########
grape <- function(minSup, minLength, data_mat, output, k1, k2, type, separateur, isConsecutive, sequen){
  ## import library Java
  library("rJava")
  outputInputTemp <- "FileForAprioriTID.txt"
  
  ## ------ Time ------as
  t1 <- Sys.time()
  
  ## ------ The beginning of the NumVersCat procedure -------
  data_diff <- diff(data_mat)
  
  ## -- Util pour APRIORI
  tab_vec <- array()
  
  ## Transform numerical data matrix to categorical data matrix containing  "+", "-" and "o" items
  data_cat <- matrix(0, nrow = nrow(data_diff), ncol = ncol(data_diff))
  for(i in 1:ncol(data_diff)){
    for(j in 1 : nrow(data_diff)){
      if (data_diff[j,i] > 0){
        data_cat[j,i] <- paste0(paste0("X",i),"=+") #Xi=+
      }else if(data_diff[j,i] < 0){
        data_cat[j,i] <- paste0(paste0("X",i),"=-") #Xi=-
      }else{
        data_cat[j,i] <- paste0(paste0("X",i),"=o") #Xi=o
      }
    }
  }
  
  # ----------------------------------
  for (i in 1:nrow(data_cat)) {
    for(j in 1:ncol(data_cat)){
      if (j == 1) {tab_vec[i] <- data_cat[i,j]}else{tab_vec[i] <- paste(tab_vec[i], data_cat[i,j])}
    }
  }
  # ----------------------------------
  
  # -- Ecrire dans un fichier tempon pour traitement par AprioriTID
  write(tab_vec, file = outputInputTemp, sep =" ")
  
  ### ----- D?but Insertion du code java -----
  # -- initialisation of the JVM
  .jinit()
  
  # -- Add .jar file to the class path
  .jaddClassPath("AprioriTID.jar")
  
  # -- instatiation of the Class
  obj <- .jnew("AlgoAprioriTID")
  
  cat("----- BEFORE APRIORI ----\n")
  # -- Call function directly with parameters
  .jcall(obj, "V","runAlgorithmModifierNotTID", outputInputTemp, output, minSup, minLength, sequen)
  #.jcall(obj, "V","runAlgorithmModifier", outputInputTemp, output, minSup, minLength)
  cat("----- AFTER APRIORI ----\n")
  t2 <- Sys.time()
  t2 - t1
  cat("\n")
}

grape_jerry <- function(minSup, minLength, data_mat, output, separateur){
###data <- read.csv(argv[3], sep =" ")
## ------ The beginning of the NumVersCat procedure -------
## Transform dataframe to matrix 
data_diff <- diff(as.matrix(data_mat))
decrease_pos <- which(data_diff < 0)
increase_pos <- which(data_diff > 0)
cons_pos <- which(data_diff == 0)
## Transform numerical data matrix to categorical data matrix containing  "+", "-" and "o" items
data_diff[increase_pos] = "+"
data_diff[decrease_pos] = "-"
data_diff[cons_pos] = "o"
## Transform categorical data matrix to dataframe
data_diff.data <- data.frame(data_diff)
## ----- The end of the NumVersCat procedure ------
## search first for frequent gradual itemsets with respect to the minSupp
data_diff.trans.itemsets <- apriori(data_diff.data, parameter=list(target="frequent",support=minSup,minlen=minLength,maxlen=ncol(data_diff.data),maxtime=0))
## Retrieve gradual itemsets containing constants items "o"
data_diff.trans.itemsets.subset <- subset(data_diff.trans.itemsets, subset = items %pin% "=o")
## Retrieve gradual itemsets non containing constants items "o"
data_diff.trans.itemsets.coEvolution <- data_diff.trans.itemsets[match(data_diff.trans.itemsets,data_diff.trans.itemsets.subset, nomatch = 0)==0]
## Write the result file containing patterns in output csv file 
## search for closed  gradual itemsets 
data_diff.trans.itemsets.coEvolution.closed <- data_diff.trans.itemsets.coEvolution[is.closed(data_diff.trans.itemsets.coEvolution)]
## sort the patterns with respect to their support 
data_diff.trans.itemsets.coEvolution.closed.sort <- sort(data_diff.trans.itemsets.coEvolution.closed) 
## Write  the frequent closed gradual patterns to the output csv file
write(data_diff.trans.itemsets.coEvolution.closed.sort, file = output, sep =separateur, col.names = NA)

}
####### FUNCTION EEP ########
EEP<-function(data_segmentation) {
  
  #################### ----- TRANSFORM DATA ------ ################
  
  matt <- transformDataSet(data_segmentation)
  mat <- matt[[1]]
  minDisc <- 0
  
  #################### ----- EXTRACT SEQUENCE AUTOMATICALY ----- #################
  vec_seq <- vector()
  pos <- 0
  sepSeq <- as.character("-")
  if (length(unlist(strsplit(mat[1,1],split = "-"))) == 3) {
    vec_seq[1] <- unlist(strsplit(mat[1,1],split = "-"))[1]
    sepSeq <- as.character("-")
    pos <- 1
  }else{
    vec_seq[1] <- unlist(strsplit(mat[1,1],split = "/"))[3]
    sepSeq <- as.character("/")
    pos <- 3
  }
  cpt <- 2
  for (i in 2:nrow(mat)) {
    if (is.na(match(unlist(strsplit(mat[i,1],split = sepSeq))[pos], vec_seq))) {
      vec_seq[cpt] <- unlist(strsplit(mat[i,1],split = sepSeq))[pos]
      cpt <- cpt + 1
    }
  }
  
  #################### ----- SEASONAL THRESHOLD         ----- ####################
  if(minDiscSeason == 0){
    minDisc <- (length(vec_seq) * 50)/100
  }else if(minDiscSeason == 1){
    minDisc <- (length(vec_seq) * 75)/100
  }else if(minDiscSeason == 3){
	minDisc <- (length(vec_seq) * 25)/100
  }else if(minDiscSeason == 4){
	minDisc <- (length(vec_seq) * 10)/100
  }else{
    minDisc <- length(vec_seq)
  }
  #################### ----- TAKE ONLY SPECIFIC SESSION ----- ####################
  bool <- TRUE
  #Modification pour la prise g?n?rale des r?sultats
  begin_session <- "01-01"
  if (bin > 12){
    end_session <- "12-01"
  }else{
    end_session <- paste(as.numeric(bin)+1, sep = "-", "01")
  }
  
  vec <- vector()
  vec_colname <- matt[[2]]
  vec_colname
  vec_cpt <- 1
  while (bool == TRUE) {
    vec_final <- vector()
    vec_semi_final <- vector()
    cpt <- 1
    cdt <- 1
    while (cdt <= length(vec_seq)) {
      debut_session <-paste(vec_seq[cdt], sep = "-", begin_session) # season is month + day. We add year
      cat("#####################################################################", debut_session, "\n")
      fin_session <- paste(vec_seq[cdt], sep = "-",end_session) # season = year-month-day
      cat("#####################################################################", fin_session ,"\n")
      data <- matrix(0, nrow = nrow(mat), ncol = (ncol(mat) -1))
      mat1 <- mat[as.numeric(as.Date(mat[,1])) < as.numeric(as.Date(fin_session)), ]
      
      if(length(mat1) > ncol(mat)){
        #---------------------------------------------
        aide <- 0
        chaine <- vector()
        for (i in 1:nrow(mat1)) {
          if(as.numeric(as.Date(mat1[i,1])) >= as.numeric(as.Date(debut_session))){
            aide <- aide + 1
            for (j in 1:ncol(mat1)) {
              if(j == 1){
                chaine[aide] <- mat1[i,j]
              }else{
                chaine[aide]  <- paste(chaine[aide],sep = ";", mat1[i,j])
              }
            }
          }
        }
        data_session <- matrix(0, nrow = length(chaine), ncol = ncol(mat1))
        if(length(chaine != 0)){
          for (i in 1:length(chaine)) {
            data_session[i,] <- unlist(strsplit(chaine[i], split = ";"))
          }
        }
        #---------------------------------------------
        #data_session <- mat1[as.numeric(as.Date(mat1[,num_date])) >= as.numeric(as.Date(debut_session)),]
        # Il faut contrôler le cas où rien n'est retourné
        if(aide > 2){
		  data <- matrix(0, nrow = nrow(data_session), ncol = (ncol(data_session) -1))
		  for (i in 1:nrow(data_session)) {
			for (j in 2:ncol(data_session)) {
			  data[i,(j-1)] <-as.numeric(data_session[i,j]) 
			}
		  }
		  data_session <- data
		  #View(data_session)
		  #################### ----- CALL METHOD ----- ####################
		  grape(minSup, minLength, data_session, output, k1, k2, type, separateur, 1, as.numeric(vec_seq[cdt]))
		  #grape_jerry(minSup, minLength, data_session, output, separateur)
		  #################### ----- INSERT INTO MATRIX
		  data <- read.csv(output, sep = " ")
          if(!is.null(data)){
			  data <- as.matrix(data)
			  if(nrow(data) > 0){
				for (i in 1:nrow(data)) {
				  #vec_final[cpt] <- paste(unlist(strsplit(unlist(strsplit(data[i,2],split = "}"))[1], split = "\\{"))[2], sep =" ", as.numeric(data[i,3]))
				  vec_final[cpt] <- paste(data[i,2], sep =" ", as.numeric(data[i,3]))
				  vec_semi_final[cpt] <- data[i,2]
				  cpt <- cpt + 1
				}
			  }
		  }
        }
      }
      cdt <- cdt + 1
    }
    
    ### On cherche ici ? retourner les items qui appartiennent ? toutes les s?quences 
    ### ou ? un seuil d?finit par l'utilisateur
    library(plyr)
    matrice_fin <- matrix("",nrow = length(vec_final), ncol = 2)
    matrice <- as.matrix(count(as.data.frame(vec_semi_final))) # compute the frequence of each gradual pattern
    
    vec_freq <- vector()
    vec_freq_count <- vector()
    len <- 0
    #Suppression des non fr?quents
    if(length(vec_semi_final) > 0){
      for (p in 1:nrow(matrice)) {
        if(as.numeric(matrice[p,2]) >= minDisc){
          #if(as.numeric(matrice[p,2]) >= (length(vec_seq)/2)){
          vec_freq[len] <- matrice[p,1]
          vec_freq_count[len] <- as.numeric(matrice[p,2])
          len <- len + 1 
        }  
      }
    }
    
    #Calcul du support cummul? (pas encore clair mais nous devons d'abord supprimer
    # les motifs non pertinents c'est-?-dire ceux dont la fr?quence est < freqSeuil)
    if(len > 0){
      for (i in 1:length(vec_final)) {
        matrice_fin[i,1] <- unlist(strsplit(vec_final[i],split = " "))[1]
        matrice_fin[i,2] <- as.numeric(unlist(strsplit(vec_final[i],split = " "))[2])
      }
      
      vec_positive <- vector()
      supp_positive <- vector() 
      compteur <- 1
      
      if(length(vec_freq)){
        for (i in 1:length(vec_freq)) { # SuppCum(m) = nbocc * sum(supp(m))/nbSequence
          s <- 0
          s <- (sum(as.numeric(matrice_fin[matrice_fin[,1] == vec_freq[i],2])) * vec_freq_count[i])/length(vec_seq)
          
          # BEGIN
          spl <- unlist(strsplit(vec_freq[i],split = ","))
          spl <- spl[!spl %in% NA]
          #cat(spl[length(spl)], "\n")
          if(length(spl) > 0){
            for (pp in 1:length(spl)) {
				if(length(unlist(strsplit(spl[pp], split = "="))) >= 2){
				    if(unlist(strsplit(spl[pp], split = "="))[1] %in% vec_positive){
					 	if(unlist(strsplit(spl[pp], split = "="))[2] == "+"){
						  supp_positive[compteur] <- supp_positive[compteur] + s 
						}else{
						  supp_positive[compteur] <- supp_positive[compteur] - s
						}
				    }else{
						if(unlist(strsplit(spl[pp], split = "="))[2] == "+"){
						  supp_positive[compteur] <- (-1) * s
						  vec_positive[compteur] <- unlist(strsplit(spl[pp], split = "="))[1]
						}else{
						  supp_positive[compteur] <- s
						  vec_positive[compteur] <- unlist(strsplit(spl[pp], split = "="))[1]
						}
				    }
				    compteur = compteur + 1
				}else{
					#cat(spl[length(spl)], "\n")
				}
            }
          }
          # END
        }
      }
      
      # extract only positive item
      position_pos <- which(supp_positive >= 0)
      vec_positive_unique <- vec_positive[position_pos]
      supp_positive_unique <- supp_positive[position_pos]
      # END
      for (i in 1:length(vec_positive_unique)) {
        vec[vec_cpt] <- paste(paste(paste(begin_session, sep = "#", end_session), sep = " ", vec_positive_unique[i]), sep = " ", supp_positive_unique[i])
        vec_cpt <- vec_cpt + 1
      }
    }
    
    begin_session <- end_session 
    val <- unlist(strsplit(end_session, split = "-"))
    if((as.numeric(val[1]) + bin) >= 12)
      end_session <- "12-31"
    else
      end_session <- paste((as.numeric(val[1]) + bin),sep = "-", val[2])
    
    if(begin_session == "12-31")
      bool <- FALSE
  }
  cat("\n", " CONVERT HERE \n")
  #Convert in use specific column
  for (p in 1:(length(vec_colname)-1)) {
    vec <- gsub(paste0("X",paste0(p, " ")), paste0(vec_colname[p+1], " "), vec)
  }
  
  return(vec)
}

############################### Return User per cluster ################################
UserPerCluster <- function(data_clust){
  return(unique(data_clust[,2]))
}

############################### Extract item for recommandation (all cluster) #################
ExtractEmergentsPatterns<- function(SegmentedData){
  dico <- hash()
  season <- ExtractSeason(bin)
  vec_items <- vector()
  vec_fre <- vector()
  cpt <- 1
  for (j in 1: length(SegmentedData)) {
    SGP <- EEP(SegmentedData[[j]])
    Users <- UserPerCluster(SegmentedData[[j]])
    cpt <- 1
    ##Modif
    for (i in 1:length(season)) {#parcour de la saison
      Items <- captureItems(season[i], SGP)
      if(length(Items) != 0){
        for (p in 1: length(Items)) {
          if(length(which(vec_items %in% as.character(unlist(strsplit(Items[p],split= " "))[2]))) == 0){
            vec_items[cpt] <- as.character(unlist(strsplit(Items[p],split= " "))[2])
            vec_fre[cpt] <- as.numeric(unlist(strsplit(Items[p],split= " "))[3])
            cpt <- cpt + 1
          }
        }
        
        for (k in 1:length(Users)) {
          vec_ic <- compute_cumul_intensity(Users[k], vec_items, vec_fre)
          dico[paste(Users[k], sep = "$$", season[i])] <- decroissantOrder(vec_items, vec_ic)
          vec_ic <- vector()
        }
      }
    }
  }
  return(dico)
}

###############################  La fonction d'assignation d'un utilisateur a un groupe  ######################
get_user_Id_Group <- function(user_id ){
  return(clusters[user_id])
}


###############################  La fonction qui détermine automatiquement une saison  ######################
determineSeasonAutommatically <- function(d, bin){
  saison <-"01-01#01-01"
  month <- as.numeric(unlist(strsplit(d, split = "-"))[2])
  month_fin <- bin
  while (month > month_fin) {
    month_fin <- month_fin +bin
    
  }
  if(bin == month_fin) {
    saison <- paste("01-01", sep = "#",paste(bin+1, sep = "-","01"))
  }
  else
    if(month == month_fin && month >= 12){
      saison <- paste(paste(12-(bin-1), sep = "-", "01"), sep = "#","12-31")
    }else if(month == month_fin){
      if((month_fin+bin) >= 12){
        saison <- paste(paste(month_fin, sep = "_", "01"), sep = "#", "12-31")
      }else{
        saison <- paste(paste(month_fin, sep = "_", "01"), sep = "#",paste(month_fin+bin, sep="-", "01"))
      }
      
    }else{
      if((month_fin-bin) <= 0){
        saison <- paste("01-01", sep = "#", paste(month_fin, sep = "-", "01"))
      }else{
        saison <- paste(paste(month_fin-bin, sep = "-", "01"), sep = "#", paste(month_fin, sep = "-", "01"))
      }
      
    }
  
}

###############################  La fonction qui capture les items de la saison  ######################
captureItems <- function(Season, S_G_P){
  
  return(unique(S_G_P[which(startsWith(S_G_P,Season))]))
  
}

###############################  La fonction qui calcul pour chaque item les itensetes cumules d'un utilisateur   ######################
compute_cumul_intensity <- function(user,items, freq){
  IC_list <- vector()
  dataset_cible1 <- dataset[dataset[,2]== user,]
  for (k in 1: length(items)) {
    dataset_cible2 <- dataset_cible1[dataset_cible1[,num_product]== items[k],]
    IC <- 0
    if(nrow(dataset_cible2) != 0){
      weight <- sum(as.numeric(dataset_cible2[,num_quantity]))
      diff_dates <- diff(as.numeric(as.Date(dataset_cible2[,num_date])))
      IC <-((weight)/sum(diff_dates)) + freq[k]
    }
    if(is.na(IC) || is.nan(IC) || is.infinite(IC)){
      IC <- 0
    }
    IC_list[k] <- IC
  }
  return( IC_list)
}

###############################  reoganisation dans l'ordre descroissant   ######################
decroissantOrder <- function( vec_items, vec_ic){
  if(length(vec_ic) > 1){
  #cat(vec_ic,"\n")
    for (i in 2: length(vec_ic)) {
      cpt<- i-1
      bool <-0
      while(cpt>=1 && bool== 0){
        
        if(as.numeric(vec_ic[cpt]) < as.numeric(vec_ic[cpt +1])){
        #if(as.numeric(sum(c(vec_ic[cpt],0), na.rm=TRUE)) < as.numeric(sum(c(vec_ic[cpt+1],0), na.rm=TRUE))){
          aux <-vec_ic[cpt]
          vec_ic[cpt]<- vec_ic[cpt+1]
          vec_ic[cpt+1]<- aux
          aux <-vec_items[cpt]
          vec_items[cpt]<- vec_items[cpt+1]
          vec_items[cpt+1]<- aux
        }else{
          bool <-1
        }
        cpt <-cpt- 1
      }
      
    }
  }
  return(vec_items)
}


############################### Train function ############################
TrainSet <- function(dataset_2){
  SegmentedData <-list()
  
  if(isClust == 1){
    cl<-constructClusters(dataset_2, number_cluster)
    SegmentedData <- constructDonnee_Segmentees(dataset_2, cl)
  }else{
    SegmentedData[[1]] <- dataset_2
  }
  
  return(ExtractEmergentsPatterns(SegmentedData))
}

############################### Extract season ####################
ExtractSeason <- function(bin){
  vec <- vector()
  vec[1] <- paste("01-01", sep = "#", paste(bin+1,sep = "-", "01"))
  cpt <- 2
  bool <- 0
  aux <- bin+1
  while (bool == 0) {
    if(as.numeric(aux+bin) > as.numeric(12)){
      bool <- 1
    }else{
      vec[cpt] <- paste(paste(aux,sep = "-", "01"), sep = "#", paste(aux+bin,sep = "-", "01"))
      aux <- aux + bin
      cpt <- cpt + 1
    }
  }
  vec[cpt] <- paste(paste(aux,sep = "-", "01"), sep = "#", "12-31")
  
  return(vec)
}


############################### Evaluate function ############
evaluate <- function(annee){
  dictionnaire <- hash()
  vec_saison <-vector()
  vec_Users <- vector()
  vec_items <- vector()
  dataset_year <- dataset[which(startsWith(as.character(dataset[,num_date]),as.character(annee))),]
  
  #cat("\n", " HERE HERE HERE : ", annee, " ", nrow(dataset_year),  "\n")
  vec_saison <- ExtractSeason(bin)
  for (i in 1:length(vec_saison)) {
    dataset_year_Season <- dataset_year[which(as.numeric(as.Date(dataset_year[,num_date]))>=as.numeric(as.Date(paste(annee,sep = "-",unlist(strsplit(vec_saison[i], split = "#"))[1])))),]
    dataset_year_Season <- dataset_year_Season[which(as.numeric(as.Date(dataset_year_Season[,num_date]))< as.numeric(as.Date(paste(annee,sep = "-",unlist(strsplit(vec_saison[i], split = "#"))[2])))),]
    #vec_Users <- unique(dataset_year_Season[,num_user])
    #### Compute relevance 
    relevance <- compute_Relevance(dataset_year_Season)
    produits <- relevance[[2]]
    rel <- relevance[[1]]
	cat(nrow(rel), " ", ncol(rel), " ", length(rel), "\n")
	#if(length(rel) > (length(produits)+1)){	
		for (j in 1:nrow(rel)) {
		  dictionnaire[paste(rel[j,1], sep = "$$", vec_saison[i])] <- decroissantOrder(produits, rel[j,2:ncol(rel)])
		}
	#}
  }
  lst <- list()
  lst[[1]] <- dictionnaire
  lst[[2]] <- rel
  lst[[3]] <- produits
  return(lst)
}

############################################### matching #############
isMatch <- function(dico, dico_attendu, user_cible){
  seasons <- ExtractSeason(bin)
  tableau <- vector()
  tab <- vector()
  for (i in  1: length(seasons)) {
    if(!is.null(dico[[as.character(paste(user_cible,sep = "$$",seasons[i]))]])){
      tableau <-paste(dico[[as.character(paste(user_cible,sep = "$$",seasons[i]))]])
      tab <- match(tableau,dico_attendu[[as.character(paste(user_cible,sep = "$$",seasons[i]))]])
    }
  }
  return(tab)
  #return(tableau[!tableau %in% NA])
}

############################### global evaluation method ##########
globalEval <- function(dico_obtenu, list_result, users, top_N){
  #Add NDCG
  DCG <- 0
  IDCG <- 0
  rele <- vector()
  irele<- vector()
  tab_NDCG <- vector() 
  
  aux <- vector()
  tab_precision <- vector()
  tab_rappel <- vector()
  tab <- list()
  maxval <- 0
  val <- 0
  cpt <- 1
  dico_attendu <- list_result[[1]]
  mat_rel <- list_result[[2]]
  saisons <- ExtractSeason(bin)
  for (p  in 1: length(saisons)) {
    for (q in 1: length(users)) {
      if(!is.null(dico_obtenu[[as.character(paste(users[q],sep = "$$",saisons[p]))]])) {
        obtenu <- dico_obtenu[[as.character(paste(users[q],sep = "$$",saisons[p]))]]
        attendu <- dico_attendu[[as.character(paste(users[q],sep = "$$",saisons[p]))]]
        if(length(mat_rel) > (length(list_result[[3]])+1)){
          rel_user <- mat_rel[mat_rel[,1] == users[q], 2:ncol(mat_rel)]
          if(length(obtenu) != 0 && length(attendu) != 0){
            if(top_N > length(attendu))
              top_N <- length(attendu)
            
            aux <- match(obtenu[1:top_N], attendu[1:top_N])
            aux_prec <- match(obtenu[1:top_N], attendu[1:top_N])  # Cas uniquement de la précision
            rele <- aux[!aux %in% NA]
            irele <- sort(aux)
            if(length(rele) > 0){
              for (i in 1: length(rele)) {
                if(!is.na(max(rel_user)) && !is.na(as.numeric(rel_user[as.numeric(rele[i])]))){
                  DCG <- DCG + (as.numeric(rel_user[as.numeric(rele[i])])/log2(i+1))
                }
              }
            }
            if(length(irele) > 0){
              for (i in 1:length(irele)) {
                if(!is.na(max(rel_user)) && !is.na(as.numeric(rel_user[as.numeric(irele[i])]))){
                  IDCG <- IDCG + (as.numeric(rel_user[as.numeric(irele[i])])/log2(i+1))
                }
              }
            }
            tab_precision[cpt] <- length(aux_prec[!aux_prec %in% NA]) / length(obtenu)
            tab_rappel[cpt] <- length(aux[!aux %in% NA]) / top_N
            
            if(IDCG == 0){
              tab_NDCG[cpt] <- 0
            }else{
              tab_NDCG[cpt] <- DCG / IDCG
            }
            DCG <- 0
            IDCG <- 0
            cpt <- cpt + 1 
          }
        }
      } 
    }
  }
  cat("Précision : ",sum(tab_precision), " ", length(tab_precision), "\n")
  cat("Rappel : ",sum(tab_rappel), " ", length(tab_rappel), "\n")
  cat("NDCG : ",sum(tab_NDCG), " ", length(tab_NDCG), "\n")
  if(length(tab_precision) == 0){
    tab[[1]] <- 0
  }else{
    tab[[1]] <- (sum(tab_precision)/length(tab_precision))
  }
  if(length(tab_rappel) == 0){
    tab[[2]] <- 0   
  }else{
    tab[[2]] <- (sum(tab_rappel)/length(tab_rappel)) 
  }
  if(length(tab_NDCG) == 0){
    tab[[3]] <- 0
  }else{
    tab[[3]] <- (sum(tab_NDCG)/length(tab_NDCG))
  }
  return(tab)
}


############################### Extract sequence #################
ExtractSequence <- function(dataset_init, numdate){
  vec_seq <- vector()
  pos <- 0
  sepSeq <- as.character("-")
  if (length(unlist(strsplit(as.character(dataset_init[1,numdate]),split = "-"))) > 1) {
    vec_seq[1] <- unlist(strsplit(as.character(dataset_init[1,numdate]),split = "-"))[1]
    sepSeq <- as.character("-")
    pos <- 1
  }else{
    vec_seq[1] <- unlist(strsplit(as.character(dataset_init[1,numdate]),split = "/"))[3]
    sepSeq <- as.character("/")
    pos <- 3
  }
  
  vec_date <- unique(dataset_init[,numdate])
  
  cpt <- 2
  for (i in 2:length(vec_date)) {
    if (is.na(match(unlist(strsplit(as.character(vec_date[i]),split = sepSeq))[pos], vec_seq))) {
      vec_seq[cpt] <- unlist(strsplit(as.character(vec_date[i]),split = sepSeq))[pos]
      cpt <- cpt + 1
    }
  }
  
  return(vec_seq)
}

############################### EXEC ############
ParamsFile <- as.matrix(read.csv("ParamWith.csv", sep = "~"))
#ParamsFile <- as.matrix(read.csv("ParamRosette.csv", sep = "~"))

#------  Importation de la dataset a utiliser
#dataset = read.csv("online_retail_II.csv", sep = ParamsFile[1,7])
#dataset = read.csv("TourOperatorSynthetique.csv", sep = ParamsFile[1,7])
dataset = read.csv("TourOperatorSynthetique.csv", sep = ParamsFile[1,7])

#View(dataset)
data <- as.data.frame(dataset)

#------  Extraction des sequences
cat("\n","################ Extraction des sequences ###################", "\n")
sequences <- ExtractSequence(data, as.numeric(ParamsFile[1,9]))
sequences <- sort(sequences)
cat("\n", sequences, "\n")

results <- vector()
for (i in 1: nrow(ParamsFile)) {
  #ParamsFile <- read.csv2(file.choose())
  #cat(ParamsFile[i,])
  Line <- ParamsFile[i,]
  linesElements <- unlist(strsplit(Line, split = "~"))
  #################### ----- INPUTS PARAMETERS ----- ################
  # -- The minimum support --
  minSup <- as.double(linesElements[1])
  
  # -- The minimum length of extracted gradual pattern
  minLength <- as.integer(linesElements[2])
  
  # -- Output file
  output <- linesElements[3]
  
  # -- The Firts constant
  k1 <- as.numeric(linesElements[4])
  
  # -- The Second constant
  k2 <- as.numeric(linesElements[5])
  
  # -- Type of gradual treshold
  type <- linesElements[6]
  
  # -- The separator
  separateur <- linesElements[7]
  
  # -- The windows
  bin <- as.numeric(linesElements[8])
  
  # -- The numero of date
  num_date <- as.numeric(linesElements[9])
  
  # -- The numero of product
  num_product <- as.numeric(linesElements[10])
  
  # -- The numero of quantity
  num_quantity <- as.numeric(linesElements[11])
  
  # -- The numero of user
  num_user <- as.numeric(linesElements[12])
  
  # -- Top N
  topN <- as.numeric(linesElements[13])
  
  # -- if (isClust) == 1 OUI else NON
  isClust <- as.numeric(linesElements[14])
  
  # -- number of cluster
  number_cluster <- as.numeric(linesElements[15])
  
  # -- threshold saisonnier 0 => 1/2, 1 => 3/4 et 2 => 1
  minDiscSeason <- as.numeric(linesElements[16])
  
  # -- One to one or one to many 0 => one to many, 1 => one to one
  OneToOneOrOneToMany <- as.numeric(linesElements[17])
  
  #################### ----- PRINT PARAMETERS ----- ####################
  cat("\n")
  cat("----- The minimum support is ", minSup, "------ \n")
  cat("\n")
  cat("----- The minimum length of extracted gradual pattern is ", minLength, "----\n")
  cat("\n")
  cat("----- The output file ", output, "----\n")
  cat("\n")
  cat("----- The firts constant is", k1, "----\n")
  cat("\n")
  cat("----- The second constant is ", k2, "----\n")
  cat("\n")
  cat("----- The  gradual threshold is ", type, "----\n")
  cat("\n")
  cat("----- The separator of data is ", separateur, "------ \n")
  cat("\n")
  cat("----- The windows of season is ", bin, "------ \n")
  cat("\n")
  cat("----- The date column number is", num_date, "----\n")
  cat("\n")
  cat("----- The product column number is ", num_product, "----\n")
  cat("\n")
  cat("----- The quantity column number is ",  num_quantity, "------ \n")
  cat("\n")
  cat("----- The user column number is ", num_user, "------ \n")
  cat("\n")
  cat("----- The top N is ", topN, "------ \n")
  cat("\n")
  # The code version is 1 if with cluster or 0 if which no cluster  
  cat("----- The code version is  ", isClust, "------ \n")
  cat("\n")
  cat("----- The number of cluster is  ", number_cluster, "------ \n")
  cat("\n")
  cat("----- The seasonal threshold is ",minDiscSeason, "------ \n")
  cat("\n")
  cat("----- The one to one or one to many ", OneToOneOrOneToMany, "----- \n")
  cat("\n")
  
  
  #------  Test de l'application
  precision <- vector()
  rapel <- vector()
  NDCG <- vector()
  for (p in 1: (length(sequences)-1)) {
    taille <- length(sequences) - p
    needDate <- paste(sequences[taille],sep = "-", "12-31")
    if(OneToOneOrOneToMany == 1){
      dataset2 <- dataset[as.numeric(as.Date(dataset[,num_date])) <= as.numeric(as.Date(needDate)), ]# (one to many) cas où on prend toute les annees qui précédent pour tester 
    }else{
      dataset2 <- dataset[which(startsWith(as.character(dataset[,num_date]),as.character(sequences[taille]))),] # (one to one) cas où on prend l'année précédente pour tester
    }
    
    #----- Resultats obtenus
    dico <- TrainSet(dataset2)
    #----- Résultat attendu
    
    eval_test <- evaluate(as.numeric(sequences[(length(sequences)-p+1)]))
    
    cat("\n\n start all evaluation here \n\n")
    
    metrique <- globalEval(dico, eval_test, dataset2[,num_user], topN)
    
    precision[p] <- metrique[[1]]
    rapel[p] <- metrique[[2]]
    NDCG[p] <- metrique[[3]]
  }
  
  write(paste(i, sep =" ", paste(sum(precision)/length(precision), sep = " ", paste(sum(rapel)/length(rapel), sep = " ", sum(NDCG)/length(NDCG)))), file = "ResultsWith.txt", append = TRUE)
  
  results[i] <- paste(i, sep =" ", paste(sum(precision)/length(precision), sep = " ", paste(sum(rapel)/length(rapel), sep = " ", sum(NDCG)/length(NDCG))))
}

write(results, file = "AllResults.txt", sep ="\n")
