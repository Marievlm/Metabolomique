# fonction pour eliminer les variables XCMS qui subissrnt un effect de compétition ionique

delete_variable <- function(df) { 
  

## multiplier les individus qui contiennet la dilution

  library('svDialogs')
  library("ggplot2")
  library('stringr')
  library('filesstrings')
  
  ## simulation avec des données
  Vec_FD <- rnorm(n = 10, mean = 50, sd = 2.0)
  Vec_pool <- rnorm(n = 10, mean = 5, sd = 0.50)
  
  ## chargement des données
  chemin <- dlg_input("Quel est le chemin de voter dossier de travail ? ")$res
  file <- dlg_input('Quel est le nom de votre fichier ? (spécifier lextension')$res
  
  df <- read_csv("pos_XCMS_galaxy_MV.csv")
  colnames(df) # on fait un tour sur les noms des colonnes
  
  ## dialogue avec l'utilisateur
  FD <- as.numeric(dlg_input("Quel est le facteur de dilution (exemple si facteur est 1/10eme mettre 10 :")$res)
  
  
  ## selectionner les individus qui contiennent le facteur spécifié demandé 
  
  index_Vec_FD <- as.vector(which(str_detect(colnames(df), paste0('D',FD)))) # prendre les colonnes 'FD' attention faire la transverse de la matrice si besoin
  
  Vec_FD <- df[,index_Vec_FD] # faire un tableau qui contient uniquement les colonnes des pools idlués
  Vec_mult <- Vec_FD*FD # le vecteur est multiplié par le facteur de dilution
  rownames(Vec_mult) <- rownames(df)
  
  
  ## selectionner les individus 'pool'
  index_Vec_pool_tmp <- as.vector(which(str_detect(colnames(df), 'pool'))) 
  index_Vec_pool <- index_Vec_pool[!index_Vec_pool %in% index_Vec_FD] # on supprime les colonnes qui sont pools mais qui nosu interesse pas
  Vec_pool <-  as.data.frame(df[,c(index_Vec_pool)])
  rownames(Vec_pool) <- df$name
  
  
  ## Calculer l'intervalle de confiance ? ou bien l'écart-type ? à voir
  # créer un tableau
  
  metrique_pool <- matrix(nrow = nrow(df), ncol = 8) # a completer 
  
  
  metrique_pool[,1] <- apply(Vec_pool,1, mean) # mettre dans une colonne (la 1ere par exemple)
  metrique_pool[,2] <- apply(Vec_pool,1, sd) # on calcule l'écart-type
  metrique_pool[,3] <- metrique_pool[,1] - metrique_pool[,2]  # colonne min
  metrique_pool[,4] <- metrique_pool[,1] + metrique_pool[,2]  # colonne max
  
  metrique_pool[,5] <- apply(Vec_mult,1 , mean) 
  metrique_pool[,6] <- apply(Vec_mult, 1, sd) # on calcule l'écart-type des valeurs des dilutions
  metrique_pool[,7] <- metrique_pool[,5] - metrique_pool[,6] # colonne min
  metrique_pool[,8] <- metrique_pool[,5] + metrique_pool[,6] # colonne max
  
  colnames(metrique_pool) <- c('mean_pool', 'sd_pool', 'min_pool', 'max-pool', 'mean_dpool', 'sd_dpool', 'min_dpool', 'max_dpool')
  rownames(metrique_pool) <- df$name
  
  write.csv(metrique_pool, 'exceed_pool.csv') # provisoire
  
  
  min_max <- function(df_min_max) {
    if (df_min_max[,7] > df_min_max[,3] && df_min_max[,8] > df_min_max[,4]) { reponse <- 'ok'}
    else if (df_min_max[,7] < df_min_max[,3]) {reponse <- 'inf'}
    else if (df_min_max[,8] > df_min_max[,4]) {reponse <- 'sup'}
    
    return(reponse)
  }

  apply(metrique_pool,2, min_max)
  
 
  
  
# faire des plots stylés pour voir ce que ça donne sur ces variables
  
  
  
  
