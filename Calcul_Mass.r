# Charger le package readxl
library("readxl")
library("openxlsx")
library("enviPat")
library("Rdisop")

# Lire le nom des feuilles dans le fichier Excel "Lipid-Class_Plasma.xlsx"
sheets <- excel_sheets("Lipid-Class_Plasma.xlsx")

# Créer une fonction pour calculer la colonne "Exact_Mass"
getExactMass <- function(x) {
  getMolecule(x)[[3]]
}

# Utiliser la fonction lapply() pour lire les données de chaque feuille
# et calculer la colonne "Exact_Mass"
Mass_Exact <- lapply(sheets, function(sheet) {
  cat("Lecture de ", sheet, "\n")
  DF <- read.xlsx("Lipid-Class_Plasma.xlsx", sheet = sheet)
  DF$Exact_Mass <- apply(DF[, 2, drop = FALSE], 1, getExactMass)
  DF$M_H <- DF$Exact_Mass + 1.007276
  DF$M_NH4 <- DF$Exact_Mass + 18.033823
  DF$M_Na <- DF$Exact_Mass + 22.989218
  DF$M_HH2O <- DF$Exact_Mass + 17.00219
  DF$Fragmented <- rep("NA", nrow(DF))
  return(DF)
  # Sys.sleep(1)
})


names(Mass_Exact) <- sheets
# Lecture des fichiers mzML -----------------------------------------------


# Chargement des libraries
library('xcms') # install.packages("xcms") # https://www.bioconductor.org/packages/release/bioc/html/xcms.html
library("msdata")
library('dplyr')
library("openxlsx")
library('magrittr')
library('tidyverse')

# Créer une liste de fichiers mzML
file_mzML <- list.files("mzML", pattern = "*mzML")

# Pré-allouer la liste de données dda
dda_list <- vector("list", length(file_mzML))

# Utiliser lapply pour lire les fichiers mzML
setwd("mzML")
dda_list <- lapply(file_mzML, function(x) {
  
  cat("Lecture de ", x, "\n")
  readMSData(files = x, mode = "inMemory", centroided. = FALSE) 
})
names(dda_list) <- file_mzML


setwd("D:/ICSN/Projets/Plasma/20230217_InjectionPlasma_n2/3.Post_Analytique/DDA/Positif/MS1_Exctract/Calcul_Mass_Exact")
load( "dda_list.RData")

# Je stocke le tout dans un tableau
Precurseur <- vector("list", length(dda_list)*length(Mass_Exact)) 
name <- rep("NA", length(dda_list)*length(Mass_Exact))
n = 1
for(i in 1:length(Mass_Exact)){
  cat("Lecture de ", names(Mass_Exact[[i]])[1], "\n")
  for(j in 1:length(dda_list)){
    cat("Lecture de ", names(dda_list[j]), "\n")
    rawdata_Filter_RT <- filterRt(dda_list[[j]], 
                      c(unique(Mass_Exact[[i]][["Start.RT"]])*60, 
                        unique(Mass_Exact[[i]][["End.RT"]])*60 )) %>%
                      pickPeaks()
    Precurseur[[n]] <- rawdata_Filter_RT
    name[n] <- paste0(names(Mass_Exact[[i]])[1], "_", gsub("Plasma_Pos_Lipidomix_DDATop4_250MS1_200MS2_", "", names(dda_list[j])))
    n = n + 1
  }
}
names(Precurseur) <- name
remove(name) 
remove(dda_list)

# Créer 11 listes vides
my_lists <- vector(mode = "list", length = length(sheets))

# Ajouter 8 data frames à chaque liste
for (i in seq_along(my_lists)) {
  my_lists[[i]] <- vector(mode = "list", length = 8)
}
names(my_lists) <- sheets
n = 1
for(i in seq_along(Precurseur)){
  
  if(n > 8){n = 1}
  rlang::env_unlock(Precurseur[[i]]@assayData)
  rlang::env_unbind(Precurseur[[i]]@assayData, nms = "assayData")
  
  nList <- which(gsub("\\.","",strsplit(names(Precurseur[i]), "_")[[1]][1]) == names(my_lists)) # 1
  x <- as.data.frame(sapply(Precurseur[[i]]@assayData, function(x) x@precursorMz),
              nrow = length(sapply(Precurseur$TG_seuil1000_001.mzML@assayData, function(x) x@precursorMz)),
              ncol = 2, byrow = TRUE)
  x3 <- sapply(Precurseur[[i]]@assayData, function(x) x@rt)
  x3 <- as.numeric(x3)/60
  x2 <- cbind(Name = rep(names(Precurseur[i]), nrow(x)), Precurseur = x, RT = x3)
  # colnames(x2) <- c("Name", "Precurseur")
  my_lists[[nList]][[n]] <- x2
  # colnames(my_lists[[nList]][[n]]) <- c("Name", "Precurseurs")
  n = n + 1
  
}  

ppm = 10
n = 0
Adduit = "M_Na"
my_list2 <- vector(mode = "list", length = length(sheets))
for(i in 1:length(my_lists)){
  my_list2[[i]] <- do.call("rbind", my_lists[[i]])
}
names(my_list2) <- sheets

for(i in 1:length(my_list2)){
  nList <- which(names(my_list2[i]) == names(Mass_Exact))
  for(j in 1:nrow(Mass_Exact[[nList]])){
    idx <- which(my_list2[[i]][[2]] >= as.numeric(Mass_Exact[[nList]][[Adduit]][j]) - as.numeric(Mass_Exact[[nList]][[Adduit]][j])*ppm/10^6   & 
                   my_list2[[i]][[2]] <=  as.numeric(Mass_Exact[[nList]][[Adduit]][j])*ppm/10^6 +  as.numeric(Mass_Exact[[nList]][[Adduit]][j]))
  
  if(length(idx)!=0) {
    # if(Mass_Exact[[nList]][["Fragmented"]][j] == "NA"){
    for(k in 1:length(idx)){
      Mass_Exact[[nList]][["Fragmented"]][j] <- paste0(Mass_Exact[[nList]][["Fragmented"]][j],"_Fragmented_",my_list2[[i]][[1]][idx[k]], "RT:",my_list2[[i]][[3]][idx[k]])
      }
    Mass_Exact[[nList]][["Fragmented"]][j] <- gsub("NA_","/",Mass_Exact[[nList]][["Fragmented"]][j])
    cat(Mass_Exact[[nList]][["Fragmented"]][j],"\n")
    cat("idx = ", idx,"\n")
    n = n + 1
    
    # }
    }
  }
}


# create workbook
wb <- createWorkbook()

#Iterate the same way as PavoDive, slightly different (creating an anonymous function inside Map())
Map(function(data, nameofsheet){     
  
  addWorksheet(wb, nameofsheet)
  writeData(wb, nameofsheet, data)
  
}, Mass_Exact, names(Mass_Exact))

## Save workbook to excel file 
setwd("D:/ICSN/Projets/Plasma/20230217_InjectionPlasma_n2/3.Post_Analytique/DDA/Positif/MS1_Exctract/Calcul_Mass_Exact")
saveWorkbook(wb, file = paste0("Lipid-Class_Plasma_M_",as.character(Adduit),"_",ppm,"ppm.xlsx"), overwrite = FALSE)


remove(Precurseur); remove(rawdata_Filter_RT); remove(my_lists); remove(x); remove(x2)
# je récupère l'indice du fichier seuil1000000 car c'est sur ce fichier que je veux faire du MS1
setwd("mzML")
idx <- which(grepl("seuil100000_001", file_mzML))
rawdataMS1 <- readMSData(file_mzML[idx], msLevel. = 1, centroided. = FALSE)
 

MS1 <- vector(mode = "list", length = length(Mass_Exact))
for(i in 1:length(Mass_Exact)){
  rawdata_Filter_RT_MS1 <- filterRt(rawdataMS1, 
                                c(unique(Mass_Exact[[i]][["Start.RT"]])*60, 
                                  unique(Mass_Exact[[i]][["End.RT"]])*60 )) %>%
    pickPeaks()
  rlang::env_unlock(rawdata_Filter_RT_MS1@assayData)
  rlang::env_unbind(rawdata_Filter_RT_MS1@assayData, nms = "assayData")
  Vector_List <- names(rawdata_Filter_RT_MS1@assayData) 
  cat("i =", i)
  MS1[[i]] <- rawdata_Filter_RT_MS1
}

MS1_lipidClass <- vector(mode = "list", length = length(Mass_Exact))

for(i in 1:length(MS1)){
  rlang::env_unlock(MS1[[i]]@assayData)
  rlang::env_unbind(MS1[[i]]@assayData, nms = "assayData")
  Vector_List <- names(MS1[[i]]@assayData)
  TMP_DFMS1 <- vector(mode = "list", length = length(Vector_List))
  for (j in 1:length(Vector_List)){
    cat("j= ",j)
    str <-paste0("MS1[[i]]@assayData$", Vector_List[j])
    print(str)
    info <- eval(parse(text = str))
    #info <- info[info@mz < Infos[k,8],]
    mz = as.numeric(info@mz[which(info@mz >= min(Mass_Exact[[i]][Adduit]) & info@mz <= max(Mass_Exact[[i]][Adduit]))])
    intensity = as.numeric(info@intensity[which(info@mz >= min(Mass_Exact[[i]][Adduit]) & info@mz <= max(Mass_Exact[[i]][Adduit]))])
    TMP_DFMS1[[j]] <- as.data.frame(cbind(mz = as.numeric(mz), intensity = as.numeric(intensity), RT = rep(info@rt/60, length(info@intensity)), Scan = info@scanIndex, Name =Vector_List[i])) # Contient pour une même energie de collision toutes les informations du précurseur
    # faire une moyenne de tous les fragments
  }
  MS1_lipidClass[[i]] <- do.call("rbind",TMP_DFMS1)
}
names(MS1_lipidClass) <- sheets

Max_Intensity <- list()
for(i in 1:length(MS1_lipidClass)){
  nList <- which(names(MS1_lipidClass[i]) == names(Mass_Exact))
  TMP_List <- list()
  n = 1
  for(j in 1:nrow(Mass_Exact[[nList]])){
    idx <- which(as.numeric(MS1_lipidClass[[i]][[1]]) >= (as.numeric(Mass_Exact[[nList]][[Adduit]][j]) - as.numeric(Mass_Exact[[nList]][[Adduit]][j])*ppm/10^6)   & 
                   as.numeric(MS1_lipidClass[[i]][[1]]) <=  as.numeric(Mass_Exact[[nList]][[Adduit]][j])*ppm/10^6 +  as.numeric(Mass_Exact[[nList]][[Adduit]][j]))
    cat("Classe = ",names(MS1_lipidClass[i]), "j = ", j, "idx = ", idx,'\n')
    if(length(idx) != 0){
      TMP_List[n] <- list(cbind( name =  Mass_Exact[[nList]][j,1], mz_Theo = as.numeric(Mass_Exact[[nList]][[Adduit]][j]),
                                      MS1_lipidClass[[i]][idx,], annoted = Mass_Exact[[nList]][j,12]))
      n = n +1
    }
    
  }
  Max_Intensity[[i]] <- do.call("rbind", TMP_List)
}

Max_Intensity2 <- list()
for(i in 1:length(Max_Intensity)){
  Max_Intensity2[[i]] <- Max_Intensity[[i]] %>%
    group_by(name) %>%
    slice(which.max(intensity)) %>%
    ungroup()
}
names(Max_Intensity2) <- sheets 
# Définir la fonction clean_data
num_data <- function(df) {
  # Convertir les 5 premières colonnes en numeric
  df[, 2:6] <- lapply(df[, 2:6], as.numeric)
  # Retourner le data frame nettoyé
  return(df)
}

# Appliquer la fonction clean_data à chaque élément de la liste my_list
Max_Intensity2 <- lapply(Max_Intensity2, num_data)



# Plotting ----------------------------------------------------------------
library("ggplot2")
library('ggrepel')
setwd("..") ; setwd("Plot_SVG")
for(i in 1:length(sheets)){
  
  # Max_Intensity2 <- Max_Intensity2[[sheets[i]]][-which(grepl("Fragmented",Max_Intensity2[[sheets[i]]]$annoted)),]
  plot_Fragmented <- ggplot(Max_Intensity2[[sheets[i]]], aes(x= mz, ymax= intensity , ymin=0
                                                             , color=ifelse(grepl("Fragmented", annoted), "red", "black")
  )) +
    geom_linerange(
      #aes(color=ifelse(grepl("Fragmented", annoted), "red", "black"))
      ) + 
    theme_bw() + guides(color=FALSE) +
    geom_text_repel(aes(label=ifelse(grepl("Fragmented", annoted), Max_Intensity2[[sheets[i]]]$name, "")),
                    data = Max_Intensity2[[sheets[i]]],
                    x = Max_Intensity2[[sheets[i]]]$mz, y = Max_Intensity2[[sheets[i]]]$intensity, 
                    hjust = 0, vjust = 0.5, size = 3, angle = 90,
                    xlim = c(1, NA),
                    seed = 1, point.padding = 0.5, max.overlaps = 8) +
    labs(x=expression(italic("m/z")), y="Intensity") +
    ggtitle(paste0("Spectre de masse (MS1) des ",sheets[i]," (",Adduit,")")) + 
    theme_bw() +
    xlim(c(min(Max_Intensity2[[sheets[i]]]$mz),max(Max_Intensity2[[sheets[i]]]$mz))) + 
    ylim(c(0,  max(Max_Intensity2[[sheets[i]]]$intensity))) +
    expand_limits(y = 0) +
   # guides(color=TRUE) +
    theme(panel.grid = element_blank()) +
    scale_color_manual(values=c("black", "red"))

  ggsave(plot = plot_Fragmented, paste0("Spectre de masse (MS1) des ",sheets[i]," (",Adduit,")",ppm,"ppm.png"), width = 1200/100, height = 500/100, dpi=500, limitsize = FALSE)

}


ggplot(na.omit(Mass_Exact2[["Cer"]]), aes(x= as.numeric(Fragmented), y = M_H)) + geom_point()

for (i in 1:nrow(Mass_Exact2[["Cer"]])) {

idCol <- which(colnames(Mass_Exact2[["Cer"]]) == "Fragmented" )
valeur_numerique <- Mass_Exact2[["Cer"]][i,idCol] %>%
  # Extraire la partie de la chaîne après "RT"
  str_extract("(?<=RT:)[0-9\\.]+") %>%
  # Convertir en format numérique
  as.numeric()
Mass_Exact2[["Cer"]][i,idCol] <- as.numeric(valeur_numerique)

}

na.omit(Mass_Exact2[["TG"]])
