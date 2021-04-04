library('svDialogs')

# Conversion des donnees .raw en .mzXML

converter <- c("N:/pwiz/msconvert.exe") # Appelle le logiciel msconvert

chemin <- gsub('\\', '/', as.character(dlg_input("Entrez le chemin de votre dossier de travail ('/' autorise) :")$res), fixed = TRUE)
# chemin <- gsub('\\', '/', as.character(readline(message(prompt = "Entrez le chemin de votre dossier de travail ('/' autorisé) :"))), fixed = TRUE)


FILES <- list.files(recursive=TRUE,full.names=TRUE,pattern="\\.raw")

for (i in 1:length(FILES)){system (paste(converter," --mzXML --32 --filter \"peakPicking true 1\" --filter \"polarity negative\"  -o Negative -v",FILES[i]))}
for (i in 1:length(FILES)){system (paste(converter," --mzXML --32 --filter \"peakPicking true 1\" --filter \"polarity positive\"  -o Positive -v",FILES[i]))}


# Utilisation du package XCMS

library("xcms")
library("Biobase")
library("multtest") 
# if(!require("multtest")){
    #BiocManager::install("multtest")
    #require("multtest")
#   }

# dlg_message("Veuillez creer les dossiers Blancs, pool et echantillons")$res
# 
dlg_message("Veuillez supprimer les fichiers 'mzxml' inutiles pour XCMS 'MSMS, QC-pool Blanc extraction")$res

projet <- dlg_input("Quel est le nom du projet ? ")$res
# projet <- readline(message(prompt = "Quel est le nom du projet ? "))

chemin <- gsub('\\', '/', as.character(dlg_input("Entrez le chemin de votre dossier de travail ('/' autorise) :")$res), fixed = TRUE)
# chemin <- gsub('\\', '/', as.character(readline(message(prompt = "Entrez le chemin de votre dossier de travail ('/' autorisé) :"))), fixed = TRUE)

setwd(chemin)
dlg_message(sprintf("Vous etes dans le repertoire suivant : %s", chemin))$res# se mettre dans le bon dossier

# groupe pour reconnaîter les fichiers et les repertorier -----------------

## obtenir le titre des fichiers présents dans le dossier de travail
file <- list.files(pattern =  "\\.mzXML$")
# message quels sont les groupes et les sous groupes

pool_file <- grep('pool', file, value=TRUE)
if (file.exists(pool_file)) {
  # Deplace le fichier dans le dossier spcifié
  filesstrings::file.move(pool_file, "pools")
}

blanc <- grep('Blan', file, value=TRUE)
if (file.exists(blanc)) {
  # Deplace le fichier dans le dossier spcifié
  filesstrings::file.move(blanc, "blancs")
}

echantillon <- file[-pmatch(c(pool_file, blanc),file)]
if (file.exists(echantillon)) {
  # Deplace le fichier dans le dossier spcifié
  filesstrings::file.move(blanc, "echantillons")
}

# Dialogue avec l'utilisateur pour demander les parametres pour XC --------

polarite <- dlg_input("Quelle polarité ? (negative ou positive) :")$res
noise <- as.numeric(dlg_input("Quelle est la valeur du bruit ? (ordre de grandeur 10^3 à 10^4) : ")$res)

dlg_message(sprintf("Vous vous apprêtez à exécuter XCMS sur vos données issus du projet %s :
                 \n - mode = %s; \n - bruit = %s; \n - dans le dossier = %s\n", projet, polarite, noise, chemin))

xset <- xcmsSet(method="centWave", peakwidth=c(2,40), snthresh=3, mzdiff=-0.00005, polarity= polarite, ppm=5, prefilter=c(4,12000), noise=10000, integrate=1) # IRSN ne, noise de 5000 500
xset
xset1 <- group(xset, method="density", bw=10, mzwid=0.015, minfrac=0.30, minsamp=3)
xset1
xset2 <- retcor(xset1, method="obiwarp", profStep=0.1, plottype="none")
xset2
xset3 <- group(xset2, method="density", bw=5, mzwid=0.015, minfrac=0.30, minsamp=5)
xset3
xset4 <- fillPeaks(xset3)
xset4


#Pour obtenir le rapport du résultat d'XCMS#

nom_projet <- paste0(projet, 'neg', "Echantillons") # faire une fonction if pour trouver le diminutif de la polarité
reporttab <- diffreport(xset4,"blancs","pools","echantillons",15000,metlin=0,h=480,w=640) # bien changer le nom du fichier

response <- dlg_message("Voulez-vous enregistrer une image de votre environnement ? (yes or no)","yesno")$res
if(response == 'yes'){ 
  save.image(file = paste("XCMS",polarite,projet,".RData",sep = "_"))
  } else (response =='no') {
    dlg_message("Vous ne desirez pas enregistrer d'image de votre environnement, tous les objets executés seront perdus.")$res
  }

savehistory(file = paste("XCMS",polarite,projet,".RData",sep = "_"))  



# Calcul du CV30  ---------------------------------------------------------


## Appel des librairies
library("xlsx")
library('stringr')
library('filesstrings')

## ouverture du fichier

chemin <- gsub('\\', '/', as.character(readline(message(prompt = "Entrez le chemin de votre dossier de travail ('/' autorisé) :"))), fixed = TRUE)
setwd(chemin)

file_TSV <- dlg_input("Entrez le nom du fichier avec l'extension '.tsv ou txt' : ")$res # # préciser le nom du fichier .tsv à importer
response <- dlg_message("Est-ce un fichier genere par VDK ?","yesno")$res


if(response == "yes"){
    require(tibble)
    intensity <- t(read.table(file = file_TSV, sep = '\t', header = TRUE)) 
    add_column(intensity, c(1:nrow(file_TSV), .after = 1))
} else{ 
    intensity <- read.table(file = file_TSV, sep = '\t', header = TRUE)
    }


Vec_pool <- as.vector(which(str_detect(colnames(intensity), "pool"))) # prendre les colonnes uniquement pour pool
pool <- intensity[,min(Vec_pool):max(Vec_pool)] # faire un tableau qui contient uniquement les colonnes pools

## Fonction pour calculer CV
CV <- function(x) {
  CV <-  sd(x)/(sum(x)/length(x))*100
  return(CV)
}

## Applique la fonciton CV sur toutes les colonnes pools

Coeff_Var <- apply(pool, MARGIN = 1, CV) # fonction pour déterminer le CV de toutes les colonnes
name <- sprintf("%03d.png", intensity[,1]) # permet de mettre des 0 dansle nom des fichiers pour l'extension pngg, correspond donc au titre des fichiers présents dans les dossiers

CpoolCV <-cbind(name,Coeff_Var, pool)
Select_pool <- subset(CpoolCV, Coeff_Var <= 30)


## Deplace les fichiers avec les CV<30
fn <- as.vector(Select_pool$name)
# Check son existence dans le dossier de , se mettre 

if (file.exists(fn)) {
  # Deplace le fichier dans le dossier spcifié
  filesstrings::file.move(fn, "EIC_CVinf30")
}

# Creation de la matrice entiere avec CV <= 30

IntensityCV <- cbind(intensity, Coeff_Var)
IntensityCV30 <- subset(IntensityCV, Coeff_Var <= 30)

title_file <- message(sprintf("Vous êtes sur le point de créer le fichier '.csv' ne possédant que les EIC avec CV <= 30%.\n
                              Quel titre voulez-vous lui donner (ex :PosCV30_IRSN_CorCer.csv "))
write.csv(IntensityCV30, title_file)
