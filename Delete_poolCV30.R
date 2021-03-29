###### Calcul du CV pour les l'intensités des pools généréés par le package XCMS ########

## Appel des librairies
library('xlsx')
library('stringr')
library('filesstrings')

## Ouverture du fichier intensité généré par XCMS

Intensity_Neg <- read.table(file = 'Neg_IRSN_CorCer.tsv', sep = '\t', header = TRUE) # préciser le nom du fichier '.tsv' à importer dans le dossier de travail
Vec_pool <- as.vector(which(str_detect(colnames(Intensity_Neg), "pool"))) # prendre les colonnes 'pool'
pool <- Intensity_Neg[,min(Vec_pool):max(Vec_pool)] # faire un tableau qui contient uniquement les colonnes pools

## Fonction pour calculer CV
CV <- function(x) {
  CV <-  sd(x)/(sum(x)/length(x))*100
  return(CV)
}

## Applique la fonction 'CV' sur toutes les colonnes pools
Coeff_Var <- apply(pool, MARGIN = 1,CV) # fonction pour déterminer le CV de toutes les colonnes
name <- sprintf("%03d.png", Intensity_Neg[,1]) # permet de mettre des 0 dans le nom des fichiers puis l'extension 'png', 
                                               # correspond donc aux titrex des fichiers présents dans les dossiers générés par XCMS

CpoolCV <-cbind(name,Coeff_Var, pool)
Select_pool <- subset(CpoolCV, Coeff_Var <= 30) #selectionne les features avec un CV <= 30%


## Deplace les fichiers correspondant aux features avec des CV<=30
fn <- as.vector(Select_pool$name)
# Check son existence dans le dossier de travail
if (file.exists(fn)) {
  # Deplace le fichier dans le dossier spcifié
  filesstrings::file.move(fn, "EIC_CVinf30")
}


