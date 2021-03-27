# fonction pour ne pas installer des packgaes déjà installés sur la machine
Useful_Package <- function(packages_Vec) # packages_Vec est un vecteur de noms de pakage ex : packages_Vec <- c("ggplot2", "plyr", "tidyr")
{
  i = 1
  for (i in 1:length(packages_Vec)){
    if (!requireNamespace(packages_Vec[i], quietly = TRUE))
    {
      install.packages(packages_Vec[i])
      sprintf("Le package %s vient d'être installé.", packages_Vec[i])
    }
    else {
      sprintf("Le package %s est déjà installé.", packages_Vec[i])
      i = i+1}
  }
}
