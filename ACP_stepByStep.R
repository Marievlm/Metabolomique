#########ACP step by step pour comprendre le déroulement de l'application d'une ACP sur un jeu de données################
library(ade4)
### Creaction du dataframe 

Id <- c(1:9) # 9 individus
Name <- c("Jean", "Aline", "Annie","Monique", "Didier", "Clara", "Pierre", "Brigitte", "Evelyne")
Math <- c(6.0, 8.0, 6.0, 14.5, 14.0, 11.0, 5.5,13.0, 9.0)
Science <- c(6.0,8.0,7.0,14.5,14.0,10.0,7.0,12.5,9.5)
Francais <- c(5.0,8.0,11.0,15.5,12.0,5.5,14.0,8.5,12.5)
latin <- c(5.5,8.0,9.5,15.0,12.5,7.0,11.5,9.5,12.0)
art <- c(8.0,9.0,11.0,8.0,10.0,13.0,10.0,12.0,18.0)
Notes <- data.frame(Math,Science,Francais,latin,art) 
row.names(Notes) <- Name
print(Notes)

df.cor <- read_excel('Intensity_C18_Neg_original.xlsx')
rownames(df.cor) <- df.cor$idsample
df.cor.finale <- df.cor[,-1]
# df.cor.finale.num <- as.data.frame(apply(df.cor.finale,2, as.numeric))
# rownames(df.cor.finale.num) <- df.cor[1,]

### Centrage de la matrice
Y <- as.matrix(Notes)
n <- dim(Y)[1] # dimension, ici nombre de lignes
X <- Y-matrix(1,n,1) %*% apply(Y,2,mean) #Correspond a la moyennes de chacune des colonnes, pas de reduction de la matrice car homogeneite de l'unite des variables
##Plus simple ! : la fonction scale
#Scale(data, center=TRUE,scale=FALSE)

n.cor <- dim(df.cor.finale)[1]
X.cor <- df.cor.finale-matrix(1,n.cor,1) %*% apply(df.cor.finale,2,mean)
X.cor.norm <- scale(df.cor.finale, center=TRUE,scale=TRUE)

### Calcul de la matrice de covariance ou de corr??lation
V <- (1/n)*t(X) %*% X
# v<-var(X)
V.cor <- (1/n.cor)*t(X.cor) %*% X.cor
V.cor <- as.matrix(x.tmp) %*% as.matrix(X.cor)

V.cor.norm <- as.matrix((1/n.cor)*t(X.cor.norm)) %*% as.matrix(X.cor.norm)

### Calcul des valeurs propres et des axes d'inertie
tmp <- eigen(V)
L <- diag(tmp$values) #diagonalistion des vecteurs propres
U <- tmp$vectors # vecteur propres


tmp.cor <- eigen(V.cor)
L.cor <- diag(tmp.cor$values) #diagonalistion des vecteurs propres
U.cor <- tmp.cor$vectors # vecteur propres



### Calcul des composantes principales des individus
C <- X %*% U # produit entre la matrice X et des vecteurs propres : C'est ton score !!
C.cor <- as.matrix(X.cor) %*% U.cor # produit entre la matrice X et des vecteurs propres : C'est ton score !!

### Calcul des contributions
COR <- diag(1/apply(X^2,1,sum)) %*% C^2 # elle repr??sente le carre du cosinus de l'angle form?? par l'individu i et le vecteur u
CTR <- (1/n)*C^2 %*% diag(1/diag(L)) # c'est la contribution relative de l'individu ?? l'axe

### Repr??sentation des variables
D <- diag(1/(sqrt((n-1)/n)*apply(X, 2, sd))) %*% U %*% sqrt(L) # c'est ton loading !! ce sont les correlations entre les variables

# apply(X, 2, sd)--> d??termine l'??cart-type de chaque colonne 


### Trac?? des graphiques

x11()

plot(-1:1,-1:1,type="n",xlab="Axe 1",ylab="Axe 2")
text(D[,1],D[,2],colnames(Notes));abline(h=0);abline(v=0)
curve(sqrt(1-x^2),-1,1,add=TRUE) #demi cercle en haut
curve(-sqrt(1-x^2),-1,1,add=TRUE) #demi cercle en bas

plot(-1:1,-1:1,type="n",xlab="Axe 1",ylab="Axe 3")
text(D[,1],D[,3],colnames(Notes));abline(h=0);abline(v=0)
curve(sqrt(1-x^2),-1,1,add=TRUE)
curve(-sqrt(1-x^2),-1,1,add=TRUE)

plot(C[,1],C[,2],type="n");text(C[,1],C[,2],rownames(Notes))
abline(h=0);abline(v=0)

plot(C.cor[,1],C.cor[,1],type="n");text(C.cor[,1],C.cor[,2],rownames(df.cor))
abline(h=0);abline(v=0)

plot(C[,1],C[,3],type="n");text(C[,1],C[,3],rownames(Notes))
abline(h=0);abline(v=0)

