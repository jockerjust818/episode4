#Test de connaissances en R

# 1) Calculer un 1+20 et le stocker dans x
x <- 1+20

#2 Stocker le vecteur (1,2,4,6,7)

y <- c(1,2,4,6,7)

#3 Creer une sequence de nombre de 0 à 100 par bond de 5 et stocker dans w

w <- seq(0,100,5)

#4 Creer une fonction qui renvoie le carré d'un nombre entré par l'utilisateur

(ma_fonction <- function(nombre){
  carre <- nombre^2
  returnValue(carre)
})

  
ma_fonction(6)

#5 Stocker une matrice

A <- matrix(c(2,4,3,6,8,9,11,12,14),nrow = 3,ncol = 3)
A

#6 Recuperer l'element qui se trouve à la 3e ligne 2e colonne

z <- A[3,2]
z

#7 Calculer le carré de la matrice A et stocker dans B

B <- A^2
B

#8 Calculer C=A+B

c <- A+B
c

#9 Nombre de caractère

phrase <- "L'expertise au service du developpement"
nombre_carac <- nchar(phrase)
nombre_carac

#10 Convertir c en dataframe

df <- data.frame(c)
df

#11 Creer une fonction qui renvoie 3 elements dans un vecteur

my_fonction <- function(a,b){
  grand <- max(a,b)
  produit <- a*b 
  somme <- a+b 
  return(c(grand,produit,somme))
  }
  
my_fonction(2,3)

#12 Genere un vecteur aleatoire de taille n=100

V1 <- runif(100,0,1)
V1

#13 Recuperer les objets supérieurs à 0.5

V2 <- V1[V1>0.5]
V2

#14 Taille de V2

length(V2)
#La taille de V2 est 52