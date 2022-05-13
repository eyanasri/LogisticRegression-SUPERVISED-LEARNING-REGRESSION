#Objectif:  Manipuler la méthode regression logistique

#Chargement des données
cardio <- read.table(file=file.choose(),sep="\t",header=TRUE)

#Affichage des données
cardio

# La fonction de la regression logistique
? glm
#GLM avec R s'utilise globalement comme la fonction lm(), 
#L'option 'family=binomial' pour le lien LOGIT

modele <- glm(coeur ~ age+taux+angine, data=cardio, family=binomial)
modele
summary(modele)
pred.proba <- predict(modele, newdata= cardio, type="response")
pred.proba
#Appliquer la fonction de décision
p <- factor(ifelse(pred.proba > 0.5, "presence", "absence")) 
p
#Matrice de confusion
mc <- table( cardio$coeur, p ) 
mc
library(caret)
precision <- posPredValue(cardio$coeur, p , positive="1")
recall <- sensitivity(cardio$coeur, p , positive="1")

F1 <- (2 * precision * recall) / (precision + recall)
#Calcul de la bonne classification 
err <- ( mc[2,1]+ mc[1,2]) / sum (mc) 
err

library(ROCR)

result= prediction(as.numeric(cardio$coeur), p)
performance.radial=performance(result,"tpr","fpr")
plot(performance.radial,col="red")
library(MASS)
modeleRéduit <- stepAIC ( modele , scope = list (lower =" coeur ~1 " , upper = "coeur ~age +taux + angine" ) , direction = "backward" ) 

modeleRéduit



       
        
        