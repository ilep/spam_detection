
library(ROCR)
library(ipred)
library(randomForest)
library(tree)

spam=read.table("~/M2MO/R/spam.txt",header=T,sep=";")
namesspam=row.names(read.table("~/M2MO/R/spambase.names",sep=":",comment.char='|'))

C10 <- 3    # le co?t de classement de spam au lieu de email
C01 <- 1    # le co?t de classement de email au lieu de spam


#ANALYSE DES DONNEES
#-------------------------------------------------------------------------------------------------------
#Smatrix=as.matrix(spam) ; COV=cov(Smatrix) ; COR=cor(Smatrix) ; write.csv(COR,"~/M2MO/R/cor.csv") ; boxplot(spam$A.1)


#CODAGE SPAMS EN 1 ET EMAILS EN 0
#-------------------------------------------------------------------------------------------------------
spam$spam <-  as.numeric(spam$spam)
spam$spam[1:1813] <- rep(1,1813)
spam$spam[1814:4601] <- rep(0,4601-1813)


#EXTRACTION D UN ECHANTILLON
#-------------------------------------------------------------------------------------------------------
ech <- sample(1:4601, size= 3500)
spamApprentissage <- spam[ech,]
spamTest <- spam[-ech,]


#REGRESSION LOGISTIQUE 
#-------------------------------------------------------------------------------------------------------
glm <- glm(spam~.,data=spamApprentissage,family=binomial)


#SEUIL INTUITIF NAIF A 0.5 

etas <- predict.glm(glm, newdata=spamTest, type="response")
y_chapeau <- as.numeric((etas>0.5))
T <- table(y_chapeau,spamTest$spam)                                                           # matrice de confusion sur base test
GLOBAL_ERROR_RATE_SEUIL_0.5_RL_BASE_TEST <- ((T[2,1]+T[1,2])/length(spamTest$spam))*100       # le taux global d'erreurs sur base test
NOMBRE_FAUX_POSITIFS_SEUIL_0.5_RL_BASE_TEST <- as.numeric(T[2,1])                             # le nombre de faux positifs sur base test
TAUX_FAUX_POSITIFS_SEUIL_0.5_RL_BASE_TEST <- (T[2,1]/length(which(spamTest$spam==0)))*100     # le taux de faux positifs (nombre d'emails pr?dits comme spams / nombre d'emails) sur base test


#UTILISATION D'UNE FONCTION DE COUT 

pred <-prediction(etas,spamTest$spam)
cutoffs <- slot(pred,"cutoffs")
fp <- slot(pred,"fp")
tp <- slot(pred,"tp")
tn <- slot(pred,"tn")
fn <- slot(pred,"fn")
fpr <- fp[[1]]/(length(which(spamTest$spam==0)))                                               # taux de faux positifs suivant les diff?rents seuils 
fnr <- fn[[1]]/(length(which(spamTest$spam==1)))                                               # taux de faux n?gatifs
tpr <- tp[[1]]/(length(which(spamTest$spam==1)))                                               # taux de vrais postifs
cout <- C10*fp[[1]] + C01*fn[[1]]                                                              # fonction de co?t  
seuil_optimal <- cutoffs[[1]][which.min(cout)]                                                 # seuil optimal minimisant la fonction de co?t
seuil_optimal <- as.numeric(seuil_optimal)
y_chapeau <- as.numeric((etas>seuil_optimal))                                                  # prediction associ?e sur base test
T_so <- table(y_chapeau,spamTest$spam)                                                            # matrice de confusion  sur base test
GLOBAL_ERROR_RATE_SEUIL_OPTIMAL_RL_BASE_TEST <- ((T_so[2,1]+T_so[1,2])/length(spamTest$spam))*100
NOMBRE_FAUX_POSITIFS_SEUIL_OPTIMAL_RL_BASE_TEST <- as.numeric(fp[[1]][which.min(cout)])
TAUX_FAUX_POSITIFS_SEUIL_OPTIMAL_RL_BASE_TEST <- fpr[which.min(cout)]*100


#COURBE ROC et cALCUL DE l'AUC

perf =  performance(pred, measure = "tpr", x.measure = "fpr") 
fp_rate=slot(perf,"x.values")                                                                   # taux de faux positifs, m?mes valeurs que fpr
tp_rate=slot(perf,"y.values")
plot(perf, colorize=TRUE,lwd=3,spread.estimate="stderror",plotCI.lwd=2)                         # courbe roc "? la main" => plot(fp_rate[[1]],tp_rate[[1]],col="blue",type="l",xlab="False positive rate",ylab="True positive rate")
polygon(c(fpr+0.01,1),c(tpr-0.01,0),col="grey", border=NA, density=14)
nc=as.numeric(length(cutoffs[[1]]))-1
AUC=0
for(i in 1:nc)
{
  AUC=AUC+tpr[i+1]*(fpr[i+1]-fpr[i])
}
AUC


#SELECTION DE MODELE FORWARD + PERFORMANCES 

glm0 <- glm(spam~1,data=spamApprentissage,family=binomial)
FOR <-  step(glm0,list(upper=glm),direction='forward')
glmFOR <-  glm(formula(FOR),data=spamApprentissage,family=binomial)
etasFOR <- predict.glm(glmFOR, newdata=spamTest, type="response")
predFOR <-prediction(etasFOR,spamTest$spam)
cutoffsFOR <- slot(predFOR,"cutoffs")
fpFOR <- slot(predFOR,"fp")
tpFOR <- slot(predFOR,"tp")
tnFOR <- slot(predFOR,"tn")
fnFOR <- slot(predFOR,"fn")
fprFOR <- fpFOR[[1]]/(length(which(spamTest$spam==0)))                                                          # taux de faux positifs
fnrFOR <- fnFOR[[1]]/(length(which(spamTest$spam==1)))                                                          # taux de faux n?gatifs
tprFOR <- tpFOR[[1]]/(length(which(spamTest$spam==1)))                                                          # taux de vrais postifs
coutFOR <- C10*fpFOR[[1]] + C01*fnFOR[[1]]
seuil_optimalFOR <- cutoffsFOR[[1]][which.min(coutFOR)]                                                         # seuil optimal minimisant la fonction de co?t
seuil_optimalFOR <- as.numeric(seuil_optimalFOR)
y_chapeauFOR <- as.numeric((etasFOR>seuil_optimalFOR))                                                          # prediction associ?e
TFOR <- table(y_chapeauFOR,spamTest$spam)                                                                       # table de confusion sur base test
GLOBAL_ERROR_RATE_SEUIL_OPTIMAL_RL_FORWARD_BASE_TEST <- ((TFOR[2,1]+TFOR[1,2])/length(spamTest$spam))*100       # le taux global d'erreurs sur base test
NOMBRE_FAUX_POSITIFS_SEUIL_OPTIMAL_RL_FORWARD_BASE_TEST <- as.numeric(fpFOR[[1]][which.min(coutFOR)])           # le nombre faux positifs sur base test 
TAUX_FAUX_POSITIFS_SEUIL_OPTIMAL_RL_FORWARD_BASE_TEST <- fprFOR[which.min(coutFOR)]*100                         # le taux de faux positifs sur base test
perfFOR <-   performance(predFOR, measure = "tpr", x.measure = "fpr")  
plot(perfFOR, colorize=TRUE,colorize.palette=rev(rainbow(150,start=4/7, end=5/7)),lwd=3,spread.estimate="stderror",plotCI.lwd=2)
ncFOR=as.numeric(length(cutoffsFOR[[1]]))-1
AUC_FOR=0
for(i in 1:ncFOR)
{
  AUC_FOR=AUC_FOR+tprFOR[i+1]*(fprFOR[i+1]-fprFOR[i])
}
AUC_FOR


#ANALYSE DISCRIMINANTE
#-------------------------------------------------------------------------------------------------------

LDA <-  lda(spam~.,data=spamApprentissage)
etas_lda <- predict(LDA, spamTest)
pred_lda <- prediction(etas_lda$posterior[,2],spamTest$spam)
cutoffs_lda <- slot(pred_lda,"cutoffs")
fp_lda <- slot(pred_lda,"fp")
tp_lda <- slot(pred_lda,"tp")
tn_lda <- slot(pred_lda,"tn")
fn_lda <- slot(pred_lda,"fn")
fpr_lda <- fp_lda[[1]]/(length(which(spamTest$spam==0)))
fnr_lda <- fn_lda[[1]]/(length(which(spamTest$spam==1)))
tpr_lda <- tp_lda[[1]]/(length(which(spamTest$spam==1)))
cout_lda <- C10*fp_lda[[1]] + C01*fn_lda[[1]]                                              # m?me technique que pour la RL, passage par minimisation de la fonction de co?t 
seuil_optimal_lda <- cutoffs_lda[[1]][which.min(cout_lda)]
seuil_optimal_lda <- as.numeric(seuil_optimal_lda)
y_chapeau_lda <- as.numeric((etas_lda$posterior[,2] > seuil_optimal_lda))
T_LDA <- table(y_chapeau_lda, spamTest$spam)                                                     
GLOBAL_ERROR_RATE_LDA_BASE_TEST <- ((T_LDA[2,1]+T_LDA[1,2])/length(spamTest$spam))*100
TAUX_FAUX_POSITIFS_LDA_BASE_TEST <- (T_LDA[2,1]*100)/(T_LDA[1,1]+T_LDA[2,1])
perf_lda <-   performance(pred_lda, measure = "tpr", x.measure = "fpr")  
plot(perf_lda, colorize=TRUE,colorize.palette=rev(rainbow(150,start=3/7, end=4/7)),lwd=3,spread.estimate="stderror",plotCI.lwd=2)
nc_lda=as.numeric(length(cutoffs_lda[[1]]))-1
AUC_lda=0
for(i in 1:nc_lda)
{
  AUC_lda=AUC_lda+tpr_lda[i+1]*(fpr_lda[i+1]-fpr_lda[i])
}
AUC_lda

#SOLUTION  2

probas_posteriori_lda <- as.matrix(etas_lda$posterior)
y_chapeau_lda_2 <- as.numeric((C10*probas_posteriori_lda[,1]<C01*probas_posteriori_lda[,2]))  # On classe x en G1 si C12P(G2/x) < C21P(G1/x)
T_LDA2 <- table(y_chapeau_lda_2, spamTest$spam)
GLOBAL_ERROR_RATE_LDA2_BASE_TEST <- ((T_LDA2[2,1]+T_LDA2[1,2])/length(spamTest$spam))*100
NOMBRE_FAUX_POSITIFS_LDA2_BASE_TEST <- as.numeric(T_LDA2[2,1])
TAUX_FAUX_POSITIFS_LDA2_BASE_TEST <- (T_LDA2[2,1]*100)/(T_LDA2[1,1]+T_LDA2[2,1])

#SANS COUTS

TLDA2 <- table(etas_lda$class, spamTest$spam)                                                      # SANS CONSIDERER LES COUTS
GLOBAL_ERROR_RATE_LDA_SANS_COUTS_BASE_TEST <- ((TLDA2[2,1]+TLDA2[1,2])/length(spamTest$spam))*100  # erreur globale sans affecter des co?ts diff?rents aux erreurs de type 1 ou 2
NOMBRE_FAUX_POSITIFS_LDA_SANS_COUTS_BASE_TEST <- as.numeric(TLDA2[2,1])                            # le nombre faux positifs 
TAUX_FAUX_POSITIFS_LDA_SANS_COUTS_BASE_TEST <- (TLDA2[2,1]*100)/(TLDA2[1,1]+TLDA2[2,1])            # le taux de faux positifs 


#ARBRE DE CLASSIFICATION
#-------------------------------------------------------------------------------------------------------

spamApprentissage$spam <- factor(spamApprentissage$spam, levels=c(0,1), labels=c("email","spam"))   # on repasse en facteurs pour avoir un arbre de classification
tree <- tree(spam~.,data=spamApprentissage)                                                         # on laisse les param?tres par d?faut pour la constructionde l'arbre
plot(tree)
text(tree)                                                                                          # comment avoir les l?gendes de spaminfo plutot que A1, A2,.... ??
spam_chapeau_tree <- predict(tree,newdata=spamTest,type="class")
TTREE <- table(spam_chapeau_tree, spamTest$spam)                                                    # matrice de confusion 
GLOBAL_ERROR_RATE_TREE_BASE_TEST <- ((TTREE[2,1]+TTREE[1,2])/length(spamTest$spam))*100             # pourcentage d'erreurs (erreur globale)
NOMBRE_FAUX_POSITIFS_TREE_BASE_TEST <- as.numeric(TTREE[2,1])                                       # le nombre faux positifs
TAUX_FAUX_POSITIFS_TREE_BASE_TEST <- (TTREE[2,1]*100)/(TTREE[1,1]+TTREE[2,1])                       # pourcentage de faux positifs      

#SOLUTION 2

tree2 <- rpart(spam~.,data=spamApprentissage, method="class", cp=1e-5,parms=list(split='information'))
plotcp(tree2, col=c("blue"))
table_tree2 = printcp(tree2)
plot(tree2,uniform=T,compress=T,margin=.05)
text(tree2,use.n=T)
dev.print(postscript,'spamplot2.ps',paper='special',height=8.5,width=11)
print(tree2,cp=.05)
spam_chapeau_tree2 <- predict(tree2,type='class')
T_TREE2= table(predicted=spam_chapeau_tree2,actual=spamApprentissage$spam)
GLOBAL_ERROR_RATE_TREE2_BASE_TEST <- ((T_TREE2[2,1]+T_TREE2[1,2])/length(spamTest$spam))*100            # pourcentage d'erreurs (erreur globale)
NOMBRE_FAUX_POSITIFS_TREE2_BASE_TEST <- as.numeric(T_TREE2[2,1])                                       # le nombre faux positifs
TAUX_FAUX_POSITIFS_TREE2_BASE_TEST <- (T_TREE2[2,1]*100)/(T_TREE2[1,1]+T_TREE2[2,1])                     # pourcentage de faux positifs    


#BAGGING
#-------------------------------------------------------------------------------------------------------


bagging <- bagging(spam~.,data=spamApprentissage)
y_chapeau_bagging <- predict(bagging, newdata=spamTest)
TBAGG <- table(y_chapeau_bagging, spamTest$spam)
((TBAGG[2,1]+TBAGG[1,2])/length(spamTest$spam))*100


#RANDOM FOREST
#-------------------------------------------------------------------------------------------------------

forest <- randomForest(spam~., data=spamApprentissage)
spam_chapeau_random_forest <- predict(forest,newdata=spamTest)
T_FOREST <- table(spam_chapeau_random_forest, spamTest$spam)
GLOBAL_ERROR_RATE_RANDOM_FOREST_BASE_TEST <- ((T_FOREST[2,1]+T_FOREST[1,2])/length(spamTest$spam))*100
FP_RATE_RANDOM_FOREST_BASE_TEST <- (T_FOREST[2,1]*100)/(T_FOREST[1,1]+T_FOREST[2,1]) 



















