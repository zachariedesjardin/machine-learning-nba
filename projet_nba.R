library(stringr)
library(tidyverse) #ggplot2 inclus
library(visdat)


### IMPORTATION DES DONNEES ###
setwd("/Documents/projet_nba")


#dataset 1
stats_nba <- read.csv2("players_stats_by_season_full_details.csv", header = TRUE, sep = ",", dec = ".")
stats_nba <- subset(stats_nba,League == "NBA" & Stage == "Regular_Season",
          select = -c(League,Stage,birth_year,birth_month,birth_date,height,weight,
                    nationality,high_school,draft_round,draft_pick,draft_team))

head(stats_nba)


stats_nba <- stats_nba[2260:4658,]
stats_nba[,c(1,3)] <- lapply(stats_nba[,c(1,3)],factor)
stats_nba$id <- paste(stats_nba$Season,stats_nba$Player, sep = ".") #on creer un id pour merge apres


#dataset 2
all_nba <- read.csv2("all_nba_players.csv", header = TRUE, sep = ",")
all_nba$Lg <- NULL ; all_nba$Tm <- NULL

head(all_nba)

all_nba <- all_nba[1:30,] #on conserve les données après 2010-2011
all_nba$Season <- sub("-"," - 20",all_nba$Season, fixed = TRUE) #on remplace 2010-11 en 2010 - 2011
all_nba <- all_nba %>% pivot_longer(-Season) #on met tout sur une seul colonne
all_nba$name <- NULL

all_nba$value <- substr(all_nba$value,1,nchar(all_nba$value)-2) #on retire les postes des joueurs
all_nba["value"] <- str_replace_all(all_nba$value, fixed("?"), "c") #on remplace les caractères qui ne sont pas passé (c avec accent des joueurs des Balkans)
all_nba["value"] <- str_replace_all(all_nba$value, fixed("ó"), "o") #on remplace les caractères qui ne sont pas passé (o avec accent 


all_nba$id <- paste(all_nba$Season,all_nba$value, sep = ".") #on creer un id pour merge apres



#fusion des 2 datasets
nba <- merge(stats_nba,all_nba, by = "id",all.x = TRUE)

nba[!is.na(nba$value),"value"] <- "Yes"
nba[is.na(nba$value),"value"] <- "No"
names(nba)[names(nba) == "value"] <- "All_NBA_Team"
nba$id <- NULL ; names(nba)[names(nba) == "Season.x"] <- "Season" ; nba$Season.y <- NULL

nba$All_NBA_Team <- as.factor(nba$All_NBA_Team)
nba$Player <- str_to_title(nba$Player)


### NETTOYAGE DU JEU DE DONNEES ###
#donnees manquantes 
sum(nba$All_NBA_Team=="Yes")
count(subset(nba, All_NBA_Team=="Yes"), Season)
#en recherchant on voit qu'il manque les données pour le joueur : dwight howard (11-12)
nba[nba$Player=="Dwight Howard",] #en effet il manque la saison 2011-2012
nba <- rbind(nba,nba["55",])
nba["5510",]$Season <- "2011 - 2012"
nba <- nba[order(nba$Season,nba$Player),]
nba$All_NBA_Team <- as.factor(nba$All_NBA_Team)
sum(nba$All_NBA_Team=="Yes")

vis_miss(nba)

nba$FGA <- round(nba$FGM/nba$FGA*100,2) ; nba$X3PA <- round(nba$X3PM/nba$X3PA*100,2) ; nba$FTA <- round(nba$FTM/nba$FTA*100,2)
nba[is.na(nba)] <- 0

nba[,c(5,6,8,10,12:20)] <- apply(nba[,c(5,6,8,10,12:20)],2,function(x) round(x/nba$GP,2))
nba <- rename(nba,'PFG' = FGA,'PX3P' = X3PA,'PFT' = FTA,'FG' = FGM,'X3P' = X3PM,'FT' = FTM,'Height' = height_cm, 'Weight' = weight_kg)


moyenne_saison <- aggregate(. ~ Season,
                            subset(nba, select = 
                                     -c(Player,Team,GP,MIN,Height,Weight,All_NBA_Team)),mean)

nba <- subset(nba,MIN > 28 & GP > 40)











### PARTIE 1 ###
summary(nba)
str(nba)

ggplot(moyenne_saison,aes(Season,X3P)) +
  geom_point() +
  labs(x = "Saison", y = "Nombre de tirs à 3 points par match", 
       title = "Évolution du nombre moyen de tirs à 3 points par match")

lebron <- subset(nba,Player == "Lebron James")
tab <- rbind(apply(lebron[,4:20],2,mean),apply(nba[,4:20],2,mean))
row.names(tab) <- c("LeBron James","Joueur moyen")
tab


ggplot(nba,aes(Season,X3P)) + geom_point() + facet_wrap(~ Team) +
  labs(x = "Saison", y = "Nombre moyen de tirs à 3 points par match", 
       title = "Évolution du nombre moyen de tirs à 3 points par match")

which.min(lebron$X3P)
which.max(lebron$X3P)

ggplot(lebron, aes(Team, BLK, fill = Team)) +
  geom_boxplot(outlier.color = "purple") +
  labs(x = "Équipe", y = "Nombre des contres par match", 
       title = "Boxplot des contres selon l'équipe de LeBron James")


nba[nba$PTS > 10 & nba$AST > 10 & nba$REB > 10,] #joueur en triple double sur une saison

apply(nba[nba$Player=="Lebron James",c(4,5,6)],2,max)

nb_all_nba <- count(subset(all_nba, select = - id), value) %>% arrange(desc(n))

heart %>% 
  filter(age >=65 ) %>% 
  group_by(sexe,coeur) %>%
  summarise(nb = n())

### PARTIE 2 : APPRENTISSAGE NON SUPERVISE ###
library(GGally)
library(gghighlight)

#Correlations
ggcorr(nba[6:20]) + labs(title = "Matrice de corrélation des variables du jeu de données NBA")
cor(nba$PTS,nba$FG)

nba_km <- data.frame(scale(subset(nba,select =-c(Season,Player,Team,GP,MIN,TOV,PF,REB,Height,Weight,All_NBA_Team)),
                             center=T,scale=T))


#Choix du nombre de classes
ratio_ss <- data.frame(cluster = seq(from = 1, to = 7, by = 1)) 
for (k in 1:7) {
  km_model <- kmeans(x=nba_km, centers= k,nstart=20 ,iter.max=50)
  ratio_ss$ratio[k] <- km_model$tot.withinss / km_model$totss
}

ratio_ss
ggplot(ratio_ss, aes(cluster, ratio)) + 
  geom_line() +
  geom_point() + labs(x = "Nombre de clusters", y = "Inertie inter-classes / Inertie totale",
                      title = "Graphique du coude")


#Clustering à 3 classes
modelisation <- kmeans(nba_km,centers = 3,iter.max = 50, nstart = 20)

modelisation$iter
modelisation$ifault
nba_km$cluster <- as.factor(modelisation$cluster)


#Graphiques
nba$cluster <- as.factor(modelisation$cluster)

pairs(nba[,c(6:11,14:17,19:20)],col=c('indianred','limegreen','dodgerblue1')[modelisation$cluster])

ggplot(nba, aes(x = PTS, y = REB, color = cluster)) + 
  geom_point() +
  labs(x = "Nombre de points par match", y = "Nombre de rebonds par match", 
       title = "Répartition des joueurs selon le nombre de points et de rebonds")

ggplot(nba, aes(x = cluster, y = Height, fill = cluster)) + 
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 2) +
  labs(x = "", y = "Taille", 
       title = "Boxplot des tailles des joueurs")
  
ggplot(nba, aes(x = BLK, fill = cluster)) + 
  geom_histogram() +
  gghighlight() +
  facet_wrap(~cluster) +
  labs(x = "Nombre de contres par match", y = "Nombre de joueurs", 
       title = "Répartition des clusters selon le nombre de contres par match")

ggplot(nba, aes(x = AST, y = FG, color = cluster)) + 
  geom_point() +
  labs(x = "Nombre de passes décisives par match", y = "Nombre de tirs réussis par match", 
       title = "Répartition des joueurs selon le nombre de passes décisives et de tirs réussis")




#Clustering à 10 classes
modelisation2 <- kmeans(nba_km,centers = 10,iter.max = 50, nstart = 20)
modelisation2
nba_km$cluster <- as.factor(modelisation2$cluster)


#Caracterisation des classes
clusters2 <- print(aggregate(nba[,6:22],by=list(nba_km$cluster),mean))
write.table(clusters2, "Clusters.xls", col=NA, sep="\t",dec=",")


#Graphiques
nba$cluster <- as.factor(modelisation2$cluster)

ggplot(data=nba, aes(x=PTS, y=REB, color=cluster )) + 
  geom_point() +
  labs(x = "Nombre de points", y = "Nombre de rebonds", 
       title = "Répartition des clusters selon le nombre de points et de rebonds")


ggplot(data=nba[nba$Player=="Stephen Curry",], aes(x=Season, y=PTS, color=cluster, group = 1)) + 
  geom_point() +
  geom_line() +
  labs(x = "Saison", y = "Nombre de points", 
       title = "Évolution du nombre de points par match de Stephen Curry")

ggplot(data=nba[nba$Player=="Zach Randolph",], aes(x=Season, y=PTS, color=cluster, group = 1)) + 
  geom_point() +
  geom_line() +
  labs(x = "Saison", y = "Nombre de points", 
       title = "Évolution du nombre de points par match de Zach Randolph")



#calcul des individus les plus proches de la moyenne de chaque cluster
distances <- rep(0, nrow(nba))

n <- 1 #choix du cluster

for (i in 1:nrow(nba)) {
  distances[i] <- sqrt((nba_km[i, 1] - centers2[n,1])^2 + (nba_km[i, 2] - centers2[n,2])^2 + 
                         (nba_km[i, 3] - centers2[n,3])^2 + (nba_km[i, 4] - centers2[n,4])^2 +
                         (nba_km[i, 5] - centers2[n,5])^2 + (nba_km[i, 6] - centers2[n,6])^2 + 
                         (nba_km[i, 7] - centers2[n,7])^2 + (nba_km[i, 8] - centers2[n,8])^2 +
                         (nba_km[i, 9] - centers2[n,9])^2 + (nba_km[i, 10] - centers2[n,10])^2 + 
                         (nba_km[i, 11] - centers2[n,11])^2 + (nba_km[i, 12] - centers2[n,12])^2)
}

rangs <- rank(distances)
nba[which(rangs <= 3), c("Player","Season")]





### PARTIE 3 : APPRENTISSAGE SUPERVISE ###
library(randomForest)
library(caret)

sum(nba$All_NBA_Team=="Yes")
#train - test
nba_ar <- subset(nba,select =-c(Season,Player,GP,MIN,FT,PF,TOV,REB,Team,Height,Weight))

set.seed(1562)
decoupage <- createDataPartition(nba_ar$All_NBA_Team, p=0.8, list = FALSE)
train  <- nba_ar[decoupage,]
test  <- nba_ar[-decoupage,]

modele <- randomForest(All_NBA_Team ~ ., data = train, ntree = 500, mtry = 3)


print(modele)
print(modele$confusion)


modele$importance[order(modele$importance[, 1], decreasing = TRUE), ]
varImpPlot(modele,main = "Importance des variables")

plot(All_NBA_Team ~ PTS, data = train, main = "Répartition des joueurs selon leur nombre de points", 
     xlab = "Nombre de points par match", ylab = "Proportion de joueurs dans la All-NBA Team")
plot(All_NBA_Team ~ DRB, data = train, main = "Répartition des joueurs selon leur nombre de rebonds défensifs",
xlab = "Nombre de rebonds défensifs par match", ylab = "Proportion de joueurs dans la All-NBA Team")
plot(All_NBA_Team ~ PX3P, data = train, main = "Répartition des joueurs selon leur pourcentage au tir à 3 points",
     xlab = "Pourcentage de réussite aux tirs à 3 points", ylab = "Proportion de joueurs dans la All-NBA Team")


modele$votes[747:757,]


#choix de ntree
oob.error.data <- data.frame(
  Trees=rep(1:nrow(modele$err.rate), times=3),
  Type=rep(c("OOB", "Yes", "No"), each=nrow(modele$err.rate)),
  Error=c(modele$err.rate[,"OOB"], 
          modele$err.rate[,"Yes"], 
          modele$err.rate[,"No"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type)) +
  labs(x = "Nombre d'arbres", y = "Erreur", 
       title = "Évolution des taux d'erreurs selon le nombre d'arbres")




# Prediction sur test avec le modele
test$predicted <- predict(modele, test)
table(test$predicted, test$All_NBA_Team) 






