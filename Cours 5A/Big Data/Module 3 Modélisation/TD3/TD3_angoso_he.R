# P(topic|document)
# P(mot|topic)
# document c'est le lieu de rencontre, le bar
# topic c'est les centres d'intérêt que l'on veut retrouver et mot sont les personnes que l'on va retrouver dans le lieu par rapport au sujet
# 

DataURL <- read.csv ("urls.csv", sep =",")
df <- as.data.frame (DataURL)
dfUrl <- df [!(is.na (df$Text) | df$Text =="") , ]
dfUrl <- dfUrl [! dfUrl$Text =="0" , ]

library (tm)
library (ggplot2)
library (lsa)
library (scatterplot3d)
library (SnowballC)
docs <- Corpus (VectorSource(dfUrl$Text [1: nrow (dfUrl) ]) )
docs <- tm_map (docs, removePunctuation)
docs <- tm_map (docs, removeNumbers)
docs <- tm_map (docs, tolower)

docs <- tm_map (docs, removeWords, stopwords("french") )
docs <- tm_map (docs, stripWhitespace)
docs <- tm_map (docs, PlainTextDocument)
dtm <- DocumentTermMatrix (docs)
tdm <- TermDocumentMatrix (docs)

# a)
# Vecteur contenant un ensemble de documents

# b)
# On l'applique au vecteur Text du dataframe pour obtenir un ensemble de documents

# c)
# Nous avons nettoyé le texte à l'aide de la fonction tm_map, on a : retiré la ponctuation et les chiffres, on a mis les mots en minuscules, puis on a retiré les mots courants en français, les espaces en trop.



# 1.2) Topic Modelling

library(topicmodels)

burnin <- 2000
iter <- 500
thin <- 500
seed <- list (2003, 5, 63, 100001, 765)
nstart <- 5
best <- TRUE

k <- 10

ldaOut <- LDA (dtm, k, method = "Gibbs", control = list (nstart = nstart, seed = seed, best = best, burnin = burnin, iter = iter, thin = thin))

ldaOut.topics <- as.matrix (topics (ldaOut))
ldaOut.topics

ldaOut.terms <- as.matrix (terms ( ldaOut ,10) )
ldaOut.terms

topicProbabilities <- as.data.frame (ldaOut@gamma)
topicProbabilities

# a) L'échantillonage de Gibbs permet de redémarrer 

# b) Nous avons paramétré k = 5 classes

# c) Elle nous permet ici de calculer la probabilité qu'un mot soit dans un topic et qu'un topic soit dans un document

# d) Best indique à l'algorithme de renvoyer les résultats avec la plus haute probabilité postérieure


# e) 


# f) 


# g)


# h)


# i) J'ai fait passer le k à 10. J'ai des topic sur le foot, la politique (harkis et hollande),...

# 1.3 Représentation matricielle


findAssocs (dtm , "françois",corlimit =0.7)

matrice_docs_termes <- (dtm)
findFreqTerms (matrice_docs_termes, 50)

# test de zipf
#je sais qu'une proba décroit, du coup je fais cette allocation pour vous proposer des topics'

inspect (removeSparseTerms(matrice_docs_termes, 0.4) )

Zipf_plot (matrice_docs_termes)
# Probabilité qu'un nouveau mot apparaisse



# plus la taille des docs parcouru augmente, plus le vocab va augmenter