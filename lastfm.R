lastfm <- read.csv("C:/Data/Datamining/lastfm.csv")
lastfm[1:19, ]
length(lastfm$user)
lastfm$user <- factor(lastfm$user)
levels(lastfm$user)
levels(lastfm$artist)
library(arules)     # load arules package for association rule and data mining and frequent item sets
playlist <- split(x = lastfm[, "artist"], f = lastfm$user)  # split into a list of artists
playlist <- lapply(playlist, unique)    # remove artist duplicates
playlist[1:2]   # get the artists listened by user 1 and 2
playlist <- as(playlist, "transactions")   # view data in a class of transactions in arules package
itemFrequency(playlist)# lists the support of 1004 bands, number of times an artist appears on 15000 users
#computes the relative frequency in relative to 15000 users
itemFrequencyPlot(playlist, support = 0.08, cex.names = 1.5)  # finally plotting our data and visualise with support(>.0.08)
musicrules <- apriori(playlist, parameter = list(support = 0.01, confidence = 0.5)) # including confidence and supp=0.01 so that it can't be rare
inspect(musicrules)
inspect(subset(musicrules, subset = lift > 5))  # inspect those having lift > 5
inspect(sort(subset(musicrules, subset = lift > 5), by = "confidence"))  # sorting the result by confidence to make it easier to understand
