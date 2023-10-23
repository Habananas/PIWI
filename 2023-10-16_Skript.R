#Importieren von Daten####
library(readr)
library(tidyr)
library(dplyr)

most <- read_delim("mostanalyse.CSV", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
gesundheit  <- read_delim("traubengesundheit.CSV", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

ertrag  <- read_delim("ertragsbildung.CSV", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

#Join von Tables
#most_ertrag <- full_join(ertrag,most,by="sorte_nr")
#gesundheit_ertrag <- full_join(ertrag,gesundheit,by="sorte_nr")

#Für den Vergleich von Ertrag, Mostanalyse und Traubengesundheit ist es sinnvoll, 
#die Ertragsergebnisse mit mean() zusammenzufassen. 
#Es gibt hierfür mehrere möglichkeiten:
#Piping####
most$ertragsdurchschnitt <-  ertrag |> 
  group_by(sorte_nr) |> 
  summarize(ertrag_durchschnitt =mean(ertrag_g))
#hier wird jetzt jedes mal die Sortennr wieder mitdrangehängt. nervg...
#daslöstman, indem man erst  piped und dann joined:
#NAs werden durch na.rm=TRUE gelöst.
durchschnitt <-  ertrag |> 
  group_by(sorte_nr) |> 
  summarize(meanET=mean(ertrag_g, na.rm=TRUE),
            meanBG=mean(beerengew_g, na.rm=TRUE),
            meanAT = mean(anz_trauben, na.rm=TRUE)
            )

Zusammenfassung <- inner_join(most,durchschnitt,gesundheit, by="sorte_nr")

#hier haben wir im DF most die means des Traubengewichts und der Anzahl hinzugefügt.
#Das problem liegt hierbei, dass manche angaben NA enthalten, warum? Weil manche Sorten nur
#zu 4 Stöcken Angaben haben, und das System das nicht verarbeiten kann. 
#Remove NA? hat  funktioniert.

#Explore mit Grafiken####

#ggplot####
library("ggplot2")
#lm methode
ggplot(Zusammenfassung$oechsle,Zusammenfassung$meanET, xlab="oechsle",ylab="Ertragsdurchschnitt")
ggplot(Zusammenfassung$meanET,Zusammenfassung$meanBG, xlab="Ertragsdurchschnitt",ylab="Beerengewicht")



#gam methode statt lm (linear)
ggplot(data=Zusammenfassung,aes(x=meanET, y=oechsle))+geom_point()+
  geom_smooth(method="gam",se=FALSE, color="red")
ggplot(data=Zusammenfassung,aes(x=meanBG, y=oechsle))+geom_point()+
  geom_smooth(method="gam",se=FALSE, color="red")

ggplot(data=ertrag,aes(x=anz_trauben, y=ertrag_g))+geom_point()+
  geom_smooth(method="gam",se=FALSE, color="red")

library(tidyverse)
hist(Zusammenfassung$meanET)
#Ertrag nach Sorte 
ggplot(ertrag, aes(x = ertrag_g, y = sorte, colour=sorte)) +
  geom_point()

#Ertrag mit Beerengewicht
ggplot(Zusammenfassung, aes(x= meanET ,y=meanBG)) +
  geom_point()+ geom_smooth()+ labs(x="Ertragsdurchschnitt", y="Beerengewicht")

#Ertrag mit Anzahl Trauben, mean
ggplot(Zusammenfassung, aes(x= meanET ,y=meanAT)) +
  geom_point()+ geom_smooth()+ labs(x="Ertragsdurchschnitt", y="Anzahl Trauben")

#Ertrag mit Anzahl Trauben, einzelne Werte
ggplot(ertrag, aes(x= ertrag_g ,y=anz_trauben)) +
  geom_point()+ geom_smooth()+ labs(x="Ertrag", y="Anzahl Trauben")

#Ertrag mit Oechsle
ggplot(Zusammenfassung, aes(x= meanET ,y=oechsle)) +
  geom_point()+ geom_smooth()+ labs(x="Ertragsdurchschnitt", y="Oechsle")

#Ertrag mit Stickstoff
ggplot(Zusammenfassung, aes(x= meanET ,y= Nhefev_mgl)) +
  geom_point()+ geom_smooth()+ labs(x="Ertragsdurchschnitt", y="Hefeverfügbarer Stickstoff")


#Tests ####
library(readxl)
#linear Regression
#zwei Variablen, von denen man denkt, dass sie voneineamnder abhängig sind, werden miteinander verglichen
lm(neuerdf$oechsle~neuerdf$ertrag_durchschnitt, data=neuerdf)


#vartest
var.test(Zusammenfassung$oechsle,Zusammenfassung$meanET)
#passt nicht, denn beim var test werden zwei Felder miteinander verglichen

#t-test:


#chisquare 
chisq.test()




