
Ertrag <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/ZHAW/1.Semester/1.Research Methods/PiWi Arbeit/ZF_Most_Vereinfacht.csv", sep=";", header=TRUE)
#damit wir direkt die variablen namen ohne Ertrag anschreiben können 
attach(Ertrag)
summary(Ertrag)
names(Ertrag)


#Boxplots der int Daten
boxplot(Beerengewicht.g)
#an welcher Stelle (welche Reihe) steht die Beere?
which(Beerengewicht.g>3.5) #178
#was ist das exakte Gewicht der übergewichtigen Beere? 
Beerengewicht.g[which(Beerengewicht.g>3.5)] #3.875
#Welche Sorte hat die Beere?
Sorte[178]

#Boxplots. zur Übersicht
boxplot(Ertrag$Ertrag.in.Gramm)

boxplot(Ertrag$Anzahl.Trauben)
##auch hier haben wir einen seltsamen Outlier mit über 40 Trauben:
which(Ertragsbildung$Anzahl.Trauben>40)
#wir wollen jetzt die Anzahl der Trauben der Sorte Divico
Ertrag[Sorte == "Divico",] #gibt uns alle Werte von Divico
Ertrag[Sorte == "Divico",Auswahl.Trauben] #sagt uns leider nicht so viel aus
DivicoTrauben <- Ertrag[Sorte == "Divico",9]
#gibt uns die Werte von Anzahl Trauben (9te Spalte) bei Sorte=Divico
mean(DivicoTrauben)

##hier haben wir eine sortierung der Werte nach Anzahl der Trauben
Ertrag[order(Anzahl.Trauben), c(2,5,23)]


#tapply&aggregate: ich möchte nun die means der Traubenanzahl sortiert nach verschiedenen Sorten berechnen. 
#sort gibt mir die Sorte mit dem kleinsten Ertrag zuerst an:
sort(with(Ertrag,tapply(Anzahl.Trauben,Sorte,mean)))

aggregate(Ertrag[,c(4,21,26)],list(Sorte),mean(,na.rm=TRUE))
#im Moment macht er trotz na.rm=TRUE NA beim mean derer Sorten,die weniger als 5 samples haben.
#vllt muss man (x,na.rm=TRUE) bei x die bedingung "wenn Sorte.samples < 5" eingeben. aber wie formuliert man das?
warnings()
#Fragen:



