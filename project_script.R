# projet d'analyse seur le teletravail et la qualité d'emploi
#importation de tous libraries nécessaires
library(dplyr)
library(questionr)
library(ade4)
library(ggplot2)
library(tidyverse)
library(gtsummary)
library(forcats)
library(hrbrthemes)
library(viridis)


#Definition de la repertoire de travail
setwd("D:/School/EGC M1/S1/Techniques Quantitatives/Projet/script_and_report")
#importation de la base des données
db_emploi <- read.csv("EWCS_2015.csv", sep = ",")

#Filtering out observations of france and variables of interest only
ewcs_fr = db_emploi %>% select(Country, Q30i, Q35e,Q2a, Q2b,Q24,Q88) %>%
  filter(Country== "France")
#Conversion to numeric form the  necessary variables
#Age
ewcs_fr$Q2b <-as.numeric(ewcs_fr$Q2b)
mean(ewcs_fr$Q2b,na.rm =T)
#Temps de travail
ewcs_fr$Q24 <-as.numeric(ewcs_fr$Q24)

#Factorisation des variables necessaires
#Niveau de satisfaction
iorder(ewcs_fr$Q88)
#Reordanancement de variable age en factor
ewcs_fr$Q2a <- factor(ewcs_fr$Q2a,
                      levels = c('Male', 'Female')
)

ewcs_fr$Q2a <- fct_recode(ewcs_fr$Q2a,
                          Homme= "Male",
                          Femme = "Female"
                          
                          )


## Réordonnancement de ewcs_fr$Q88 en ewcs_fr$Q88_corr(Niveau de satisfaction)
ewcs_fr$Q88_corr <- factor(ewcs_fr$Q88,
  levels = c(
    "Very satisfied", "Satisfied", "Not very satisfied", "DK/no opinion (spontaneous)",
    "Not at all satisfied"
  )
) 


iorder(ewcs_fr$Q30i)
iorder(ewcs_fr$Q35e)

## Réordonnancement de ewcs_fr$Q35e(Travail à domicile)
ewcs_fr$Q35e <- factor(ewcs_fr$Q35e,
  levels = c(
    "Daily", "Several times a week", "Several times a month", "Less often",
    "Never", "DK (spontaneous)", "Refusal (spontaneous)"
  )
)## Réordonnancement de ewcs_fr$Q30i(Frequence d'utilisation des ordinateurs)
ewcs_fr$Q30i <- factor(ewcs_fr$Q30i,
  levels = c(
    "All of the time", "Almost all of the time", "Around 3/4 of the time",
    "Almost never", "Around 1/4 of the time", "Around half of the time",
    "Never", "DK (spontaneous)"
  )
)
#Gestion des variables manquantes

ewcs_fr$Q35e <-fct_recode(ewcs_fr$Q35e,
                          NULL = "DK (spontaneous)",
                          NULL = "Refusal (spontaneous)"
                          
                          )
ewcs_fr$Q30i <-fct_recode (ewcs_fr$Q30i,
                           NULL = "DK (spontaneous")
ewcs_fr$Q88_corr<- fct_recode(ewcs_fr$Q88_corr,
                              NULL="DK/no opinion (spontaneous)")

#Combinaison des variables Q30i et Q35e
ewcs_fr <- ewcs_fr %>%
  mutate(digital_worker=if_else(Q30i=="All of the time" | Q30i=="Almost all of the time"| Q30i=="Around 3/4 of the time"| Q30i=="Around half of the time", "digital", "non-digital"))%>%
  mutate(home_worker=if_else(Q35e=="Daily" |Q35e=="Several times a week" |Q35e=="Several times a month", "home_worker", "office_worker"))%>%
  mutate(teleworker = if_else(digital_worker=='digital'& home_worker=="home_worker","teleworker", "non_teleworker"))


summary(ewcs_fr$teleworker)

#Conversion de teleworker en factor
ewcs_fr$teleworker <- factor(ewcs_fr$teleworker, 
                             
                            levels = c("teleworker", "non_teleworker")
                              )

#Analyse Univaries
#Age
summary(ewcs_fr$Q2b)
hist(ewcs_fr$Q2b, breaks=30, xlim=c(0,80), col=rgb(1,0,0,0.5), xlab="Age", 
     ylab="Nombre des personnes", main="Distribution de l'age de la population" )
      x <- seq(min(!is.na(ewcs_fr$Q2b)), max(!is.na(ewcs_fr$Q2b)), length = 40) 
      y <- dnorm(!is.na(ewcs_fr$Q2b), mean = mean(!is.na(ewcs_fr$Q2b)), sd = sd(!is.na(ewcs_fr$Q2b))) 
      #lines(x, y, col = "black", lwd = 2)
      
      
#Age Distribution
#courbe <- rnorm(1526, mean=43.05,  sd = sd(!is.na(ewcs_fr$Q2b)))
ewcs_fr %>%
  filter(!is.na(Q2b))%>%
  ggplot( aes(x=Q2b)) +
  geom_histogram(
    binwidth= 5, color="#e9ecef", fill ="#69b3a2",alpha=0.9, position = 'identity') +
  xlab("Age") + ylab("Nombre de personnes")+
  labs(fill="") +
  stat_function(fun = dnorm, args = list(mean = mean(!is.na(ewcs_fr$Q2b)), sd = sd(!is.na(ewcs_fr$Q2b))))

#temps de travail
summary(ewcs_fr$Q24)
hist(ewcs_fr$Q24, breaks=20, xlim=c(0,140), col="#423475", xlab="Temps Travaillé", 
     ylab="Nombre des personnes", main="Distribution du temps travaillé de la population" )

#Sexe
summary(ewcs_fr$Q2a)
#Distribution
#genderColors <- c(if_else(levels(ewcs_fr$Q2a)=="Homme", "#479DF5", "#F785F7"))
#Graphique de temps trvaillé par sexe
ewcs_fr %>%
  filter(!is.na(Q2a) & !is.na(Q2b) & !is.na(Q24)) %>%
  ggplot(aes(x=Q2a, y=Q24, fill=Q2a)) + 
  geom_violin(width=1, size=0.2,trim=FALSE,) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete= TRUE)+
  xlab("Sexe") + ylab("Temps travaillé")+
  stat_summary(fun=mean, geom="point", shape=23, size=2)
  

boxplot(ewcs_fr$Q2b ~ ewcs_fr$Q2a , 
        col="#ED62F5" , 
        ylab="Age" , xlab="Sexe")

#teleworker
ewcs_fr %>%
  select (teleworker) %>%
  tbl_summary(
    include = c("teleworker")
  )

#Niveau de satisfaction
ewcs_fr %>%
  select (Q88_corr) %>%
  tbl_summary(
    include = c("Q88_corr")
  )

#Analyses Bivaries
#Teletravail sur Le niveau de satisfaction
ewcs_fr %>%
  tbl_summary(
    include = c("teleworker","Q88_corr", "Q2a"),
    by = teleworker
    ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2))

xtabs(~ teleworker + Q88_corr, ewcs_fr)
prop.table(table(ewcs_fr$teleworker, ewcs_fr$Q2a))

t.test( !is.na(Q2b) ~ !is.na(Q24), data = ewcs_fr) # significativité de l'age sur le temps de travail
t.test( !is.na(Q2b) ~ !is.na(Q88_corr), data = ewcs_fr) # Age et niveau de satisfaction
t.test( !is.na(Q24) ~ !is.na(Q88_corr), data = ewcs_fr) # Temps de travail sur niveau de satisfaction
#Sexe sur la satisfaction

#Temps de travail et la niveau de satisfaction
ewcs_fr %>%
  filter(!is.na(Q88_corr) & !is.na(Q24)) %>%
  ggplot(aes(x=Q88_corr, y=Q24, fill=Q88_corr)) + 
  geom_violin(width=1, size=0.2,trim=FALSE,) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete= TRUE)+
  coord_flip() +
  xlab("Niveau de satisfaction") + ylab("Temps travaillé")

#Age et Niveau de satisfaction
ewcs_fr %>%
  filter(!is.na(Q88_corr) & !is.na(Q2b)) %>%
  ggplot(aes(x=Q88_corr, y=Q2b, fill=Q88_corr)) + 
  geom_violin(width=1, size=0.2,trim=FALSE,) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete= TRUE)+
  coord_flip() +
  xlab("Niveau de satisfaction") + ylab("Age")
t.test(!is.na(Q2b) ~ !is.na(Q88_corr), data = ewcs_fr)

#Sexe et Niveau de satisfaction
ewcs_fr %>%
  tbl_summary(
    include = c("Q2a","Q88_corr"),
    by = Q2a
  ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2))
t.test(!is.na(Q2a) ~ !is.na(Q88_corr), data = ewcs_fr)

xtabs(~ teleworker + Q88_corr, ewcs_fr)

## Bivariés entre les variables Independants/Explicatives
#Teletravail et Age
ewcs_fr %>%
  filter(!is.na(teleworker) & !is.na(Q2b)) %>%
  ggplot(aes(x=teleworker, y=Q2b, fill=teleworker)) + 
  geom_violin(width=0.5, size=1,trim=FALSE,) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete= TRUE)+
  xlab("Teleworker") + ylab("Age")

t.test(!is.na(Q2b) ~ !is.na(teleworker), data = ewcs_fr)

##Temps de travail vs Age
cor.test(ewcs_fr$Q2b,ewcs_fr$Q24)
t.test(!is.na(Q2b) ~ !is.na(Q24), data = ewcs_fr)
ewcs_fr %>%
  filter(!is.na(Q2b) & !is.na(Q24)) %>%
  ggplot(aes(x=Q2b, y=Q24)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  xlab('Age') + ylab('Temps de travail')
  theme_ipsum()




##Temps de travail vs Teletravail
t.test(!is.na(Q24) ~ !is.na(teleworker), data = ewcs_fr)

### Preparation des données pour une regression logistique



write.csv(ewcs_fr,"D:/School/EGC M1/S1/Techniques Quantitatives/Projet/projectdfml.csv", row.names = FALSE)






  
  

