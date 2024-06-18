################################################################################

# This is the script to validate the "numeric exchange results".

################################################################################



###############
# Preparation #
###############

# Cleaning the environment
rm(list=ls());gc();

# Loading dependencies
library(readxl,warn.conflicts = F, quietly = T)
library(httr,warn.conflicts = F, quietly = T)
library(tidyverse)
library(dplyr)
source("dependencies.R")



################
# Loading data #
################

source("preparacio_dades_numeric.R")



########################################################
# Preparing the data and defining some useful functions#
########################################################

## Proportion correct some given threshold value T
## Cal fixar-se que parlem de certesa en l'opinió binària, no en 
## l'estimador numèric
correctAtT = function(dist, t, truth) {
  mean((t<=dist) == (t<=truth))
}

## Inverse CDF
distAsPercentile = function(dist, p) {
  as.numeric(quantile(dist, probs=seq(0,1,by=0.01))[p*100+1])
}

## % Correct given threshold as percentile
correctAtP = function(dist1, dist2, p, truth) {
  t = distAsPercentile(dist1, p)
  correctAtT(dist2, t, truth)
}

## Crawl along all the possible threshold values
## And measure outcomes for each dataset and question
# (This takes some time) #
reanalysis = do.call(rbind, lapply(head(seq(0,1,by=0.01), -1)[-1], function(p) {
  do.call(rbind, lapply(unique(d$trial), function(x){
    samp = subset(d, trial==x)
    
    t = distAsPercentile(samp$pre_influence, p)
    
    mu1 = mean(samp$pre_influence)
    med1 = median(samp$pre_influence)
    mu2 = mean(samp$post_influence)
    truth = unique(samp$truth)
    
    predict_shrink = ((t<mu2) & (t>med1)) | ((t>mu2) & (t<med1))

    data.frame(p=p 
               , trial=x
               , pre_influence=correctAtP(samp$pre_influence, samp$pre_influence, p, unique(samp$truth))
               , post_influence = correctAtP(samp$pre_influence, samp$post_influence, p, unique(samp$truth))
               , predict_amplify = !predict_shrink
               , dataset=unique(samp$dataset)
    )
  }))
})) %>% 
  mutate( amplify = ((pre_influence > 0.5) & (post_influence>pre_influence)) | ((pre_influence < 0.5) & (post_influence<pre_influence))
          , change = post_influence - pre_influence
  )



########################################################
########################################################
########################################################



###########################################
# Validació empírica de la Proposició 2a) #
###########################################



########################
# Fem la Fig.3 del TFG #
########################

reanalysis <- reanalysis %>%
  mutate(dataset = case_when(
    dataset == "gurcay"    ~ "Gürçay 2015",
    dataset == "becker"    ~ "Becker 2017",
    dataset == "lorenz2011" ~ "Lorenz 2011"))

# Making the figure
reanalysis %>% 
  mutate(
    accuracy_round = round(pre_influence,01)
  ) %>%
  group_by(accuracy_round, predict_amplify, dataset
           ) %>%
  summarize(
    pre_influence = mean(pre_influence)
    , post_influence = mean(post_influence)
    , change=mean(change)
  ) %>%
  ggplot(aes(x=accuracy_round, y=change, color=predict_amplify)) + 
  geom_line() +
  facet_wrap(.~dataset, scales="free_y") +
  scale_color_manual(values = c("red","black")) +
  labs(y="Change in Accuracy\n(Positive = More Accurate)", x="Initial Accuracy", color="") +
  guides(color=F) +
  #scale_y_continuous(lim=c(-0.11,0.15))+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0.5, linetype="dashed") +
  theme(panel.spacing = unit(2, "lines")) +
  scale_y_continuous(lim=c(-0.5,0.5))+
  scale_x_continuous(lim=c(0,1))+
  neat_theme

# Saving the figure
ggsave("Fig_3.eps", width=8.2, height=2.8)


#EXPLICACIÓ DEL CODI:

#La funció correctAtT calcula la proporció de respostes correctes d'un conjunt de dades en relació amb un valor de llindar 
#(threshold) específic, anomenat t. Per a cada observació en el conjunt de dades, la funció compara si el valor d'aquesta 
#observació és inferior o igual al llindar t, i ho compara amb si el valor real (veritat) d'aquesta observació és inferior 
#o igual al llindar t. Si aquestes dues comparacions coincideixen, significa que la resposta és correcta en relació amb el 
#llindar t, i la funció assigna un valor de 1; en cas contrari, assigna un valor de 0.
#Després, la funció calcula la mitjana d'aquests valors (0s i 1s), que representa la proporció de respostes correctes en 
#relació amb el llindar t. Això es fa comparant si la relació entre el llindar t i les respostes coincideix amb la relació 
#entre el llindar t i els valors reals (veritat) de les observacions.

#La funció, distAsPercentile, calcula el valor corresponent a un percentil específic d'una distribució de dades. La funció 
#Utilitza la funció de distribució acumulativa inversa (CDF inversa) per fer aquest càlcul.
#- Argument dist: Aquest argument representa la distribució de dades per a la qual volem calcular el valor del percentil.
#- Argument p: Aquest argument és el percentil que volem calcular, expressat com un nombre entre 0 i 1.
#La funció realitza els següents passos per calcular el valor corresponent al percentil específic:
#- Utilitza la funció quantile per calcular els percentils de la distribució dist. Amb l'argument probs=seq(0,1,by=0.01), 
#especifica que vol calcular els percentils des del 0% fins al 100% amb un interval del 1%.
#- Multiplica el percentil especificat p per 100 per obtenir la seva equivalent en valor numèric (ja que quantile treballa 
#amb valors del 0 al 100).
#- Suma 1 a aquest valor per compensar l'indexació que comença en 1 a R (ja que els percentils s'indexen començant per 1, no per 0).
#- Finalment, converteix el resultat en un valor numèric utilitzant la funció as.numeric.

#La funció, correctAtP, calcula el percentatge de predicció correcta donat un valor de percentil com a llindar.
#Per entendre com funciona, aquí tens una descripció detallada de cada pas:
#- Argument dist1: Aquest argument representa la primera distribució de dades (per exemple, les prediccions 
#abans de la influència social).
#- Argument dist2: Aquest argument representa la segona distribució de dades (per exemple, les prediccions després de la influència 
#social).
#- Argument p: Aquest argument és el percentil que volem utilitzar com a llindar per calcular el percentatge de predicció correcta, 
#expressat com un nombre entre 0 i 1.
#- Argument truth: Aquest argument representa el valor real o veritat del fenomen que s'està estudiant, que es compara amb les 
#prediccions.
#La funció realitza els següents passos per calcular el percentatge de predicció correcta:
#- Utilitza la funció distAsPercentile per calcular el valor corresponent al percentil p de la primera distribució, dist1, i 
#guarda aquest valor en la variable t.
#- Utilitza la funció correctAtT per calcular el percentatge de predicció correcta utilitzant el valor t com a llindar per a 
#la segona distribució, dist2, comparant-lo amb el valor de veritat truth.


#La part del codi "## Crawl along all the possible threshold values" realitza una anàlisi exhaustiva sobre tots els possibles 
#valors de llindar i mesura els resultats per a cada conjunt de dades i pregunta. Tot seguit podem veure una descripció detallada 
#de com funciona:
#1. Funció do.call(rbind, lapply(head(seq(0,1,by=0.01), -1)[-1], function(p) {...})):
  #- Aquesta línia crea una seqüència de valors de 0 a 1 amb increments de 0,01, que es fan servir com a possibles valors de percentil.
  #- Per a cada valor de percentil, s'executen les següents accions:
    #. La funció lapply es fa servir per aplicar una funció anònima a cada valor de percentil.
    #. Dins de la funció anònima, s'aplica la funció do.call(rbind, lapply(unique(d$trial), function(x) {...})) per a cada conjunt de 
    #dades de prova única.
#2. Funció anònima dins de lapply(unique(d$trial), function(x) {...}):
  #- Aquesta funció recorre cada prova única en el conjunt de dades.
  #- Per a cada prova, s'obté una mostra corresponent a aquesta prova del conjunt de dades.
  #- Es calculen estadístiques com la mitjana i la mediana de les prediccions abans i després de la influència social.
  #- Es calculen els valors de predicció correctes utilitzant els valors de percentil com a llindar, tant abans com després de la 
  #influència social.
  #- Es determina si les prediccions s'amplifiquen o es redueixen després de la influència social.
#3. Funció mutate al final del codi:
  #- Es crea un nou conjunt de dades amb les columnes afegides amplify i change.
  #- La columna amplify indica si les prediccions es van amplificar després de la influència social.
  #- La columna change calcula el canvi en el percentatge de prediccions correctes després de la influència social.

#La part del codi de la funció 'reanalysis' s'interpreta de la següent manera:
#A la part del principi es defineixen els percentils que s'estudiaran: seran 100 percentils (de 0 fins a 1 amb pas 0.01). Tot seguit, 
#cada valor de 'threshold' donat pels 100 percentils provats es provarà amb cada 'trial' (les dades s'han agrupat per 'trials'). 
#Tot seguit es calcula el 'threshold' amb 'valor real' i no amb percentil i els paràmetres mu1, mu2... Llavors es crea un data frame amb
#els següents paràmetres:
#- p: Fa referència al quantil sobre el qual s'està calculant el threshold.
#- trial: Fa referència al 'trial' en qüestió. Cal remarcar que cada 'trial' està format per 20 agents, i per tant les dades següents 
#seran 'globals' de tots els agents.
#- pre_influence: Mostra la precisió abans que els agents del 'trial' en qüesitó hagin intercanviat tota la informació.
#- post_influence: Mostra la precisió després que els agents el 'trial' en qüesitó hagin intercanviat tota la informació.
#- predict_amplify: És TRUE si ha augmentat la opinió majoritària inicial i FALSE en cas contrari.
#- dataset: Significa a quin conjunt de dades correspon el 'trial' en qüestió (Gürçay, Becker o Lorenz).
#S'afegeixen amb 'mutate' les noves variables següents:
#- amplify: Indica si ha augmentat (TRUE) la precisió pre i post discussió o ha disminuït (FALSE).
#- change: Indica la diferència de precisió entre la pre i la post discussió.

#Interpretació del gràfic (Fig.3 del TFG):
#En el gràfic es mostra la precisió inicial (eix OX) i l'increment de precisió pre i post discussió (eix OY). De color negre es poden 
#veure els casos en què s'espera que la majoria augmenti i de color vermell els casos en què s'espera que la majoria disminueixi
#(utilitzant sempre com a criteri les hipòtesis de la Proposició 2a)). Finalment, s'acaba veient que quan es compleixen les hipòtesis
#de la Proposició 2a), es verifica que aquells 'trials' que eren inicialment precisos van acabar sent més precisos; aquells 'trials' que 
#eren inicialment imprecisos van acabar sent més imprecisos. 



###########################################
# Validació empírica de la Proposició 2b) #
###########################################


## Per tal de verificar empíricament la Proposició 2b), cal veure que si es compleix el que s'anomena "condition", 
## que es la hipotesi de la Proposició 2b), aleshores la mitjana numerica es fa mes acurada, es a dir, 
## la distancia entre C i theta es menor que la distancia entre mu i theta (per la "condition"),
## pero l'elecció binaria pot ser menys acurada. El conjunt de casos en els que es compleix la "condition"
## l'anomenem "prova":
prova <- do.call(rbind, lapply(head(seq(0,1,by=0.01), -1)[-1], function(p) {
  do.call(rbind, lapply(unique(d$trial), function(x) {
    samp <- subset(d, trial == x)
    
    t <- distAsPercentile(samp$pre_influence, p)
    
    mu1 <- mean(samp$pre_influence)
    med1 <- median(samp$pre_influence)
    mu2 <- mean(samp$post_influence)
    truth <- unique(samp$truth)
    
    # Verifica les condicions M < μ < C < T < θ o θ < T < C < μ < M
    condition <- (med1 < mu1 && mu1 < mu2 && mu2 < t && t < truth) || 
      (truth < t && t < mu2 && mu2 < mu1 && mu1 < med1)
    
    # Guarda les dades si es compleix la condició
    if (condition) {

      vv<- truth < t
      
      v <- samp$pre_influence < t
      correct_inici = (v == vv)    # ens diu si abans de la discusio la eleccio binaria era correcta
      
      u <- samp$post_influence < t
      correct_final = (u == vv)   # ens diu si despres de la discusio la eleccio binaria era correcta
      
      correct_inici_proportion <- mean(correct_inici) # proporcio d'eleccions correctes abans de discustir
      correct_final_proportion <- mean(correct_final) # proporcio d'eleccions correctes despres de discustir
      change = correct_inici_proportion - correct_final_proportion  # canvi en les proporcions. 
                                   # Positiu indica que el binary choice es menys acurate despres de discutir
      
      data.frame(p = p
                 , trial = x
                 , pre_influence = samp$pre_influence
                 , post_influence = samp$post_influence
                 , mu = mu1
                 , M = med1
                 , C = mu2
                 , theta = truth
                 , threshold = t
                 , prop.correct.inici = correct_inici_proportion
                 , prop.correct.final = correct_final_proportion
                 , difference = change
                 #########################################
                 , dataset = unique(samp$dataset)
                 )
    } else {
      NULL  # Retorna NULL si no es compleix la condició
    }
  }))
}))

# Ara agrupem el conjunt prova per jurat i llindar (això origina "nous jurats"):
prova_agrupats <- prova %>%
  group_by(trial, p) %>%
  summarise(mean_pre_influence = abs(mean(pre_influence-theta)), #Error d'estimació numèrica del grup (l'estimació numèrica del grup es calcula com la mitjana d'estimacions numèriques)
            mean_post_influence = abs(mean(post_influence-theta)),
            prop_correct_inici = first(prop.correct.inici),
            prop_correct_final = first(prop.correct.final),
            .groups = "keep")

#Llavors el que es fa és comptar aquells jurats tals que la mitjana d'error d'estimació numèrica ha 
#disminuït després de discutir (és a dir que verifiquen la Proposició 2b)):
trials_hyp_2b_thesis_2b <- filter(prova_agrupats, mean_pre_influence > mean_post_influence)
nrow(trials_hyp_2b_thesis_2b)

#Com que ens trobem a curt termini (no hi ha consens asimptòtic empíricament), es verifica quins
#jurats compleixen que tots els seus agents donen estimadors monòtons cap a C:
prova <- prova %>%
  mutate(
    millora = abs(post_influence - C) < abs(pre_influence - C),
    direccio_coherent = (pre_influence < C & post_influence < C) | (pre_influence > C & post_influence > C)
  )
# Identifiquem els jurats que compleixen ambdues condicions:
jurats_valids <- prova %>%
  group_by(trial) %>%
  filter(all(millora) & all(direccio_coherent)) %>%
  summarise()
nrow(jurats_valids)
#No hi ha cap jurat que verifiqui les hipòtesis de monotonia. Per tant, seria normal que
#no es verifiqués sempre la Proposició 2b).

#Hi podria haver alguns jurats que es trobessin en les hipòtesis de la Proposició 2b) però en canvi
#no verifiquessin la tesi, ja que estem en un escenari a curt termini i la proposició només es verifica
#si hi ha monotonia en les respostes cap a C (i hem vist que no n'hi ha).
odd_cases <- filter(prova_agrupats, mean_pre_influence < mean_post_influence)
nrow(odd_cases)

#Finalment es comprova quins dels jurats empitjoren pel que fa a l'OPINIÓ BINÀRIA. D'aquesta manera 
#es comprova la part de: "fins i tot en aquest cas tal que la decisió final és més errònia (més 
#agents s'han canviat a l'opinió binària equivocada)".                                             
trials_hyp_2b_thesis_binaryacc_worse <- filter(trials_hyp_2b_thesis_2b, prop_correct_inici > prop_correct_final)
nrow(trials_hyp_2b_thesis_binaryacc_worse)



#####################################################################
# Representació del 'crowd classification problem (Fig. 4 del TFG): #
#####################################################################

grafic_crowd_classification_problem <- ggplot(trials_hyp_2b_thesis_2b, aes(x = prop_correct_inici, y = prop_correct_final)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "",
       x = "Precisió binària inicial",
       y = "Precisió binària final") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 0.75)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 0.75)) +
  theme(axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12))

ggsave("Fig_4.eps")



