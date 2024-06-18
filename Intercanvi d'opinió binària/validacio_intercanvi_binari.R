################################################################################

# This script is called to prepare the empirical data for the "binary exchange" 
# and validate the "binary exchange results".

################################################################################



###############
# Preparation #
###############

# Cleaning environment
rm(list=ls());gc()

# Loading dependencies
source("dependencies.R")
require(tidyverse)
require(xtable)



##################################
# Prepping the experimental data #
##################################

# Reading the data
d =  
    read.csv("Dades intercanvi binari.csv", stringsAsFactors=F) %>%
    mutate(
    correct_1 = response_1 == correctAnswer
    , correct_2 = response_2 == correctAnswer
    , correct_3 = response_3 == correctAnswer
    , is.valid = !is.na(response_1) & !is.na(response_2) & !is.na(response_3)
  ) %>% mutate(
    questionSet = NA  
  ) %>% mutate_when(
    grepl("cola", question), list(questionSet = "Pepsi vs Coke")
    , grepl("candies", question), list(questionSet = "Candies")
    , grepl("employ", question), list(questionSet = "Employment")
    , grepl("tech", question), list(questionSet = "Technology")
    , grepl("dessert", question), list(questionSet = "Calories")
  )

# Aggregate the data by trial
ag = d %>% group_by(trial_id, question, questionSet) %>%
  summarize(
    N=length(response_1)
    , correct_1 = mean(correct_1[is.valid]) #Això equival a la opiniò majoritària en el cas en què els pesos són iguals per cada agent, ja que
    # S = sum(Wi * Bi), i Bi és 1 o 0 (exactament igual a si la decisió és certa o falsa; és a dir en les hipòtesis de la Proposició 1a), 
    # la precisió mitjana equival a la decisió binària del jurat en qüestió, en canvi això NO es pot assumir en les hipòtesis de la Proposició 
    # 2b)). El mateix passa amb correct_2 i correct_3. Per tant, aquí s'assumeixen les hipòtesis de la Proposició 1a) i aquest codi i el 
    # serveix per verificar-la empíricament.
    , correct_2 = mean(correct_2[is.valid])
    , correct_3 = mean(correct_3[is.valid])
    , change_13 = correct_3 - correct_1
  )
ag<-as.data.frame(ag)

# Summarize the experimental data
empirical_sum = ag %>% 
  group_by(correct_1, change_13, questionSet, question,trial_id) %>%
  summarize(
    change_accuracy=mean(change_13)
    , initial_accuracy = mean(correct_1)
    , final_accuracy=mean(correct_3)
    #, expected_accuracy = unique(expected_accuracy)
    , N=length(correct_1)
  )

View(as.data.frame(empirical_sum))

## A "empirical_sum" simplement agafem les 60 observacions que hi ha a "ag", i les 
## ordenem de menor a major segons el valor de l'accuracy inicial. És a dir, segons
## la coordenada x per a fer la gràfica de la Fig.1 del TFG. A banda d'això, mantenim
## el "trial_id", que indica quina posició ocupa l'observació a "ag".
##########################################
## Afegim el següent per veure entre quins valors, en %, es mouran 
## l'eix OX (accuracy inicial) i l'eix OY (increment en l'accuracy)
summary(empirical_sum$correct_1)  # veiem que va entre 5% i 95% aprox. es pot agafar de 0 a 100%
summary(empirical_sum$change_13)  # veiem que va entre -27% i 43% aprox. es pot agafar de -50 a 50%



########################
# Fem la Fig.1 del TFG #
########################

xlabs=paste0(seq(0,100,by=025),"%")
ylabs=paste0(seq(-50,50,by=25),"%")

ggplot() + 
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0.5, linetype="dashed") + 
  geom_point(data=empirical_sum %>% arrange(desc(N))
             , aes(x=initial_accuracy
                   , y=change_accuracy
                   , fill=factor(questionSet)
             ), size=2, shape=21
  ) +
   # scale_size_continuous(range=c(2,4))+  ## ROSARIO: CREC QUE AQUESTA LINIA NO FA RES
   scale_x_continuous(lim=c(0,1), labels=xlabs)+
   scale_y_continuous(lim=c(-0.43,0.43), 
                      labels=ylabs, breaks=seq(-0.5,0.5,by=0.25))+
  labs(  color="", fill=""
         , x="Initial Accuracy"
         , y="Change in Accuracy\n(Positive = More Accurate)"
         , size="Number of Outcomes") +
  neat_theme

ggsave("Fig_1.eps", width=5.5, height=2.5)



########################################################
########################################################
########################################################



###########################################
# Validació empírica de la Proposició 1a) #
###########################################

majority_response <- function(x) {
  counts <- table(x)
  return(names(counts)[which.max(counts)])
}
d <- na.omit(d)

d_with_prop_majority_and_prob_2 <- d %>%
  group_by(trial_id) %>%
  summarise(majority_response_1 = majority_response(response_1),
            Pi_response_1 = sum(response_1 == majority_response_1) / n(),
            Pi_have_response_1_at_response_3 = sum(response_3 == majority_response_1) / n(),
            
            Fmaj = sum(response_1 == majority_response_1 & response_3 != majority_response_1) / sum(response_1 == majority_response_1),
            Fmin = sum(response_1 != majority_response_1 & response_3 == majority_response_1) / sum(response_1 != majority_response_1)
            )


#Jurats que estan en les hipòtesis de la Proposició 1a):
trials_hyp_prop1a <- d_with_prop_majority_and_prob_2 %>%
  filter(Fmin >= Fmaj * Pi_response_1 / (1 - Pi_response_1) & Fmaj > 0)

#Proporció de jurats que estan en les hipòtesis i verifiquen la tesis:
verify_thesis_prop1a <- sum(trials_hyp_prop1a$Pi_response_1 > 0.5 & trials_hyp_prop1a$Pi_have_response_1_at_response_3 > 0.5)/nrow(trials_hyp_prop1a)
print(verify_thesis_prop1a)
#Pi_response_1 > 0.5 es compleix sempre per hipòtesi  

#Proporció de jurats que estan en les hipòtesis i verifiquen la tesis "extra":
verify_extra_thesis_prop1a <- sum(trials_hyp_prop1a$Pi_response_1 <= trials_hyp_prop1a$Pi_have_response_1_at_response_3)/nrow(trials_hyp_prop1a)
print(verify_extra_thesis_prop1a)

#Jurats que NO estan en les hipòtesis de la Proposició 1a):
trials_nohyp_prop1a <- d_with_prop_majority_and_prob_2 %>%
  filter(Fmin < Fmaj * Pi_response_1 / (1 - Pi_response_1) | Fmaj == 0)

#Proporció de jurats que NO estan en les hipòtesis de la Proposició 1a) però verifiquen igualment la tesi:
trials_nohyp_prop1a_but_yesthesis <- sum(trials_nohyp_prop1a$Pi_response_1 > 0.5 & trials_nohyp_prop1a$Pi_have_response_1_at_response_3 > 0.5)/nrow(trials_nohyp_prop1a)
print(trials_nohyp_prop1a_but_yesthesis)
trials_nohyp_prop1a_but_yes_extra_thesis <- sum(trials_nohyp_prop1a$Pi_response_1 <= trials_nohyp_prop1a$Pi_have_response_1_at_response_3)/nrow(trials_nohyp_prop1a)
print(trials_nohyp_prop1a_but_yes_extra_thesis)

#Jurats que NO estan en les hipòtesis de la Proposició 1a) i tampoc verifiquen la tesi:
trials_nohyp_prop1a_and_nothesis <- trials_nohyp_prop1a %>% filter(trials_nohyp_prop1a$Pi_have_response_1_at_response_3 <= 0.5)
nrow(trials_nohyp_prop1a_and_nothesis)
trials_nohyp_prop1a_and_no_extra_thesis <- trials_nohyp_prop1a %>% filter(trials_nohyp_prop1a$Pi_response_1 > trials_nohyp_prop1a$Pi_have_response_1_at_response_3)
nrow(trials_nohyp_prop1a_and_no_extra_thesis)



####################
# Tests d'hipòtesi #
####################

#VÍDEO MOLT ÚTIL PER ENTENDRE BÉ AQUEST TEST:
#https://www.youtube.com/watch?v=zOvUQWOzTlc

table.1 <- matrix(c(27,4,0,17,6,5),nrow=3,byrow=TRUE)
colnames(table.1)<-c("Accurate","Inaccurate")
rownames(table.1)<-c("Increased accuracy","Decreased accuracy","Unchanged")
table.1

result<-chisq.test(table.1,correct=FALSE)
result
result$expected

## Per a que el test de la khi-quadrat sigui valid, totes les frequencies esperades
## haurien de ser >= 5. Hi ha una que per poc no ho compleix. 
## Es pot deixar, perque no es compleix per poc, pero tambe es pot fer el test 
## exacte de Fisher, que no fa servir cap hipotesi sobre les dades: 

fisher.test(table.1)



########################
# Fem la Fig.2 del TFG #
########################

## Per a veure en quin sentit és la relacio vista
## als tests anteriors, es pot fer un barplot:

prop.table.1<-round(100*prop.table(table.1,2),digits=0)
prop.table.1
prop.table(table.1,2)
barplot(prop.table.1,
        xlab='Initial accuracy',ylab='Change in accuracy',
        main="Percentage change by initial accuracy",beside=T,col=c("gray","brown","blue"),
        ylim = c(0, 100),
        legend=rownames(table.1),
        args.legend = list(x = "topright")
        )

ggsave("Fig_2.eps", width=5.5, height=2.5)







