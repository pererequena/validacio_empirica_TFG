################################################################################

# These are the dependencies used throughout the analysis. This script will 
# be sourced from other scripts.

################################################################################

### ggplot themes and settings
require(ggplot2)

neat_theme =   theme(panel.background=element_rect(fill="white", color="black", size=1.1), 
                      axis.text=element_text(size=rel(1), color="black"), 
                      strip.text=element_text(size=rel(1.1)), 
                      legend.text=element_text(size=rel(1.1)), strip.background=element_blank(),
                      title=element_text(size=rel(1.1)),
                      panel.grid=element_blank(),
                      plot.title=element_text(hjust=0.5))

rotateX = theme(axis.text.x=element_text(angle=90))

adjX = theme(axis.text.x=element_text(vjust=0.5))


### THIS FUNCTION COURTESY
### KEVIN USHEY
### https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-on-a-subset-of-rows
mutate_when <- function(data, ...) {
  dots <- eval(substitute(alist(...)))
  for (i in seq(1, length(dots), by = 2)) {
    condition <- eval(dots[[i]], envir = data)
    mutations <- eval(dots[[i + 1]], envir = data[condition, , drop = FALSE])
    data[condition, names(mutations)] <- mutations
  }
  data
}


#Aquest codi defineix algunes funcions i temes que s'utilitzaran al llarg de l'anàlisi. Tot seguit hi ha una explicació de cada part
#del codi:

#Temes de ggplot i configuració (neat_theme, rotateX, adjX): Aquest bloc defineix tres temes de ggplot2 i configuracions per a 
#gràfics. Aquests temes defineixen aspectes visuals com el fons dels panells, els colors del text i els títols dels gràfics, entre 
#altres coses.

#Funció mutate_when: Aquesta funció és una adaptació de la funció mutate de dplyr que permet aplicar mutacions a un data frame 
#només en les files que compleixen una condició específica. Això és útil per aplicar transformacions només a un subconjunt de 
#les dades. La funció pren com a arguments les dades i una sèrie d'expressions que especifica les condicions i les mutacions 
#a aplicar.