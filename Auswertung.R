##### PREAMBLE -----

## ENVIRONMENT LEEREN / WORKING DIRECTORY SETZEN

rm(list = ls())

setwd("/Volumes/GoogleDrive/Meine Ablage/+++ Universität/Psych/PsyBSc6_Differentielle Psychologie/Seminar/Umfrage")



## PACKAGE NAMEN
packages <- c("ggplot2", "readxl", "dplyr", "multcomp", "tidyr", "knitr", "car", "psych", "tidyverse", "lmtest", "ggpubr", "ggstatsplot", "jsonlite", "pander", "abind", "RColorBrewer", "rococo", "shiny", "gvlma")



## PACKETE INSTALLIEREN, WENN NICHT INSTALLIERT
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}



## PAKETE LADEN
invisible(lapply(packages, library, character.only = TRUE))


##### DATEN LADEN -----

data <- as.data.frame(jsonlite::fromJSON("data.json"))

##### DATEN FILTERN -----

data_filtered <- na.omit(subset(data, beziehung == 2, c("gender", "alter", "eigene_attr", "partner_attr", "attr_vergleich", "erscheinung")))

vergleich <- data.frame(variable = c("eigene_attr", "partner_attr"),
                        mean = c(mean(data_filtered$eigene_attr), mean(data_filtered$partner_attr)),
                        sd = c(sd(data_filtered$eigene_attr), sd(data_filtered$partner_attr)))

erscheinung <- data.frame(eitel = (data_filtered$erscheinung > 3),
                         eigene_attr = data_filtered$eigene_attr,
                         partner_attr = data_filtered$partner_attr)

differenz <- data.frame(diff = (data_filtered$partner_attr - data_filtered$eigene_attr),
                        eitel = as.factor((data_filtered$erscheinung > 3)))

length(data_filtered$gender)

data_all <- data

vergleich_bz <- na.omit(data.frame(beziehung = as.factor(data_all$beziehung),
                                  eigene_attr = data_all$eigene_attr))

##### PLOTS -----

n1 <- length(data_filtered$gender)

# VERGLEICH EIGEN-PARTNER

plot <- ggplot(vergleich, aes(x = variable, y = mean)) +
              geom_bar(stat = "identity", fill = "grey", color = "black", width = .4,
                       position=position_dodge()) +
              geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width=.2,
                           position = position_dodge(.9)) +
              labs(title = "Einschätzung der physischen Attraktivität \n innerhalb einer monogamen Beziehung",
                   y = "Eingeschätzte physischer Attraktivität",
                   x = "",
                   caption = sprintf("Eingeschätzte physischer Attraktivität auf Fünf-Punkt-Skala \n n = %s", n1)) +
              theme_classic()

ggsave(file="vergleich.svg", plot = plot, width = 5,  height = 4)

# EITELKEIT

boxplot1 <- ggplot(differenz, aes(x = eitel, y = diff, color = eitel)) + 
             geom_boxplot(outlier.colour="red", outlier.shape=8,
                          outlier.size=8) +
            labs(title = "Differenz in der Attraktivitätsbeurteilung",
                 subtitle = "Relativ zur Eitelkeit",
                 caption = sprintf("Eitelkeit kodiert als 'Achten auf das äußere Erscheinungsbild' > 3 \n Differenz > 0: Partner*in als attraktiver eingeschätzt \n n = %s", n1),
                 y = "Differenz in der Beurteilungs \n der physischen Attraktivität",
                 x = "") + 
            theme_classic() +
            geom_dotplot(binaxis='y', stackdir='center', dotsize=1)

ggsave(file="diff_eitelkeit.svg", plot=boxplot1, width=5, height=4)


# EINSCHÄTZUNG RELATIV ZUR BEZIEHUNG

n2 <- length(vergleich_bz$beziehung)

boxplot2 <- ggplot(vergleich_bz, aes(x = beziehung, y = eigene_attr, color = beziehung)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=8) +
  labs(title = "Differenz in der eigenen Attraktivitätsbeurteilung",
       subtitle = "Relativ zum Beziehungsstatus",
       caption = sprintf("Item: ‘Ich empfinde mich als überdurchschnittlich attraktiv.' \n n = %s", n2),
       y = "Beurteilung der eigenen \n physischen Attraktivität",
       x = "",
       color = "Status") +
  scale_color_discrete(labels = c("liiert", "Single")) +
  stat_summary(fun = mean, colour = "grey", shape = 2, size = 1) +
  theme_classic() +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1)

ggsave(file="diff_eigen_attr.svg", plot=boxplot2, width=5, height=4)

