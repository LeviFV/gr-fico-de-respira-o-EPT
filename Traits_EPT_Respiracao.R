installed.packages('stringr')
installed.packages('ggplot2')
library(stringr)
library(ggplot2)

setwd("C:/Users/Lenovo/Downloads")

csv <- read.csv("Traits_EPT_Respiração.csv",na.strings = c(""),stringsAsFactors = FALSE)

Baetidae <- csv[csv$Family == 'Baetidae',]

branquial <- sum(apply(csv[2:83,3:30], 1, function(x) any(x %in% c("Branquial","Branquial "))))

cutanea <- sum(apply(csv[2:83,3:30], 1, function(x) any(x %in% c("cutânea","Cutânea"))))

branquial_cutanea <- sum(apply(csv[2:83,3:30], 1, function(x) any(x %in% "Branquial/cutânea")))

resp.integer <- data.frame(Resp = c("branquial","cutanea","branquial_cutanea"), Genera = c(branquial,cutanea,branquial_cutanea))



ggplot(data=resp.integer, aes(x=Resp, y=Genera)) + geom_bar(stat="identity") +
  theme_minimal()

##########
Check_duplicates <- resp.integer$Genera
as.data.frame(table(Check_duplicates))

resp.logical <- data.frame(Genera = csv[2:83,2],
                           Branquial = apply(csv[2:83,3:30], 1, function(x) any(x %in% c("Branquial","Branquial "))),
                           Cutanea = apply(csv[2:83,3:30], 1, function(x) any(x %in% c("cutânea","Cutânea"))),
                           Branquial_Cutanea = apply(csv[2:83,3:30], 1, function(x) any(x %in% "Branquial/cutânea")))

######### consegui selecionar a coluna de familia

resp.logical.family <- data.frame(Family = csv[2:83,1],
                                  Branquial = apply(csv[2:83,3:30], 1, function(x) any(x %in% c("Branquial","Branquial "))),
                                  Cutanea = apply(csv[2:83,3:30], 1, function(x) any(x %in% c("cutânea","Cutânea"))),
                                  Branquial_Cutanea = apply(csv[2:83,3:30], 1, function(x) any(x %in% "Branquial/cutânea")))
Check_duplicates <- resp.logical.family$Family
as.data.frame(table(Check_duplicates))


###### teste louco
resp.logical.family <- data.frame(Family = csv[2:83,1:2],
                                  Branquial = apply(csv[2:83,3:30], 1, function(x) any(x %in% c("Branquial","Branquial "))),
                                  Cutanea = apply(csv[2:83,3:30], 1, function(x) any(x %in% c("cutânea","Cutânea"))),
                                  Branquial_Cutanea = apply(csv[2:83,3:30], 1, function(x) any(x %in% "Branquial/cutânea")))

installed.packages('unite')

unite("Family2", Family:Genera, na.rm = TRUE, remove = FALSE)