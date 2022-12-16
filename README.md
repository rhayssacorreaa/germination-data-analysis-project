# germination-data-analysis-project
Germination data analysis project of Carolina pepper and black bean seeds. The data were taken from a practice of the restoration ecology course.

# GRAPHICS #

![carolina + feijão](https://user-images.githubusercontent.com/119629370/208103209-581e6c47-bde3-46da-bfcc-a28998fac54f.png)
![número sementes germinadas carolina](https://user-images.githubusercontent.com/119629370/208103212-548fd490-5353-423a-ad8d-7eec9aee311d.png)

## R PROGRAM SCRIPT ##

install.packages("ggplot2") 
install.packages("cowplot") 

library(ggplot2) 
library(cowplot)

#dados carolina
tratamentos <- factor(c(rep("Controle", 5), rep("Escarificação", 5))) 
germinacao <- c(0,0,0,0,0,10,9,10,10,10) 

carolina <- data.frame(tratamentos, germinacao)

View(carolina)

#Anova Carolina
carolina_anova <- aov(germinacao ~ tratamentos, data = carolina)

summary(carolina_anova)

ggplot(carolina, aes(x = tratamentos, y = germinacao, fill = tratamentos)) +
  stat_summary(geom = "bar", fun.y = mean, show.legend = F) +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se) +
  labs(y = "Número de sementes germinadas", x = "Tratamentos") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(from = 0, to = 10, by = 2)) +
  theme_classic(base_size = 16) 

#dados Feijao
tratamentos <- factor(c(rep("Controle", 5), rep("Escarificação", 5))) 
germinacao <- c(10,9,10,10,10,10,9,10,10,10) 

Feijao <- data.frame(tratamentos, germinacao)

View(Feijao)

#Anova Feijao
Feijao_anova <- aov(germinacao ~ tratamentos, data = Feijao)

summary(Feijao_anova)

Fig1 <- ggplot(carolina, aes(x = tratamentos, y = germinacao, fill = tratamentos)) +
  stat_summary(geom = "bar", fun.y = mean, show.legend = F) +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se) +
  labs(y = "Número de sementes germinadas", x = "Tratamentos") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(from = 0, to = 10, by = 2)) +
  theme_classic(base_size = 16)

Fig2 <- ggplot(Feijao, aes(x = tratamentos, y = germinacao, fill = tratamentos)) +
  stat_summary(geom = "bar", fun.y = mean, show.legend = F) +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se) +
  labs(y = "Número de sementes germinadas", x = "Tratamentos") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(from = 0, to = 10, by = 2)) +
  theme_classic(base_size = 16)

plot_grid(Fig1, Fig2)
