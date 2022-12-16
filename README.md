# germination-data-analysis-project
Germination data analysis projec, in order to reveal which treatment is most effective for each seed. The data were extracted from a practice in the restoration ecology course.

# DATA IN GOOGLESHEETS #
![googlesheets](https://user-images.githubusercontent.com/119629370/208107957-ea15fa0c-8648-4e8f-9eea-3a15868f8b06.png)

# GRAPHICS #
FIG 1: carolina pepper + black beans seeds
FIG 2: carolina pepper seeds

![been + carolina](https://user-images.githubusercontent.com/119629370/208105746-21a7d5f4-5d42-4611-8a99-393febf4d309.png)
![carolina pepper](https://user-images.githubusercontent.com/119629370/208105748-015ec00c-a6f6-46c0-af72-f50e17955b42.png)

# Analysis of variance #
![beens anova](https://user-images.githubusercontent.com/119629370/208106362-09855af2-9308-4614-800f-3d3937bdb3ab.png)
![anova carolina](https://user-images.githubusercontent.com/119629370/208106374-5806670b-3fcd-4d06-b2f7-66adb1cfe4d5.png)


## R PROGRAM SCRIPT ##


install.packages("ggplot2") 
install.packages("cowplot") 

library(ggplot2) 
library(cowplot)

#carolina data
treatment <- factor(c(rep("Control", 5), rep("scarification", 5))) 
germination <- c(0,0,0,0,0,10,9,10,10,10) 

carolina <- data.frame(treatment, germination)

View(carolina)

#Anova Carolina
carolina_anova <- aov(germination ~ treatment, data = carolina)

summary(carolina_anova)

ggplot(carolina, aes(x = treatment, y = germination, fill = treatment)) +
  stat_summary(geom = "bar", fun.y = mean, show.legend = F) +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se) +
  labs(y = "number of germinated seeds", x = "Treatment") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(from = 0, to = 10, by = 2)) +
  theme_classic(base_size = 16) 

#black beans data
treatment <- factor(c(rep("Control", 5), rep("scarication", 5))) 
germination <- c(10,9,10,10,10,10,9,10,10,10) 

beens <- data.frame(treatment, germination)

View(beeans)

#Anova Feijao
beens_anova <- aov(germination ~ treatment, data = beens)

summary(beens_anova)

Fig1 <- ggplot(carolina, aes(x = treatment, y = germination, fill = treatment)) +
  stat_summary(geom = "bar", fun.y = mean, show.legend = F) +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se) +
  labs(y = "Number of germinated seeds", x = "treatment") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(from = 0, to = 10, by = 2)) +
  theme_classic(base_size = 16)

Fig2 <- ggplot(Feijao, aes(x = treatment, y = germination, fill = treatment)) +
  stat_summary(geom = "bar", fun.y = mean, show.legend = F) +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se) +
  labs(y = "Number of germinated seeds", x = "treatment") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(from = 0, to = 10, by = 2)) +
  theme_classic(base_size = 16)

plot_grid(Fig1, Fig2)
