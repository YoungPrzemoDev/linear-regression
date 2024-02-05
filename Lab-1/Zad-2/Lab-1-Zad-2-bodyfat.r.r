library(tidyverse)
library(corrplot)
library(olsrr)
library(car)

Facik <- read.csv(file = 'C:/Users/Pemo/Desktop/bf.csv')

glimpse(Facik)

Facik_correlations <- cor(Facik) 


corrplot(Facik_correlations,type = "upper")


summary(Facik)

Facik%>%
ggplot() +
  geom_histogram (mapping = aes (x= Pct.BF), fill = "lightblue",
                  color = "black") +
  theme_minimal()


Facik%>%
select(-Pct.BF)%>%
  gather()%>%
  ggplot() +
  geom_histogram (mapping = aes (x= value,fill = key), fill = "lightblue",
                  color = "black") +
  facet_wrap(~ key, scales = "free")
  theme_minimal()


zmienna_Y <- Facik$target


facik_model1 <- lm(data = Facik,Pct.BF ~ Weight + Abdomen )
 
summary(facik_model1)

mean(facik_model1$residuals)

ols_plot_resid_hist(facik_model1)
ols_plot_resid_fit(facik_model1)

durbinWatsonTest(facik_model1)

ols_plot_cooksd_chart(facik_model1)









ggplot(Facik, aes(x = Weight, y = Pct.BF)) +
  geom_point(aes(color = Abdomen), alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red") +
  labs(title = "Model regresji", x = "Weight", y = "Pct.BF", color = "Abdomen") +
  theme_minimal()

set.seed(123)

#Podział danych na zestaw treningowy (80%) i testowy (20%)
indeksy <- sample(nrow(Facik), 0.8 * nrow(Facik))
zestaw_treningowy <- Facik[indeksy, ]
zestaw_testowy <- Facik[-indeksy, ]



wybrane_zmienne <- c("cecha1", "cecha2", "cecha3")  

model <- lm()