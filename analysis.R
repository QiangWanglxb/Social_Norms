library(car)
library(lme4)
library(nlme)
library(effects) 
library(jtools) 
library(GLMMadaptive)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr) 
library(patchwork)
library(cowplot)

z <- read.csv(file = "data.csv", header=TRUE)

source("add_country_data.R")

z <- z %>% mutate_at(c('Cultural_Tightness','GNI_per_ATLAS','GNI_ATLAS'  ,'health_expenditure', 'population_density', 'Urbanization', 'informationKOF',
                        'cultureKOF'), ~(scale(.) %>% as.vector))
z <- z %>% mutate_at(c('Power_Distance','Individualism','Masculinity'  ,'Uncertainty_Avoidance', 'Long_Term_Orientation', 'Indulgence'), ~(scale(.) %>% as.vector))


modelnull<- glmer(VH_COVID_1~1 + (1|COUNTRY) + (1|Time), data = z26, family = binomial(logit))

model_age <- glmer(VH_COVID_1~1 +  as.factor(Age_code_wq)  + (1|COUNTRY) , data = z26, family = binomial(logit))
model_gender <- glmer(VH_COVID_1~1 + as.factor(Gender_code_wq) + (1|COUNTRY) , data = z26, family = binomial(logit))
model_education <- glmer(VH_COVID_1~1 +  as.factor(Education_code_wq)  + (1|COUNTRY) , data = z26, family = binomial(logit))
model_Religious <- glmer(VH_COVID_1~1 +  as.factor(Religious_code_wq)  + (1|COUNTRY) , data = z26, family = binomial(logit))
model_percerptions <- glmer(VH_COVID_1~1 +  Social_norms_wq  + (1|COUNTRY) , data = z26, family = binomial(logit))
model_culture <- glmer(VH_COVID_1~1 +  Cultural_Tightness  + (1|COUNTRY) , data = z26, family = binomial(logit))
model_GNI <- glmer(VH_COVID_1~1 +  GNI_ATLAS  + (1|COUNTRY) , data = z26, family = binomial(logit))
model_population_density <- glmer(VH_COVID_1~1 +  population_density  + (1|COUNTRY) , data = z26, family = binomial(logit))
model_Time <- glmer(VH_COVID_1~1 +  as.factor(Time)  + (1|COUNTRY) , data = z26, family = binomial(logit))
model_Power_Distance <- glmer(VH_COVID_1~1 + Power_Distance + (1|COUNTRY) , data = z26, family = binomial(logit))
model_Individualism <- glmer(VH_COVID_1~1 +  Individualism  + (1|COUNTRY) , data = z26, family = binomial(logit))
model_Masculinity <- glmer(VH_COVID_1~1 +  Masculinity  + (1|COUNTRY) , data = z26, family = binomial(logit))
model_Uncertainty_Avoidance <- glmer(VH_COVID_1~1 +  Uncertainty_Avoidance  + (1|COUNTRY) , data = z26, family = binomial(logit))
model_Long_Term_Orientation <- glmer(VH_COVID_1~1 +  Long_Term_Orientation   + (1|COUNTRY) , data = z26, family = binomial(logit))
model_Indulgence <- glmer(VH_COVID_1~1 + Indulgence  + (1|COUNTRY) , data = z26, family = binomial(logit))

model1.2<- glmer(VH_COVID_1~1 + as.factor(Age_code_wq)+ as.factor(Gender_code_wq)+ as.factor(Education_code_wq) + as.factor(Religious_code_wq) 
                 + Social_norms_wq + Cultural_Tightness + (1|COUNTRY), data = z26, family = binomial(logit))

model1.3 <- glmer(VH_COVID_1~1 +  as.factor(Age_code_wq)+ as.factor(Gender_code_wq)+ as.factor(Education_code_wq) + as.factor(Religious_code_wq) 
                  + Social_norms_wq + Cultural_Tightness +  GNI_ATLAS + population_density + as.factor(Time) 
                   + Individualism + Cultural_Tightness*Individualism + Indulgence + Long_Term_Orientation + Power_Distance 
                   + (1|COUNTRY), data = z26, family = binomial(logit))
m <- model1.3age
se <- sqrt(diag(vcov(m)))
tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 * se)
round(exp(tab), digits = 2)
summary(m)


###################################################stratfied analysis
z26_age <- filter(z26, Age_code_wq == "5")
model1.3age <- glmer(VH_COVID_1~1 +  as.factor(Gender_code_wq)+ as.factor(Education_code_wq) + as.factor(Religious_code_wq) 
                     + Social_norms_wq + Cultural_Tightness +  GNI_ATLAS + population_density + as.factor(Time) 
                     + Individualism + Cultural_Tightness*Individualism + Indulgence + Long_Term_Orientation + Power_Distance 
                     + (1|COUNTRY), data = z26_age, family = binomial(logit))

z26_gender <- filter(z26, Gender_code_wq == "2")
model1.4gender <- glmer(VH_COVID_1~1 +  as.factor(Age_code_wq)+ as.factor(Education_code_wq) + as.factor(Religious_code_wq) 
                        + Social_norms_wq + Cultural_Tightness +  GNI_ATLAS + population_density + as.factor(Time) 
                        + Power_Distance + Individualism + Masculinity + Uncertainty_Avoidance + Long_Term_Orientation + Indulgence
                        + (1|COUNTRY), data = z26, family = binomial(logit))

z26_education <- filter(z26, Education_code_wq == "4")
model1.4edcuation <- glmer(VH_COVID_1~1 +  as.factor(Age_code_wq)+ as.factor(Gender_code_wq) + as.factor(Religious_code_wq) 
                           + Social_norms_wq + Cultural_Tightness +  GNI_ATLAS + population_density + as.factor(Time) 
                           + Power_Distance + Individualism + Masculinity + Uncertainty_Avoidance + Long_Term_Orientation + Indulgence
                           + (1|COUNTRY), data = z26, family = binomial(logit))

z26_religion <- filter(z26, Religious_code_wq == "3")
model1.4religion <- glmer(VH_COVID_1~1 +  as.factor(Age_code_wq)+ as.factor(Gender_code_wq)+ as.factor(Education_code_wq)  
                          + Social_norms_wq + Cultural_Tightness +  GNI_ATLAS + population_density + as.factor(Time) 
                          + Power_Distance + Individualism + Masculinity + Uncertainty_Avoidance + Long_Term_Orientation + Indulgence
                          + (1|COUNTRY), data = z26, family = binomial(logit))


##################################sensitivity analysis
model2_age <- glmer(VH_covid~1 +  as.factor(Age_code_wq)  + (1|COUNTRY) , data = z26, family = binomial(logit))
model2_gender <- glmer(VH_covid~1 + as.factor(Gender_code_wq) + (1|COUNTRY) , data = z26, family = binomial(logit))
model2_education <- glmer(VH_covid~1 +  as.factor(Education_code_wq)  + (1|COUNTRY) , data = z26, family = binomial(logit))
model2_Religious <- glmer(VH_covid~1 +  as.factor(Religious_code_wq)  + (1|COUNTRY) , data = z26, family = binomial(logit))
model2_percerptions <- glmer(VH_covid~1 +  Social_norms_wq  + (1|COUNTRY) , data = z26, family = binomial(logit))
model2_culture <- glmer(VH_COVID_2~1 +  Cultural_Tightness  + (1|COUNTRY) , data = z26, family = binomial(logit))
model2_GNI <- glmer(VH_covid~1 +  GNI_ATLAS  + (1|COUNTRY) , data = z26, family = binomial(logit))
model2_population_density <- glmer(VH_covid~1 +  population_density  + (1|COUNTRY) , data = z26, family = binomial(logit))
model2_Time <- glmer(VH_covid~1 +  as.factor(Time)  + (1|COUNTRY) , data = z26, family = binomial(logit))
model_Power_Distance <- glmer(VH_COVID_2~1 + Power_Distance + (1|COUNTRY) , data = z26, family = binomial(logit))
model_Individualism <- glmer(VH_COVID_2~1 +  Individualism  + (1|COUNTRY) , data = z26, family = binomial(logit))
model_Long_Term_Orientation <- glmer(VH_COVID_2~1 +  Long_Term_Orientation   + (1|COUNTRY) , data = z26, family = binomial(logit))
model_Indulgence <- glmer(VH_COVID_2~1 + Indulgence  + (1|COUNTRY) , data = z26, family = binomial(logit))


model2.2<- glmer(VH_COVID_2~1 + as.factor(Age_code_wq)+ as.factor(Gender_code_wq)+ as.factor(Education_code_wq) + as.factor(Religious_code_wq) 
                 + Social_norms_wq + Cultural_Tightness + (1|COUNTRY), data = z26, family = binomial(logit))

model2.3 <- glmer(VH_COVID_2~1 +  as.factor(Age_code_wq)+ as.factor(Gender_code_wq)+ as.factor(Education_code_wq) + as.factor(Religious_code_wq) 
                  + Social_norms_wq + Cultural_Tightness +  GNI_ATLAS + population_density + as.factor(Time) 
                  + Power_Distance + Individualism + Long_Term_Orientation  + Indulgence + Cultural_Tightness*Individualism
                  + (1|COUNTRY), data = z26, family = binomial(logit))

m <- model2.3
se <- sqrt(diag(vcov(m)))
tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 * se)
round(exp(tab), digits = 2)
summary(m)


tau2 <- VarCorr(modelnull)[[1]]^2
ICC <- tau2 / (tau2 + (pi^2 / 3) )

summary(model1.1)
summ(model1.2, exp = T)
car::vif(model1.3) 
confint(model1.3)


jvalues <- with(z26, seq(from = min(Cultural_Tightness), to = max(Cultural_Tightness), length.out = 100))

# calculate predicted probabilities and store in a list
pp <- lapply(jvalues, function(j) {
  z26$Cultural_Tightness <- j
  predict(model1.3, newdata = z26, type = "response")
})

plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
plotdat <- as.data.frame(cbind(plotdat, jvalues))

colnames(plotdat) <- c("PredictedProbability", "Lower", "Upper", "Cultural_Tightness")

################################################### plot predicted probability
ggplot(plotdat, aes(x = Cultural_Tightness, y = PredictedProbability)) + geom_line(linewidth = 0.8) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "#e66101", alpha = 0.25) +
  scale_x_continuous(expand=c(0,0), limits=c(-2, 2)) +
  scale_y_continuous(expand=c(0,0), limits=c(0.0, 1), breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  theme_bw() +  
  theme(panel.grid=element_blank()) + 
  theme(plot.background = element_blank() ,
        panel.grid.major = element_blank() ,
        panel.grid.minor = element_blank() ,
        panel.border = element_blank() ,
        panel.background = element_blank() ) +
  theme(axis.line = element_line(color = 'black'), axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10)) +
  xlab("Culture Tightness") +  ylab("Predicted Probability of COVID-19 vaccine confidence")




##########################scatter plot
library(ggrepel) 
scatter<-read.csv(file = "scatter data.csv", header=TRUE)
names(scatter)[1]<-"Country"
p_scatter <- ggplot(scatter, mapping=aes(y=Vaccine.confidence2,x=Culture.tightless))  +
  geom_point(shape=19, size=2, colour = '#cc0033') +
  ylab("COVID-19 Vaccine confidence (%)") +
  xlab("Culture tightless") + 
  geom_smooth(method=lm, se= F, colour="black",linetype=1, na.rm= FALSE, fill = "#fee090")  +
  scale_x_continuous(expand=c(0,0), limits=c(-1.0, 1.0)) +
  scale_y_continuous(expand=c(0,0), limits=c(0, 80), breaks=c(0,20,40,60,80)) +
  geom_text_repel(size = 2, aes(x=Culture.tightless, y=Vaccine.confidence2, label =Country)) +
  theme_bw() +  
  theme(panel.grid=element_blank()) + 
  theme(plot.background = element_blank() ,
        panel.grid.major = element_blank() ,
        panel.grid.minor = element_blank() ,
        panel.border = element_blank() ,
        panel.background = element_blank() ) +
  theme(axis.line = element_line(color = 'black'), axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10))

##############################################
library(viridis)
individual_norms <-read.csv(file = "Individual social norms.csv", header=TRUE)
names(individual_norms)[1]<-"Country"

ggplot(individual_norms, aes(fill=as.factor(Answer), y=Percentage, x=as.factor(Answer))) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~Country, ncol=4, labeller = label_both) +
  xlab("") + ylab("Percentage (%)") + 
  theme(panel.grid=element_blank()) + 
  scale_fill_manual("legend", values = c("1" = "#225ea8", "2" = "#1d91c0", "3" = "#41b6c4","4" = "#7fcdbb", "5" = "#c7e9b4")) +
  theme_bw() +  
  theme(panel.grid=element_blank()) + 
  theme(plot.background = element_blank() ,
        panel.grid.major = element_blank() ,
        panel.grid.minor = element_blank() ,
        panel.border = element_blank() ,
        panel.background = element_blank() ) +
  theme(axis.line = element_line(color = 'black'), axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10)) +
  theme(legend.position = "none")

###############################################
library(forestplot)
forestplot(labeltext = as.matrix(subgroup[,1:3]),
           mean = subgroup$mean,
           lower = subgroup$lower,
           upper = subgroup$upper,
           xticks = c(0,0.5, 1,1.5,2),
           graphwidth = unit(80,"mm"),
           txt_gp=fpTxtGp(label=gpar(cex=1.0), ticks=gpar(cex=1.0), xlab=gpar(cex = 1.2), title=gpar(cex = 1.0)),
           xlab="AOR (95%CI)",
           clip = c(0,1.4),
           col=fpColors(line = "black", 
                        box="#0571b0"), 
           ci.vertices = TRUE,
           lwd.ci=3,
           lineheight = unit(6,'mm'),
           zero = 1,
           colgap = unit(2,'mm'),
           boxsize = 0.3,
           graph.pos = 4)

