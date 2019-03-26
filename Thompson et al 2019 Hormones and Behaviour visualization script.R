library(tidyverse)
library(interplot)

juv_allo <- read.csv("Thompson et al juvenile allostatic load data_git.csv")
z. <- function(x) scale(x)

######
# Aim 1 - (Figure 3) influences of ecological & life history variables on energy balance
#####

# Figure 3a - coefficient plot
a<-lmer(cp ~ sex + z.(age) + z.(mat_rank) + z.(fai) + z.(rain) + (1|group/subj), data = juv_allo)

coefs = as.data.frame(summary(a)$coefficients[-1,1:2]) #social var #minus intercept, estimate and se only
coefs$vars = c("Sex (M)","Age","Mom rank","FAI", "Rainfall")
coefs$vars <- factor(coefs$vars, levels=c("Sex (M)","Age","Mom rank","FAI", "Rainfall"))
names(coefs)[2] = "se"
colors<-c("chartreuse4","chartreuse3")

g<-ggplot(coefs, aes(vars, Estimate)) + 
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.y = element_text(angle = 90, hjust = 1))+
  theme(axis.text.y = element_text(size=20))+
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se), 
                lwd=1, colour=colors[2], width=0) +
  geom_errorbar(aes(ymin=Estimate - se, ymax=Estimate + se), 
                lwd=2.5, colour=colors[1], width=0) +
  labs(x ="")+
  geom_point(size=4, pch=21, fill=colors[1])+
  theme(panel.background = element_rect(fill = "white"))
g

# Figure 3b - energy balance by sex
with(juv_allo, boxplot(cp ~ sex,
        cex.lab=1.5,cex.axis=1.5,
        col=c("chartreuse4","chartreuse3"), ylab="Avg. uCP residual", xlab="Sex"))

# Figure 3c - energy balance by ripe fruit availability
with(juv_allo, plot(jitter(fai/1e8), cp,
     cex.lab=1.5,cex.axis=1.5,
     col="darkgreen",pch=1,cex=1, ylab="Avg. uCP residual", xlab="FAI (no. ripe fruits x 10^8)"))
fai.small <- juv_allo$fai/1e8
with(juv_allo, abline(lm(cp ~ fai.small),col="darkgreen"))

# Figure 3d - energy balance by rain
with(juv_allo, plot(jitter(rain),cp,
     cex.lab=1.5,cex.axis=1.5,
     col="darkgreen",pch=1,cex=1, ylab="Avg. uCP residual", xlab="Avg. daily rainfall"))
with(juv_allo, abline(lm(cp ~ rain), col="darkgreen"))

# Figure 3e - energy balance by age
with(juv_allo, plot(age,cp,
                    cex.lab=1.5,cex.axis=1.5,
                    col="darkgreen",pch=1,cex=1, ylab="Avg. uCP residual", xlab="Age (yrs)"))
with(juv_allo, abline(lm(cp ~ age), col="darkgreen"))

######
# Aim 2 - (Figure 4) influences of energy balance, life history, and competitive environment on dfGCs
######

b <- lmer(z.(dfgc) ~ sex + z.(age) + z.(groupmates) + z.(mat_rank) + z.(cp) + (1|group/subj), data = juv_allo)

coefs = as.data.frame(summary(b)$coefficients[-1,1:2]) 
colors<-c("royalblue1","blue")
coefs$vars = c("Sex (M)","Age","Group size","Mom rank","uCP")
coefs$vars <- factor(coefs$vars, levels=c("Sex (M)","Age", "Group size", "Mom rank","uCP"))
names(coefs)[2] = "se"

g<-ggplot(coefs, aes(vars, Estimate)) + 
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.y = element_text(angle = 90, hjust = 1))+
  theme(axis.text.y = element_text(size=20))+
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se), 
                lwd=1, colour=colors[2], width=0) +
  geom_errorbar(aes(ymin=Estimate - se, ymax=Estimate + se), 
                lwd=2.5, colour=colors[1], width=0) +
  labs(x ="")+
  geom_point(size=4, pch=21, fill=colors[1])+
  theme(panel.background = element_rect(fill = "white"))
g

with(juv_allo, plot(cp, dfgc, col="blue", pch=1, cex=.75,
     cex.lab=1.5,cex.axis=1.5,
     ylab="Avg. deviation subject baseline fGCs", xlab="Avg. uCP residual")) #
with(juv_allo, abline(lm(dfgc ~ cp), col="blue"))


#####
# Aim 3 - (Figure 5) social strategies influence on dfGCs
#####

# Figure 5a: conditional coefficient of energy balance on dfgcs

# the coefficient of energy balance on dfgcs increases as time spent grooming increases
dfgc_cp_gm <- lmer(dfgc ~ time_groom_groomed + cp + cp*time_groom_groomed + (1|group/subj), data = juv_allo)
interplot(m = dfgc_cp_gm, var1 = "cp", var2 = "time_groom_groomed") +
  theme(axis.text.y=element_text(size=15))+
  theme(axis.text.x=element_text(size=15))

# Figure 5b: variation in the relatonship between energy balance and dfgcs among individuals
# in different quartiles of time spent grooming

# data for gm quartiles
gm_quant <- juv_allo %>%
  mutate(quant_gm = as.factor(as.numeric(quantcut(time_groom_groomed)))) %>%
  select(subj, sex, age, period, group, cp, dfgc, prop_gm_kin, prop_gm_nonkin, quant_gm) %>%
  rename(gm_nonkin = prop_gm_nonkin, gm_kin = prop_gm_kin) %>%
  gather(.,key = kin_status, value = prop_gm, gm_kin, gm_nonkin) %>%
  mutate(quant_var = interaction(kin_status, quant_gm))

# relationship between energy balance and dfgcs is positive among juveniles that groom the most
g <- ggplot(gm_quant, aes(x = cp, y = dfgc, colour = quant_gm)) + geom_point()
g + geom_smooth(method="lm", se = FALSE) +
  labs(x="Avg. uCP residual", y = "Avg. deviation subject baseline fGCs", color = "Quartiles of \n % time grooming") +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15)) +
  theme(legend.position = c(0,1)) +
  theme(legend.justification = c(0,1)) +
  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
g

# Figure 5C: Individuals by quartile of time spent grooming - time grooming kin v nonkin

# individuals that spend the most tim grooming spend more time grooming with nonkin than kin
# unlike individuals in other grooming quartiles
ggplot(gm_quant, aes(x=quant_var, y=prop_gm, fill=kin_status)) + geom_boxplot() +
  scale_fill_manual(values=c("purple4","mediumpurple1")) +
  ggtitle("% Time grooming kin vs. non-kin \n by grooming quartile") +
  theme(plot.title = element_text(hjust=0.5)) +
  xlab("% Time grooming quartile") +
  ylab("% Time grooming") +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=15,angle=90)) +
  theme(axis.title.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15))


