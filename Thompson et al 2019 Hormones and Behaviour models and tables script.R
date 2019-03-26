library(lmerTest)
library(tidyverse)
library(gtools)

juv_allo <- read.csv("Thompson et al juvenile allostatic load data_git.csv")
z. <- function(x) scale(x)
vif.mer <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

######
# Aim 1 - influences of ecological & life history variables on energy balance
######

# main models - energy balance is higher among females, and increases with ripe fruit availability and decreases with rainfall
cp_eco_lh <- lmer(cp ~  z.(age) + sex + z.(mat_rank) + z.(groupmates) + z.(fai) + z.(rain) + (1|group/subj), data = juv_allo)
summary(cp_eco_lh)

cp_eco_lh_int <- lmer(cp ~  z.(age) + sex + z.(mat_rank) + z.(groupmates) + z.(fai) + z.(rain) + sex*z.(age) + (1|group/subj), data = juv_allo)
summary(cp_eco_lh_int)
confint(cp_eco_lh_int)

vif.mer(cp_eco_lh)
vif.mer(cp_eco_lh_int)

#sex differences in time feeding on fruit
tf_sex <-lmer(time_feeding_fruit ~  sex + (1|group/subj), data = juv_allo)
summary(tf_sex)
confint(tf_sex)


# create table 1
cp<-cp_eco_lh
a<-as.numeric(coef(cp)$subj[1,]) #all are exactly the same
b<-sqrt(diag(vcov(cp))) 
names(b)<-colnames(vcov(cp))
c<-confint(cp)
cc<-c[4:nrow(c),]
lo<-paste("[",round(cc[,1],2),sep="")
up<-paste(round(cc[,2],2),"]",sep="")
CI<-paste(lo,up,sep=", ")
d<-round(data.frame(coef=a,se=b),2)
cp.tab<-data.frame(d,CI)
cp.tab


######
# Aim 2 - influences of energy balance, life history, and competitive environment on dfGCs
######

# main model - dfgcs decrease with higher energy balance
dfgc_eco_lh_cp <- lmer(z.(dfgc) ~ z.(age) +  sex + z.(mat_rank) + + z.(groupmates) + z.(cp) + (1|group/subj),data = juv_allo)
summary(dfgc_eco_lh_cp)
vif.mer(dfgc_eco_lh_cp)

# create table 2
dfgc <- dfgc_eco_lh_cp
a<-as.numeric(coef(dfgc)$group[1,])
b<-sqrt(diag(vcov(dfgc))) 
names(b)<-colnames(vcov(dfgc))
c<-confint(dfgc)
cc<-c[4:nrow(c),]
lo<-paste("[",round(cc[,1],2),sep="")
up<-paste(round(cc[,2],2),"]",sep="")
CI<-paste(lo,up,sep=", ")
d<-round(data.frame(coef=a,se=b),2)
gc.tab<-data.frame(d,CI)
gc.tab


######
# Aim 3 - social strategies influence on dfGCs
######

# Aim 3a: establish and describe social strategies ----

PC1_predictors <- lmer(PC1 ~ sex + z.(age) + z.(num_mat_kin) + z.(mat_rank) + z.(fai) + z.(rain) + (1|group/subj), data = juv_allo)
summary(PC1_predictors)

PC2_predictors <- lmer(PC2 ~ sex + z.(age) + z.(num_mat_kin) + z.(mat_rank) + z.(fai) + z.(rain) + (1|group/subj), data = juv_allo)
summary(PC2_predictors)

PC3_predictors <- lmer(PC3 ~ sex + z.(age) + z.(num_mat_kin) + z.(mat_rank) + z.(fai) + z.(rain) + (1|group/subj), data = juv_allo)
summary(PC3_predictors)

vif.mer(PC1_predictors)
vif.mer(PC2_predictors)
vif.mer(PC3_predictors)

#create table S2 w confidence intervals
mod <- PC1_predictors
mod <- PC2_predictors
mod <- PC3_predictors

a<-as.numeric(coef(mod)$group[1,])
b<-sqrt(diag(vcov(mod))) 
names(b)<-colnames(vcov(mod))
c<-confint(mod)
cc<-c[4:nrow(c),]
lo<-paste("[",round(cc[,1],2),sep="")
up<-paste(round(cc[,2],2),"]",sep="")
CI<-paste(lo,up,sep=", ")
d<-round(data.frame(coef=a,se=b),2)
pc.tab<-data.frame(d,CI)
pc.tab


# Aim 3b: assess relationship social strategies and dfGCs  -----

### 3b main models  -----
# dfgcs increase with higher scores on pc1, e.g. individuals that spend more time grooming and less time playing
dfgc_social <- lmer(z.(dfgc) ~ PC1 + PC2 + PC3  + sex + z.(age) + z.(mat_rank) + z.(cp) + (1|group/subj), data = juv_allo)
summary(dfgc_social)
vif.mer(dfgc_social)

#no interaction of PC1 with sex
dfgc_social_int <-lmer(z.(dfgc) ~ PC1 + PC2 + PC3  + sex + z.(age) + z.(mat_rank) + z.(groupmates) + z.(cp) + PC1*sex + (1|group/subj), data = juv_allo)
summary(dfgc_social_int)
confint(dfgc_social_int)
vif.mer(dfgc_social_int)

#no effect of moving on dfgc
dfgc_moving <- lmer(z.(dfgc) ~ time_moving  + sex + z.(age) + z.(mat_rank) + z.(cp) + (1|group/subj), data = juv_allo)
summary(dfgc_moving)
confint(dfgc_moving)

#table 3 w confidence intervals
mod <- dfgc_social
a<-as.numeric(coef(mod)$group[1,])
b<-sqrt(diag(vcov(mod))) 
names(b)<-colnames(vcov(mod))
c<-confint(mod)
cc<-c[4:nrow(c),]
lo<-paste("[",round(cc[,1],2),sep="")
up<-paste(round(cc[,2],2),"]",sep="")
CI<-paste(lo,up,sep=", ")
d<-round(data.frame(coef=a,se=b),2)
dfgc.soc.tab<-data.frame(d,CI)
dfgc.soc.tab

### effect of PC1 is driven by both play and grooming----
dfgc_play <- lmer(z.(dfgc) ~ z.(time_playing) + sex + z.(age) + z.(mat_rank) + z.(cp) + (1|group/subj), data = juv_allo)
dfgc_groom <- lmer(z.(dfgc) ~ z.(time_groom_groomed) + sex + z.(age) + z.(mat_rank) + z.(cp) + (1|group/subj), data = juv_allo)
summary(dfgc_play)
summary(dfgc_groom)
vif.mer(dfgc_play)
vif.mer(dfgc_groom)

### general activity levels don't predict dfGCs ----
dfgc_activity <- lmer(z.(dfgc) ~ sex + z.(age) + z.(time_moving + time_groom_groomed + time_playing) + z.(cp) + (1|group/subj), data = juv_allo)
summary(dfgc_activity)

### assess whether affiliation buffers relationship of energy balance with dfGCs ----

# focus on interaction - play does not buffer rl of energy balnce and dfgcs
buffer_pl <- lmer(z.(dfgc) ~ z.(time_playing) + sex + z.(age) + z.(mat_rank) + z.(cp) + z.(cp)*z.(time_playing) + (1|group/subj), data = juv_allo)
summary(buffer_pl)
vif.mer(buffer_pl)

# focus on interaction - grooming also does not buffer relationship bt energy balance and dfgcs
# instead, dfgcs increase with energy balance as individuals spend more time grooming
buffer_gm <- lmer(z.(dfgc) ~ z.(time_groom_groomed) + sex + z.(age) + z.(mat_rank) + z.(cp) + z.(cp)*z.(time_groom_groomed) + (1|group/subj), data = juv_allo)
summary(buffer_gm)
vif.mer(buffer_gm)

### time spent grooming kin vs. nonkin by gm quartile ----

#set up data to group juveniles into quartiles of total time spent grooming
#then identify time spent grooming with kin vs nonkin among individuals within each quartile

gm_quant <- juv_allo %>%
  mutate(quant_gm = as.factor(as.numeric(quantcut(time_groom_groomed)))) %>%
  select(subj, sex, age, period, group, cp, dfgc, prop_gm_kin, prop_gm_nonkin, quant_gm) %>%
  rename(gm_nonkin = prop_gm_nonkin, gm_kin = prop_gm_kin) %>%
  gather(.,key = kin_status, value = prop_gm, gm_kin, gm_nonkin) %>%
  mutate(quant_var = as.factor(paste(kin_status, quant_gm, sep = "_")))
str(gm_quant)


# quartile 1 - compare time spent grooming with kin vs. nonkin among individuals in the first grooming quartile 1 (i.e. those that groomed the least)
gm_quant$quant_var<-relevel(gm_quant$quant_var, "gm_kin_1")
b <- summary(lmer(prop_gm ~  quant_var + (1|group/subj), data = gm_quant)) #compare ki base with it's non kin counterpart
kin_vs_nonkin_1 <- b$coefficients[rownames(b$coefficients)=="quant_vargm_nonkin_1",]
kin_vs_nonkin_1

# quartile 2 - compare time spent grooming with kin vs. nonkin among individuals in the first grooming quartile 2
rm(b)
gm_quant$quant_var<-relevel(gm_quant$quant_var, "gm_kin_2")
b<-summary(lmer(prop_gm ~  quant_var + (1|group/subj), data = gm_quant)) #compare ki base with it's non kin counterpart
kin_vs_nonkin_2 <- b$coefficients[rownames(b$coefficients)=="quant_vargm_nonkin_2",]
kin_vs_nonkin_2

# quartile 3 - compare time spent grooming with kin vs. nonkin among individuals in the first grooming quartile 3
rm(b)
gm_quant$quant_var<-relevel(gm_quant$quant_var, "gm_kin_3")
b<-summary(lmer(prop_gm ~  quant_var + (1|group/subj), data = gm_quant)) #compare ki base with it's non kin counterpart
kin_vs_nonkin_3 <- b$coefficients[rownames(b$coefficients)=="quant_vargm_nonkin_3",]
kin_vs_nonkin_3

# quartile 4 - compare time spent grooming with kin vs. nonkin among individuals in the first grooming quartile 4
rm(b)
gm_quant$quant_var<-relevel(gm_quant$quant_var, "gm_kin_4")
b<-summary(lmer(prop_gm ~  quant_var + (1|group/subj), data = gm_quant)) #compare ki base with it's non kin counterpart
kin_vs_nonkin_4 <- b$coefficients[rownames(b$coefficients)=="quant_vargm_nonkin_4",]
kin_vs_nonkin_4
