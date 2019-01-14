library(interplot)

######
# Aim 1 - influences of ecological & life history variables on energy balance
#####
a<-lmer(avg.cp.resid~ sex + z.(age) + z.(m.rank.sp) + z.(prod.ripe.mid) + z.(avg.rain.per) + (1|group/subj),pc.df)
library(ggplot2)
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


######
# Aim 2 - influences of energy balance, life history, and competitive environment on dfGCs
######
full.e<-lmer(z.(s.dev.fgc) ~ sex + z.(age) + z.(gp.mates) + z.(m.rank.sp) + z.(avg.cp.resid) + (1|group/subj), data=cdf)
summary(full.e)

library(ggplot2)
coefs = as.data.frame(summary(full.e)$coefficients[-1,1:2]) 
colors<-c("royalblue1","blue")
coefs$vars = c("Sex (M)","Age","Mom rank","uCP")
coefs$vars <- factor(coefs$vars, levels=c("Sex (M)","Age","Mom rank","uCP"))
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

plot(pc.df$avg.cp.resid,pc.df$s.dev.fgc,col="blue",pch=1,cex=.75,
     cex.lab=1.5,cex.axis=1.5,
     ylab="Avg. deviation subject baseline fGCs", xlab="Avg. uCP residual") #
abline(lm(pc.df$s.dev.fgc~pc.df$avg.cp.resid), col="blue")

#####
# Aim 3 - social strategies influence on dfGCs
#####

# gm effect on coefficient of cp and fGCs
mi<-lmer(s.dev.fgc ~ gm.p.gmd + avg.cp.resid + avg.cp.resid*gm.p.gmd + (1|group), data=pc.df)
interplot(m = mi, var1 = "avg.cp.resid", var2 = "gm.p.gmd") +
  theme(axis.text.y=element_text(size=15))+
  theme(axis.text.x=element_text(size=15))

# gm quartiles & ucps
pc.df$quant.gm<- as.factor(as.numeric(quantcut(pc.df$gm.p.gmd))) #quantcut divides into q=4 by defaultg<-ggplot(pc.df, aes(x = avg.cp.resid, y = s.dev.fgc, colour = quant.gm)) + geom_point()
g<-ggplot(pc.df, aes(x = avg.cp.resid, y = s.dev.fgc, colour = quant.gm)) + geom_point()
g + geom_smooth(method="lm", se = FALSE) +
  labs(x="Avg. uCP residual", y = "Avg. deviation subject baseline fGCs", color = "Quartiles of \n % time grooming") +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15)) +
  theme(legend.position = c(0,1)) +
  theme(legend.justification = c(0,1)) +
  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))


# gm quartile kin v nonkin boxplots
xx<-melt(pc.df, id.var=c(colnames(x)[!(colnames(x)%in%c("kin","nonkin"))])) #kinship
xx$quant.var<-interaction(xx$variable, xx$quant.gm)
ggplot(xx,aes(x=quant.var, y=value, fill=variable)) + geom_boxplot() +
  scale_fill_manual(values=c("purple4","mediumpurple1")) +
  ggtitle("% Time grooming kin vs. non-kin \n by grooming quartile") +
  theme(plot.title = element_text(hjust=0.5)) +
  xlab("% Time grooming quartile") +
  ylab("% Time grooming") +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=15,angle=90)) +
  theme(axis.title.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15))