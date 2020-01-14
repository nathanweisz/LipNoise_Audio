setwd('/Users/b1019548/Dropbox/paper/LipNoise_Audio/SixChanVoc/')

library(R.matlab)
library(lme4)
library(dplyr)
library(ggeffects)
#library(extrafont)
library(ggplot2)
#library(reshape2)
#library(Cairo)
#library(varhandle)
#loadfonts(quiet = T)

##READ DATA

tmp<-readMat('alpha.mat')

alpha<-matrix(nrow=length(tmp$alpha.vec), ncol=6)

for(ii in 1:length(tmp$alpha.vec)){
  alpha[ii,]<-unlist(tmp$alpha.vec[[ii]])
}

tmp<-readMat('speech_coh.mat')

speech_coup<-matrix(nrow=length(tmp$coh.vec), ncol=6)

for(ii in 1:length(tmp$coh.vec)){
  speech_coup[ii,]<-unlist(tmp$coh.vec[[ii]])
}


tmp<-readMat('behav.mat')

behav<-matrix(nrow=length(tmp$behav.vec), ncol=6)

for(ii in 1:length(tmp$behav.vec)){
  behav[ii,]<-unlist(tmp$behav.vec[[ii]])
}

## MAKE SOME LOW LEVEL STUFF

#alpha_trans<-(1/(alpha^2)) #lower alpha --> higher value
alpha_trans<-t(apply(alpha, 1,
                     function(x){
                       -(x-mean(x))+mean(x)
                     }
                     )) #Flip around mean

alpha_trans<-t(apply(alpha_trans, 1, 
                     function(x){
                       (x/x[1])
                     }
                    )) #Flip around mean

speech_coup_trans<-t(apply(speech_coup, 1, 
                     function(x){
                       (x/x[1])
                     }
                    )) #Flip around mean


plot(apply(alpha_trans, 2, mean))
plot(apply(speech_coup_trans, 2, mean))

#beh_pred<-speech_coup/(alpha_trans)
beh_pred_lin<-speech_coup_trans/(alpha_trans)
beh_pred_quad<-speech_coup_trans/(alpha_trans^2)
plot(apply(beh_pred_lin, 2, mean))

#scale observed and predicted behavior
beh_pred_lin_s<-t(apply(beh_pred_lin, 1,
                     function(x){
                       (x-mean(x))/mean(x)
                     }
))

beh_pred_quad_s<-t(apply(beh_pred_quad, 1,
                        function(x){
                          (x-mean(x))/mean(x)
                        }
))

behav_s<-t(apply(behav, 1,
                  function(x){
                    (x-mean(x))/mean(x)
                  }
))

activation_s<-t(apply(alpha_trans, 1,
                 function(x){
                   (x-mean(x))/mean(x)
                 }
))

tracking_s<-t(apply(speech_coup_trans, 1,
                      function(x){
                        (x-mean(x))/mean(x)
                      }
))

#Dirty correlation test ... Drop unvocoded and test on "modulation part"
tmpcorr_lin<-c(1:nrow(behav_s))
tmpcorr_quad<-c(1:nrow(behav_s))
for(ii in 1:nrow(behav_s)){
  tmpcorr_lin[ii]<-cor(beh_pred_lin_s[ii,2:6],behav_s[ii,2:6], met='pear')
  tmpcorr_quad[ii]<-cor(beh_pred_quad_s[ii,2:6],behav_s[ii,2:6], met='pear')
}

ii<-15
plot(beh_pred_lin_s[ii,], pch=21, col='red',ylim=c(-1, 1))
points(behav_s[ii,], pch=23)


t.test(tmpcorr_lin) #t = 2.4916, df = 16, p-value = 0.02408
t.test(tmpcorr_quad) #t = 3.0678, df = 16, p-value = 0.00736

## 

subj_id <- rep(factor(c(1:nrow(behav_s))), 6)
voc_lev <- factor(rep(c(1:6), each = nrow(behav_s)))

all.df<-data.frame(subj = subj_id, vocoding = voc_lev, intelligibility = c(behav_s),
                   ratio_lin=c(beh_pred_lin_s), ratio_quad=c(beh_pred_quad_s),
                   activation = c(activation_s), tracking = c(tracking_s))

all.df <- filter(all.df, vocoding != 1)

test_lin<-lmer(intelligibility ~ -1 + ratio_lin + (-1 + ratio_lin | subj), 
           data=all.df, REML = FALSE)

test_quad<-lmer(intelligibility ~ -1 + ratio_quad + (-1 + ratio_quad | subj) , 
               data=all.df, REML = FALSE)

test_act<-lmer(intelligibility ~ -1 + activation + (-1 + activation | subj), 
               data=all.df, REML = FALSE)

test_track<-lmer(intelligibility ~ -1 + tracking + (-1 + tracking | subj), 
               data=all.df, REML = FALSE)

Anova(test_lin)
#Response: intelligibility
#Chisq Df Pr(>Chisq)   
#ratio_lin 7.2863  1   0.006948 **
anova(test_track, test_act, test_lin) # compare to tracking and actication alone
#Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#test_track  3 24.123 31.451 -9.0614   18.123                             
#test_act    3 22.777 30.105 -8.3887   16.777 1.3455      0  < 2.2e-16 ***
#test_lin    3 19.677 27.005 -6.8386   13.677 3.1002      0  < 2.2e-16 ***

ggplot(all.df, aes(x = ratio_lin, y = intelligibility, group=subj, col=subj)) +
  facet_wrap(~subj, nrow=4) +   # a panel for each mountain range
  geom_point(alpha = 0.5) +
  theme_classic()

## Depict Models

pred_lin.mm <- ggpredict(test_lin, terms=c("ratio_lin")) 

ggplot(pred_lin.mm) + 
  geom_line(aes(x = x, y = predicted)) +          # slope
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  geom_point(data = all.df,                      # adding the raw data (scaled values)
             aes(x = ratio_lin, y = intelligibility, colour = subj)) +
  labs(x = "Predicted Intelligibility", y = "Observed Intelligibility", 
       title = "Combined Model") + 
  theme_minimal()


pred_act.mm <- ggpredict(test_act, terms=c("activation")) 

ggplot(pred_act.mm) + 
  geom_line(aes(x = x, y = predicted)) +          # slope
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  geom_point(data = all.df,                      # adding the raw data (scaled values)
             aes(x = ratio_lin, y = intelligibility, colour = subj)) +
  labs(x = "Activation Model", y = "Observed Intelligibility", 
       title = "Blabla") + 
  theme_minimal()

