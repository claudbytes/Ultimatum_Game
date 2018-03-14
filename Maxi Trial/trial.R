
setwd("~/Maxi/Maxi Trial")


library(tidyverse)
library(lme4)
library(scales)

## create one dataframe 

results = list.files(pattern = ".csv")
data = lapply(results, read.csv, header = FALSE, stringsAsFactors = FALSE)
data_frame <- do.call("rbind", data)


data_frame <- data_frame[-1: -2, ] %>%  ## remove first 2 rows (completion code) 
  dplyr::select(V2, V7, V27, V28, V29, V33, V34)  ## and select relevant columns

colnames(data_frame) <- data_frame[1,]   ####### set the first row to be the header of the data frame 
final_data <- data_frame[-1, ] 

final_data <- filter(final_data, trialNo != "na", trialNo != "", trialNo != "trialNo") 

final_data <- final_data %>%  # add a column for sujb_id, 275 represents n° of trials per participant
  mutate("subj_id" = rep(1:55,each=275))   ### change 48 with actual number of participants


ultimatum <- final_data %>%   ## create a dataframe with only UltimatumGame results 
  group_by(subj_id) %>%   #### recode response as acceptance -> accepted=1, rejected = 0
  filter(game == "ultimatum") %>% 
  mutate(acceptance = if_else(response == 1, 0L, 1L)) %>% 
  dplyr::select(subj_id, everything()) 

####### boxplot 

box_summary <- ultimatum %>%
  group_by(stim_type, subj_id, offer) %>%
  dplyr::summarise(p = mean(acceptance))

box_summary$offer <- as.factor(box_summary$offer)
box_summary$stim_type <- as.factor(box_summary$stim_type)


offer_types <- c(
  `1` = "£10",
  `2` = "£25",
  `3` = "£40"
)

ggplot(box_summary, aes(x=stim_type, y=p, fill = stim_type)) + geom_boxplot(colour = "black", alpha = 0.7) + 
  facet_grid(~offer, labeller = as_labeller(offer_types)) +
  labs(fill = "Proposer Type", x = NULL, y = "Proportion of Accepted Offers", 
       title = "Distribution of Accepted Offers by Offer Type and Proposer Type", 
       subtitle = "Offer Amount", caption = "Data from the Ultimatum Game") +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = .5))


ultim_summary <- ultimatum %>%    #### descriptive statistics 
  group_by(stim_type, offer) %>%
  dplyr::summarise(p = mean(acceptance)) 

accepted_only <- ultimatum %>%
  filter(acceptance == 1) %>%
  group_by(offer) %>%
  dplyr::count(acceptance) %>%
  mutate(perc = n/4125*100) ## 4125 = NUMBER OF TRIALS PER PARTICIPANT BY OFFER = 75 * 55


### recoding for analysis 

ultimatum_dev <- ultimatum %>%
  dplyr::mutate(offer1 = ifelse(offer == "1", 0.7, -0.3),
                offer3 = ifelse(offer == "3", 0.7, -0.3)) %>%
  dplyr::mutate(Brand = ifelse(stim_type == "brand", 0.7, -0.3),
                Computer = ifelse(stim_type == "computer", 0.7, -0.3))
  
  
  
model <- glmer(acceptance ~ (offer1 + offer3) * (Brand + Computer) + ((offer1 + offer3)*(Brand+Computer)||subj_id), 
                   ultimatum_dev, binomial,control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))  

model_offer <- glmer(acceptance ~ offer1+ offer3 + ((offer1 + offer3)||subj_id), ultimatum_dev, binomial,
                     control = glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))
summary(model_offer)


summary(model)
anova(model)
cc <- confint(model,parm="beta_",method="Wald")
ctab <- cbind(Estimate=fixef(model),cc)
rtab <- exp(ctab)
print(rtab, digits = 3)

mod2 <- update(model, . ~ . -offer1 -offer3)
summary(mod2)
anova(model, mod2) # test main effect of type of offer

mod3 <- update(model, . ~ . -Brand - Computer)
summary(mod3)
anova(model, mod3) # test main effect of source (stim_type)

mod4 <- update(model, . ~ . -offer1:Brand - offer1:Computer - offer3:Brand - offer3:Computer)
anova(model, mod4) # test interaction


## homemade function to turn from logit (model estimates eg intercept + B estimates) into probability
logit_to_p <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

trust <- final_data %>%
  group_by(subj_id) %>%
  filter(game == "trust") %>%
  mutate(rating_binned = ifelse(response <=4, 1,
                                ifelse(response %in% 5:7, 2,
                                       ifelse(response >= 8, 3, 0)))) %>%
  mutate(rating1 = ifelse(response <= 4, .7, -.3 ),
         rating3 = ifelse(response >= 8, .7, -.3))

ultimatum_no_computer <- ultimatum_dev%>%
  filter(stim_type != "computer") %>%
  dplyr::select(stim_type, stim1, subj_id, acceptance, offer, offer1, offer3)

rating <- trust %>%
  group_by(stim1) %>%
  dplyr::select(rating_binned, subj_id, stim1, stim_type, rating1, rating3)

joined <- merge(rating, ultimatum_no_computer)
joined_model <- joined %>%
  dplyr::mutate(Brand = ifelse(stim_type == "brand", 0.5, -0.5))

join_summary <- joined %>%
  dplyr::group_by(subj_id, rating_binned, stim_type, offer) %>%
  dplyr::summarise(p = mean(acceptance))

join_summary$rating_binned <- as.factor(join_summary$rating_binned)

ggplot(join_summary, aes(x = rating_binned, y= p, fill=stim_type)) + 
  geom_boxplot(colour = "black", alpha = 0.7, position = "dodge") +
  coord_cartesian(ylim = c(0, 1), xlim = c(1,2,3)) +
  facet_grid(~offer, labeller = as_labeller(offer_types)) + labs(fill = "Proposer Type", x = "Ratings of Trustworthiness",
  y = "Proportion of Accepted Offers", title = "Proportion of Accepted Offers by Offer Type and Proposer Type \nat Different Rating Categories", 
  subtitle = "Offer Amount", caption = "Data from the Ultimatum Game") + 
  scale_y_continuous(labels = percent) + theme_bw()


join_rating$rating_binned <- as.factor(join_rating$rating_binned)

ggplot(join_rating, aes(x = rating_binned, y= p, fill=stim_type)) + 
  geom_col(alpha = 0.7, position = "dodge" ) + 
  coord_cartesian(ylim = c(0, 1), xlim = c(1,2,3)) +
  facet_grid(~offer, labeller = as_labeller(offer_types)) + labs(fill = "Proposer Type", x = "Ratings of Trustworthiness",
   y = "Proportion of Accepted Offers", title = "Proportion of Accepted Offers by Offer Type and Proposer Type \nat Different Rating Categories", 
   subtitle = "Offer Amount", caption = "Data from the Ultimatum Game") + 
  scale_y_continuous(labels = percent) + scale_x_discrete(breaks = c(1,2,3)) +
theme_bw() 

join_rating <- joined %>%
  dplyr::group_by(rating_binned, stim_type, offer) %>%
  dplyr::summarise(p = mean(acceptance), sd = sd(acceptance))
View(join_rating)

ggplot(join_offer1, aes(x = stim_type, y= p, fill=stim_type)) + 
  geom_boxplot(colour = "black", alpha = 0.7, position = "dodge") +
  coord_cartesian(ylim = c(0, 1), xlim = c(1,2,3)) +
  facet_grid(~rating_binned) + 
  labs(fill = "Proposer Type", x = NULL, y = "Proportion of Accepted Offers", 
  title = "Distribution of Acceptance of Unfair Offers (£10) by Proposer Type \nat Different Rating Categories", 
  subtitle = "Rating of Trustworthiness", caption = "Data from the Ultimatum Game") + 
  scale_y_continuous(labels = percent) + theme_bw() +
  theme(plot.title = element_text(hjust = .5))

######## model 

model.trust <- glmer(acceptance ~ (offer1 + offer3) * Brand  * (rating1 + rating3) + ((offer1 + offer3) * (Brand)||subj_id), joined_model, binomial, 
               control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))
exp(0.488)
summary(model.trust)

exp(-0.659)
mod2.trust <- update(model.trust, . ~ . -offer1 -offer3)
summary(mod2.trust)
anova(model.trust, mod2.trust) # test main effect of type of offer

mod3.trust <- update(model.trust, . ~ . -Brand )
summary(mod3.trust)
anova(model.trust, mod3.trust) # test main effect of source (stim_type)


### test main effect of ratings 

mod4.trust <- update(model.trust, . ~. -rating1 -rating3)
anova(model.trust, mod4.trust)

mod5.trust <- update(model.trust, . ~ . -offer1:brand - offer3:brand - offer1:rating1 - offer1:rating3 - offer3:rating1 - offer3:rating2 - brand:rating1 - brand:rating2)
anova(model.trust, mod5.trust) # test interaction

mod6.trust <- update(model.trust, . ~ . -offer1:Brand:rating1 -offer1:Brand:rating3 -offer3:Brand:rating1 -offer3:Brand:rating3)
anova(model.trust, mod5.trust)

##REACTION TIMES

ultimatum$RT <- as.numeric(ultimatum$RT)
ultim_RT <- ultimatum %>%
  dplyr::group_by(subj_id, stim_type, offer, stim1) %>%
  dplyr::summarise(mean_rt = mean(RT))

library(plyr)

se_rt <- ddply(ultimatum, c("stim_type", "offer", "subj_id"), summarise,
               mean = mean(RT), sd = sd(RT),
               se = sd(RT)/sqrt(length(RT)))
se_rt <- se_rt <- se_rt %>%
  filter(mean < 3000)

ggplot(se_rt, aes(x = offer, y= mean, fill=stim_type)) + 
  geom_boxplot(colour = "black", alpha = 0.7, position = "dodge") +
  scale_x_discrete(breaks = c(1,2,3), labels = c("£10", "£25","£40")) +
  facet_grid(~stim_type) + 
  labs(fill = "Proposer Type", x = NULL, y = "Mean Reaction Times [ms]", 
       title = "Distribution of Reaction Times in ms to Ultimatum Game offers by Proposer Type \nat and Offer Amount", caption = "Data from the Ultimatum Game") + theme_bw() +
  theme(plot.title = element_text(hjust = .5))
