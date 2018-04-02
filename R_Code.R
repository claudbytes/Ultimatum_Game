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
  
ultimatum_model <- ultimatum %>%
  dplyr::mutate(offer1 = ifelse(offer == "1", 0.7, -0.3),
                offer3 = ifelse(offer == "3", 0.7, -0.3)) %>%
  dplyr::mutate(Brand = ifelse(stim_type == "brand", 0.7, -0.3),
                Computer = ifelse(stim_type == "computer", 0.7, -0.3)) %>%
  dplyr::filter(subj_id != 12) %>%
  dplyr::filter(subj_id != 1)

model <- glmer(acceptance ~ (offer1 + offer3) * (Brand + Computer) + ((offer1 + offer3)*(Brand+Computer)||subj_id), 
                   ultimatum_model, binomial,control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))  


summary(model)
anova(model)

mod2 <- update(model, . ~ . -offer1 -offer3)
summary(mod2)
anova(model, mod2) # test main effect of type of offer

mod3 <- update(model, . ~ . -Brand - Computer)
summary(mod3)
anova(model, mod3) # test main effect of source (stim_type)

mod4 <- update(model, . ~ . -offer1:Brand - offer1:Computer - offer3:Brand - offer3:Computer)
anova(model, mod4) # test interaction



######## contrasts
### subset 

### brand & computer
## £25
ultimatum1a <- ultimatum %>%
  dplyr::filter( stim_type == "brand" | stim_type == "computer") %>%
  dplyr::filter(offer == 2)
 
ultimatum1a$stim_type <- as.factor(ultimatum1a$stim_type)
ultimatum1a$stim_type <- relevel(ultimatum1a$stim_type, ref = "computer")

ultimatum1b <- ultimatum %>%
  dplyr::filter( stim_type == "brand" | stim_type == "computer") %>%
  dplyr::filter(offer == 1)

ultimatum1b$stim_type <- as.factor(ultimatum1b$stim_type)
ultimatum1b$stim_type <- relevel(ultimatum1b$stim_type, ref = "computer")


ultimatum1c <- ultimatum %>%
  dplyr::filter( stim_type == "brand" | stim_type == "computer") %>%
  dplyr::filter(offer == 3)


ultimatum1c$stim_type <- as.factor(ultimatum1c$stim_type)
ultimatum1c$stim_type <- relevel(ultimatum1c$stim_type, ref = "computer")


model_1a <- glmer(acceptance ~ stim_type + (stim_type||subj_id), 
               ultimatum1a, binomial,control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))  


model_1b <- glmer(acceptance ~ stim_type + (stim_type||subj_id), 
                  ultimatum1b, binomial,control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))  


model_1c <- glmer(acceptance ~ stim_type + (stim_type||subj_id), 
                  ultimatum1c, binomial,control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))  

summary(model_1a) #£25
summary(model_1b) #£10
summary(model_1c) #£40


### humans vs computers 

ultimatum2a <- ultimatum %>%
  dplyr::filter( stim_type == "human" | stim_type == "computer") %>%
  dplyr::filter(offer == 2)

ultimatum2a$stim_type <- as.factor(ultimatum2a$stim_type)
ultimatum2a$stim_type <- relevel(ultimatum2a$stim_type, ref = "computer")

ultimatum2b <- ultimatum %>%
  dplyr::filter( stim_type == "human" | stim_type == "computer") %>%
  dplyr::filter(offer == 1)

ultimatum2b$stim_type <- as.factor(ultimatum2b$stim_type)
ultimatum2b$stim_type <- relevel(ultimatum2b$stim_type, ref = "computer")


ultimatum2c <- ultimatum %>%
  dplyr::filter( stim_type == "human" | stim_type == "computer") %>%
  dplyr::filter(offer == 3)

ultimatum2c$stim_type <- as.factor(ultimatum2c$stim_type)
ultimatum2c$stim_type <- relevel(ultimatum2c$stim_type, ref = "computer")


model_2a <- glmer(acceptance ~ stim_type + (stim_type||subj_id), 
                  ultimatum2a, binomial,control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))  


model_2b <- glmer(acceptance ~ stim_type + (stim_type||subj_id), 
                  ultimatum2b, binomial,control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))  


model_2c <- glmer(acceptance ~ stim_type + (stim_type||subj_id), 
                  ultimatum2c, binomial,control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))  

summary(model_2a) ## offer £25
summary(model_2b) ## offer £10
summary(model_2c) ## offer £40

### brand vs human 


ultimatum3a <- ultimatum %>%
  dplyr::filter( stim_type == "human" | stim_type == "brand") %>%
  dplyr::filter(offer == 2)

ultimatum3a$stim_type <- as.factor(ultimatum3a$stim_type)
ultimatum3a$stim_type <- relevel(ultimatum3a$stim_type, ref = "human")

ultimatum3b <- ultimatum %>%
  dplyr::filter( stim_type == "human" | stim_type == "brand") %>%
  dplyr::filter(offer == 1)

ultimatum3b$stim_type <- as.factor(ultimatum3b$stim_type)
ultimatum3b$stim_type <- relevel(ultimatum3b$stim_type, ref = "human")


ultimatum3c <- ultimatum %>%
  dplyr::filter( stim_type == "human" | stim_type == "brand") %>%
  dplyr::filter(offer == 3)

ultimatum3c$stim_type <- as.factor(ultimatum3c$stim_type)
ultimatum3c$stim_type <- relevel(ultimatum3c$stim_type, ref = "human")


model_3a <- glmer(acceptance ~ stim_type + (stim_type||subj_id), 
                  ultimatum2a, binomial,control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))  


model_3b <- glmer(acceptance ~ stim_type + (stim_type||subj_id), 
                  ultimatum2b, binomial,control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))  


model_3c <- glmer(acceptance ~ stim_type + (stim_type||subj_id), 
                  ultimatum2c, binomial,control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))  

summary(model_3a) ## offer £25
summary(model_3b) ## offer £10
summary(model_3c) ## offer £40


################# 

ultimatum_offer1 <- ultimatum %>%
  dplyr::filter(offer == 1)

ultimatum_offer2 <- ultimatum %>%
  dplyr::filter(offer == 2)

ultimatum_offer3 <- ultimatum %>%
  dplyr::filter(offer == 3)

## brand vs computer and brand vs human 
model_offer1 <- glmer(acceptance ~ stim_type + ((stim_type)||subj_id), 
               ultimatum_offer1, binomial,control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))  


model_offer2 <- glmer(acceptance ~ stim_type + ((stim_type)||subj_id), 
                      ultimatum_offer2, binomial,control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))  


model_offer3 <- glmer(acceptance ~ stim_type + ((stim_type)||subj_id), 
                      ultimatum_offer3, binomial,control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000))) 

summary(model_offer1) #£10

summary(model_offer2) #£25

summary(model_offer3) #£40

### set computer as baseline 

ultimatum_offer1$stim_type <- as.factor(ultimatum_offer1$stim_type)
ultimatum_offer1$stim_type <- relevel(ultimatum_offer1$stim_type, ref = "computer")


ultimatum_offer2$stim_type <- as.factor(ultimatum_offer2$stim_type)
ultimatum_offer2$stim_type <- relevel(ultimatum_offer2$stim_type, ref = "computer")


ultimatum_offer3$stim_type <- as.factor(ultimatum_offer3$stim_type)
ultimatum_offer3$stim_type <- relevel(ultimatum_offer3$stim_type, ref = "computer")

#############

model_offer1a <- glmer(acceptance ~ stim_type + ((stim_type)||subj_id), 
                      ultimatum_offer1, binomial,control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))  


model_offer2a <- glmer(acceptance ~ stim_type + ((stim_type)||subj_id), 
                      ultimatum_offer2, binomial,control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))  


model_offer3a <- glmer(acceptance ~ stim_type + ((stim_type)||subj_id), 
                      ultimatum_offer3, binomial,control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000))) 

summary(model_offer1a) #£10

summary(model_offer2a) #£25

summary(model_offer3a) #£40



##  function to turn from logit (model estimates eg intercept + B estimates) into probability
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

summary(model.trust)

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

## contrasts 

### offer £10, rating 1 

trust_1a <- joined %>%
  dplyr::filter(offer == 1) %>%
  dplyr::filter(rating_binned == 1)

trust_1b <- joined %>%
  dplyr::filter(offer == 2) %>%
  dplyr::filter(rating_binned == 1)

trust_1c <- joined %>%
  dplyr::filter(offer == 3) %>%
  dplyr::filter(rating_binned == 1)

model_trust1a <- glmer(acceptance ~ stim_type + (stim_type||subj_id), trust_1a, binomial, 
                       control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))

model_trust1b <- glmer(acceptance ~ stim_type + (stim_type||subj_id), trust_1b, binomial, 
                       control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))


model_trust1c <- glmer(acceptance ~ stim_type + (stim_type||subj_id), trust_1c, binomial, 
                       control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))


summary(model_trust1a) # offer1, rating1
summary(model_trust1b) # offer2, rating1
summary(model_trust1c) #offer3, rating 1 

###### rating 2 


trust_2a <- joined %>%
  dplyr::filter(offer == 1) %>%
  dplyr::filter(rating_binned == 2)

trust_2b <- joined %>%
  dplyr::filter(offer == 2) %>%
  dplyr::filter(rating_binned == 2)

trust_2c <- joined %>%
  dplyr::filter(offer == 3) %>%
  dplyr::filter(rating_binned == 2)

model_trust2a <- glmer(acceptance ~ stim_type + (stim_type||subj_id), trust_2a, binomial, 
                       control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))

model_trust2b <- glmer(acceptance ~ stim_type + (stim_type||subj_id), trust_2b, binomial, 
                       control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))


model_trust2c <- glmer(acceptance ~ stim_type + (stim_type||subj_id), trust_2c, binomial, 
                       control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))



summary(model_trust2a) # offer1, rating2
summary(model_trust2b) # offer2, rating2
summary(model_trust2c) #offer3, rating 2 

####### rating 3 


trust_3a <- joined %>%
  dplyr::filter(offer == 1) %>%
  dplyr::filter(rating_binned == 3)

trust_3b <- joined %>%
  dplyr::filter(offer == 2) %>%
  dplyr::filter(rating_binned == 3)

trust_3c <- joined %>%
  dplyr::filter(offer == 3) %>%
  dplyr::filter(rating_binned == 3)

model_trust3a <- glmer(acceptance ~ stim_type + (stim_type||subj_id), trust_3a, binomial, 
                       control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))

model_trust3b <- glmer(acceptance ~ stim_type + (stim_type||subj_id), trust_3b, binomial, 
                       control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))


model_trust3c <- glmer(acceptance ~ stim_type + (stim_type||subj_id), trust_3c, binomial, 
                       control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))



summary(model_trust3a) # offer1, rating3
summary(model_trust3b) # offer2, rating3
summary(model_trust3c) #offer3, rating 3 


###### further ratings 
######### offer 1 
trust_4a <- joined %>%
  dplyr::filter(offer == 1) 

trust_4b <- joined %>%
  dplyr::filter(offer == 2) 


trust_4c <- joined %>%
  dplyr::filter(offer == 3) 

trust_4a$rating_binned <- as.factor(trust_4a$rating_binned)

trust_4b$rating_binned <- as.factor(trust_4b$rating_binned)

trust_4c$rating_binned <- as.factor(trust_4c$rating_binned)

trust_4c$rating_binned <- relevel(trust_4c$rating_binned, ref = "2")

model_trust4a <- glmer(acceptance ~ rating_binned + (1|subj_id), trust_4a , binomial, 
                       control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))


model_trust4b <- glmer(acceptance ~ rating_binned + (1|subj_id), trust_4b , binomial, 
                       control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))


model_trust4c <- glmer(acceptance ~ rating_binned + (1|subj_id), trust_4c, binomial, 
                       control=glmerControl(optimizer="bobyqa", optCtrl= list(maxfun=100000)))



summary(model_trust4a) ## offer £10
summary(model_trust4b) ## offer £25
summary(model_trust4c) ## offer £40


##REACTION TIMES

ultimatum$RT <- as.numeric(ultimatum$RT)
ultim_RT <- ultimatum %>%
  dplyr::group_by(subj_id, stim_type, offer, stim1) %>%
  dplyr::summarise(mean_rt = mean(RT)) %>%


library(plyr)

se_rt <- ddply(ultimatum_dev, c("stim_type", "offer", "subj_id"), summarise,
               mean = mean(RT), sd = sd(RT),
               se = sd(RT)/sqrt(length(RT)))
se_rt <- se_rt <- se_rt %>%
  filter(mean < 3000)

ggplot(se_rt, aes(x = offer, y= mean, fill=stim_type)) + 
  geom_boxplot(colour = "black", alpha = 0.7, position = "dodge") +
  scale_x_discrete(breaks = c(1,2,3), labels = c("£10", "£25","£40")) +
  facet_grid(~stim_type) + 
  labs(fill = "Proposer Type", x = NULL, y = "Mean Reaction Times [ms]", 
       caption = "Data from the Ultimatum Game") + theme_bw() +
  theme(plot.title = element_text(hjust = .5))

model.RT <- lmer(RT ~ (offer1 + offer3) * (Brand + Computer) + ( (offer1 + offer3) * (Brand + Computer)||subj_id), ultimatum_dev)
summary(model.RT)

anova(model.RT)
model.RT2<- update(model.RT, . ~ . -offer1 -offer3)
anova(model.RT, model.RT2) # test main effect of first factor

model.RT3 <- update(model.RT, . ~ . -Brand - Computer)
anova(model.RT, model.RT3) # test main effect of second factor

model.RT4 <- update(model.RT, . ~ . -offer1:Brand - offer1:Computer - offer3:Brand - offer3:Computer)
anova(model.RT, model.RT4) # test interaction

###### planned contrasts for reaction times 
ultimatum$offer <- as.factor(ultimatum$offer)
ultimatum$offer <- relevel(ultimatum$offer, ref = "2")
model_RT_overall <- lmer(RT ~ offer + (offer|subj_id), ultimatum)

model_RT_proposer <- lmer(RT ~ stim_type + (stim_type|subj_id), ultimatum)

model45 <- lmer(RT ~ offer * stim_type + (offer * stim_type || subj_id), ultimatum)
summary(model45)
times_a <- ultimatum %>%
  dplyr::filter(stim_type == "brand")


times_b <- ultimatum %>%
  dplyr::filter(stim_type == "computer")

times_c <- ultimatum %>%
  dplyr::filter(stim_type == "human")

times_a$offer <- as.factor(times_a$offer)
times_b$offer <- as.factor(times_b$offer)
times_c$offer <- as.factor(times_c$offer)

model.times_a <- lmer(RT ~ offer + (offer|subj_id), times_a)
model.times_b <- lmer(RT ~ offer + (offer|subj_id), times_b)
model.times_c <- lmer(RT ~ stim_type + (stim_type|subj_id), times_c)

summary(model.times_a)

