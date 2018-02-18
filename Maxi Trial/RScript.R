library(tidyverse)

setwd("~/Maxi/Maxi Trial") 

############### getting all the .csv imported as one #################
results = list.files(pattern = ".csv")  ### list the files with extension .csv

data = lapply(results, read.csv, header = FALSE, stringsAsFactors = FALSE)  ## read them all in one list 

data1 <- do.call("rbind", data) ## join data frames in the list in a singular data.frame

###### preprocessing################

data1 <- data1[-1: -2, ]  ## remove first 2 rows (completion code)

dat <- data1 %>%
  select(V2, V7, V27, V28, V29, V33, V34) ### select relevant columns 

colnames(dat) <- dat[1,]   ####### set the first row to be the header of the data frame 
dat2 <- dat[-1, ] 

dat3 <- filter(dat2, trialNo != "na", trialNo != "", trialNo != "trialNo") ### get rid of more useless shit

dat3$RT <- as.numeric(dat3$RT)      ###### set some variables to be as.numeric for future things
dat3$response <- as.numeric(dat3$response)

dat3<- dat3 %>%                         #### add a column for sujb_id, change 275 with n° of trials per participant
  mutate("subj_id" = rep(1:31,each=275))   ### change 5 with actual number of participants

ultimatum <- dat3 %>%                  ##### create a dataframe with only UltimatumGame results + 
  group_by(subj_id) %>%          ########## recode response as acceptance -> accepted=1, rejected = 0
  filter(game == "ultimatum") %>% 
  mutate(acceptance = if_else(response == 1, 0L, 1L)) %>% 
  mutate(RT_S = RT / 1000) %>%
    select(subj_id, everything())      ### move subj_id to be the first column 

##### logistic regression

log <- glm(acceptance ~ offer*stim_type, binomial, ultimatum)
summary(log)

mod2 <- glm(acceptance ~ stim_type*offer + (subj_id / (stim_type * offer)), family = binomial, ultimatum)

summary(mod2)

############## anova

ultim_summary <- ultimatum %>%
  group_by(subj_id, stim_type, offer) %>%
   summarise(p = mean(acceptance), sd = sd(acceptance)) 

anova <- with(ultim_summary,
              aov(p ~ offer * stim_type +
                    Error(subj_id / (offer * stim_type)))
)

mod1 <- aov( p ~ offer * stim_type + Error(subj_id), ultim_summary)
summary(anova)

library(lme4)

######  plots 
boxplot <- ggplot(ultim_summary, aes(stim_type, p)) + geom_boxplot()
print(boxplot)

col_plot <- ggplot(ultim_summary, aes(x = stim_type, y = p, fill = stim_type)) + geom_col() +
  facet_grid(~offer) 

print(col_plot)


#####################

trust <- dat3 %>%
  group_by(subj_id) %>%         #### create a df with trust game results only + 
    filter(game == "trust") %>%    ###### bin ratings of trustworthiness in 3 categories:
      mutate(rating_binned = floor((response + .5) /3.5))  # split = [0,10,20,30]= 1                          
  #                                                                 [40, 50, 60] = 2
#########################################                           [70, 80, 90] = 3


##### plot frequency of acceptance in UG per proposer category (human/brand/computer)


trust_binned <- trust %>% ###### still need to figure out if I actually need this 
  group_by(stim_type, subj_id) %>%
  summarise(p2 = mean(rating_binned)) 

ultimatum_no_computer <- ultimatum %>%
  filter(stim_type != "computer") %>%
  select(stim_type, stim1, subj_id, acceptance, offer)

rating <- trust %>%
  group_by(stim1) %>%
select(rating_binned, subj_id, stim_type)

joined <- merge(rating, ultimatum_no_computer)

join_summary <- joined %>%
  group_by(rating_binned, stim_type, offer) %>%
    summarise(p = mean(acceptance))
      
rating_summary <- joined %>%
  group_by(rating_binned) %>%
    summarise(prob = mean(acceptance))

plot_carino <- ggplot(join_summary, aes(rating_binned, p, colour=stim_type)) + 
        geom_line() +   geom_point() +   coord_cartesian(ylim = c(0, 1)) +
           labs(y = "probability") + facet_grid(~offer)

print(plot_carino)

plot_bruttissimo <- ggplot(join_summary, aes(x=rating_binned,y=p, colour=stim_type)) +
geom_smooth(method="loess", aes(fill=rating_binned), span = 0.8, alpha=0.2) +
  theme_bw() + facet_grid(~offer) + 
  coord_cartesian(xlim = NULL, ylim = c(0,1.0))

log_trust <- glm(acceptance ~ offer * rating_binned * stim_type, binomial, joined)
summary(log_trust)

log_trust2 <- glm(acceptance ~ offer*rating_binned*stim_type + subj_id / (offer*rating_binned*stim_type), binomial, joined)
summary(log_trust2)

######################################## PART II, REACTION TIMES 

ultim_RT <- ultimatum %>%
  group_by(subj_id, stim_type, offer) %>%
  summarise(mean_rt = mean(RT), mean_p = mean(acceptance)) 

boxplot <- ggplot(ultim_RT, aes(offer, mean_rt)) + geom_boxplot()
print(boxplot)

col_plot <- ggplot(ultim_RT, aes(x = offer, y = mean_rt, fill = offer)) + geom_col() + facet_grid(~stim_type)

print(col_plot)

#### from the col_plot we can see that stim_intensity (fairness,unfairness) is related to shorter RTS.
###### uncertainty is correlated to longer reaction times --pierons law 

an2 = aov(mean_rt ~ stim_type*offer + Error(subj_id), ultim_RT)
summary(an2)

anova_rt <- lme(mean_rt ~ offer, ultim_RT, random = ~ 1 | subj_id)

summary(anova_rt)

anova_rt2 <- lme(mean_rt ~ stim_type, ultim_RT, random = ~ 1 | subj_id)

summary(anova_rt2)
anova(anova_rt, anova_rt2)

for (i in 1:length(results)) {
  assign(results[i],   
         read.csv(results[i], header = FALSE))
}







##### in case you need to import .csv files AND keep them as separate data frames use a for loop
