# Brand anthropomorphism in value-based decision making: a behavioural study on the responders' preferences in the Ultimatum Game 

The aim of this experiment is to investigate the behavioural correlates of brands perception. To our knowledge, no other study in the past has compared interaction between brands or people in an economic setting, nor has used the paradigm of the ultimatum game to investigate it. Therefore, we aim at bridging the conceptual gap between brands, objects and humans, investigating whether human beings tend to interact with brands in the same way they would interact with an object, or by ascribing it human-like traits and thus treating it as a human being.

## Methods 

Design and Apparatus
The experiment was built with the online platform Testable (Testable, 2018), lasted a total of 15
minutes on average and followed a full within-subjects design. Prior to the beginning of the task,
participants were asked to set their browsing window to full screen, to minimise possible distractions
that may affect performance (such as TV, Phone etc.), to position themselves at arm’s length from
the monitor and to maintain constant viewing distance throughout the experiment. Successively, they
were presented information on the nature and purpose of the study, followed by a specification of
the participants rights which allowed to give consent and proceed to the experiment. As an incentive,
participants could enter a raffle to win a £20 Amazon Voucher by emailing a completion code randomly
generated at the end of the experiment to the lead researcher. Demographic information such as age,
nationality and biological gender was also collected. The full experiment comprised of two main tasks:
a trustworthiness rating task and the Ultimatum Game.

The .csv file for the experiment on Testable is Testable_File.csv 
The folder \Results holds all the .csv files downloaded from Testable for each participant. 


The stimuliutilised in the two tasks can be found in the folder \Stimuli. 

With the first task of the experiment we gathered indirect ratings of trustworthiness that the partici-
pants produced towards brand logos and human faces.
The second task required participants to play the role of the responder
in a one-shot multiple-trials Ultimatum Game against three different types of proposers: humans,
brands and the computer. For humans and brands proposers we used the same stimuli presented in
the previous task and participants were instructed to take 4 brief breaks distributed evenly across the
225 trials. In order to assess the effect of different levels of offer unfairness and decision uncertainty,
the following three splits were employed: a fair offer in which the proposer offered the participants
£40 and retained £60 for themselves, an ambiguous offer in which the proposer suggested £25 for the
responder and £75 for themselves and lastly an unfair offer in which £10 were offered and £90 retained.


The data were processed and analysed using the R version 1.1.383 (R Core Team,
2018) with the IDE R Studio. The functions glmer and lmer from the lme4 package version 1.1-15
(Bates, Maechler, Bolker, & Walker, 2014) were utilised, respectively for generalized logit mixed-effects
and for linear mixed-effects models. Deviation coding was used to deal with polytomous categorical
variables. The optimal effects structure was obtained through models comparisons with likelihood
ratio tests. The analysis consisted of two main parts, namely regarding the acceptance rates and the
reaction times to the offers in the Ultimatum Game.

The file R_Code.R will allow to reproduce the analysis. 

The packages used were:

``` 
library(tidyverse)
library(lme4)
library(scales)

```
Before running the code make sure to set the working directory to the folder in which the .csv files from \Results were downloaded. 

```
setwd("~/UltimatumGame/Results") 

```


