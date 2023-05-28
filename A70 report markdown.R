---
  title: "Prevar report"
  author: "Duncan Hedderley"
  date: "October 2021"
output: html_notebook
editor_options: 
  chunk_output_type: console
---
  
```{r library}
library(readxl)
library(openxlsx)
library(tidyr)
library(dplyr)
library(purrr)
library(lme4)
library(predictmeans)
library(car)
library(knitr)
library(emmeans)
library(httr)
```

```
Orchard to orchard differences, comparing elites with commercial varieties   (This analysis is using all the commercial trees at a site as references, rather than just the ones bordering the specific line being tested)
(The control trees here are the same as for A133 and A205)

```

```{r data iplant, eval=FALSE}
#read in data ----
#username <- Sys.getenv("USER")
#password <- Sys.getenv("PASSWORD")

#url <- "https://iplant.plantandfood.co.nz/project2/P227001-01/Research/Mussel-Up-Study2-data-for-stats.xlsx"
#httr::GET(url, authenticate(username, password, type = "ntlm"),  write_disk(tf <- tempfile(fileext = ".xlsx")))

```

```{r data downloaded}
All <-read_excel("H:/PreVar 2022/PREVAR-Stats-A70A133-21+22.xlsx", sheet='Sheet1',
               range='a6:ac70')
#biomark <-readxl::read_excel(tf, sheet='Biomarkers', range='a1:ak201', na=c('','NA','Missing'))

A70 <- All[All$Cultivar %in% c('A70','Galaxy','FujiS'),]
names(A70) <- make.names(names(A70))
#str(A70)

A70$Region <- as.factor(A70$Region)          
A70$Site  <- as.factor(A70$Site)          
A70$Rep  <- as.factor(A70$Rep)           
A70$Cultivar <- as.factor(A70$Cultivar)

```
##Tree structure and flowering measures
There was a lot of orchard to orchard variation. A70 was shorter than either Fuji or Galaxy. S&T clusters were lower, more like Fuji than Galaxy. Total fruit cluster numbers were high in HB, but lower, more like Fuji in Nelson. 
```

```{r Tree structure and flowering ANOVAs}
## Kate
# model ------
TSF_mod <- A70 %>% 
  select("Region", "Site", "Rep", "Cultivar", "mean.TCA", "Hgt", "S.TFCD", "TotFCD") %>% 
  # make long
  pivot_longer(names_to = "variable", values_to = "value", cols=c(mean.TCA:TotFCD)) %>% 
  # so runs for each variable
  group_by(variable) %>% nest() %>% 
  # model
  mutate(fit=map(data, ~lm(value ~ Region + Region:Site + Cultivar + Cultivar:Region + 
                             Cultivar:Region:Site, data=.x)))

# if you wanted to check residuals---------

#residplot(TSF_mod$fit[[1]], newwd=FALSE)
#residplot(TSF_mod$fit[[2]], newwd=FALSE)
#residplot(TSF_mod$fit[[3]], newwd=FALSE)
#residplot(TSF_mod$fit[[4]], newwd=FALSE)

# anova--------

TSF_anova<-TSF_mod %>% 
  #run anova
  mutate(anovaR=map(fit, ~Anova(.x))) %>% 
  #tidy the anova output
  mutate(tidyV=map(anovaR, broom::tidy)) %>%
  #unstack
  unnest(tidyV, .drop=TRUE)

#long table
#kable(TSF_anova[, c(1,5,7,8,9)])
TSF_anova_longTable <- TSF_anova[, c(1,5,7,8,9)]

names(TSF_anova)[8] <- 'F'
names(TSF_anova)[9] <- 'p'
names(TSF_anova_longTable)[4] <- 'F'
names(TSF_anova_longTable)[5] <- 'p'


# wide table
TSF_anova_wide<-TSF_anova %>% select(1,5,7,8,9) %>% pivot_wider(names_from = "variable", values_from = c("F","p"))
#kable(TSF_anova_wide[c(1,3,2,4,5,6), c(1,2,3,7,4,8,5,9,6,10 )], digits=c(1,3,1,3,1,3,1,3))
TSF_anova_wideTable<- TSF_anova_wide[c(1,3,2,4,5,6), c(1,2,3,7,4,8,5,9,6,10 )]

f <- as.numeric(TSF_anova_wideTable[1,3]/TSF_anova_wideTable[2,3])
TSF_anova_wideTable[1,3] <- f
TSF_anova_wideTable[1,4] <- pf(f,1,2, lower.tail = F)

f <- as.numeric(TSF_anova_wideTable[1,5]/TSF_anova_wideTable[2,5])
TSF_anova_wideTable[1,5] <- f
TSF_anova_wideTable[1,6] <- pf(f,1,2, lower.tail = F)

f <- as.numeric(TSF_anova_wideTable[1,7]/TSF_anova_wideTable[2,7])
TSF_anova_wideTable[1,7] <- f
TSF_anova_wideTable[1,8] <- pf(f,1,2, lower.tail = F)

f <- as.numeric(TSF_anova_wideTable[1,9]/TSF_anova_wideTable[2,9])
TSF_anova_wideTable[1,9] <- f
TSF_anova_wideTable[1,10] <- pf(f,1,2, lower.tail = F)

kable(TSF_anova_wide[c(1,3,2,4,5,6), c(1,2,3,7,4,8,5,9,6,10 )], digits=c(1,3,1,3,1,3,1,3))

#options(knitr.kable.NA = '')
```
Region is tested compared to the Orchgard to Orchard variation.

The means for each variety and each variety in each region are below (Orchard x variety means in appendix)
```{r Tree structure and flowering Means tables}
# means------------------------

TSF_means1<-TSF_mod %>% 
  #predictmeans
#  mutate(pm=map(fit, ~predictmeans(., "Cultivar", plot=FALSE, pairwise = TRUE, adj="fdr"))) %>% 
  mutate(pm=map(fit, ~predictmeans(., "Cultivar", plot=FALSE))) %>% 
  mutate(meantab=map(pm, ~((.x)$`mean_table`))) %>% 
  mutate(lsdtab=map(pm, ~((.x)$`LSD`)))

#mean table
TSF_means1_unnest<-TSF_means1 %>% unnest(meantab, .drop=TRUE) %>% select(1,5,6)
TSF_means1_wide<-TSF_means1_unnest %>% pivot_wider(names_from="variable", values_from = "Predicted means")
TSF_means1_wide <- TSF_means1_wide[c(2,3,1),]

#lsd table
TSF_LSD_unnest<- TSF_means1 %>% unnest(lsdtab, .drop=TRUE) 
TSF_LSD_unnest$LSD.n=rep(c("Max.LSD","Min.LSD","Aveg.LSD"),4)
TSF_LSD_tab<-TSF_LSD_unnest %>% select(1,6,7) %>% pivot_wider(names_from="variable", values_from = "lsdtab")

#trying to combine these does my head in, so maybe just print one then the other?
kable(TSF_means1_wide, digits=c(1,1,1,1))
kable(TSF_LSD_tab[(TSF_LSD_tab$LSD.n %in% 'Aveg.LSD'),], digits=c(1,1,1,1))



TSF_means2<-TSF_mod %>% 
  #predictmeans
  #  mutate(pm=map(fit, ~predictmeans(., "Cultivar", plot=FALSE, pairwise = TRUE, adj="fdr"))) %>% 
  mutate(pm=map(fit, ~predictmeans(., "Region:Cultivar", plot=FALSE))) %>% 
  mutate(meantab=map(pm, ~((.x)$`mean_table`))) %>% 
  mutate(lsdtab=map(pm, ~((.x)$`LSD`)))

#mean table
TSF_means2_unnest<-TSF_means2 %>% unnest(meantab, .drop=TRUE) %>% select(1,5,6,7)
TSF_means2_wide<-TSF_means2_unnest %>% pivot_wider(names_from="variable", values_from = "Predicted means")
TSF_means2_wide <- TSF_means2_wide[c(2,3,1,5,6,4),]

#lsd table
TSF_LSD2_unnest<- TSF_means2 %>% unnest(lsdtab, .drop=TRUE) 
TSF_LSD2_unnest$LSD.n=rep(c("Max.LSD","Min.LSD","Aveg.LSD"),4)
TSF_LSD2_tab<-TSF_LSD2_unnest %>% select(1,6,7) %>% pivot_wider(names_from="variable", values_from = "lsdtab")

#trying to combine these does my head in, so maybe just print one then the other?
kable(TSF_means2_wide, digits=c(1,1,1,1))
kable(TSF_LSD2_tab[(TSF_LSD2_tab$LSD.n %in% 'Aveg.LSD'),], digits=c(1,1,1,1))

```

##Yield measures
There was a lot of orchard to orchard variation in the yield measures. Fruit drop was more of a problem with A70 than Galaxy.  A70 mean fruit weight was high, more like Fuji than Galaxy; the variability was proportionally higher too. Yield, yield efficiency and crop density were comparable or better than Fuji or Galaxy in HB, but lower in Nelson.
```

```{r Yield ANOVAs}
## Kate
# model ------
#DropWt missing for Controls in HB 
Y_mod <- A70 %>% 
  select("Region", "Site", "Rep", "Cultivar", "DropWt","MFW","SD.MFW","yield.ha","Yield.Efficiency",
         "crop.density") %>% 
  # make long
  pivot_longer(names_to = "variable", values_to = "value", cols=c(DropWt:crop.density)) %>% 
  # so runs for each variable
  group_by(variable) %>% nest() %>% 
  # model
  mutate(fit=map(data, ~lm(value ~ Region + Region:Site + Cultivar + Cultivar:Region + 
                             Cultivar:Region:Site, data=.x)))

# if you wanted to check residuals---------

#residplot(Y_mod$fit[[1]], newwd=FALSE)
#residplot(Y_mod$fit[[2]], newwd=FALSE)
#residplot(Y_mod$fit[[3]], newwd=FALSE)
#residplot(Y_mod$fit[[4]], newwd=FALSE)

# anova--------

Y_anova<-Y_mod %>% 
  #run anova
  mutate(anovaR=map(fit, ~Anova(.x))) %>% 
  #tidy the anova output
  mutate(tidyV=map(anovaR, broom::tidy)) %>%
  #unstack
  unnest(tidyV, .drop=TRUE)

#long table
#kable(Y_anova[, c(1,5,7,8,9)])
Y_anova_longTable <- Y_anova[, c(1,5,7,8,9)]

names(Y_anova)[8] <- 'F'
names(Y_anova)[9] <- 'p'
names(Y_anova_longTable)[4] <- 'F'
names(Y_anova_longTable)[5] <- 'p'


# wide table
#Y_anova_wide<-Y_anova %>% select(1,5,7,8,9) %>% pivot_wider(names_from = "variable", values_from = c("F","p"))
#kable(Y_anova_wide[c(1,3,2,4,5,6), c(1,2,3,7,4,8,5,9,6,10 )], digits=c(1,3,1,3,1,3,1,3))
#Y_anova_wideTable<- Y_anova_wide[c(1,3,2,4,5,6), c(1,2,3,9,4,10,5,11,6,12,7,13,8,14 )]

#missing data for DropWt, so can't include DF in the 'conserved' columns
Y_anova_wide<-Y_anova %>% select(1,5,7,8,9) %>% pivot_wider(names_from = "variable", values_from = c("df","F","p"))
Y_anova_wideTable<- Y_anova_wide[c(1,3,2,4,5,6), c(1,2,8,14,9,15,10,16,11,17,12,18,13,19)]


f <- as.numeric(Y_anova_wideTable[1,3]/Y_anova_wideTable[2,3])
Y_anova_wideTable[1,3] <- f
Y_anova_wideTable[1,4] <- pf(f,1,2, lower.tail = F)

f <- as.numeric(Y_anova_wideTable[1,5]/Y_anova_wideTable[2,5])
Y_anova_wideTable[1,5] <- f
Y_anova_wideTable[1,6] <- pf(f,1,2, lower.tail = F)

f <- as.numeric(Y_anova_wideTable[1,7]/Y_anova_wideTable[2,7])
Y_anova_wideTable[1,7] <- f
Y_anova_wideTable[1,8] <- pf(f,1,2, lower.tail = F)

f <- as.numeric(Y_anova_wideTable[1,9]/Y_anova_wideTable[2,9])
Y_anova_wideTable[1,9] <- f
Y_anova_wideTable[1,10] <- pf(f,1,2, lower.tail = F)

f <- as.numeric(Y_anova_wideTable[1,11]/Y_anova_wideTable[2,11])
Y_anova_wideTable[1,11] <- f
Y_anova_wideTable[1,12] <- pf(f,1,2, lower.tail = F)

f <- as.numeric(Y_anova_wideTable[1,13]/Y_anova_wideTable[2,13])
Y_anova_wideTable[1,13] <- f
Y_anova_wideTable[1,14] <- pf(f,1,2, lower.tail = F)

kable(Y_anova_wideTable, digits=c(0,1,3,1,3,1,3,1,3,1,3,1,3))

#options(knitr.kable.NA = '')
```
Region is tested compared to the Orchard to Orchard variation.

The means for each variety and each variety in each region are below (Orchard x variety means in appendix)
```{r Yueld Means tables}
# means------------------------

Y_means1<-Y_mod %>% 
  #predictmeans
  #  mutate(pm=map(fit, ~predictmeans(., "Cultivar", plot=FALSE, pairwise = TRUE, adj="fdr"))) %>% 
  mutate(pm=map(fit, ~predictmeans(., "Cultivar", plot=FALSE))) %>% 
  mutate(meantab=map(pm, ~((.x)$`mean_table`))) %>% 
  mutate(lsdtab=map(pm, ~((.x)$`LSD`)))

#mean table
Y_means1_unnest<-Y_means1 %>% unnest(meantab, .drop=TRUE) %>% select(1,5,6)
Y_means1_wide<-Y_means1_unnest %>% pivot_wider(names_from="variable", values_from = "Predicted means")
Y_means1_wide <- Y_means1_wide[c(2,3,1),]

#lsd table
Y_LSD_unnest<- Y_means1 %>% unnest(lsdtab, .drop=TRUE) 
Y_LSD_unnest$LSD.n=rep(c("Max.LSD","Min.LSD","Aveg.LSD"),6)
Y_LSD_tab<-Y_LSD_unnest %>% select(1,6,7) %>% pivot_wider(names_from="variable", values_from = "lsdtab")

#trying to combine these does my head in, so maybe just print one then the other?
kable(Y_means1_wide, digits=c(1,1,1,1))
kable(Y_LSD_tab[(Y_LSD_tab$LSD.n %in% 'Aveg.LSD'),], digits=c(1,1,1,1))



Y_means2<-Y_mod %>% 
  #predictmeans
  #  mutate(pm=map(fit, ~predictmeans(., "Cultivar", plot=FALSE, pairwise = TRUE, adj="fdr"))) %>% 
  mutate(pm=map(fit, ~predictmeans(., "Region:Cultivar", plot=FALSE))) %>% 
  mutate(meantab=map(pm, ~((.x)$`mean_table`))) %>% 
  mutate(lsdtab=map(pm, ~((.x)$`LSD`)))

#mean table
Y_means2_unnest<-Y_means2 %>% unnest(meantab, .drop=TRUE) %>% select(1,5,6,7)
Y_means2_wide<-Y_means2_unnest %>% pivot_wider(names_from="variable", values_from = "Predicted means")
Y_means2_wide <- Y_means2_wide[c(5,6,1,3,4,2),]

#lsd table
Y_LSD2_unnest <- Y_means2 %>% unnest(lsdtab, .drop=TRUE) 
Y_LSD2_unnest$LSD.n = rep(c("Max.LSD","Min.LSD","Aveg.LSD"),6)
Y_LSD2_tab <- Y_LSD2_unnest %>% select(1,6,7) %>% pivot_wider(names_from="variable", values_from = "lsdtab")

#trying to combine these does my head in, so maybe just print one then the other?
kable(Y_means2_wide, digits=c(1,1,1,1))
kable(Y_LSD2_tab[(Y_LSD2_tab$LSD.n %in% 'Aveg.LSD'),], digits=c(1,1,1,1))

```
##Cosmetic measures
Of the cosmetic defects, only russet, blemish, sunburn, mis-shape and parrot peak occurred at over 10% in any of the A70 plots.
A70 had less in-grade fruit than Galaxy. A70 and Fuji suffered from a lot of russeting in Nelson. A70 had more blemishes than Fuji. A70 had more split  and mis-shapen fruit than Fuji or Galaxy.
```{r Cosmetic GLMs}
## Kate
# model ------
C_mod <- A70 %>% 
  select("Region", "Site", "Rep", "Cultivar","X.FruitCos", "in.grade","russet","blemish","sunburn","mishape","Parrot.beak") %>% 
  # make long
  pivot_longer(names_to = "variable", values_to = "value", cols=c(in.grade:Parrot.beak)) %>% 
  # so runs for each variable
  group_by(variable) %>% nest() %>% 
  # model
  mutate(fit=map(data, ~glm(value/100 ~ Region + Region:Site + Cultivar + Cultivar:Region + 
                             Cultivar:Region:Site, data=.x, family=binomial, weight=X.FruitCos)))

# if you wanted to check residuals---------

#residplot(C_mod$fit[[1]], newwd=FALSE)
#residplot(C_mod$fit[[2]], newwd=FALSE)
#residplot(C_mod$fit[[3]], newwd=FALSE)
#residplot(C_mod$fit[[4]], newwd=FALSE)

# anova--------

C_anova<-C_mod %>% 
  #run anova
  mutate(anovaR=map(fit, ~Anova(.x))) %>% 
  #tidy the anova output
  mutate(tidyV=map(anovaR, broom::tidy)) %>%
  #unstack
  unnest(tidyV, .drop=TRUE)

#long table
#kable(Y_anova[, c(1,5,7,8,9)])
C_anova_longTable <- C_anova[, c(1,5,7,6,8)]

names(C_anova)[6] <- 'dev'
names(C_anova)[8] <- 'p'
names(C_anova_longTable)[4] <- 'dev'
names(C_anova_longTable)[5] <- 'p'


# wide table
C_anova_wide<-C_anova %>% select(1,5,7,6,8) %>% pivot_wider(names_from = "variable", values_from = c("dev","p"))
#kable(Y_anova_wide[c(1,3,2,4,5,6), c(1,2,3,7,4,8,5,9,6,10 )], digits=c(1,3,1,3,1,3,1,3))
C_anova_wideTable<- C_anova_wide[c(1,3,2,4,5,6), c(1,2,3,9,4,10,5,11,6,12,7,13,8,14)]


#cant do the same F-test-relative-to-Orchard-to-Orchard-variation here; just make p-value for Region missing
#f <- as.numeric(Y_anova_wideTable[1,3]/Y_anova_wideTable[2,3])
#Y_anova_wideTable[1,3] <- f
#Y_anova_wideTable[1,4] <- pf(f,1,2, lower.tail = F)

C_anova_wideTable[1,c(4,6,8,10,12,14)] <- NA

kable(C_anova_wideTable, digits=c(1,3,1,3,1,3,1,3,1,3))

#options(knitr.kable.NA = '')
```

The means for each variety and each variety in each region are below (Orchard x variety means in appendix)
```{r Cosmetic Means tables}
# means------------------------
#model without Site, coz zeros mess up mean calcs
C_mod2 <- A70 %>% 
  select("Region", "Site", "Rep", "Cultivar","X.FruitCos", "in.grade","russet","blemish","sunburn","mishape","Parrot.beak") %>% 
  pivot_longer(names_to = "variable", values_to = "value", cols=c(in.grade:Parrot.beak)) %>% 
  group_by(variable) %>% nest() %>% 
  mutate(fit=map(data, ~glm(value/100 ~ Region*Cultivar, data=.x, family=binomial, weight=X.FruitCos)))

C_means1<-C_mod2 %>% 
  #predictmeans
  #  mutate(pm=map(fit, ~predictmeans(., "Cultivar", plot=FALSE, pairwise = TRUE, adj="fdr"))) %>% 
  mutate(pm=map(fit, ~predictmeans(., "Cultivar", plot=FALSE))) %>% 
  mutate(meantab=map(pm, ~((.x)$`mean_table`))) %>% 
  mutate(lsdtab=map(pm, ~((.x)$`LSD`)))

#mean table
C_means1_unnest <- C_means1 %>% unnest(meantab, .drop=TRUE) %>% select(1,5,11,12,13)
C_means1_wide <- C_means1_unnest %>% pivot_wider(names_from="variable", values_from = c("BK mean","LL of 95% BK CI","UL of 95% BK CI"))
C_means1_wide <- C_means1_wide[c(2,3,1),c(1,2,8,14,3,9,15,4,10,16,5,11,17,6,12,18,7,13,19)]

#CIs rather than LSDs?
#lsd table
#C_LSD_unnest<- C_means1 %>% unnest(lsdtab, .drop=TRUE) 
#C_LSD_unnest$LSD.n=rep(c("Max.LSD","Min.LSD","Aveg.LSD"),5)
#C_LSD_tab<-C_LSD_unnest %>% select(1,6,7) %>% pivot_wider(names_from="variable", values_from = "lsdtab")

kable(C_means1_wide, digits=c(2,2,2))
#kable(Y_LSD_tab[(Y_LSD_tab$LSD.n %in% 'Aveg.LSD'),], digits=c(1,1,1,1))



C_means2<-C_mod2 %>% 
  #predictmeans
  #  mutate(pm=map(fit, ~predictmeans(., "Cultivar", plot=FALSE, pairwise = TRUE, adj="fdr"))) %>% 
  mutate(pm=map(fit, ~predictmeans(., "Region:Cultivar", plot=FALSE))) %>% 
  mutate(meantab=map(pm, ~((.x)$`mean_table`))) %>% 
  mutate(lsdtab=map(pm, ~((.x)$`LSD`)))

#mean table
C_means2_unnest <- C_means2 %>% unnest(meantab, .drop=TRUE) %>% select(1,5,6,12,13,14)
C_means2_wide <- C_means2_unnest %>% pivot_wider(names_from="variable", values_from = c("BK mean","LL of 95% BK CI","UL of 95% BK CI"))
C_means2_wide <- C_means2_wide[c(2,3,1),c(1,2,3,9,15,4,10,16,5,11,17,6,12,18,7,13,19,8,14,20)]

#CIs rather than LSDs?
#lsd table
#C_LSD_unnest<- C_means1 %>% unnest(lsdtab, .drop=TRUE) 
#C_LSD_unnest$LSD.n=rep(c("Max.LSD","Min.LSD","Aveg.LSD"),5)
#C_LSD_tab<-C_LSD_unnest %>% select(1,6,7) %>% pivot_wider(names_from="variable", values_from = "lsdtab")

kable(C_means2_wide, digits=c(2,2,2))
#kable(Y_LSD_tab[(Y_LSD_tab$LSD.n %in% 'Aveg.LSD'),], digits=c(1,1,1,1))

```
## Appendix: orchard x variety means

```{r orchard x variety Means tables, echo=FALSE, message=FALSE, warning=FALSE}
# means------------------------

TSF_means3<-TSF_mod %>% 
  #  mutate(pm=map(fit, ~predictmeans(., "Cultivar", plot=FALSE, pairwise = TRUE, adj="fdr"))) %>% 
  mutate(pm=map(fit, ~predictmeans(., "Region:Site:Cultivar", plot=FALSE))) %>% 
  mutate(meantab=map(pm, ~((.x)$`mean_table`))) %>% 
  mutate(lsdtab=map(pm, ~((.x)$`LSD`)))

#mean table
TSF_means3_unnest<-TSF_means3 %>% unnest(meantab, .drop=TRUE) %>% select(1,5,6,7,8)
TSF_means3_wide<-TSF_means3_unnest %>% pivot_wider(names_from="Cultivar", values_from = "Predicted means")
TSF_means3_wide <- TSF_means3_wide[,c(1,2,3,5,6,4)]

#lsd table
TSF_LSD3_unnest<- TSF_means3 %>% unnest(lsdtab, .drop=TRUE) 
TSF_LSD3_unnest$LSD.n=rep(c("Max.LSD","Min.LSD","Aveg.LSD"),4)
TSF_LSD3_tab<-TSF_LSD3_unnest %>% select(1,6,7) %>% pivot_wider(names_from="LSD.n", values_from = "lsdtab")

#trying to combine these does my head in, so maybe just print one then the other?
#kable(TSF_means3_wide, digits=c(1,1,1,1))
#kable(TSF_LSD3_tab[,c(1,4)], digits=c(1,1,1,1))

for (i in seq_along(unique(TSF_means3_wide$variable))) { print(kable(TSF_means3_wide[(TSF_means3_wide$variable %in% unique(TSF_means3_wide$variable)[i]),],   digits=c(1,1,1,1)))
  print(kable(TSF_LSD3_tab[(TSF_LSD3_tab$variable %in% unique(TSF_means3_wide$variable)[i]),c(1,4)], digits=c(1,1,1,1)))
}


Y_means3<-Y_mod %>% 
  #  mutate(pm=map(fit, ~predictmeans(., "Cultivar", plot=FALSE, pairwise = TRUE, adj="fdr"))) %>% 
  mutate(pm=map(fit, ~predictmeans(., "Region:Site:Cultivar", plot=FALSE))) %>% 
  mutate(meantab=map(pm, ~((.x)$`mean_table`))) %>% 
  mutate(lsdtab=map(pm, ~((.x)$`LSD`)))

#mean table
Y_means3_unnest <- Y_means3 %>% unnest(meantab, .drop=TRUE) %>% select(1,5,6,7,8)
Y_means3_wide <- Y_means3_unnest %>% pivot_wider(names_from="Cultivar", values_from = "Predicted means")
Y_means3_wide <- Y_means3_wide[,c(1,2,3,5,6,4)]

#lsd table
Y_LSD3_unnest<- Y_means3 %>% unnest(lsdtab, .drop=TRUE) 
Y_LSD3_unnest$LSD.n=rep(c("Max.LSD","Min.LSD","Aveg.LSD"),6)
Y_LSD3_tab<-Y_LSD3_unnest %>% select(1,6,7) %>% pivot_wider(names_from="LSD.n", values_from = "lsdtab")

#trying to combine these does my head in, so maybe just print one then the other?
#kable(Y_means3_wide, digits=c(1,1,1,1))
#kable(Y_LSD3_tab[,c(1,4)], digits=c(1,1,1,1))
for (i in seq_along(unique(Y_means3_wide$variable))) { print(kable(Y_means3_wide[(Y_means3_wide$variable %in% unique(Y_means3_wide$variable)[i]),],   digits=c(1,1,1,1)))
  print(kable(Y_LSD3_tab[(Y_LSD3_tab$variable %in% unique(Y_means3_wide$variable)[i]),c(1,4)], digits=c(1,1,1,1)))
}


C_means3<-C_mod %>% 
  #  mutate(pm=map(fit, ~predictmeans(., "Cultivar", plot=FALSE, pairwise = TRUE, adj="fdr"))) %>% 
  mutate(pm=map(fit, ~predictmeans(., "Region:Site:Cultivar", plot=FALSE))) %>% 
  mutate(meantab=map(pm, ~((.x)$`mean_table`))) 

#mean table
C_means3_unnest <- C_means3 %>% unnest(meantab, .drop=TRUE) %>% select(1,5,6,7,13)
C_means3_wide <- C_means3_unnest %>% pivot_wider(names_from="Cultivar", values_from = "BK mean")
C_means3_wide <- C_means3_wide[,c(1,2,3,5,6,4)]

#trying to combine these does my head in, so maybe just print one then the other?
#kable(C_means3_wide, digits=2)
for (i in seq_along(unique(C_means3_wide$variable))) print(kable(C_means3_wide[(C_means3_wide$variable %in% unique(C_means3_wide$variable)[i]),],   digits=c(3,3,3,3)))

```


#good to here ----

```

