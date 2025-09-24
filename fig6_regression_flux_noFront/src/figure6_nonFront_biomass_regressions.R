library(tidyverse)

allTrapData <- read_csv('../../data_directory/sediment_trap_data.csv')%>%
  mutate(trap_depth = Trap_Depth)

wcTrapData <- allTrapData %>%
  filter(trap_depth >= 8)%>%
  drop_na(inFront)

#### Report Flux ####
trapMeans_for_reporting <- allTrapData %>%
  mutate(iceTethered = ifelse(trap_depth <= 8, T, F))%>%
  group_by(inFront, iceTethered)%>%
  summarize(chlMean = mean(fluxChl, na.rm = T),
            chlsd = sd(fluxChl, na.rm = T),
            pocMean = mean(fluxC, na.rm = T),
            pocsd = sd(fluxC, na.rm = T),
            ponMean = mean(fluxN, na.rm = T),
            ponsd = sd(fluxN, na.rm = T))

## Statistical Analysis ####

noFront <- filter(wcTrapData, inFront == F)

# Does depth integrated POC correlate with fluxC outside the front? - YES Significant
summary(lm(fluxC ~ diPOC_aboveTrap_mg_m2, noFront))#filter(wcTrapData, inFront ==F)))
cor.test(noFront$fluxC, noFront$diPOC_aboveTrap_mg_m2, method = 'spearman')
ggplot(wcTrapData, aes(x = diPOC_aboveTrap_mg_m2, y = fluxC, color = inFront))+
  geom_point()

## What about under ice?
summary(lm(fluxC ~ diPOC_aboveTrap_mg_m2, filter(wcTrapData, iceObs == 'ow')))

cor.test(filter(wcTrapData, iceObs =='ow')$fluxC,
         filter(wcTrapData, iceObs =='ow')$diPOC_aboveTrap_mg_m2,
         method = 'spearman')

filter(wcTrapData, iceObs =='ow')%>%
ggplot(., aes(x = diPOC_aboveTrap_mg_m2, y = fluxC, shape = inFront, color = iceObs))+
  geom_point()

# Does depth integrated PON correlate with fluxN outside the front? - NO significant linear regression
summary(lm(fluxN ~ diPON_aboveTrap_mg_m2, filter(wcTrapData, inFront ==F)))

# Significant spearman correlation (p = 0.010)
cor.test(filter(wcTrapData, inFront == F)$fluxN,
         filter(wcTrapData, inFront == F)$diPON_aboveTrap_mg_m2,
         method = 'spearman')


ggplot(wcTrapData, aes(x = diPON_aboveTrap_mg_m2, y = fluxN, color = inFront))+
  geom_point()

# Does diChl correlate with fluxChl outside the front? - YES Significant
summary(lm(fluxChl ~ diChl_aboveTrap_mg_m2, filter(wcTrapData, inFront ==F)))

cor.test(filter(wcTrapData, inFront == F)$fluxChl,
         filter(wcTrapData, inFront == F)$diChl_aboveTrap_mg_m2,
         method = 'spearman')

ggplot(wcTrapData, aes(x = diChl_aboveTrap_mg_m2, y = fluxChl, color = inFront))+
  geom_point()

# Does diChl correlate with fluxChl overall? - NO
summary(lm(fluxChl ~ diChl_aboveTrap_mg_m2, wcTrapData))

# Significant Spearman correlation
cor.test(wcTrapData$fluxChl,
         wcTrapData$diChl_aboveTrap_mg_m2,
         method = 'spearman')

ggplot(wcTrapData, aes(x = diChl_aboveTrap_mg_m2, y = fluxChl))+
  geom_point()

# Does diPOC correlate with fluxC overall? - NO
summary(lm(fluxC ~ diPOC_aboveTrap_mg_m2, wcTrapData))
ggplot(wcTrapData, aes(x = diPOC_aboveTrap_mg_m2, y = fluxC))+
  geom_point()

# Does diPON correlate with fluxN overall? - NO
summary(lm(fluxN ~ diPON_aboveTrap_mg_m2, wcTrapData))
ggplot(wcTrapData, aes(x = diPON_aboveTrap_mg_m2, y = fluxN))+
  geom_point()

# Does lateral density grad significantly correlate with POC flux? - YES
summary(lm(fluxC ~ LateralDensityGrad, wcTrapData))
ggplot(wcTrapData, aes(x = LateralDensityGrad, y = fluxC, color = iceObs))+
  geom_point()+
  geom_text(aes(label = Sample_Number))+
  geom_smooth(method = 'lm', alpha = 0)

# Does lateral density grad significantly correlate with PON flux? - YES
summary(lm(fluxN ~ LateralDensityGrad, wcTrapData))
ggplot(wcTrapData, aes(x = LateralDensityGrad, y = fluxN, color = iceObs))+
  geom_point()+
  geom_text(aes(label = Sample_Number))+
  geom_smooth(method = 'lm', alpha = 0)

# Does lateral density grad significantly correlate with Chl flux? - YES
summary(lm(fluxChl ~ LateralDensityGrad, wcTrapData))
wcTrapData%>%
  mutate(iceShift = paste(iceObs, '_', iceObsRecover))%>%
  ggplot(., aes(x = LateralDensityGrad, y = fluxChl, color = iceShift))+
  geom_point()

ggplot(wcTrapData, aes(x = LateralDensityGrad, y = fluxChl, color = iceObs))+
  geom_point()+
  geom_text(aes(label = Sample_Number))+
  geom_smooth(method = 'lm', alpha = 0)

## Plotting Functions for extracting pvalue and adjR2 from models ##iceObsRecover## Plotting Functions for extracting pvalue and adjR2 from models ####
pValue <- function(model){
  pval <- summary(model)$coef[2,4]
  if(pval > 0.05){
    return('Not Significant')
  }else if(pval < 0.001){
    return('p<0.001')
  }else if(pval < 0.01){
    return('p<0.01')
  }else{
    return('p<0.05')
  }
}

rSquared <- function(model){
  # r2 <- summary(model)$r.squared
  r2 <- summary(model)$adj.r.squared
  result <- paste('R\u00B2', signif(r2, digits = 2), sep='=')
  return(result)
}

## Plots for Publication ####
noFront <- filter(wcTrapData, inFront == F)

## Plotting Flux by DIChl inside outside front
fluxChl_diChl_noFront_model <- lm(fluxChl ~ diChl_aboveTrap_mg_m2, noFront)
summary(fluxChl_diChl_noFront_model)
cor.test(noFront$fluxChl, noFront$diChl_aboveTrap_mg_m2, method = 'spearman')

fluxChl_diChl_noFront_plot <- ggplot(wcTrapData,
                                 aes(x = diChl_aboveTrap_mg_m2,
                                     y = fluxChl))+
  geom_point(size = 5, aes(
    color = inFront,
    ))+
  geom_smooth(data = noFront,
              method = "lm",
              formula = y ~ x,
              alpha=0,
              linewidth = 2,
              color = 'black')+
  annotate("text",
           x = min(wcTrapData$diChl_aboveTrap_mg_m2, na.rm = TRUE),
           y = max(wcTrapData$fluxChl, na.rm = TRUE),
           label = 'a',
           hjust = 0, vjust = 1, size = 10)+
  annotate("text",
           x = max(wcTrapData$diChl_aboveTrap_mg_m2, na.rm = TRUE),
           y = max(wcTrapData$fluxChl, na.rm = TRUE),
           label = pValue(fluxChl_diChl_noFront_model),
           hjust = 1, vjust = 1, size = 10)+
  annotate("text",
           x = max(wcTrapData$diChl_aboveTrap_mg_m2, na.rm = TRUE),
           y = max(wcTrapData$fluxChl, na.rm = TRUE),
           label = format(rSquared(fluxChl_diChl_noFront_model)),
           hjust = 1, vjust = 3, size = 10)+
  xlab(bquote('Depth Integrated Chl '*italic(a)*' Above Trap ('*mg~m^-2*')'))+
  ylab(bquote('Chl '*italic(a)*' Flux ('*mg~m^-2~day^-1*')'))+
  scale_color_manual(values = c('black', 'orange'))+
  theme_classic()+
  theme(
    text= element_text(size = 20),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    legend.position = 'none'
  )
fluxChl_diChl_noFront_plot

## Saving fluxChl by diChl outside front
ggsave("../plot/regression_chl_by_diChl_noFront.png", 
       plot = fluxChl_diChl_noFront_plot, 
       width = 6, height = 6, dpi = 600, 
       bg = "transparent")

## Plotting fluxC by diPOC outside front
fluxC_diPOC_noFront_model <- lm(fluxC ~ diPOC_aboveTrap_mg_m2, noFront)
cor.test(noFront$fluxC, noFront$diPOC_aboveTrap_mg_m2, method = 'spearman')

fluxC_diPOC_noFront_plot <- ggplot(wcTrapData,
                                 aes(x = diPOC_aboveTrap_mg_m2,
                                     y = fluxC))+
  geom_point(size = 5, aes(
    color = inFront, 
    ))+
  geom_smooth(data = noFront,
              method = "lm",
              formula = y ~ x,
              alpha=0,
              linewidth = 2,
              color = 'black')+
  annotate("text",
           x = min(wcTrapData$diPOC_aboveTrap_mg_m2, na.rm = TRUE),
           y = max(wcTrapData$fluxC, na.rm = TRUE),
           label = 'b',
           hjust = 0, vjust = 1, size = 10)+
  ### Not Significant, so text does not go on plot
  ### Uncomment this block to print text
  # annotate("text",
  #          x = max(wcTrapData$diPOC_aboveTrap_mg_m2, na.rm = TRUE),
  #          y = max(wcTrapData$fluxC, na.rm = TRUE),
  #          label = pValue(fluxC_diPOC_noFront_model),
  #          hjust = 1, vjust = 1, size = 10)+
  # annotate("text",
  #          x = max(wcTrapData$diPOC_aboveTrap_mg_m2, na.rm = TRUE),
  #          y = max(wcTrapData$fluxC, na.rm = TRUE),
  #          label = format(rSquared(fluxC_diPOC_noFront_model)),
  #          hjust = 1, vjust = 3, size = 10)+
  xlab(bquote('Depth Integrated POC Above Trap ('*mg~m^-2*')'))+
  ylab(bquote('POC Flux ('*mg~m^-2~day^-1*')'))+
  scale_color_manual(values = c('black', 'orange'))+
  theme_classic()+
  theme(
    text= element_text(size = 20),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    legend.position = 'none'
  )
fluxC_diPOC_noFront_plot

## Saving fluxC by diPOC outside front
ggsave("../plot/regression_poc_by_diPOC_noFront.png", 
       plot = fluxC_diPOC_noFront_plot, 
       width = 6, height = 6, dpi = 600, 
       bg = "transparent")


## Plotting fluxN by diPON outside front
fluxN_diPON_noFront_model <- lm(fluxN ~ diPON_aboveTrap_mg_m2, noFront)
cor.test(noFront$fluxN, noFront$diPON_aboveTrap_mg_m2, method = 'spearman')

fluxN_diPON_noFront_plot <- ggplot(wcTrapData,
                                   aes(x = diPON_aboveTrap_mg_m2,
                                       y = fluxN))+
  geom_point(size = 5, aes(
    color = inFront, 
    # shape = iceObs
    ))+
  annotate("text",
           x = min(wcTrapData$diPON_aboveTrap_mg_m2, na.rm = TRUE),
           y = max(wcTrapData$fluxN, na.rm = TRUE),
           label = 'c',
           hjust = 0, vjust = 1, size = 10)+
  ## NOT SIGNIFICANT - NO LINE OR TEXT SHOWN
  # geom_smooth(data = filter(noFront, !is.na(inFront)),
  #             method = "lm",
  #             formula = y ~ x, #*diChl_aboveTrap_mg_m2,
  #             alpha=0,
  #             linewidth = 2,
  #             color = wes_palette("Zissou1Continuous")[1])+
  annotate("text",
           x = max(wcTrapData$diPON_aboveTrap_mg_m2, na.rm = TRUE),
           y = max(wcTrapData$fluxN, na.rm = TRUE),
           label = pValue(fluxN_diPON_noFront_model),
           hjust = 1, vjust = 1, size = 10)+
  annotate("text",
           x = max(wcTrapData$diPON_aboveTrap_mg_m2, na.rm = TRUE),
           y = max(wcTrapData$fluxN, na.rm = TRUE),
           label = format(rSquared(fluxN_diPON_noFront_model)),
           hjust = 1, vjust = 3, size = 10)+
  xlab(bquote('Depth Integrated PON Above Trap ('*mg~m^-2*')'))+
  ylab(bquote('PON Flux ('*mg~m^-2~day^-1*')'))+
  labs(color = NULL)+
  scale_color_manual(values = c('black', 'orange'),
                     labels = c('TRUE'='Inside Front', 'FALSE'='Outside Front'))+
  theme_classic()+
  theme(
    text= element_text(size = 20),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )
fluxN_diPON_noFront_plot

ggsave("../plot/regression_pon_by_diPON_noFront.png", 
       plot = fluxN_diPON_noFront_plot + theme(legend.position = 'none'), 
       width = 6, height = 6, dpi = 600, 
       bg = "transparent")

ggsave("../plot/regression_pon_by_diPON_noFront_withKey.png", 
       plot = fluxN_diPON_noFront_plot, 
       width = 7, height = 6, dpi = 600, 
       bg = "transparent")
