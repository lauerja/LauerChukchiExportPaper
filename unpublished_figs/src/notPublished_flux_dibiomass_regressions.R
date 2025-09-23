library(tidyverse)

flux_grad <- read_csv('../../data_general/processed_data/all_trap_data_for_figs_3_4_5_6_7.csv')

flux_grad_wcOnly <- filter(flux_grad, depth >= 8)

#### statistical functions for annotating plots #### 
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

#### Flux by Depth Integrated Biomass Regressions (Figures not used, but possibly suppliment) ####
chl_diChl <- lm(fluxChl ~ diChl_aboveTrap_mg_m2, flux_grad_wcOnly)
summary(chl_diChl)

chl_diChlPlot <- ggplot(flux_grad_wcOnly, aes(x = diChl_aboveTrap_mg_m2, y=fluxChl))+ #,
  # color = LateralDensityGrad))+
  # color = iceObs))+
  # geom_point(aes(color = iceObs))+
  geom_point(size = 5)+ #aes(color = transect))+
  geom_smooth(method = "lm",
              formula = y ~ x,
              alpha=0,
              linewidth = 2,
              color = 'gray')+
  geom_text(aes(label = pValue(chl_diChl),
                x = -Inf, y = Inf, hjust = -0.2, vjust = 5),
            size = 10)+
  geom_text(aes(label = rSquared(chl_diChl),
                x = -Inf, y = Inf, hjust = -0.15, vjust = 7),
            size = 10)+
  xlab('Mean Density Gradient (kg/m3/km)')+
  xlab(bquote('Depth Integrated Chl '*italic(a)*' Above Trap ('*mg~m^-2*')'))+
  ylab(bquote('Chl '*italic(a)*' Flux ('*mg~m^-2~day^-1*')'))+
  theme_classic()+
  theme(
    text= element_text(size = 20),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )
chl_diChlPlot

### POC by diChl and diPOC
poc_diChl <- lm(fluxC ~ diChl_aboveTrap_mg_m2, flux_grad_wcOnly)
summary(poc_diChl)

poc_diPOC <- lm(fluxC ~ diPOC_aboveTrap_mg_m2, flux_grad_wcOnly)
summary(poc_diPOC)

poc_diPOCPlot <- ggplot(flux_grad_wcOnly,
                        aes(x = diPOC_aboveTrap_mg_m2,
                            # x = diChl_aboveTrap_mg_m2,
                            y=fluxC))+
  geom_point(size = 5)+
  geom_smooth(method = "lm",
              formula = y ~ x,
              alpha=0,
              linewidth = 2,
              color = 'gray')+
  geom_text(aes(label = pValue(poc_diPOC),
                x = -Inf, y = Inf, hjust = -0.2, vjust = 5),
            size = 10)+
  geom_text(aes(label = rSquared(poc_diPOC),
                x = -Inf, y = Inf, hjust = -0.15, vjust = 7),
            size = 10)+
  xlab(bquote('Depth Integrated POC Above Trap ('*mg~m^-2*')'))+
  ylab(bquote('POC Flux ('*mg~m^-2~day^-1*')'))+
  theme_classic()+
  theme(
    text= element_text(size = 20),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )
poc_diPOCPlot

## PON by diChl and diPON
pon_diChl <- lm(fluxN ~ diChl_aboveTrap_mg_m2, flux_grad_wcOnly)
summary(pon_diChl)

pon_diPON <- lm(fluxN ~ diPON_aboveTrap_mg_m2, flux_grad_wcOnly)
summary(pon_diPON)

pon_diPONPlot <- ggplot(flux_grad_wcOnly,
                        aes(x = diPON_aboveTrap_mg_m2,
                            y=fluxN))+
  geom_point(size = 5)+
  geom_smooth(method = "lm",
              formula = y ~ x,
              alpha=0,
              linewidth = 2,
              color = 'gray')+
  geom_text(aes(label = pValue(pon_diPON),
                x = -Inf, y = Inf, hjust = -0.2, vjust = 5),
            size = 10)+
  geom_text(aes(label = rSquared(pon_diPON),
                x = -Inf, y = Inf, hjust = -0.15, vjust = 7),
            size = 10)+
  xlab(bquote('Depth Integrated PON Above Trap ('*mg~m^-2*')'))+
  ylab(bquote('PON Flux ('*mg~m^-2~day^-1*')'))+
  theme_classic()+
  theme(
    text= element_text(size = 20),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )
pon_diPONPlot

## saves
ggsave("../plot/regression_chla_by_diChla.png", 
       plot = chl_diChlPlot, 
       width = 7, height = 6, dpi = 600, 
       bg = "transparent")

ggsave("../plot/regression_poc_by_diPOC.png", 
       plot = poc_diPOCPlot, 
       width = 7, height = 6, dpi = 600, 
       bg = "transparent")

ggsave("../plot/regression_pon_by_diPON.png", 
       plot = pon_diPONPlot, 
       width = 7, height = 6, dpi = 600, 
       bg = "transparent")
