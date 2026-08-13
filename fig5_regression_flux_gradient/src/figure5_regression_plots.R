library(tidyverse)
library(lmodel2)

flux_grad <- read_csv('../../data_directory/sediment_trap_data.csv')

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
  r2 <- summary(model)$r.squared
  result <- paste('R\u00B2', signif(r2, digits = 2), sep='=')
  return(result)
}

#### SMA regression annotation functions ####
pValue_l2 <- function(pval){
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

rSquared_l2 <- function(r2){
  # r2 <- summary(model)$r.squared
  result <- paste('R\u00B2', signif(r2, digits = 2), sep='=')
  return(result)
}

#### Regression Relationships (Figure 5) ####
## POC by gradient
C_meanGrad <- lm(fluxC ~ LateralDensityGrad, flux_grad_wcOnly)
summary(C_meanGrad)

C_meanGrad_l2 <- lmodel2(fluxC ~ LateralDensityGrad, flux_grad_wcOnly)
C_meanGrad_l2

C_meanGradPlot <- ggplot(flux_grad_wcOnly,
                         aes(x = LateralDensityGrad,
                             # y = normflux_poc))+
                             y=fluxC))+
  geom_point(size = 5)+
  geom_smooth(method = "lm",
              formula = y ~ x,
              alpha = 0,
              linewidth = 2,
              color = 'black')+
  
  # geom_abline(slope = C_meanGrad_l2$regression.results[3,]$Slope,
  #             intercept = C_meanGrad_l2$regression.results[3,]$Intercept,
  #             linewidth = 2,
  #             linetype = 2,
  #             color = 'gray')+
  
  annotate("text",
           x = min(flux_grad_wcOnly$LateralDensityGrad, na.rm = TRUE),
           y = max(flux_grad_wcOnly$fluxC, na.rm = TRUE),
           label = 'b',
           hjust = 0, vjust = 1, size = 10) +
  annotate("text",
           x = min(flux_grad_wcOnly$LateralDensityGrad, na.rm = TRUE),
           y = max(flux_grad_wcOnly$fluxC, na.rm = TRUE),
           label = pValue(C_meanGrad),
           # label = pValue_l2(C_meanGrad_l2$P.param),
           hjust = 0, vjust = 3, size = 10) +
  annotate("text",
           x = min(flux_grad_wcOnly$LateralDensityGrad, na.rm = TRUE),
           y = max(flux_grad_wcOnly$fluxC, na.rm = TRUE),
           label = rSquared(C_meanGrad),
           # label = rSquared_l2(C_meanGrad_l2$rsquare),
           hjust = 0, vjust = 5, size = 10) +
  scale_x_continuous(breaks = c(-0.04, -0.02, 0.00, 0.02, 0.04))+
  xlab(bquote('Lateral Density Gradient ('*kg~m^-3~km^-1*')'))+
  ylab(bquote('POC Flux ('*mg~m^-2~d^-1*')'))+
  theme_classic()+
  theme(
    text= element_text(size = 20),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )
C_meanGradPlot

## PON by gradient
N_meanGrad <- lm(fluxN ~ LateralDensityGrad, flux_grad_wcOnly)
summary(N_meanGrad)

N_meanGrad_l2 <- lmodel2(fluxN ~ LateralDensityGrad, flux_grad_wcOnly)
N_meanGrad_l2

N_meanGradPlot <- ggplot(flux_grad_wcOnly,
                         aes(x = LateralDensityGrad,
                             y=fluxN))+
  geom_point(size=5)+
  geom_smooth(method = "lm", 
              formula = y ~ x,
              alpha=0,
              linewidth = 2,
              color = 'black')+
  annotate("text",
           x = min(flux_grad_wcOnly$LateralDensityGrad, na.rm = TRUE),
           y = max(flux_grad_wcOnly$fluxN, na.rm = TRUE),
           label = "c",
           hjust = 0, vjust = 1, size = 10) +
  annotate("text",
           x = min(flux_grad_wcOnly$LateralDensityGrad, na.rm = TRUE),
           y = max(flux_grad_wcOnly$fluxN, na.rm = TRUE),
           label = pValue(N_meanGrad),
           # label = pValue_l2(N_meanGrad_l2$P.param),
           hjust = 0, vjust = 3, size = 10) +
  annotate("text",
           x = min(flux_grad_wcOnly$LateralDensityGrad, na.rm = TRUE),
           y = max(flux_grad_wcOnly$fluxN, na.rm = TRUE),
           label = rSquared(N_meanGrad),
           # label = rSquared_l2(N_meanGrad_l2$rsquare),
           hjust = 0, vjust = 5, size = 10) +
  scale_x_continuous(breaks = c(-0.04, -0.02, 0.00, 0.02, 0.04))+
  xlab(bquote('Lateral Density Gradient ('*kg~m^-3~km^-1*')'))+
  ylab(bquote('PN Flux ('*mg~m^-2~d^-1*')'))+
  theme_classic()+
  theme(
    text= element_text(size = 20),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )
N_meanGradPlot

## Chl by gradient
chl_meanGrad <- lm(fluxChl ~ LateralDensityGrad, flux_grad_wcOnly)
summary(chl_meanGrad)

chl_meanGrad_l2 <- lmodel2(fluxChl ~ LateralDensityGrad, flux_grad_wcOnly)
chl_meanGrad_l2

chl_meanGradPlot <- ggplot(flux_grad_wcOnly,
                           aes(x = LateralDensityGrad,
                               y=fluxChl))+
  geom_point(size = 5)+
  geom_smooth(method = "lm", 
              formula = y ~ x, #*diChl_aboveTrap_mg_m2,
              alpha=0,
              linewidth = 2,
              color = 'black')+
  
  # geom_abline(slope = chl_meanGrad_l2$regression.results[3,]$Slope,
  #             intercept = chl_meanGrad_l2$regression.results[3,]$Intercept,
  #             linewidth = 2,
  #             linetype = 2,
  #             color = 'gray')+
  
  annotate("text",
           x = min(flux_grad_wcOnly$LateralDensityGrad, na.rm = TRUE),
           y = max(flux_grad_wcOnly$fluxChl, na.rm = TRUE),
           label = "a",
           hjust = 0, vjust = 1, size = 10) +
  annotate("text",
           x = min(flux_grad_wcOnly$LateralDensityGrad, na.rm = TRUE),
           y = max(flux_grad_wcOnly$fluxChl, na.rm = TRUE),
           label = pValue(chl_meanGrad),
           # label = pValue_l2(chl_meanGrad_l2$P.param),
           hjust = 0, vjust = 3, size = 10) +
  annotate("text",
           x = min(flux_grad_wcOnly$LateralDensityGrad, na.rm = TRUE),
           y = max(flux_grad_wcOnly$fluxChl, na.rm = TRUE),
           label = rSquared(chl_meanGrad),
           # label = rSquared_l2(chl_meanGrad_l2$rsquare),
           hjust = 0, vjust = 5, size = 10) +
  scale_x_continuous(breaks = c(-0.04, -0.02, 0.00, 0.02, 0.04))+
  xlab(bquote('Lateral Density Gradient ('*kg~m^-3~km^-1*')'))+
  ylab(bquote('Chl '*italic(a)*' Flux ('*mg~m^-2~d^-1*')'))+
  theme_classic()+
  theme(
    text= element_text(size = 20),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )
chl_meanGradPlot


ggsave("../plot/regression_poc_by_meanGrad.png", 
       plot = C_meanGradPlot, 
       width = 6, height = 6, dpi = 600, 
       bg = "transparent")

ggsave("../plot/regression_pon_by_meanGrad.png", 
       plot = N_meanGradPlot, 
       width = 6, height = 6, dpi = 600, 
       bg = "transparent")

ggsave("../plot/regression_chla_by_meanGrad.png", 
       plot = chl_meanGradPlot, 
       width = 6, height = 6, dpi = 600, 
       bg = "transparent")


#### Combine into single figure using Patchwork ####
library(patchwork)

patchPlot <- chl_meanGradPlot + C_meanGradPlot + N_meanGradPlot
patchPlot

ggsave("../plot/regression_fluxByGrad_multipanel.png", 
       plot = patchPlot, 
       width = 18, height = 6, dpi = 600, 
       bg = "transparent")
