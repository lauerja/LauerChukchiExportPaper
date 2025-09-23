library(tidyverse)

flux_grad <- read_csv('../../data_general/processed_data/all_trap_data_for_figs_3_4_5_6_7.csv')

flux_grad_wcOnly <- filter(flux_grad, depth >= 8)


#### Simple flux by iceObs boxplot (Figure 5) ####
chl_ice_anova <- aov(fluxChl ~ iceObs, data = flux_grad)
summary(chl_ice_anova)
TukeyHSD(chl_ice_anova)

chl_boxplot <- ggplot(data = flux_grad,
                      aes(
                        x = iceObs,
                        y = fluxChl,
                      ))+
  geom_boxplot()+
  annotate("text",
           x = -Inf, y = Inf,
           label = "a",
           hjust = -1, vjust = 1, size = 10) +
  scale_x_discrete(labels=c('ice' = 'Consolidated\nIce',
                            # 'miz' = 'Marginal Ice Zone\n(Front)',
                            'miz' = 'Marginal Ice\nZone',
                            'ow' = 'Open\nWater'))+
  labs(x = NULL, #'Deployment Station Ice Coverage',
       y = bquote('Chl '*italic(a)*' Flux ('*mg~m^-2*~day^-1*')'),
       color = 'Trap Depth')+
  theme_classic()+
  theme(
    text= element_text(size = 15),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )
chl_boxplot

c_ice_anova <- aov(fluxC ~ iceObs, data = flux_grad)
summary(c_ice_anova)
TukeyHSD(c_ice_anova)

c_boxplot <- ggplot(flux_grad,
                    aes(
                      x = iceObs,
                      y = fluxC
                    ))+
  # geom_point()+
  geom_boxplot()+
  annotate("text",
           x = -Inf, y = Inf,
           label = "b",
           hjust = -1, vjust = 1, size = 10) +
  scale_x_discrete(labels=c('ice' = 'Consolidated\nIce',
                            # 'miz' = 'Marginal Ice Zone\n(Front)',
                            'miz' = 'Marginal Ice\nZone',
                            'ow' = 'Open\nWater'))+
  labs(x = NULL, #'Deployment Station Ice Coverage',
       y = bquote('POC Flux ('*mg~m^-2*~day^-1*')'),
       color = 'Trap Depth')+
  theme_classic()+
  theme(
    text= element_text(size = 15),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )
c_boxplot

n_ice_anova <- aov(fluxN ~ iceObs, data = flux_grad)
summary(n_ice_anova)
TukeyHSD(n_ice_anova)

n_boxplot <- ggplot(flux_grad,
                    aes(
                      x = iceObs,
                      y = fluxN
                    ))+
  # geom_point()+
  geom_boxplot()+
  annotate("text",
           x = -Inf, y = Inf,
           label = "c",
           hjust = -1, vjust = 1, size = 10) +
  scale_x_discrete(labels=c('ice' = 'Consolidated\nIce',
                            # 'miz' = 'Marginal Ice Zone\n(Front)',
                            'miz' = 'Marginal Ice\nZone',
                            'ow' = 'Open\nWater'))+
  labs(x = NULL, #'Deployment Station Ice Coverage',
       y = bquote('PON Flux ('*mg~m^-2*~day^-1*')'),
       color = 'Trap Depth')+
  theme_classic()+
  theme(
    text= element_text(size = 15),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )
n_boxplot

ggsave(plot = chl_boxplot,
       filename = '../plot/boxplot_chlaFlux_iceObs.png',
       height = 6, width = 4.5, dpi = 600,
       # height = 7, width = 6, dpi = 600,
       bg = 'transparent')
ggsave(plot = c_boxplot,
       filename = '../plot/boxplot_pocFlux_iceObs.png',
       height = 6, width = 4.5, dpi = 600,
       # height = 7, width = 6, dpi = 600,
       bg = 'transparent')
ggsave(plot = n_boxplot,
       filename = '../plot/boxplot_ponFlux_iceObs.png',
       height = 6, width = 4.5, dpi = 600,
       # height = 7, width = 6, dpi = 600,
       bg = 'transparent')

### Gradient Boxplot
grad_boxplot <- ggplot(flux_grad,
                       aes(x = iceObs,
                           y = LateralDensityGrad * 1000,
                           color = depthCat))+
  geom_boxplot()+
  geom_point()+
  geom_hline(yintercept = 10, color = 'green')+
  scale_x_discrete(labels=c('ice' = 'Consolidated Ice',
                            'miz' = 'Marginal Ice Zone\n(Front)',
                            'ow' = 'Open Water'))+
  labs(x = 'Deployment Station Ice Coverage',
       # y = bquote('Mean Gradient Above Trap ('*kg*~m^-3*~km^-1*')'),
       # y = bquote('Lateral Density Gradient at Trap Deployment Position ('*kg*~m^-3*~km^-1*')'),
       y = bquote('Lateral Density Gradient at Trap Deployment Position ('*kg*~m^-4*')'),
       color = 'Trap Depth')+
  # scale_color_manual(values = c("#d63a41","#FCCE50", "#44C7FF"))+
  theme_classic()

grad_boxplot
