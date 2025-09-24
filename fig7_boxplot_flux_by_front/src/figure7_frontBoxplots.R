# source('compare_flux_and_gradient.R')
library(tidyverse)

flux_grad <- read_csv('../../data_directory/sediment_trap_data.csv')

flux_grad_wcOnly <- filter(flux_grad, depth >= 8)

front_true <- 'Inside Front'
front_false <- 'Outside Front'

data_prepped <- flux_grad_wcOnly%>%
  mutate(inFront = ifelse(Sample_Number == 'T011D', F, inFront))%>% ## T011D was deployed on CN, so grad is NAN, but was out of front
  filter(!is.na(inFront))%>%
  mutate(frontText = ifelse(inFront == T, front_true, front_false),
         statsGroup = paste(iceObs, frontText))

pocFront <- ggplot(data_prepped,
                        aes(x = frontText,
                            y = fluxC,
                            color = iceObs))+
  geom_boxplot()+
  annotate("text",
           x = -Inf, y = Inf,
           label = "b",
           hjust = -1, vjust = 1, size = 10) +
  scale_color_manual(values = c("#d63a41","#FCCE50","#44C7FF"),
                     labels=c('ice' = 'UI',
                              'miz' = 'MIZ',
                              'ow' = 'OW'))+
  labs(x = NULL, y = bquote('POC Flux ('*mg~m^-2~day^-1*')'))+
  theme_classic()+
  theme(text = element_text(size = 16),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA)
  )
pocFront

ponFront <- ggplot(data_prepped,
                   aes(x = frontText,
                       y=fluxN,
                       color = iceObs))+
  geom_boxplot()+
  annotate("text",
           x = -Inf, y = Inf,
           label = "c",
           hjust = -1, vjust = 1, size = 10) +
  labs(x = NULL, y = bquote('PON Flux ('*mg~m^-2~day^-1*')'))+
  scale_color_manual(values = c("#d63a41","#FCCE50","#44C7FF"),
                     labels=c('ice' = 'UI',
                              'miz' = 'MIZ',
                              'ow' = 'OW'))+
  theme_classic()+
  theme(text = element_text(size = 16), legend.position = 'none',
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA)
  )
ponFront

chlFront <- ggplot(data_prepped,
                   aes(x = frontText,
                       y = fluxChl,
                       color = iceObs))+
  geom_boxplot()+
  annotate("text",
           x = -Inf, y = Inf,
           label = "a",
           hjust = -1, vjust = 1, size = 10) +
  labs(x = NULL, y = bquote('Chlorophyll '*italic(a)*' Flux ('*mg~m^-2~day^-1*')'))+
  scale_color_manual(values = c("#d63a41","#FCCE50","#44C7FF"),
                     labels=c('ice' = 'UI',
                              'miz' = 'MIZ',
                              'ow' = 'OW'))+
  theme_classic()+
  theme(text = element_text(size = 16), legend.position = 'none',
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )
chlFront

#### Saves ####
# note save key version first
ggsave('../plot/boxplot_fluxPOC_byFront_withKey.png', pocFront, height = 6, width = 4.5, dpi = 600, 
       bg = "transparent")

## no keys
ggsave('../plot/boxplot_fluxPOC_byFront.png', pocFront + theme(legend.position = 'none'), height = 6, width = 4.5, dpi = 600, 
       bg = "transparent")
ggsave('../plot/boxplot_fluxChl_byFront.png', chlFront, height = 6, width = 4.5, dpi = 600, 
       bg = "transparent")
ggsave('../plot/boxplot_fluxPON_byFront.png', ponFront, height = 6, width = 4.5, dpi = 600, 
       bg = "transparent")


#### Statistics ####

pocFront_anova <- aov(fluxC ~ statsGroup,
                      data = data_prepped)
summary(pocFront_anova)
TukeyHSD(pocFront_anova)

ponFront_anova <- aov(fluxN ~ statsGroup,
                      data = data_prepped)
summary(ponFront_anova)
TukeyHSD(ponFront_anova)

chlFront_anova <- aov(fluxChl ~ statsGroup,
                      data = data_prepped)
summary(chlFront_anova)
TukeyHSD(chlFront_anova)

