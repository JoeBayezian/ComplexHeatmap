library(readr)
library(tidyverse)
library(ComplexHeatmap)
library(circlize)

setwd('~/Misc')

df <- read_csv('countries-aggregated.csv')

countries <- c('China','India','US','Indonesia','Brazil',
               'United Kingdom','Germany','France','Italy','Spain')

sub <- df %>% 
  group_by(Country) %>%
  mutate(Confirmed_by_day=Confirmed-lag(Confirmed,default=first(Confirmed)),
         Deaths_by_day=Deaths-lag(Deaths,default=first(Deaths))) %>%
  ungroup %>%
  mutate(ym=substring(Date,1,7)) %>%
  group_by(Country,ym) %>%
  summarise(Country,ym,SC=sum(Confirmed_by_day),SD=sum(Deaths_by_day),.groups='drop_last') %>%
  distinct

mat <- sub %>%
  select(Country,ym,SD) %>%
  pivot_wider(values_from=SD,names_from=Country) %>%
  arrange(ym) %>%
  column_to_rownames('ym') %>%
  as.matrix

mat <- mat[,countries]

zmat <- t(pnorm(scale(mat)))

smat <- sub %>% select(Country,ym,SC) %>%
  pivot_wider(values_from=SC,names_from=Country) %>%
  arrange(ym) %>%
  column_to_rownames('ym') %>%
  as.matrix

smat <- smat[,countries]

smat <- t(scale(smat))
smat['China','2020-02'] <- 3.6

ht <- Heatmap(zmat,
              col = viridisLite::viridis(256),
              cluster_rows = T,
              cluster_columns = F,
              na_col = 'white',
              show_heatmap_legend = F,
              cell_fun = function(j, i, x, y, width, height, fill){
                grid.rect(x=x, 
                          y=y+unit((smat[i,j]/70),'npc'), 
                          width=width,
                          height=0.0035,
                          gp=gpar(col=NA,fill='red'))
                grid.rect(x=x, 
                          y=y, 
                          width=width,
                          height=0.002,
                          gp=gpar(col=NA,fill='black'))
              })

col_fun = colorRamp2(seq(0,1,length.out=256),viridisLite::viridis(256))
lgd_list <- list(Legend(col_fun = col_fun,
                        title = '\u03d5(z(Tot. Deaths))',
                        at = c(0,0.5,1),
                        direction = "horizontal",
                        legend_width = unit(3, "cm")),
                 Legend(title='z(Tot. Confirmed Cases)',
                        labels=c('z','z=0'),
                        type = 'lines', 
                        legend_gp = gpar(col=c('red','black'),lty=1,lwd=c(2,1)),
                        grid_width = unit(1,"cm"),
                        background = "white"
                        ))


cairo_pdf('Heatmap-example.pdf',height=7,width=12)
draw(ht,annotation_legend_list=lgd_list,annotation_legend_side='bottom')
dev.off()






