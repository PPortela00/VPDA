#Load dataset, rename variables and filter records
library(readxl) ; library(dplyr) ; library(lubridate) ; library( ggplot2 ) ; library(janitor) ; theme_set(theme_bw())

ds <- read_xlsx("PORDATA_Rendimento-médio-disponível-das-famílias.xlsx", col_names = TRUE, )
ds <- ds %>% 
  rename ( Rendimento = 'Rendimento médio disponível das famílias' ) %>% 
  mutate ( seq = row_number() ) %>% ## sequential number
  mutate_at(c('Anos', 'Rendimento'), as.numeric) %>%
  select ( seq, 'Anos', 'Rendimento') 

ds <- ds[ 1:27 , c(1,2,3) ]
print(ds, n =40)

consumo <- read_xlsx("PORDATA_Médias-de-consumo-final-por-tipo-de-bens-e-serviços.xlsx", col_names = TRUE, )
consumo <- consumo %>% 
  rename ( TotalConsumo = 'Total' ) %>% 
  mutate ( seq = row_number() ) %>% ## sequential number
  mutate_at(c('Anos', 'TotalConsumo'), as.numeric) %>%
  select ( seq, 'Anos', 'TotalConsumo') 

consumo <- consumo[ 1:27 , c(1,2,3) ]
print(consumo, n =28)


pib <- read_xlsx("PORDATA_PIB-per-capita-(base-2016).xlsx", col_names = TRUE, )
pib <- pib %>% 
  rename ( PIB = 'PIB per capita' ) %>% 
  #mutate ( seq = row_number() ) %>% ## sequential number
  mutate_at(c('Anos', 'PIB'), as.numeric) %>%
  select ('Anos', 'PIB') 

pib <- pib[ 36:62 , c(1,2) ]
print(pib, n =70)

new_ds <- merge(ds, consumo)
new_ds1 <- merge(new_ds, pib)
new_ds1 %>% 
arrange ( seq ) 



library ( ggplot2 )

 
 # labels and breaks for X axis text
 #brks <- new_ds$Anos[seq(1, length(new_ds$Anos), 12)]
 #lbls <- lubridate::year(brks)
 theme_set(theme_bw())
 
 # plot
 ggplot(new_ds1, aes(x=Anos)) + 
   geom_line(aes(y=Rendimento, col="Rendimento")) + 
   geom_line(aes(y=TotalConsumo, col="TotalConsumo")) + 
   geom_line(aes(y=PIB, col="PIB")) +
   labs(title="Time Series of ...", 
        subtitle="Comparação da evolução do rendimento e despesa das familias", 
        caption="Source: Pordata", y="Valor") +  # title and caption
   #scale_x_date(labels = lbls, breaks = brks) +  # change to monthly ticks and labels
   scale_color_manual(name="", 
                      values = c("Rendimento"="#00ba38", "TotalConsumo"="#f8766d","PIB"="darkorange2")) +  # line color
   theme(panel.grid.minor = element_blank())  # turn off minor grid
