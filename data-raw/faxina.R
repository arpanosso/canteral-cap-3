# Faxina do Banco de Dados
library(tidyverse)
SHEETS<-readxl::excel_sheets("data-raw/Arco_desmatamento.xlsx")
arco_desmatamento <- readxl::read_excel("data-raw/Arco_desmatamento.xlsx",
                   sheet = SHEETS[1],
                   na = "NA") %>%
  janitor::clean_names()
skimr::skim(arco_desmatamento)
arco_desmatamento %>%
  filter(ano >= 2016) %>%
  ggplot(aes(x=x,y=y,color = xco2)) +
  geom_point()

arco_desmatamento %>% select(sif, ano, estacao ) %>% drop_na() %>%
  select(ano, estacao) %>%
  table()

write_rds(arco_desmatamento,
          "data/arco_desmatamento.rds")
