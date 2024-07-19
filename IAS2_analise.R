#Artigo original DOI:https://doi.org/10.1016/S0140-6736(94)92998-X
#Effect of vitamin A supplementation on diarrhoea 
#and acute lower-respiratory-tract infections in young children in Brazil

# 1.Instalar e carregar pacotes ---------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  tableone, #fazer tabela 1
  gtsummary, # tabelas
  readxl, #ler arquivo excel
  descr, #analise descritiva
  kableExtra,#renderizar tabela
  knitr, #renderizar tabela
  survival, #analise de sobrevivencia
  survminer,#plotar graficos KM
  ggsurvfit, #graficos ggplot diagnostico
  gtsummary, # tabela OR e IC 
  tidycmprsk, 
  finalfit, #tabelas dos modelos
  tidyverse,
  ResourceSelection, #qualidade do ajuste
  ggplot2,
  gridExtra, # forest plot OR e IC 
)

# 2.Leitura e organização BD ------------------------------------------------

#Read a txt file
diar <- read.table(file.choose(), header = TRUE) 
names(diar)
str(diar)

#transformar covariaveis em fator
#diar[,2:3]<-lapply(diar[,2:3],as.factor)

#categorizar n. de episodios
summary(diar$mediadej)

diar <- diar |>
  mutate(
  episod = case_when(
    mediadej <3 ~ "<3",
    mediadej >=3 & mediadej <4 ~ ">=3",
    mediadej >=4 & mediadej <5 ~ ">=4",
    mediadej >=5 & mediadej <6 ~ ">=5", 
    mediadej >=6 ~ ">=6"
    )
)

#Total por categoria
diar |> 
   count(episod)

#Conferencia - ficou alguem de fora?
`%ni%` <- Negate(`%in%`) 
diar |> 
  filter(episod %ni% c("<3",">=3", ">=4", ">=5",">=6"))

str(diar)
#transformar covariaveis em fator
diar[,c(1:3,12)]<-lapply(diar[,c(1:3,12)],as.factor)


# 3.Tabela 1 --------------------------------------------------------------

#Transformar o dataset em formato wide

diar_w <- diar |> 
  select(numcri, grupo,sexo,idade) |> 
  unique() |> 
left_join(
diar |> group_by(numcri) |> 
  summarise(max_epi = max(enum)),
by = "numcri"
) |> 
left_join(
diar |> group_by(numcri) |> 
  summarise(media_dia = mean(diasant)),
by = "numcri"
) |> 
left_join(
diar |> group_by(numcri) |> 
  summarise(media_dej = mean(mediadej)),
by = "numcri"
)

hist(diar_w$idade)
shapiro.test(diar_w$idade)

#Tabela 1
tab_1 <-CreateTableOne(vars = c("sexo","idade", "max_epi", 
"media_dia", "media_dej"),
strata = "grupo", test = TRUE, includeNA = T, data = diar_w)

#com gtsummary
diar_w |>
  select(!numcri) |> 
  tbl_summary(by = grupo, 
              label = list(sexo ~ "Sexo", idade ~ "Idade",
                                       max_epi ~ "Número de episódios",
                                       media_dia ~ "Duração média dos episódios (dias)",
                                       media_dej ~ "Média de dejeções líquidas diárias")) |> 
  #add_p() |> 
  modify_header(label ~ "**Variável**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Tratamento Recebido**") |> 
  add_overall(col_label = "**Total**") |> 
  modify_footnote(
    all_stat_cols() ~ "Frequência (%) ou Mediana (IIQ)")
  
#grupos balanceados, ensaio clínico comunitário

# 4.Efeitos brutos --------------------------------------------------------



