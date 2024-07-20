#Artigo original DOI:https://doi.org/10.1016/S0140-6736(94)92998-X
#Effect of vitamin A supplementation on diarrhoea 
#and acute lower-respiratory-tract infections in young children in Brazil

# 1.Instalar e carregar pacotes ---------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  tableone, #fazer tabela 1
  gtsummary, # tabelas
  survival, #analise de sobrevivencia
  survminer,#plotar graficos ggplot
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

# Eventos ordenados - AG - incrementos independentes
#risco basal igual para qq evento - historico nao importa
#acrescenta cluster para corrigir variancia - dado correlacionado

#Efeito bruto - sexo
ag_sexo <- coxph(Surv(ini, fim, status) ~ sexo + cluster(numcri),data = diar)
summary(ag_sexo)
# de acordo com AG nao ha diferenca entre os sexos
#robust se = 2.37*se(coef)
#indica que o risco de novos episodios e correlacionado a episodios anteriores
#o modelo AG nao e indicado,testar PWP e fragilidade

#Efeito bruto - tratamento
ag_grupo <- coxph(Surv(ini, fim, status) ~ grupo + cluster(numcri),data = diar)
summary(ag_grupo)
# segundo AG nao ha diferenca entre os tratamentos
#robust se > 2*se(coef)
#o modelo AG nao e indicado,testar PWP e fragilidade

#Efeito bruto - idade
ag_idade <- coxph(Surv(ini, fim, status) ~ idade + cluster(numcri),data = diar)
summary(ag_idade)
#cada mes acrescido na idade da crianca, diminui o risco em 3,3%
#robust se > 2*se(coef)
#o modelo AG nao e indicado,testar PWP e fragilidade

#Eventos ordenados estruturados - PWP

#Efeito bruto sexo
pwp_sexo <- coxph(Surv(ini, fim, status) ~ sexo + cluster(numcri) + strata(enum)
                  ,data = diar)
summary(pwp_sexo)
#nao ha diferenca entre os sexos

a#Efeito bruto tratamento
pwp_grupo <- coxph(Surv(ini, fim, status) ~ grupo + cluster(numcri) + strata(enum)
                  ,data = diar)
summary(pwp_grupo)
#nao ha diferenca entre os grupos

#Efeito bruto tratamento
pwp_idade <- coxph(Surv(ini, fim, status) ~ idade + cluster(numcri) + strata(enum)
                   ,data = diar)
summary(pwp_idade)
#cada mes adicional diminui o risco em 1.6%


# 5.Efeito do tratamento ajustado -----------------------------------------

#Modelo 1: idade e sexo
#de importancia teorica sao ajustadas conjuntamente
pwp_1 <- coxph(Surv(ini, fim, status) ~ idade + sexo + cluster(numcri) + strata(enum)
               ,data = diar)
summary(pwp_1)

#Modelo 2: acrescenta o tratamento
pwp_2 <- coxph(Surv(ini, fim, status) ~ idade + sexo + grupo + cluster(numcri) + strata(enum)
               ,data = diar)
summary(pwp_2)
#tratamento nao tem efeito significativo ajustado por idade e sexo

# nao e possivel comparar os modelos com
# teste da razao de verossimilhanca 
#por causa da variancia robusta


# 6.Fragilidade -----------------------------------------------------------
#considerar como fragilidade a media e dejecoes liquidas do episodio anterior

#efeito aleatorio gamma
pwp_3 <- coxph(Surv(ini, fim, status) ~ idade + sexo + grupo + 
                 cluster(numcri) + strata(enum) + frailty(episod, sparse = F)
               ,data = diar)
summary(pwp_3)

#efeito aleatorio lognormal
pwp_4 <- coxph(Surv(ini, fim, status) ~ idade + sexo + grupo + 
                 cluster(numcri) + strata(enum) + 
                 frailty(episod, sparse = F, dist = "gauss")
               ,data = diar)
summary(pwp_4)

#a variancia do efeito alatorio é pequena, mas significativa a 5%

#incluir o numero de dias do episodio anterior
pwp_5 <- coxph(Surv(ini, fim, status) ~ idade + sexo + grupo + 
                 cluster(numcri) + strata(enum) + 
                 frailty(episod, sparse = F) +
                 frailty(diasant, sparse = F)
               ,data = diar)
summary(pwp_5)

#Plot das fragilidades estimadas


# 7.Análise de resíduos ---------------------------------------------------


