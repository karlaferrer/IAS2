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
  survminer,#graficos ggplot
  ggplot2
  )

# 2.Leitura e organização BD ------------------------------------------------

#Read a txt file
diar <- read.table(file.choose(), header = TRUE) 
names(diar)
str(diar)

#transformar covariaveis em fator
#diar[,2:3]<-lapply(diar[,2:3],as.factor)

#categorizar gravidade de acordo com os autores
#classificacao de gravidade do episodio anterior
diar <- diar |>
  mutate(
    gravidade = case_when(
      diasant > 0 & diasant < 3 ~ "leve",
      diasant >=3 & mediadej <= 4 ~ "moderado",
      diasant >=3 & mediadej > 4 ~ "grave",
      )
  )

#Total por categoria
diar |> 
   count(gravidade)

#Conferir os episodios nao classificados
diar |> 
  filter(gravidade %in% NA) |> 
  summarise(mean(diasant), mean(mediadej))
#nao sao diarreia, sem ejecoes e dias de diarreia  

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
) |> 
left_join(
diar |> group_by(numcri) |>
  filter(gravidade %in% "leve") |> #episodios leves por crianca
  summarise(leve = n()),
by = "numcri"
) |> 
left_join(
diar |> group_by(numcri) |>
  filter(gravidade %in% "moderado") |> #episodios moderados por crianca
  summarise(moderado = n()),
by = "numcri"
) |> 
  left_join(
diar |> group_by(numcri) |>
  filter(gravidade %in% "grave") |> #episodios graves por crianca
  summarise(grave = n()),
by = "numcri"
  )


hist(diar_w$idade)
shapiro.test(diar_w$idade)

#Tabela 1
tab_1 <-CreateTableOne(vars = c("sexo","idade", "max_epi", 
"media_dia", "media_dej", "leve", "moderado", "grave"),
strata = "grupo", test = TRUE, includeNA = T, data = diar_w)

#com gtsummary
diar_w |>
  select(!numcri) |> 
  tbl_summary(by = grupo, 
              label = list(sexo ~ "Sexo", idade ~ "Idade (meses)",
                          max_epi ~ "Número de episódios",
                          media_dia ~ "Duração dos episódios (dias)",
                          media_dej ~ "Dejeções líquidas diárias",
                          leve ~ "Episódios leves",
                          moderado ~ "Episódios moderados",
                          grave ~ "Episódios graves"),
              missing = "no", digits = list(all_continuous() ~ c(0, 1),
                                            all_categorical() ~ c(0, 1))) |> 
  #add_p() |> 
  modify_header(label ~ "**Variável**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Tratamento Recebido**") |> 
  add_overall(col_label = "**Total**") |> 
  modify_footnote(
    all_stat_cols() ~ "Frequência (%) ou Mediana (IIQ)")
  
#grupos balanceados, ensaio clínico comunitário

# 4.Efeitos brutos --------------------------------------------------------

#Efeitos brutos sem considerar dados repetidos

fit_sexo <- coxph(Surv(ini, fim, status) ~ sexo, data = diar)
summary(fit_sexo)

fit_idade <- coxph(Surv(ini, fim, status) ~ idade, data = diar)
summary(fit_idade)

fit_gravidade <- coxph(Surv(ini, fim, status) ~ gravidade, data = diar)
summary(fit_gravidade)

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

#Efeito bruto idade
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
# teste da razao de verossimilhanca (anova)
#por causa da variancia robusta


# 6.Fragilidade -----------------------------------------------------------
#considerar como fragilidade a media e dejecoes liquidas do episodio anterior

#efeito aleatorio gamma
pwp_3 <- coxph(Surv(ini, fim, status) ~ idade + sexo + grupo + 
                 cluster(numcri) + strata(enum) + frailty(gravidade, sparse = F)
               ,data = diar)
summary(pwp_3)

#efeito aleatorio lognormal
pwp_4 <- coxph(Surv(ini, fim, status) ~ idade + sexo + grupo + 
                 cluster(numcri) + strata(enum) + 
                 frailty(gravidade, sparse = F, dist = "gauss"),x = TRUE,
               ,data = diar)
summary(pwp_4)

#a variancia do efeito alatorio é pequena, mas significativa a 5%

#Plot das fragilidades estimadas
#Necessita do sparse = T, que nao esta funcionando

# 7.Análise de resíduos ---------------------------------------------------

#Proporcionalidade - Shoenfeld

# nao funciona acessar os residuos dos modelos pwp 3 e 4
#sugestao: ficar com o modelo pwp_2, sem fragilidade
# a variancia dos efeitos aleatorios e pequena
#a inclusao nao diminuiu o erro padrao dos outros coeficientes
#nao ajuda muito a explicar a variabilidade
#ainda dimunuiu a concordancia do modelo
#alem disso os grupos de comparacao estao equilibrados
#com relacao a gravidade - tabela 1


res.sho <- cox.zph(pwp_2)

covar <- c("Beta (t) para Idade","Beta (t) para Sexo",
           "Beta (t) para Tratamento")

jpeg(file = "figure/Shoenfeld%2d.jpg")
for (k in 1:length(pwp_2$coefficients)){
  plot(res.sho[k], xlab = "Meses", col= c("red", "blue"), ylab = covar[k], 
       resid = TRUE, se= TRUE, lwd = 2)
  abline(h=pwp_2$coefficients[k], lty=4, col=2, lwd = 2)
}
dev.off()

#outra opcao grafica
ggcoxzph(res.sho, font.x = 10, font.y=10)

#Log-linearidade - Martingale

# Analisando a forma funcional através do gráfico dos resíduos de Martingale x idade
#grafico do residuo Martingale no modelo nulo x idade

mod0 <- coxph(Surv(ini, fim, status) ~ 1, data = diar, x = T)
summary(mod0)
mod0.mar <- resid(mod0, type= 'martingale')
plot (diar$idade, mod0.mar, xlab = "Idade", ylab = "Resíduos martingale",
      main= "Avaliação da
forma funcional da Idade", lwd=1, col= "grey" )
lines(lowess(diar$idade, mod0.mar, iter = 0), lty = 2)

#outra forma
                       
ggcoxfunctional(Surv(ini, fim, status) ~ idade, data = diar,
                ggtheme = theme_minimal(),
                ylab = "Resíduos Martingale - modelo nulo",
                xlab = "Idade (meses)", 
                ylim=c(-5,1)
)

# Observacoes Atípicas - Residuos deviance
res.dev <-resid(pwp_2, type = "deviance")
plot(res.dev, col= "grey", ylab = "Resíduos deviance", xlab = "Índice")
abline(h=0, col="red")

#Proporcao de observacoes atipicas - 9.3%
(sum(res.dev  > 2) + sum(res.dev  < (-2)))/length(diar$numcri)

#outra forma
ggcoxdiagnostics(pwp_2, type = "deviance",
                 linear.predictions = FALSE, sline = FALSE, 
                 ggtheme = theme_minimal(),
                 ylab = "Resíduos deviance",
                 xlab = "Índice",
                 main = " "
)


# 8.Gráfico dos efeitos  ----------------------------------------------------
exp_coef_m7<-exp(pwp_2$coefficients)
d_forest <- exp(confint(m7))
d_forest <- round(d_forest[-4,],2)
dat <- data.frame(
  Index = c(1:3), ## This provides an order to the data
  label = c("Idade", "Sexo (M)", "Vitamina A"),
  HR = exp(pwp_2$coefficients),
  LL = exp(confint(pwp_2))[,1],
  UL = exp(confint(pwp_2))[,2]
)

## Plot forest plot
ggplot(dat, aes(y = Index, x = HR)) +
  geom_point(shape = 18, size = 2) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.10) +
  geom_vline(xintercept = 1, color = "blue", linetype = "dashed", cex = 0.5) +
  scale_y_continuous(name = "", breaks=1:3, labels = dat$label, trans = "reverse") +
  xlab("Razão de riscos (IC 95%)") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x.bottom = element_text(size = 10, colour = "black"),
        axis.title.x = element_text(size = 10, colour = "black"))



# Modelo exercício 12.5 ---------------------------------------------------
#Nesse modelos os efeitos aleatorios
#sao atribuidos às criancas
#demora alguns minutos

frag_cri <- coxph(Surv(ini, fim, status) ~ grupo + idade + sexo + 
                  + frailty(numcri,sparse = F, dist = "gamma")
                  , data = diar)
summary(frag_cri)

#tem uma concordancia melhor que o pwp_2 e o pwp_3 e 4
#ao inves de usar a variancia robusta(cluster), usa a fragilidade
#para tratar as medidas repetidas jogando efeitos aleatorios para cada crianca

#nesse modelo é verificado o efeito protetor da suplementacao com vitamina A
