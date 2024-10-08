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
  ggplot2,
  gridExtra #arrange plots
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
      diasant >=3 & mediadej > 4 ~ "grave"
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


# 4. Gráfico de sobrevivência por ordem de episódio -----------------------
# Checar se os eventos possuem riscos basais distintos 
#para optar por pwp ou ag

jpeg(file="figure/sob_episodios.jpg", width = 10, height = 15, units = "cm", pointsize = 12,
     res = 600, quality = 85)
plot(survfit(Surv(tempo, status) ~ enum, data = diar, 
     subset = (enum < 10)), col = 1:9, lty = 1:9,mark.time = F,
     xlab = "n. de dias desde o último evento", ylab = "S(t)")
legend("topright",col = 1:9, lty = 1:9,
       legend=c("ep.1", "ep.2", "ep.3", "ep.4", "ep.5", "ep.6", "ep.7",
                "ep.8", "ep.9"), box.lty=0, )
dev.off()

#com ggplot
ggsurvplot(survfit(Surv(tempo, status) ~ enum, data = diar, 
         subset = (enum < 10)), palette = "lancet", lty = 0.2, linetype = "solid") +
  ylab("S(t)")+
  xlab("dias desde o último evento")


#Verifica-se risco basal distinto dos episodios
#opcao pelo modelo de eventos estrutrados ordenados pwp

# 5.Efeitos brutos - PWP --------------------------------------------------------

#Eventos ordenados estruturados - PWP

#Efeito bruto sexo
pwp_sexo <- coxph(Surv(ini, fim, status) ~ sexo + cluster(numcri) + strata(enum)
                  ,data = diar)
summary(pwp_sexo)
#nao ha diferenca entre os sexos
#Concordance= 0.5 

#Efeito bruto idade
pwp_idade <- coxph(Surv(ini, fim, status) ~ idade + cluster(numcri) + strata(enum)
                   ,data = diar)
summary(pwp_idade)
#cada mes adicional diminui o risco em 1.6%
#Concordance= 0.586

#Efeito bruto tratamento
pwp_grupo <- coxph(Surv(ini, fim, status) ~ grupo + cluster(numcri) + strata(enum)
                  ,data = diar)
summary(pwp_grupo)
#nao ha diferenca entre os grupos
#Concordance= 0.516

#Tabela efeitos brutos pwp
bruto_pwp <- rbind(
  cbind("Modelo 1",exp(pwp_sexo$coefficients),exp(confint(pwp_sexo)), pwp_sexo$concordance[6]),
  cbind("Modelo 2",exp(pwp_idade$coefficients),exp(confint(pwp_idade)), pwp_idade$concordance[6]),
  cbind("Modelo 3",exp(pwp_grupo$coefficients),exp(confint(pwp_grupo)), pwp_grupo$concordance[6])
)
bruto_pwp <- as.data.frame(bruto_pwp)
bruto_pwp[,2:5]<-lapply(bruto_pwp[,2:5],as.numeric)
bruto_pwp[,2:5] <- round(bruto_pwp[,2:5],2)
bruto_pwp <- cbind(c("Sexo (masculino)", "Idade", 
                     "Tratamento (Vit.A)"), bruto_pwp)

bruto_pwp <- bruto_pwp[,c(2,1,3:6)]
names(bruto_pwp) <- c("Modelo","Variável","RR", "LI", "LS", "Concordância")

write_csv(bruto_pwp,"data/bruto_pwp.csv")

# 6.Efeito do tratamento ajustado PWP -----------------------------------------

#Modelo 1: idade e sexo

#incluindo idade sobre o modelo pwp_sexo 
pwp_1 <- coxph(Surv(ini, fim, status) ~ sexo + idade + cluster(numcri) + strata(enum)
               ,data = diar)
summary(pwp_1)
#Concordance= 0.585

#Modelo 2: acrescenta o tratamento
pwp_2 <- coxph(Surv(ini, fim, status) ~ sexo + idade + grupo + cluster(numcri) + strata(enum)
               ,data = diar)
summary(pwp_2)
#tratamento nao tem efeito significativo ajustado por idade e sexo
#Concordance= 0.586 

# nao e possivel comparar os modelos com
# teste da razao de verossimilhanca (anova)
#por causa da variancia robusta

#Incluir uma fragilidade com a gravidade dos episodios
#considerar como fragilidade a gravidade do episodio anterior
#gravidade 2 sem NAs para modelagem

diar <- diar |>
  mutate(
    grav2 = case_when(
      diasant > 0 & diasant < 3 ~ "leve",
      diasant >=3 & mediadej <= 4 ~ "moderado",
      diasant >=3 & mediadej > 4 ~ "grave",
      status == 0 ~ "sem evento",
      diasant == 0 & mediadej == 0 ~ "primeiro ep"
    )
  )

#Total por categoria
diar |> 
  count(grav2)

#efeito aleatorio gamma
pwp_3 <- coxph(Surv(ini, fim, status) ~ sexo + idade + grupo + 
                 cluster(numcri) + strata(enum) + frailty(grav2, sparse = F)
               ,data = diar)
summary(pwp_3)
#Concordance= 0.627

#efeito aleatorio lognormal
pwp_4 <- coxph(Surv(ini, fim, status) ~ sexo + idade + grupo + 
                 cluster(numcri) + strata(enum) + 
                 frailty(grav2, sparse = F, dist = "gauss"),
                 data = diar)
summary(pwp_4)

#a variancia do efeito alatorio é pequena, mas significativa a 5%
#Concordance= 0.627

#Plot das fragilidades estimadas
#Necessita do sparse = T, que nao esta funcionando

#os modelos pw3 e pw4 não são adequados 

#Tabela comparando modelos

res_pwp_1 <- cbind("Modelo 4",
  c("Sexo (masculino)","Idade"),exp(pwp_1$coefficients),
                   exp(confint(pwp_1)), pwp_1$concordance[6])

res_pwp_2 <- cbind("Modelo 5",
  c("Sexo (masculino)","Idade", "Tratamento (Vit. A)"),
                   exp(pwp_2$coefficients),exp(confint(pwp_2)), pwp_2$concordance[6])
  

mult_pwp<- rbind(res_pwp_1, res_pwp_2)

mult_pwp <- as.data.frame(mult_pwp)
mult_pwp[,3:6]<-lapply(mult_pwp[,3:6],as.numeric)
mult_pwp[,3:6]<-round (mult_pwp[,3:6],2)
names(mult_pwp) <- c("Modelo","Variável","RR", "LI", "LS", "Concordância")

write_csv(mult_pwp,"data/mult_pwp.csv")

#juntar com modelos simples numa tabela
pwp_models <- rbind(bruto_pwp,mult_pwp)

write_csv(pwp_models,"data/pwp_models.csv")

# 7.Eventos múltiplos com fragilidade ---------------------------------------

#Modelos múltiplos
#Sexo + idade
frag_1 <- coxph(Surv(ini, fim, status) ~ sexo + idade
                  + frailty(numcri,sparse = T, dist = "gamma")
                  , data = diar)
summary(frag_1)

#Sexo + idade + grupo
frag_2 <- coxph(Surv(ini, fim, status) ~ sexo + idade + grupo
                + frailty(numcri,sparse = T, dist = "gamma")
                , data = diar)
summary(frag_2)
#Concordance= 0.769

frag_3 <- coxph(Surv(ini, fim, status) ~ sexo + idade + grupo
                + frailty(numcri,sparse = T, dist = "gauss")
                , data = diar)
summary(frag_3)

#Tabela resumo dos modelos multiplos - efeitos fixos
mult_frag <- rbind(
  cbind("Modelo 6",c("Sexo (masculino)", "Idade"),exp(frag_1$coefficients),exp(confint(frag_1)), frag_1$concordance[6]),
  cbind("Modelo 7",c("Sexo (masculino)", "Idade", 
          "Tratamento (Vit.A)"),exp(frag_2$coefficients),exp(confint(frag_2)), frag_2$concordance[6]),
  cbind("Modelo 8",c("Sexo (masculino)", "Idade", 
          "Tratamento (Vit.A)"),exp(frag_3$coefficients),exp(confint(frag_3)), frag_3$concordance[6])
)
mult_frag <- as.data.frame(mult_frag)
mult_frag[,3:6]<-lapply(mult_frag[,3:6],as.numeric)
mult_frag[,3:6]<-round (mult_frag[,3:6],2)
names(mult_frag) <- c("Modelo","Variável","RR", "LI", "LS", "Concordância")

write_csv(mult_frag,"data/mult_frag.csv")


#histograma base das fragilidades
par(mfrow = c(1, 2))
hist(frag_2$frail, main = "Gama", ylab = " ", xlab = " ")
hist(frag_3$frail, main = "Gauss", ylab = " ", xlab = " ")
dev.off()

#histograma das fragilidades ggplot
hist_gama <- ggplot(as.data.frame(frag_2$frail), aes(x = frag_2$frail)) +
  geom_histogram(aes(y = ..density..), fill = "darkgrey") +
  geom_vline(aes(xintercept = 0), color = "red", linewidth = 0.5) +
  geom_density(color = "green", linewidth = 0.5)+
  ylab ("Densidade") +
  xlab("Fragilidade")+
  ggtitle("Gama")+
theme_classic()

hist_gauss <-ggplot(as.data.frame(frag_3$frail), aes(x = frag_3$frail)) +
  geom_histogram(aes(y = ..density..), fill = "darkgrey") +
  geom_vline(aes(xintercept = 0), color = "red", linewidth = 0.5) +
  geom_density(color = "green", linewidth = 0.5)+
  ylab ("Densidade") +
  xlab("Fragilidade")+
  ggtitle("Gauss")+
  theme_classic()

plot_frag <- gridExtra::grid.arrange(hist_gama, hist_gauss,ncol=2)  
ggsave("figure/hist_fragilidades.jpg",plot_frag, dpi = 300)


# multiple Density plot 

gama <- data.frame(rep("Gama", times = length(frag_2$frail)),frag_2$frail)
gauss <- data.frame(rep("Gauss", times = length(frag_3$frail)),frag_3$frail)  
names(gama) <- c("Dist.", "Fragilidade")
names (gauss) <- c("Dist.", "Fragilidade")
frail <- rbind(gama, gauss)

#density plot
density_frail <- ggplot(frail, aes(x=Fragilidade ,fill = Dist.)) + 
  # color property for changing color of plot
  # geom_density() function plots the density plot
  geom_density(alpha = 0.5, , color = "darkgrey",size = 0.3) +
  geom_vline(aes(xintercept = 0), color = "red", linewidth = 0.3) +
  scale_fill_manual(values = c("orange", "darkgreen"), name = " ") +
  ylab("Densidade") +
  xlab("Fragilidade") +
  theme_classic()

ggsave("figure/dens_fragilidades.jpg",density_frail, dpi = 300) 
  #tem uma concordancia melhor que o pwp_2 e o pwp_3 e 4
#ao inves de usar a variancia robusta(cluster), usa a fragilidade
#para tratar as medidas repetidas jogando efeitos aleatorios para cada crianca

#nesse modelo é verificado o efeito protetor da suplementacao com vitamina A

#não há diferenca de ajuste entre os modelos
anova(frag_2,frag_3)
#a likelihood ratio test can be carried out 
#to formally explore whether the frailty models provide significantly 
#better fits to the data. 

# 8.Análise de resíduos ---------------------------------------------------

#Proporcionalidade - Shoenfeld
#modelo 6 - fragilidade (criancas) gama - frag_2

res.sho <- cox.zph(frag_2)

covar <- c("Beta (t) para Idade","Beta (t) para Sexo",
           "Beta (t) para Tratamento")

jpeg(file = "figure/Shoenfeld%2d.jpg")
for (k in 1:length(frag_2$coefficients)){
 plot(res.sho[k], xlab = "Tempo", col= c("red", "blue"), 
       resid = TRUE, se= TRUE, lwd = 2)
  abline(h=frag_2$coefficients[k], lty=4, col=2, lwd = 2)
}
dev.off()

#Ajustar escalas

jpeg(file = "figure/Shoenfeld_facet.jpg", width = 15, 
     height = 18, units = "cm", res = 300)
par(mfrow = c(2,2))
#sexo
plot(res.sho[1], xlab = "Tempo", col= c("red", "blue"), 
     resid = TRUE, se= TRUE, lwd = 2,
     ylab = "Beta (t) para sexo", main = "Sexo")
abline(h=frag_2$coefficients[1], lty=2, col=2, lwd = 2)
#idade
plot(res.sho[2], xlab = "Tempo", col= c("red", "blue"), 
     resid = TRUE, se= TRUE, lwd = 2, ylim = c(-0.06,0.06),
     ylab = "Beta (t) para idade", main = "Idade")
abline(h=frag_2$coefficients[2], lty=2, col=2, lwd = 2)
#grupo
plot(res.sho[3], xlab = "Tempo", col= c("red", "blue"), 
     resid = TRUE, se= TRUE, lwd = 2,
     ylab = "Beta (t) para tratamento", main = "Tratamento")
abline(h=frag_2$coefficients[3], lty=2, col=2, lwd = 2)
dev.off()

#resultados dos testes
res.sho$table
#Rejeita a proporcionalidade dos riscos ao longo do tempo para as tres
#variaveis do modelo
#no grafico, os efeitos de grupo de tratamento e sexo parecem mudar no fim
#do periodo 
# mas o intervalo contem a reta do coeficiente beta
# no grupo de tratamento ha uma discreta inclinacao indicando 
#diminuicao do efeito protetor ao longo do tempo
#aparentemente, o efeito da idade se modifica ao longo do tempo
#pq as criancas sao seguidas por um ano, faz sentido,
#cada mes adicionado muda o risco, mas podemos considerar que
# o intervalo contem a reta do beta, em alguns pontos tocando no
#limite inferior.


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
                       
mart <- ggcoxfunctional(Surv(ini, fim, status) ~ idade, data = diar,
                ggtheme = theme_minimal(),
                ylab = "Resíduos Martingale - modelo nulo",
                xlab = "Idade (meses)", 
                ylim=c(-5,1)
)

library(gridExtra)
ggsave("figure/martingale.jpg", arrangeGrob(grobs = mart), dpi = 300)

# a forma funcional esta adequada


# Observacoes Atípicas - Residuos deviance
res.dev <-resid(frag_2, type = "deviance")
plot(res.dev, col= "grey", ylab = "Resíduos deviance", xlab = "Índice")
abline(h=0, col="red")

#Proporcao de observacoes atipicas - 10%
(sum(res.dev  > 2) + sum(res.dev  < (-2)))/length(diar$numcri)

#outra forma
devian <- ggcoxdiagnostics(frag_2, type = "deviance",
                 linear.predictions = FALSE, sline = FALSE, 
                 ggtheme = theme_minimal(),
                 ylab = "Resíduos deviance",
                 xlab = "Índice",
                 main = " "
)

ggsave("figure/deviance.jpg", devian, dpi = 300)

#Residuos escore - pontos influentes

res.esc <- resid(frag_2, type= 'dfbetas')

jpeg(file="figure/escore_1.jpg", width = 15, height = 20, units = "cm", pointsize = 12,
     res = 600, quality = 85)
par(mfrow = c(2,2))
plot (factor(diar$sexo) , res.esc[,1],xlab = "Sexo", ylab = "Resíduos escore", col = 0)
plot (diar$idade , res.esc[,2],xlab = "Idade (década)", ylab = "Resíduos escore")
plot (factor(diar$grupo) , res.esc[,3],xlab = "Tratamento", ylab = "Resíduos escore",col = 0)
dev.off()

#outra forma
escore <- ggcoxdiagnostics(frag_2, type = "dfbetas",
                   linear.predictions = FALSE,
                   sline = FALSE,
                   ylab = "Resíduos dfbetas",
                  xlab = "Índice",
                  ggtheme = theme_minimal())


ggsave("figure/escore.jpg", escore, dpi = 300)

# 9.Gráfico dos efeitos fixos e aleatorios  ----------------------------------------------------

#Efeitos fixos do modelo
exp_coef<-(frag_2$coefficients)
d_forest <- exp(confint(frag_2))
d_forest <- round(d_forest[-4,],2)
dat <- data.frame(
  Index = c(1:3), ## This provides an order to the data
  label = c("Sexo (M)","Idade", "Grupo (Vit. A)"),
  HR = exp(frag_2$coefficients),
  LL = exp(confint(frag_2))[,1],
  UL = exp(confint(frag_2))[,2]
)

## Plot forest plot

forest <- ggplot(dat, aes(y = Index, x = HR)) +
  geom_point(shape = 18, size = 4) +  
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

ggsave("figure/forest_fix.jpg",forest, dpi = 300)

#Grafico dos efeitos aleatorios
#source("Rfun.r")
# Para o gráfico o modelo precisa ter sido gerado com sparse=T (pg. 385)
#Funcao que gera o grafico modificada:
plot.frail<-function(unidade,model,...){
  if (!is.null(model$frail)){
    fragil <- data.frame(unidade=unique(unidade),fragil=model$frail,inf=(model$frail - 1.96*sqrt(model$fvar)),sup=(model$frail + 1.96*sqrt(model$fvar)))
    ordenado <- fragil[order(fragil[,2]),]                     
    x<-matrix(1:nrow(fragil),ncol=nrow(fragil),nrow=2,byrow=T) 
    y<-t(ordenado[,3:4])                                       
    matplot(x,y,type='l',col=1,lty=1,axes=F,...)               
    box();axis(2); axis(1,at=x[1,],labels=ordenado$unidade,las=2)
    matpoints(x[1,],y[1,],pch=24,col=1)                        
    matpoints(x[2,],y[2,],pch=25,col=1)                        
    points(1:ncol(x),ordenado[,2],pch=19, cex=0.1)                      
    abline(h=0)
  }
  else{
    warning("Esta funcao somente implementada para sparse=TRUE")
  }
  
}

plot.frail(diar$numcri,frag_2)
title("Fragilidades Estimadas - Gama")

# igual no Rfun
fragil <- data.frame(unidade=unique(diar$numcri),fragil=frag_2$frail,inf=(frag_2$frail - 1.96*sqrt(frag_2$fvar)),sup=(frag_2$frail + 1.96*sqrt(frag_2$fvar)))
ordenado <- fragil[order(fragil[,2]),]                     
x<-matrix(1:nrow(fragil),ncol=nrow(fragil),nrow=2,byrow=T) 
y<-t(ordenado[,3:4])   

jpeg(file = "figure/random_effect.jpg")
matplot(x,y,type='l',col= 1,lty=1,axes=F, xlab = "Criança",
        ylab = "Fragilidade")               
box();axis(2); axis(1,at=x[1,],labels=ordenado$unidade,las=2)
matpoints(x[1,],y[1,],pch=24,col=1)                        
matpoints(x[2,],y[2,],pch=25,col=1)                        
points(1:ncol(x),ordenado[,2],pch=19, cex=0.1, col = "green")                      
abline(h=0, col="red")
dev.off()


#com ggplot
grupos <- as.factor(rep(1:10, each = 86))
ordenado <- cbind(ordenado, grupos)
index <- 1:length(ordenado$unidade)
ordenado <- cbind(ordenado, index)

#com ggplot dem 10 grupos para poder visualizar cada criança
ggplot(ordenado, aes(y = index, x = fragil)) +
  geom_point(shape = 18, size = 1.0) +  
  geom_errorbarh(aes(xmin = inf, xmax = sup), height = 0.08) +
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed", cex = 0.5) +
  scale_y_continuous(name = " ", breaks=1:length(ordenado$unidade), labels = ordenado$unidade) +
  xlab("Fragilidade") + 
  #ylab("Criança") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 4, colour = "black"),
        axis.text.x.bottom = element_text(size = 8, colour = "black"),
        axis.title.x = element_text(size = 8, colour = "black"),
        axis.title.y = element_text(size = 8, colour = "black"))


# 10. Sugestão Raquel  ----------------------------------------------------

#colocar o numcri como fragilidade ao invés de cluster (dois frailty)

#efeito aleatorio gamma
two_frailty <- coxph(Surv(ini, fim, status) ~ sexo + idade + grupo + 
                 #strata(enum) + #ricos basais diferentes
                 frailty(grav2, sparse = F, dist = "gamma") +
                 frailty(numcri, sparse = F, dist = "gamma"),  
                 data = diar)
summary(two_frailty)

                   
