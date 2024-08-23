library(KMsurv)
data(larynx)
larynx$age <- as.numeric(scale(larynx$age))
larynx$diagyr <- as.numeric(scale(larynx$diagyr))
larynx$stage <- as.factor(larynx$stage)

library(INLA)
library(INLAjoint)
library(frailtyHL)

#modelo tutorial do artigo
data(kidney)
kidney$sex <- kidney$sex - 1 # 0: male (reference)
frlt<-joint(formSurv = inla.surv(time = time, event = status) ~ sex
            + (1 | id), basRisk = "weibullsurv", id = "id", dataSurv = kidney)

#modelo diarreia
summary(frlt2)
summary(frlt2, hr=T)$SurvEff[[1]]["sex_S1",]

frlt2 <- joint(formSurv = inla.surv(time = tempo, event = status) ~ sexo + 
              idade + grupo + (1 | numcri), basRisk = "weibullsurv",
              id = "numcri", dataSurv = diar)

#Grafico dos efeitos aleatorios no INLA
# igual no Rfun
fr <- data.frame(uni=unique(diar$numcri),fragi=frlt2$summary.random$IDIntercept_S1[,5],
                 inf=frlt2$summary.random$IDIntercept_S1[,4],
                 sup=frlt2$summary.random$IDIntercept_S1[,6])

ord <- fr[order(fr[,2]),]                     
x2<-matrix(1:nrow(fr),ncol=nrow(fr),nrow=2,byrow=T) 
y2<-t(ord[,3:4])   

#jpeg(file = "figure/random_effect.jpg")
matplot(x2,y2,type='l',col= 1,lty=1,axes=F, xlab = "Criança",
        ylab = "Fragilidade")               
box();axis(2); axis(1,at=x[1,],labels=ord$uni,las=2)
matpoints(x2[1,],y2[1,],pch=24,col=1)                        
matpoints(x2[2,],y2[2,],pch=25,col=1)                        
points(1:ncol(x2),ord[,2],pch=19, cex=0.1, col = "green")                      
abline(h=0, col="red")
#dev.off()


#histograma
ggplot(as.data.frame(frlt2$summary.random$IDIntercept_S1), 
       aes(x = frlt2$summary.random$IDIntercept_S1[,5])) +
  geom_histogram(aes(y = ..density..), fill = "darkgrey") +
  geom_vline(aes(xintercept = 0), color = "red", linewidth = 0.5) +
  geom_density(color = "green", linewidth = 0.5)+
  ylab ("Densidade") +
  xlab("Fragilidade")+
  ggtitle("INLAjoint")+
  theme_classic()


#Efeitos fixos
#Efeitos fixos do modelo

data <- data.frame(
  Index = c(1:3), ## This provides an order to the data
  label = c("Sexo (M)","Idade", "Grupo (Vit. A)"),
  HR = exp(frlt2$summary.fixed[-1,4]),
  LL = exp(frlt2$summary.fixed[-1,3]),
  UL = exp(frlt2$summary.fixed[-1,5])
)

## Plot forest plot

ggplot(data, aes(y = Index, x = HR)) +
  geom_point(shape = 18, size = 4) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.10) +
  geom_vline(xintercept = 1, color = "blue", linetype = "dashed", cex = 0.5) +
  scale_y_continuous(name = "", breaks=1:3, labels = data$label, trans = "reverse") +
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


