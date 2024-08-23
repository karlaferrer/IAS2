cat("=============================================================\n")
cat("\n")
cat("                      Funcoes do livro \n")
cat("\n")
cat("     Analise de Sobrevivencia: Teoria e Aplicacoes em Saude \n")
cat("\n")
cat("               Ultima atualizacao: Julho de 2011 \n")
cat("\n")
cat("==============================================================\n")
cat("\n")
cat("\n")
cat(" Funcoes disponiveis: plot.pi(), plot.frail() \n")
cat(" Funcao para estimar quantis do tempo de sobrevivencia,\n")
cat(" desenvolvida pelo Prof. John Fox \n")

#=============================================================  
#    Quantis do tempo de sobrevivencia
#SINTAXE:
#quantiles("survfit object",  quantiles=c(.25,.5,.75)) 
#em quantiles coloca-se os valores dos quantis desejados
#a funcao opera sobre um objeto resultante da funcao survfit

quantile.survfit <- 
  function (x, quantiles = c(0.25, 0.5, 0.75), ...) 
  {
    quants <- function(surv, lower, upper, t) {
      if (length(surv) == 1) 
        return(NA)
      warn <- options(warn = -1)
      on.exit(options(warn))
      select <- sapply(quantiles, function(q) q >= surv)
      posn <- apply(select, 2, function(x) min(which(x)))
      q <- t[posn]
      select <- sapply(quantiles, function(q) q >= lower)
      posn <- apply(select, 2, function(x) min(which(x)))
      low <- t[posn]
      select <- sapply(quantiles, function(q) q >= upper)
      posn <- apply(select, 2, function(x) min(which(x)))
      up <- t[posn]
      rbind(low, q, up)
    }
    summary <- summary(x)
    conf.level <- x$conf.int
    strata <- summary$strata
    if (is.null(strata)) {
      table <- quants(summary$surv, summary$lower, summary$upper, 
                      summary$time)
      dimnames(table) <- list(Estimate = c(paste("lower", conf.level, 
                                                 "CL"), "quantile", paste("upper", conf.level, "CL")), 
                              `Survival Probability` = as.character(round(quantiles, 
                                                                          3)))
    }
    else {
      levels <- levels(strata)
      table <- array(0, c(3, length(quantiles), length(levels)))
      dimnames(table) <- list(Estimate = c(paste("lower", conf.level, 
                                                 "CL"), "quantile", paste("upper", conf.level, "CL")), 
                              `Survival Probability` = as.character(round(quantiles, 
                                                                          3)), Stratum = levels)
      for (s in levels) {
        select <- strata == s
        table[, , s] <- quants(summary$surv[select], summary$lower[select], 
                               summary$upper[select], summary$time[select])
      }
    }
    table
  }



#=============================================================  
#           Grafico do indice de prognostico

#SINTAXE:
#plot.pi("coxph object")

"plot.pi"  <-   function(fit,lwd=1,...)
{
  if(is.null(fit$x)) 
    cat("Rode coxph() com o argumento x=TRUE")
  else{
    x<-fit$x
    ss <- survfit(fit)
    cat("The solid line is the fitted model, the dashed one is the K-M\n")
    if(ncol(x)>1) xbar <- apply(x,2,"mean")
    else xbar <- mean(as.vector(x))
    
    b<-fit$coefficients
    bb<-b%*%xbar
    bx<-x%*%b
    j<-order(bx)  
    n<-fit$n
    bx1<-bx[j][round(n/3)]
    bx2<-bx[j][round(2*n/3)]
    
    i1<-bx<=bx1
    i2<-(bx>bx1)&(bx<=bx2)
    i3<-(bx>bx2)
    b1<-mean(bx[i1])
    b2<-mean(bx[i2])
    b3<-mean(bx[i3])
    s<-ss$surv
    s1<-s^{exp(b1-bb)}
    s2<-s^{exp(b2-bb)}
    s3<-s^{exp(b3-bb)}
    plot(ss$time,s1,type="s",ylim=c(0,1),xlim=c(0,max(ss$time)),lwd=lwd,...)
    #    mtext("Time",1,l=2.3,cex=1.7)
    #    mtext("Survival",2,l=2.3,cex=1.7)
    lines(ss$time,s2,type="s",lwd=lwd)
    lines(ss$time,s3,type="s",lwd=lwd)
    if(length(terms(fit$formula)[[2]])==4){
      start<-fit$y[,1]
      stop<-fit$y[,2]
      cens<-fit$y[,3]
      ss1<-survfit(Surv(start[i1],stop[i1],cens[i1])~1)
      lines(ss1$time,ss1$surv,lty=2,type="s",lwd=lwd)
      ss1<-survfit(Surv(start[i2],stop[i2],cens[i2])~1)
      lines(ss1$time,ss1$surv,lty=2,type="s",lwd=lwd)
      ss1<-survfit(Surv(start[i3],stop[i3],cens[i3])~1)
      lines(ss1$time,ss1$surv,lty=2,type="s",lwd=lwd)
    }
    else{
      time<-fit$y[,1]
      cens<-fit$y[,2]
      ss1<-survfit(Surv(time[i1],cens[i1])~1)
      lines(ss1$time,ss1$surv,lty=2,type="s",lwd=lwd)
      ss1<-survfit(Surv(time[i2],cens[i2])~1)
      lines(ss1$time,ss1$surv,lty=2,type="s",lwd=lwd)
      ss1<-survfit(Surv(time[i3],cens[i3])~1)
      lines(ss1$time,ss1$surv,lty=2,type="s",lwd=lwd)
    }
    cat("\n")
  }
}


#=============================================================  
#    Grafico das fragilidades e seus intervalos de confianca

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
    points(1:ncol(x),ordenado[,2],pch=19)                      
    abline(h=0)
  }
  else{
    warning("Esta funcao somente implementada para sparse=TRUE")
  }
  
}

