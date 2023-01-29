 
########################################################################################################
########################################################################################################
########################################################################################################
#######Rotina desenvolvida conforme o protocolo harmonizado, disponível em português no link: #######
####https://www.gov.br/inmetro/pt-br/centrais-de-conteudo/publicacoes/protocolo-harmonizado-ct5.pdf####
########################################################################################################
########################################################################################################
########################################################################################################

library(outliers)

###Dados e critério

R1<-c(10.5000,9.6000,10.4000,9.5000,10.0000,9.6000,9.8000,9.8000,10.8000,10.2000,9.8000,10.2000)
R2<-c(10.4000,9.5000,9.9000,9.9000,9.7000,10.1000,10.4000,10.2000,10.7000,10.0000,9.5000,10.0000)
sigmap<-1.140

###
###
###Teste de Cochran, para outliers nas duplicatas
###(deve ser realizado)
###
###

m<-sum(length(R1),length(R2))/2
colunas<-ncol(cbind(R1,R2))

Di2<-(R1-R2)^2
sum_Di2<-sum(Di2)
max_Di2<-max(Di2)

coch_cal<-max_Di2/sum_Di2
coch_tab<-qcochran(0.95,colunas,m)



dec_coch<-ifelse(coch_cal<=coch_tab,
                 "não deve ser removida nenhuma duplicata",
                 "a duplicata de maior diferença deve ser removida")

print(dec_coch)

###
###
###Teste de homogeneidade, conforme o Protocolo Harmonizado
###(se Cochran validar, então deve ser realizado como abaixo)
###
###

###Cálculo da Variação Analítica

s2an<-sum_Di2/(2*m)

###Cálculo da Variação Amostral

si<-R1+R2
Vs<-var(si)
s2sam<-((Vs/2)-s2an)/2

###Cálculo do Valor Crítico

F1<-qchisq(0.95,m-1)/(m-1)
F2<-(qf(0.95,m-1,m,lower.tail=TRUE)-1)/2
C<-(F1*(0.3*sigmap)^2)+(F2*s2an)

###Decisão Final

dec<-ifelse(s2sam<=C,"aprovada","reprovada")

print(paste("a variância amostral foi de ",round(s2sam,4),sep=""))
print(paste("a variância do EP foi de ",round((sigmap)^2,4),sep=""))
print(paste("o valor crítico foi de ",round(C,4),sep=""))
print(paste("A homogeneidade foi ",dec," segundo o protocolo harmonizado",sep=""))
