
################################################################################
#######################    Simple Random walk               ####################
################################################################################

##################   Installing and Reading packages   #########################
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)){ 
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}
packages<-c("ggplot2", "dplyr", "tidyverse", "plotly", "ggthemes", "tidyr","stringr")
check.packages(packages)


################################################################################
#########################   Random walk function  ##############################
################################################################################
simple.random.walk <- function(n.steps,n.sim,prob.r=0.5){
  n <- n.steps
  a <- prob.r
  
  x.left = -1
  x.right = 1
  
  Sn_mat <- matrix(0,ncol=n+1,nrow=n.sim)
  for(i in 1:n.sim){
    for(j in 2:(n+1)){
      step <- sample(c(x.left,x.right),1,prob=c(1-a,a),replace=F)
      Sn_mat[i,j] <- Sn_mat[i,j-1] + step  
    }
  }
  # nombre de columnas
  names <- sapply(1:(n+1),function(i) paste('step',i,sep=''))
  # data frame
  result_df <- data.frame('sim'=sapply(1:n.sim, function(i) paste('sim',i,sep='')),
                   'Sn'=Sn_mat)
  return(result_df)
}
################################################################################
#######  Ejemplo (app):
n.steps <- 10000   # número de pasos
n.sim <- 1000     # número de trajectorias
a <- 0.5          # probabilidad a la derecha

# simulación
df <- simple.random.walk(n.steps,n.sim,prob.r=a)

##  Base para gráfico
df_rw <- df  %>%
  gather(key='t',value='valor',-sim) %>%
  mutate(t = as.numeric(substring(t,4,10))) %>%
  arrange(sim)

moments_rw <- data.frame('t'=c(1:n.steps),'a'=a) %>%
  mutate('mean'=t*(a-(1-a)),
         'sd_sup'=mean + 2*sqrt(4*t*a*(1-a)),
         'sd_inf'=mean - 2*sqrt(4*t*a*(1-a)))

# Gráfico de trajectorias  
p1 <- ggplot(df_rw,aes(x=t,y=valor,color=sim)) +
  geom_line() +
  geom_line(moments_rw, mapping=aes(x=t,y=mean),col='red',size=0.7) +
  geom_line(moments_rw, mapping=aes(x=t,y=sd_sup),col='blue',size=0.7,linetype = "dashed") +
  geom_line(moments_rw, mapping=aes(x=t,y=sd_inf),col='blue',size=0.7,linetype = "dashed") +
  scale_colour_grey(start = 0.2,end = 0.6) +
  theme(legend.position="none") +
  ggtitle(paste(n.sim," trayectorias del camino aleatorio simple.",sep=''))


## Distribución al tiempo t
t.selected <- 1000

df_dist <- df_rw %>% filter(t==t.selected)

p2 <- ggplot(df_dist,aes(valor)) +
  geom_histogram(bins=20,fill='red',col="white") +
  ggtitle(paste("Distribución al tiempo t = ",t.selected,sep="")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())

################################################################################
##################### Caminata simple en dos dimensiones  ######################
################################################################################
randomWalk2d_plot <- function(base, n.steps){
  
  df <- base
  df_2d <- df  %>%
    gather(key='t',value='valor',-sim) %>%
    filter(sim == 'sim1' | sim=='sim2') %>%
    spread(sim,valor)  %>%
    mutate(t = as.numeric(substring(t,4,10))) %>%
    arrange(t) %>%
    filter(t <= n.steps)
  b2 <- ggplot(df_2d,aes(x=sim1,y=sim2))+
    geom_point(color="blue") +
    geom_point(df_2d%>%filter(t == 1),mapping=aes(x=sim1,y=sim2),color="green") +
    geom_point(df_2d%>%filter(t == max(t)),mapping=aes(x=sim1,y=sim2),color="red") +
    geom_path() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank())
  return(b2)
}

################################################################################
################## Caminata simple en tres dimensiones  ########################
################################################################################
randomWalk3d_plot <- function(base, n.steps){
  
  df <- base
  df_2d <- df  %>%
    gather(key='t',value='valor',-sim) %>%
    filter(sim == 'sim1' | sim=='sim2' | sim == 'sim3') %>%
    spread(sim,valor)  %>%
    mutate(t = as.numeric(substring(t,4,10))) %>%
    arrange(t) %>%
    filter(t <= n.steps)
  b3 <- plot_ly(x=df_2d$sim1, y=df_2d$sim2, z=df_2d$sim3, type="scatter3d", mode="lines")
  
  return(b3)
}

