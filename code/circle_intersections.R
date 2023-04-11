



# https://stackoverflow.com/questions/59825256/create-a-point-at-an-intersection-of-two-circles-using-r


A <- c(10, 11, 12, 7,9) #added another circle to test multiple intersections
X <- c(0.245188, -1.199507,-42.31990, -39.98215, -35)
Y <- c(-40.27914, -39.12006, -2.734103, 3.181081,-5)
Di <- c(3.74, 5.18, 5.39, 5.11,6)
Mp <- c(19, 19, 19, 19,19)
df <- data.frame(Mp, A,  X, Y, Di)

library(ggplot2)
library(ggforce)
library(tidyverse)


combinations=combn(df$A,2) #find all potential combinations of circles in the data
remove(df_intersections) #if you rerun the script, the df would become ever longer ;-)
for(i in 1:ncol(combinations)){ #run the calculation for all circle combinations
  circle1=combinations[1,i]
  circle2=combinations[2,i]
  dfs=df[df$A==circle1|df$A==circle2,]

  #distance between circles
  dbc=sqrt(diff(dfs$X)**2+diff(dfs$Y)**2)

  dfs$sign=c(1,-1) #will have two rows in this case

  dfs=dfs %>% 
    mutate(x_intersect=0.5*(sum(X)) + ((diff(Di**2))/(2*dbc**2)) * (diff(X))*-1 + 
             0.5*sqrt((2*sum(Di**2)/dbc**2)-((diff(Di**2)**2)/dbc**4)-1)*(diff(Y))*sign,
           y_intersect=0.5*(sum(Y)) + ((diff(Di**2))/(2*dbc**2)) * (diff(Y))*-1 - 
             0.5*sqrt((2*sum(Di**2)/dbc**2)-((diff(Di**2)**2)/dbc**4)-1)*(diff(X))*sign)
  if(exists("df_intersections")){
    df_intersections=rbind(df_intersections,dfs)
  }else{
    df_intersections=dfs
  }
}

df_intersections=df_intersections %>% filter(!is.na(x_intersect)) #filter out combinations that don't intersect
ggplot(data=df) + 
  geom_point(aes(X, Y)) +
  geom_circle(data=df, aes(x0 =X, y0 = Y, r = Di)) +
  geom_text(aes(X, Y, label = A), data = df, hjust = 1, vjust = 1) +
  coord_fixed() +
  geom_point(data=df_intersections,aes(x=x_intersect,y=y_intersect),size=4, color="red")+
  theme_bw()
