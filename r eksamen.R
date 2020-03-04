    grades <- read.table(file="grades.txt",header=T)


#4.2
sum(grades$Freq)
grade_yes_bottom <- (1841*(296+238+219+301))
grade_no_bottom <-  (1841*(244+149+202+192))
p1 <- ((296+238)*(296+219))/grade_yes_bottom

p2 <- ((296+238)*(238+301))/grade_yes_bottom

p3 <- ((219+301)*(219+296))/grade_yes_bottom

p4<- ((219+301)*(238+301))/grade_yes_bottom

p5 <- ((149+244)*(202+244))/grade_no_bottom

p6 <- ((244+149)*(192+149))/grade_no_bottom

p7 <- ((192+202)*(244+202))/grade_no_bottom

p8 <- ((192+202)*(192+149))/grade_no_bottom

p <- c(p1,p2,p3,p4,p5,p6,p7,p8)

#4.3
x <- 0
for (i in 1:8){
x <- x-2*grades$Freq[i]*log(p[i]/(grades$Freq[i]/1841))  
  
} 
x

#4.4
#saturated model har frihedsgrad 7
1-pchisq(x,df=2)



#4.6
p01 <- ((296+238+244+149)*(296+219))/1841^2

p02 <- ((296+238+244+149)*(238+301))/1841^2

p03 <- ((219+301+202+192)*(219+296))/1841^2

p04<- ((219+301+202+192)*(238+301))/1841^2

p05 <- ((296+238+244+149)*(202+244))/1841^2

p06 <- ((296+238+244+149)*(192+149))/1841^2

p07 <- ((219+301+202+192)*(244+202))/1841^2

p08 <- ((219+301+202+192)*(192+149))/1841^2

p0 <- c(p01,p02,p03,p04,p05,p06,p07,p08)



#4.7
x <- 0
for (i in 1:8){
  x <- x-2*grades$Freq[i]*log(p0[i]/p[i])  
  
} 
x

1-pchisq(x,df=1)#0.75


#4.9
simulation <- function(m){
loglik <- numeric(1000)

for (i in 1:1000){
y <- rmultinom(1,m,p0)

  p1_hat <- ((y[1]+y[2])*(y[1]+y[3]))/(m*(y[1]+y[2]+y[3]+y[4]))
  p2_hat <- ((y[1]+y[2])*(y[2]+y[4]))/(m*(y[1]+y[2]+y[3]+y[4]))
  p3_hat <- ((y[3]+y[4])*(y[3]+y[1]))/(m*(y[1]+y[2]+y[3]+y[4]))
  p4_hat <- ((y[3]+y[4])*(y[2]+y[4]))/(m*(y[1]+y[2]+y[3]+y[4]))
  p5_hat <- ((y[6]+y[5])*(y[7]+y[5]))/(m*(y[5]+y[6]+y[7]+y[8]))
  p6_hat <- ((y[5]+y[6])*(y[8]+y[6]))/(m*(y[5]+y[6]+y[7]+y[8]))
  p7_hat <- ((y[8]+y[7])*(y[5]+y[7]))/(m*(y[5]+y[6]+y[7]+y[8]))
  p8_hat <- ((y[8]+y[7])*(y[8]+y[6]))/(m*(y[5]+y[6]+y[7]+y[8]))
  p_hat <- c(p1_hat,p2_hat,p3_hat,p4_hat,p5_hat,p6_hat,p7_hat,p8_hat)

  p01 <- ((y[1]+y[2]+y[5]+y[6])*(y[1]+y[3]))/m^2
  p02 <- ((y[1]+y[2]+y[5]+y[6])*(y[2]+y[4]))/m^2
  p03 <- ((y[3]+y[4]+y[7]+y[8])*(y[3]+y[1]))/m^2
  p04 <- ((y[3]+y[4]+y[7]+y[8])*(y[2]+y[4]))/m^2
  p05 <- ((y[1]+y[2]+y[5]+y[6])*(y[7]+y[5]))/m^2
  p06 <- ((y[1]+y[2]+y[5]+y[6])*(y[8]+y[6]))/m^2
  p07 <- ((y[3]+y[4]+y[7]+y[8])*(y[5]+y[7]))/m^2
  p08 <- ((y[3]+y[4]+y[7]+y[8])*(y[8]+y[6]))/m^2
  p0_hat  <- c(p01,p02,p03,p04,p05,p06,p07,p08)
  
  x <- 0
  for (j in 1:8){
    x <- x-2*y[j]*log(p0_hat[j]/p_hat[j])  
    
  } 
  loglik[i] <- x

}
loglik
}
sim18 <- simulation(18)
sim180 <- simulation(180)
sim1800 <- simulation(1800)


loglik
plot(density(loglik,na.rm = T),col="red")



#4.10
chi2 <- rchisq(100000,df=1)
plot(density(chi2),xlab = "Observations",main="Chi square comparison",xlim=c(0,7))
lines(density(sim18,na.rm = T),col="red")
lines(density(sim180,na.rm = T),col="blue")
lines(density(sim1800,na.rm = T),col="green")
legend("topright", c("true chisq"," m=18","m=180",
                        "m=1800"),
       col= c("black","red","blue","green"),
       lwd = c(1,1,1,1),cex = 0.9, bty = "n")

