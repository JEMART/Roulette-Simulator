#color function 2=green, 1=black, 0=red
color<-function(x){  #x=number given by the roulette
  numbers<-0:36
  colors<-c(2,0,1,0,1,0,1,0,1,0,1
            ,1,0,1,0,1,0,1,0,#11-18
            0,1,0,1,0,1,0,1,0,1, #19-28
            1,0,1,0,1,0,1,0 #29-36
  ) 
  
  roulette<-data.frame(numbers,colors)
  numbers.pos<-which(roulette$numbers==x)  #which colour is number x?
  out<-roulette$colors[numbers.pos]
  as.vector(out)
}


#Create function to get a number AND a color (as number)
roll<-function(){
  numbers<-0:36
  s<-sample(numbers,1)
  color(s)
}
