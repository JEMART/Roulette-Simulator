################ Dalembert ##############

#Purpose: Create a simulation of x number of rolls on the roulette using 
#the Dalembert system while tracking the money meanwhile

################################################################
## Add betting to the spins
#Keep track of the amount of money in your pocket
#Assumptions: 
#1: You have unlimited amount of money
#2: You can bet unlimited amount of money

#Disclaimer
  #This script comes with absolutely no warranty
  #We strongly recommend not to bet real money on roulette, the simulations will also show you that you will loose if you would do so

source("basic.func.R")


#Create function to calculate results from a cycle
Lets.play.Dalembert<-function(b,c,x){  #b= amount of min money bet (unit)   c=colour bet on in the first spin, x=total number of spins

      spin<-function(b,c){  
            numbers<-0:36
            s<-sample(numbers,1)
            res<-color(s)
            inv.res<-abs(res-1)   #if 2 or 0=red, if 1=black
            win<-ifelse(res==c,T,F)
            money.gained<-ifelse(res==c,b*2,-b)
            money.total<-ifelse(res==c,b*2-b,-b)
            cycle<-1
            col.row<-1
            d<-data.frame(inv.res,res,c,win, b,b,money.gained,money.total,money.total,cycle,col.row)
            colnames(d)<-c("Inv._Bet","Result","Colour_bet","Win", "Money_bet", "Cum.bet", "Money_won","Money_total", "Net_winnings","Cycle","Col_row")
            d<-as.matrix(d)
            d
      }

      #create first row
      z<-matrix(ncol=11,nrow=x)
      z[1,]<-spin(b,c)
      
      z[2:x,2]<-replicate(x-1,roll())

      for(i in 2:x){ #create inv.res for all rows
          z[i,1]<-abs(z[i,2]-1)
      }
      

      for (i in 2:x){   #create bets for all rows
          z[i,3]<-ifelse(z[i-1,2]==z[i-1,3],z[i-1,1],z[i-1,3])
      }  
      

      for (i in 2:x){   #create win vector for all rows, 1=win, 2=loose
          z[i,4]<-ifelse(z[i,2]==z[i,3],1,0)
      }  
      

      for (i in 2:x){   #create money bet in the ongoing cycle vector for all rows
          z[i,5]<-ifelse(z[i-1,4]==1,10,z[i-1,5]*2)
      }  
      
  
      for (i in 2:x){   #create cumulative money bet in the ongoing cycle for all rows
          z[i,6]<-ifelse(z[i-1,4]==1,10,sum(z[i-1,6],z[i,5]))
      }  
    

      for (i in 2:x){   #create money to/from bank vector for all rows
          z[i,7]<-ifelse(z[i,4]==1,z[i,5]*2,-z[i,5])
      }
    

      for (i in 2:x){   #create total amount of money vector for all rows
        z[i,8]<-sum(z[i-1,8],z[i,7])
      }
    

      for(i in 2:x){ #create net winnings for all rows
        z[i,9]<-ifelse(z[i,4]==1,z[i-1,9]-z[i,5]+z[i,7],z[i-1,9]-abs(z[i,7]))
      }

      for (i in 2:x){   #create cycle number vector for all rows
        z[i,10]<-ifelse(z[i,4]==1,z[i-1,10]+1,z[i-1,10])
      }


      for (i in 2:x){  #create number of the same colour in a row for all rows
        z[i,11]<-ifelse(z[i,2]==z[i-1,2],z[i-1,11]+1,1)
      }


    RESULT<-as.data.frame(z)
    colnames(RESULT)<-c("Inv._Bet","Result","Colour_bet","Win", "Money_bet", "Cum.bet", "Money_won","Money_total","Net_win","Cycle_nr","Col_row")
    RESULT<-RESULT[,-1]
    red<-which(RESULT$Result==0)
    black<-which(RESULT$Result==1)
    green<-which(RESULT$Result==2)

    RESULT$Result[red]<-"red"
    RESULT$Result[black]<-"black"
    RESULT$Result[green]<-"green"

    red.2<-which(RESULT$Colour_bet==0)
    black.2<-which(RESULT$Colour_bet==1)
    green.2<-which(RESULT$Colour_bet==2)
    
    RESULT$Colour_bet[red.2]<-"red"
    RESULT$Colour_bet[black.2]<-"black"
    RESULT$Colour_bet[green.2]<-"green"
    RESULT.1<-which(RESULT[,3]==1)
    RESULT.3<-RESULT[1:max(RESULT.1),]   #the dataframe is ending on a winnning
    
    min.money<-min(RESULT.3$Net_win)   #Biggest amount back
    max.bet<-max(RESULT.3$Money_bet)      #Largest bet
    max.row<-max(RESULT.3$Col_row)        #Largest number in a row
    black.n<-length(which(RESULT.3$Result=="black"))   #How many black?
    red.n<-length(which(RESULT.3$Result=="red"))     #How many red?
    green.n<-length(which(RESULT.3$Result=="green"))   #How many green?
    black.p<-black.n/length(RESULT.3$Result)
    red.p<-red.n/length(RESULT.3$Result)
    green.p<-green.n/length(RESULT.3$Result)
    money.end<-RESULT.3$Net_win[length(RESULT.3$Net_win)]

    res<-data.frame(money.end,min.money,max.row,max.bet,black.p,red.p,green.p)
    colnames(res)<-c("Balance","Lowest_Money_Amount","Max_nr_in_a_row", "Max bet",
                     "p black","p red","p green")

    list(res,length(RESULT.3$Net_win),RESULT.3$Net_win)
}




