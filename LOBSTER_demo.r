#_____________________________________________________________________________

#						Introduction
#_____________________________________________________________________________


# 				Welcome to the LOBSTER R demo.

#              http://LOBSTER.wiwi.hu-berlin.de
#___________________________________________________________________________



# The code provided below might help you get started with your LOBSTER data. 
# The demo focuses on the two LOBSTER output files 'orderbook' and 'message'.

# You can find a detailed description of the LOBSTER data structure 
# at http://LOBSTER.wiwi.hu-berlin.de

# Data used: AMZN - 2012-June-21 - 10 Levels
#_____________________________________________________________________________



#_____________________________________________________________________________
#
# Set up the Basics
# load the libraries, the package gplots and graphics need to be installed
#_____________________________________________________________________________

library(graphics)

# Set the working directory
# setwd("set your work directory")

# Note: The files must be in the same working directory as the LOBSTER_demo.r file.
 
ticker  <- "AMZN"                     #TICKER 

# DATE for which data is downloaded, the file name you downloaded contains this string , say if you downloaded from 1st july 2009 , type here 2009-07-01
demodate = "2012-06-21"
starttime <- 34200000 
endtime <- 57600000
# Levels
nlevels         = 10;
# Name of Orderbook
ORDERBOOK <- paste(paste(ticker , demodate ,starttime,endtime,"orderbook" ,nlevels ,sep = "_"),"csv",sep = ".")

# Name of MSGbook
MSGBOOK <- paste(paste(ticker , demodate ,starttime,endtime,"message" ,nlevels ,sep = "_"),"csv",sep = ".")

#_____________________________________________________________________________
#
# Load Messsage File
#_____________________________________________________________________________


# Load data
dataM <- read.csv ( MSGBOOK )   

# Name the columns 
columns <- c ( "Time" , "Type" , "OrderID" , "Size" , "Price" , "TradeDirection" )
colnames ( dataM ) <- columns

# Message file information:
# ----------------------------------------------------------
#
#   - Dimension:    (NumberEvents x 6)
#
#   - Structure:    Each row:
#                   Time stamp (sec after midnight with decimal
#                   precision of at least milliseconds and
#                   up to nanoseconds depending on the period),
#                   Event type, Order ID, Size (# of shares),
#                   Price, Direction
#
#                   Event types:
#                       - '1'   Submission new limit order
#                       - '2'   Cancellation (partial)
#                       - '3'   Deletion (total order)
#                       - '4'   Execution of a visible limit order
#                       - '5'   Execution of a hidden limit order
# 			- '7'   Trading Halt (Detailed 
#                               information below)
#
#                   Direction:
#                       - '-1'  Sell limit order
#                       - '-2'  Buy limit order
#                       - NOTE: Execution of a sell (buy)
#                               limit order corresponds to
#                               a buyer-(seller-) initiated
#                               trade, i.e. a BUY (SELL) trade.
#
# ----------------------------------------------------------

#_____________________________________________________________________________
#
# Data Preparation - Message File
# Remove observations outside the official trading hours
#_____________________________________________________________________________
#              


# Trading hours (start & end)
startTrad   = 9.5*60*60;       # 9:30:00.000 in ms after midnight
endTrad     = 16*60*60;        # 16:00:00.000 in ms after midnight

# dataM_part is data frame containing messages during the continue trading period


dataM_part = dataM[dataM$Time>=startTrad & dataM$Time<=endTrad,]


# Note: As the rows of the message and orderbook file correspond to each other, the time index of
# the message file can also be used to 'cut' the orderbook file.



# Check for trading halts
# ----------------------------------------------------------


tradehaltIdx = which(dataM[,2] == 7 & dataM[,5] == -1 );
tradequoteIdx = which(dataM[,2] == 7 & dataM[,5] == 0 );
traderesumeIdx = which(dataM[,2] == 7 & dataM[,5] == 1 );
  
		
if(length(tradehaltIdx)==0 & length(tradequoteIdx)==0  & length(traderesumeIdx)==0 )
	print("No trading halts detected.")
		
if(length(tradehaltIdx) !=0)
	cat("Data contains trading halt! at time stamp(s)", dataM[tradehaltIdx,1],"\n" )
		
if(length(tradequoteIdx) !=0)
	cat(" Data contains quoting message! at time stamp(s)", dataM[tradequoteIdx,1], "\n")
			
if(length(traderesumeIdx) !=0)
	cat(" Data resumes trading! at time stamp(s) ", dataM[traderesumeIdx,1],"\n")
		
	

#
#		When trading halts, a message of type '7' is written into the 
#		'message' file. The corresponding price and trade direction 
#		are set to '-1' and all other properties are set to '0'. 
#		Should the resume of quoting be indicated by an additional 
#		message in NASDAQ's Historical TotalView-ITCH files, another 
#		message of type '7' with price '0' is added to the 'message' 
#		file. Again, the trade direction is set to '-1' and all other 
#		fields are set to '0'. 
#		When trading resumes a message of type '7' and 
#		price '1' (Trade direction '-1' and all other 
#		entries '0') is written to the 'message' file. For messages 
#		of type '7', the corresponding order book rows contain a 
#		duplication of the preceding order book state. The reason 
#		for the trading halt is not included in the output.
#						
#		Example: Stylized trading halt messages in 'message' file.				
#	
#		Halt: 			36023	| 7 | 0 | 0 | -1 | -1
#											...
#		Quoting: 		36323 	| 7 | 0 | 0 | 0  | -1
#											...
#		Resume Trading:		36723   | 7 | 0 | 0 | 1  | -1
#											...
#		The vertical bars indicate the different columns in the  
#		message file.






# Set Bounds for Intraday Intervals
# ----------------------------------------------------------

# Define interval length
freq = 5*60;   # Interval length in ms 5 minutes

# Number of intervals from 9:30 to 4:00
noint= (endTrad-startTrad)/freq
dataM_part$index = seq(from=1,to=dim(dataM_part)[1])

# Variables for 'for' loop
j= 0
l =0
bound =0               	  # Variable for inverval bound
visible_count = 0         # visible_count calculates the number of visible trades in an interval of 5 min
hidden_count = 0          # hidden_count calculates the number of visible trades in an interval of 5 min
visible_size = 0          # Total volume of visible trades in an interval of 5 minutes
hidden_size = 0           # Total volume of hidden trades in an interval of 5 minutes

# Set Bounds for Intraday Intervals
for(j in c(1:noint)) {

	bound[j+1] = startTrad + j * freq
	}
bound[1] = startTrad

#_____________________________________________________________________________
#   
# Plot - Number of Executions and Trade Volume by Interval
#_____________________________________________________________________________
              
# Note: Difference between trades and executions
#
#    The LOBSTER output records limit order executions
#    and not what one might intuitively consider trades.
#
#    Imagine a volume of 1000 is posted at the best ask
#    price. Further, an incoming market buy order of
#    volume 1000 is executed against the quote.
#
#    The LOBSTER output of this trade depends on the
#    composition of the volume at the best ask price.
#    Take the following two scenarios with the best ask
#  	 volume consisting of ...
#    	(a) 1 sell limit order with volume 1000
#    	(b) 5 sell limit orders with volume 200 each
#       	(ordered according to time of submission)
#
#     The LOBSTER output for case ...
#       (a) shows one execution of volume 1000. If the
#           incoming market order is matched with one
#           standing limit order, execution and trade
#           coincide.
#       (b) shows 5 executions of volume 200 each with the
#           same time stamp. The incoming order is matched
#           with 5 standing limit orders and triggers 5
#           executions.
#
#       Bottom line:
#       LOBSTER records the exact limit orders against
#       which incoming market orders are executed. What
#       might be called 'economic' trade size has to be
#       inferred from the executions.


# Logic to calculate number of visible/hidden trades and their volume

for(l in c(1:noint)) 
{
         visible_count[l] = nrow(dataM_part[dataM_part$Time > bound[l] & dataM_part$Time < bound[l+1] & dataM_part$Type == 4,])
         visible_size[l] = sum(dataM_part[dataM_part$Time > bound[l] & dataM_part$Time < bound[l+1] & dataM_part$Type == 4,4])/100
         
         hidden_count[l] = nrow(dataM_part[dataM_part$Time > bound[l] & dataM_part$Time < bound[l+1] & dataM_part$Type == 5,])
         hidden_size[l] = sum(dataM_part[dataM_part$Time > bound[l] & dataM_part$Time < bound[l+1] & dataM_part$Type == 5,4])/100
         
}
      
# Divide plot area into two windows
par(mfrow=c(1,2) )
	  
# Plot no of visible trades
plot(c(1:noint),visible_count , type ='h' , lwd = 5 , col = 'red' , ylim = c(-max(hidden_count), max(visible_count)) ,ylab ="Number of Executions" ,xlab = "Interval" )
title(sprintf("Number of Executions by Interval for %s " ,ticker ,cex = 0.8 ) )

# No of hidden trades in an invterval
lines(c(1:noint),-hidden_count, type ='h' , lwd = 5 , col = 'blue' )

# Legend
legend("top", c('Hidden' ,'Visible'),  col=c('blue','red'), horiz = TRUE , lty = 1 ,  inset = .05)

# Second plot of visible volume in an interval
plot(c(1:noint),visible_size, type ='h' , lwd = 5 , col = 'red' , ylim = c(-max(hidden_size)-200, max(visible_size)) , ylab ="Volume of Trades(x100 shares)" ,xlab ="Interval")
title( sprintf("Trade Volume by Interval for %s " ,ticker ,cex = 0.8 ))

# Hidden volume in an interval
lines(c(1:noint),-hidden_size, type ='h' , lwd = 5 , col = 'blue' )

# Legend to second plot
legend("top", c('Hidden' ,'Visible'),  col=c('blue','red'), horiz = TRUE , lty = 1 ,  inset = .05)


#_____________________________________________________________________________
#
# Load Order Book File
#_____________________________________________________________________________

# Load data
dataOB <- read.csv( ORDERBOOK )   
                                  
	# Note: The file contains more than 250 000 entries. It takes a few seconds to load.

columns2 <- c("ASKp1" , "ASKs1" , "BIDp1",  "BIDs1")

# naming the columns of data frame                                          
if (nlevels > 1)
{
	for ( i in 2:nlevels )
	{ 
  		columns2 <- c (columns2,paste("ASKp",i,sep=""), paste("ASKs",i,sep=""),paste("BIDp",i,sep=""),paste("BIDs",i,sep="")) 
	}
}
	
colnames ( dataOB ) <- columns2


# Orderbook file information:
# ----------------------------------------------------------
#
#   - Dimension:    (NumberEvents x (NumberLevels*4))
#
#   - Structure:    Each row:
#                   Ask price 1, Ask volume 1, Bid price 1,
#                   Bid volume 1, Ask price 2, Ask volume 2,
#                   Bid price 2, Bid volume 2, ...
#
#   - Note:         Unoccupied bid (ask) price levels are
#                   set to -9999999999 (9999999999) with volume 0.
#				      
# ----------------------------------------------------------


#_____________________________________________________________________________
#
# Data Preparation - Order Book File
#_____________________________________________________________________________


# Take only order books during the continuous trading period
# from 9:30:00 to 16:00:00
# ----------------------------------------------------------

# Trading hours (start & end)  16:00:00.000 in ms after midnight
timeindex <-dataM$Time>=startTrad & dataM$Time<=endTrad

dataOB_part = dataOB[timeindex,]
# Convert prices into dollars
#    Note: LOBSTER stores prices in dollar price times 10000

for ( i in c(seq(from = 1, length=2*nlevels, by = 2)) ) 
{ 
	dataOB_part[,i ]  = dataOB_part[ ,i]/10000 
}


#_____________________________________________________________________________
#
# Plot - Snapshot of the Limit Order Book
#_____________________________________________________________________________

par(mfrow=c(1,2))
# Note: Pick a random row/event from the order book
totalrows <- nrow(dataOB_part)
random_no <- sample(1:totalrows, 1, replace=F)


# Select colour for ASK and BID prices bars, greeen for BID and Red for ASK
colmatrix = matrix(0,1,4)
colmatrix = c("red","green","red","green","red","green","red","green" )#

# Plot
plot(x=as.numeric(dataOB_part[random_no,seq(from=1,by=2,length=2*nlevels)]),y=as.numeric(dataOB_part[random_no,seq(from=2,by=2,length=2*nlevels)]),type="h" , lwd = 5,col = colmatrix ,xlab ="Price($)", ylab = "Volume" )

# Title 
title(sprintf("Limit Order Book Volume for %s at %s ms" ,ticker,dataM_part[random_no,1]) , cex = 0.8)

# Legend
legend("top",c('BID', 'ASK'), lty = 1, col=c('green','red'),ncol=1 , , horiz = TRUE,inset = .03)


#_____________________________________________________________________________
#
# Plot - Relative Depth in the Limit Order Book
#_____________________________________________________________________________

# Plot variables
percASK =0
x <- c(1:nlevels)

# Separating volume data from data_frame DataOB
randoms = dataOB_part[random_no,seq(from=2,by=2,length=2*nlevels)]

# ASK side cumulative sum
totalsize = cumsum( c(randoms[1,seq(from=1,by=2,length=nlevels),]))
percASK = totalsize/totalsize[nlevels]

# Plot
plot(x,percASK ,type = "s" , col = 'red' ,lwd = 4 , ylim= c(-1,1) , xlab = "Level" , ylab = "% of volume" )

# Title 
title(sprintf("Relative Depth in the Limit Order Book for %s at %s ms for %s levels" ,ticker,dataM_part[random_no,1] , nlevels) , cex = 0.8 )

# BID side
totalsize2 = cumsum(c(randoms[1,seq(from=2,by=2,length=nlevels),] ))
percBID = -totalsize2/totalsize2[nlevels]

# BID side
lines(x,percBID ,type = "s" , col = 'green' , lwd = 4)

# Legend
legend("bottomleft",c('Ask','Bid' ), lty = 1, col=c('red','green' ),ncol=1 ,horiz = TRUE,  inset = .05)

#_____________________________________________________________________________
#
# Plot - Intraday Evolution of Depth
#_____________________________________________________________________________

par(mfrow=c(1,1))
# Calculate the max/ min volume to set limit of y-axis
maxaskvol = max(max(dataOB_part$ASKs1/100), max(dataOB_part$ASKs2/100),  max(dataOB_part$ASKs3/100) )  # calculate the maximum ask volume

# Calculate the max Bid volume , we use negative here and calculate min as we plot Bid below X-axis
maxbidvol = min(min(-dataOB_part$BIDs1/100), min(-dataOB_part$BIDs2/100),  min(-dataOB_part$BIDs3/100) )

# Plot ASK VOLUME level 3
plot ( (dataM_part$Time/(60*60)), ((dataOB_part$ASKs1/100)+(dataOB_part$ASKs2/100)+ (dataOB_part$ASKs3/100)),type = "h" , col = 'red' , axes=FALSE,ylim = c(maxbidvol ,maxaskvol) , ,ylab = "BID              No of Shares(x100)               ASK" , xlab = "Time",frame=TRUE)

# ASK VOLUME level 2
lines( (dataM_part$Time/(60*60)), ((dataOB_part$ASKs1/100)+(dataOB_part$ASKs2/100)),type = "h" , col = 'green')

# ASK VOLUME level 1
lines ( (dataM_part$Time/(60*60)),(dataOB_part$ASKs1/100),type = "h" , col = 'blue')

# BID VOLUME level 3
lines ( (dataM_part$Time/(60*60)), (-(dataOB_part$BIDs1/100)-(dataOB_part$BIDs2/100)-(dataOB_part$BIDs3/100)),type = "h" , col = 'burlywood1' )

# BID VOLUME level 2
lines( (dataM_part$Time/(60*60)), -(dataOB_part$BIDs2/100)-(dataOB_part$BIDs1/100),type = "h" , col = 'purple')

# BID VOLUME level 1
lines ( (dataM_part$Time/(60*60)), -(dataOB_part$BIDs1/100),type = "h" , col = 'cyan')


# Labels
axis(side=1, at=c(10,11,12,13,14,15,16), label=c(10,11,12,13,14,15,16), lwd=1,cex.axis=0.8,padj=0.55)
axis(side=2, at=c(maxbidvol,0,maxaskvol), label=c(-maxbidvol,0,maxaskvol), lwd=1,cex.axis=0.8,padj=0.55)
title(sprintf("Intraday Evolution of Depth for %s for %s levels" ,ticker, nlevels-7) , cex = 0.8 )

# Legend
legend("bottom",c('ASK 1','ASK 2', 'ASK 3', 'BID 1' , 'BID 2' , 'BID 3'),  col=c('blue','green','red','cyan','purple','darkorange'), horiz = TRUE , lty = 1 ,  inset = .05,cex = 0.7)


#_____________________________________________________________________________
#
# Feedback
#_____________________________________________________________________________
#
# 	Should you have any questions or suggestions regarding 
# 	this demo code, please contact us via 
# 	http://LOBSTER.wiwi.hu-berlin.de/ 
#
# 	The LOBSTER Team




