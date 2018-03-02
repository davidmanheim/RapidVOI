###########################
# bookshelf
###########################

##############################################
# example https://stackoverflow.com/questions/14591167/variable-width-bar-plot
# (also availavel in ggplot)

widths = c(0.5, 0.5, 1/3,1/4,1/5, 3.5, 0.5)
heights = c(25, 10, 5,4.5,4,2,0.5)

# Then we use the standard barplot command, but specify the space between blocks to be zero:
  
  ##Also specify colours
  barplot(heights, widths, space=0, 
          col = colours()[1:6])

#Since we specified widths, we need to specify the axis labels:
  
  axis(1, 0:6)

#To add grid lines, use the grid function:
  
  ##Look at ?grid to for more control over the grid lines
  grid()

#and you can add arrows and text manually:
  
  arrows(1, 10, 1.2, 12, code=1)
text(1.2, 13, "A country") 

#To add your square in the top right hand corner, use the polygon function:
  
  polygon(c(4,4,5,5), c(20, 25, 25, 20), col="antiquewhite1")
text(4.3, 22.5, "Hi there", cex=0.6)

# if required
#par(mar=c(3,3,2,1), 
#    mgp=c(2,0.4,0), tck=-.01,
#    cex.axis=0.9, las=1)



# END EXAMPLE
#######################################################################
########################################################################



# need to be in order of lowest to highest ICER
# p5, p2, p4, p1        p3, p6
# widths are NETSCC costs
P1_cost <- 601481
P2_cost <- 3310883
P3_cost <- 2522710
P4_cost <- 882177
P5_cost <- 2854000
P6_cost <- 855403

widths = c(P5_cost,P2_cost , P4_cost,P1_cost,P6_cost, P3_cost)

# QALYs from project
P1_QALYs <- 11.1
P2_QALYs <- 772
P3_QALYs <- -3511
P4_QALYs <- 52 
P5_QALYs <- 50770
P6_QALYs <- -660
#Pdud <- 0 # introduce to make space on the graph

#heights = c(P5_QALYs,P2_QALYs , P4_QALYs,P1_QALYs,P6_QALYs, P3_QALYs)

# QALYs per cost
NETSCCspend <- 15000

P1_payoff <- P1_QALYs*NETSCCspend/P1_cost
P2_payoff <- P2_QALYs*NETSCCspend/P2_cost
P3_payoff <- P3_QALYs*NETSCCspend/P3_cost
P4_payoff <- P4_QALYs*NETSCCspend/P4_cost
P5_payoff <- P5_QALYs*NETSCCspend/P5_cost # 30 # real payoff is P5_QALYs*NETSCCspend/P5_cost
P6_payoff <- P6_QALYs*NETSCCspend/P6_cost

# realy payoffs of each proposal in order (including from p5 correct)
P1_payoff 
P2_payoff 
P3_payoff 
P4_payoff 
P5_payoff
P6_payoff 

heights = c(P5_payoff,P2_payoff , P4_payoff,P1_payoff,P6_payoff, P3_payoff)


# Then we use the standard barplot command, but specify the space between blocks to be zero:
options(scipen=999) # trun off scientifc notation 

# reset par
#dev.off()
# adjust margins 


##Also specify colours
# suppress axis and add later
barplot(heights, widths, space=0, 
        col = colours()[1:6], ylim = c(-50, 250), axes = FALSE)


#Since we specified widths, we need to specify the axis labels:
# axis labels
# 
axis(1, at=seq(from =0, to = 12000000, by = 1000000 ), labels=sprintf("£%sM", seq(from =0, to = 12, by = 1 )))
axis(2, las = 1)

title(main="")
title(xlab="NETSCC expenditure (£ million)", ylab="QALYs per £15,000 NETSCC")


#To add grid lines, use the grid function:

##Look at ?grid to for more control over the grid lines
#grid()

#and you can add arrows and text manually:

text(1500000, 35, "P5 (£2.8M)", srt=45) 
text(4500000, 35, "P2 (£3.3M)", srt=45) 
text(7200000, 35, "P4 (£0.9M)", srt=45) 
text(7850000, 35, "P1 (£0.6M)", srt=45) 
text(8600000, 35, "P6 (£0.9M)", srt=45) 
text(10000000, 35, "P3 (£2.5M)", srt=45) 

# add budget line
location <- P5_cost + 150000
lines(c(location,location), c(-50, 150) , col= "firebrick", lwd = 2)
text(5900000, 125, "NETSCC budget: £3M", col= "firebrick") 





##############################################################
###### zoom in ############################################


dev.off()
heights = c(10,P2_payoff , P4_payoff,P1_payoff,P6_payoff, P3_payoff)


# Then we use the standard barplot command, but specify the space between blocks to be zero:
options(scipen=999) # trun off scientifc notation 

# reset par
#dev.off()
# adjust margins 

#par(mar=c(6,5,2,2)) # par(mar=c(bottom,left,top,right))

##Also specify colours
# suppress axis and add later
barplot(heights, widths, space=0, 
        col = colours()[1:6], ylim = c(-22, 9), axes = FALSE)


text(1500000, 8, "267 Q/£15K", cex=0.9)
points(200000,8, pch = 8 )

#Since we specified widths, we need to specify the axis labels:
# axis labels
# 
axis(1, at=seq(from =0, to = 12000000, by = 1000000 ), labels=sprintf("£%sM", seq(from =0, to = 12, by = 1 )))
axis(2, at=seq(from= -24, to= 20, by=2 ), las = 1)

title(main="", line = 0.5)
title(xlab="NETSCC expenditure (£ million)", ylab="QALYs per £15,000 NETSCC")


#To add grid lines, use the grid function:

##Look at ?grid to for more control over the grid lines
#grid()

#and you can add arrows and text manually:

text(1500000, 3.8, "P5 (£2.8M)", srt=45) 
text(4000000, 6.3, "P2 (£3.3M)", srt=45) 
text(7200000, 3.8, "P4 (£0.9M)", srt=45) 
text(7890000, 3.8, "P1 (£0.6M)", srt=45) 
text(8600000, 3.8, "P6 (£0.9M)", srt=45) 
text(10000000, 3.8, "P3 (£2.5M)", srt=45) 


# add budget line
location <- P5_cost+P2_cost + 100000
lines(c(location,location), c(-1.5, 5) , col= "firebrick", lwd = 2)
text(4200000, -2.5, "NETSCC budget: £6.3M", col= "firebrick") 



########## end zoom in





##############################################################
###### zoom in DELAY  ############################################


dev.off()
heights = c(10,P2_payoff , P4_payoff,P1_payoff,P6_payoff, P3_payoff)
widths = c(30000,P2_cost , P4_cost,P1_cost,P6_cost, P3_cost)

# payoff from delay
15000*(4333/30000)


# Then we use the standard barplot command, but specify the space between blocks to be zero:
options(scipen=999) # trun off scientifc notation 

# reset par
#dev.off()
# adjust margins 

#par(mar=c(6,5,2,2)) # par(mar=c(bottom,left,top,right))

##Also specify colours
# suppress axis and add later
barplot(heights, widths, space=0, 
        col = colours()[1:6], ylim = c(-22, 9), axes = FALSE)


text(800000, 6, "P5 Delay") 
text(1200000, 8, "2167 Q/£15K", cex=0.9)
points(200000,8, pch = 8 )

#Since we specified widths, we need to specify the axis labels:
# axis labels
# 
axis(1, at=seq(from =0, to = 12000000, by = 1000000 ), labels=sprintf("£%sM", seq(from =0, to = 12, by = 1 )))
axis(2, at=seq(from= -24, to= 20, by=2 ), las = 1)

title(main="", line = 0.5)
title(xlab="NETSCC expenditure (£ million)", ylab="QALYs per £15,000 NETSCC")


#To add grid lines, use the grid function:

##Look at ?grid to for more control over the grid lines
#grid()

#and you can add arrows and text manually:

#text(1500000, 3.8, "P5 (£2.8M)", srt=45) 
text(2500000, 4.5, "P2") 
text(3600000, 2, "P4") 
text(4500000, 2, "P1") 
text(5000000, 2, "P6") 
text(7000000, 2, "P3") 


# add budget line
location <- 4000000
lines(c(location,location), c(-1.5, 5) , col= "firebrick", lwd = 2)
text(2500000, -2.5, "NETSCC budget: £4M", col= "firebrick") 



########## end zoom in DELAY




###### zoom in £5.3 million ############################################


dev.off()
heights = c(10,P2_payoff , P4_payoff,P1_payoff,P6_payoff, P3_payoff)
widths = c(P5_cost,P2_cost , P4_cost,P1_cost,P6_cost, P3_cost)


# Then we use the standard barplot command, but specify the space between blocks to be zero:
options(scipen=999) # trun off scientifc notation 

# reset par
#dev.off()
# adjust margins 

#par(mar=c(6,5,2,2)) # par(mar=c(bottom,left,top,right))

##Also specify colours
# suppress axis and add later
barplot(heights, widths, space=0, 
        col = colours()[1:6], ylim = c(-22, 10), axes = FALSE)


text(1500000, 8, "267 Q/£15K", cex=0.9)
points(200000,8, pch = 8 )

#Since we specified widths, we need to specify the axis labels:
# axis labels
# 
axis(1, at=seq(from =0, to = 12000000, by = 1000000 ), labels=sprintf("£%sM", seq(from =0, to = 12, by = 1 )))
axis(2, at=seq(from= -24, to= 20, by=2 ), las = 1)

title(main="", line = 0.5)
title(xlab="NETSCC expenditure (£ million)", ylab="QALYs per £15,000 NETSCC")


#To add grid lines, use the grid function:

##Look at ?grid to for more control over the grid lines
#grid()

#and you can add arrows and text manually:

text(1500000, 3.8, "P5 (£2.8M)", srt=45) 
text(4000000, 6.7, "P2 (£3.3M)", srt=45) 
text(7200000, 4.5, "P4 (£0.9M)", srt=45) 
text(7890000, 4.5, "P1 (£0.6M)", srt=45) 
text(8600000, 4.5, "P6 (£0.9M)", srt=45) 
text(10000000, 4.5, "P3 (£2.5M)", srt=45) 


# add budget line
location <- 5300000
lines(c(location,location), c(-1.5, 5) , col= "firebrick", lwd = 2)
text(4200000, -2.5, "NETSCC budget: £5.3M", col= "firebrick") 



########## end zoom in £5.3M













#abline(v=5500000) # add budget line

arrows(1, 10, 1.2, 12, code=1)



#To add your square in the top right hand corner, use the polygon function:

polygon(c(4,4,5,5), c(20, 25, 25, 20), col="antiquewhite1")
text(4.3, 22.5, "Hi there", cex=0.6)

# if required
#par(mar=c(3,3,2,1), 
#    mgp=c(2,0.4,0), tck=-.01,
#    cex.axis=0.9, las=1)










############# gap plot ###########################
# example https://stackoverflow.com/questions/23884991/how-to-add-break-in-axis-for-grouped-barplot


library(plotrix)
x1=c(3,5,6,9,375,190);
x1
x2=c(2,2,3,30,46,60);
x2
data=rbind(x1,x2);
data
colnames(data)=c("Pig","Layer","Broiler","Dairy","Beef","Sheep")
rownames(data)=c("1980","2010")

newdata <-data
newdata[newdata>200]<-newdata[newdata>200]-140
barpos<-barplot(newdata,names.arg=colnames(newdata),
                ylim=c(0,250),beside=TRUE,col=c("darkblue","red"),axes=FALSE)
axis(2,at=c(0,50,100,150,200,235),
     labels=c(0,50,100,150,200,375))
box()
axis.break(2,210,style="gap")



heights = c(P5_payoff,P2_payoff , P4_payoff,P1_payoff,P6_payoff, P3_payoff)
# reset par
#dev.off()
# adjust margins 

barplot(heights, widths, space=0, 
        col = colours()[1:6], ylim = c(-50, 250), axes = FALSE)


axis(1, at=seq(from =0, to = 12000000, by = 1000000 ), labels=sprintf("£%sM", seq(from =0, to = 12, by = 1 )))
axis(2, las = 1)






##########################################################
# remove proposal 5
##############################################

# need to be in order of highest to lowest
# p5, p2, p4, p1        p3, p6
# widths are NETSCC costs
P1_cost <- 601481
P2_cost <- 3310883
P3_cost <- 2522710
P4_cost <- 882177
#P5_cost <- 2854000
P6_cost <- 855403

widths = c(P4_cost,P2_cost , P1_cost,P3_cost, P6_cost)

# heights are QALYs from project
P1_QALYs <- 5.12
P2_QALYs <- 772
P3_QALYs <- 9
P4_QALYs <- 999 
#P5_QALYs <- 50770
P6_QALYs <- 0.9

heights = c( P4_QALYs,P2_QALYs ,P1_QALYs,P3_QALYs, P6_QALYs)

# Then we use the standard barplot command, but specify the space between blocks to be zero:

##Also specify colours
barplot(heights, widths, space=0, 
        col = colours()[1:5])










