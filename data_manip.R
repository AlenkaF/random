######################################
fill_purpose <- function (purpose_number, dat_frame_purposes, var_name)
# Alenka Frim
# Date: 13.04.2011

# Function filling_in_purposes selects the MYIDs that have purpose_number in any of the
# given columns of dat_frame_purposes, where purposes are written according to importance

# input  selected purpose to be rewritten and given purposes written according to importance
# output Yes-1 if the purpose was selected, 2 otherwise
######################################
{
 all_purposes = length(dat_frame_purposes)

 dat <- NA
 dat[dat_frame_purposes[1]==purpose_number] <- 1
 dat[dat_frame_purposes[1]==-1] <- -1
 dat[dat_frame_purposes[1]==-2] <- -2

 if (length(dat_frame_purposes)>1){
  for(i in c (2:all_purposes)) {
	dat[dat_frame_purposes[i]==purpose_number] <- 1
  }
 }

 dat[is.na(dat)] <- 2
 names(dat)   <- var_name
 return(dat)
} # fill_purpose


######################################
fill_ownership <- function (own_number, ownership, dat_frame_own, var_name)
# Alenka Frim
# Date: 13.04.2011

# Function filling_in_ownership selects the MYIDs that have own_numer in any of the
# given columns of dat_frame_own, where ownerships are written from first to last

# input  selected ownership to be rewritten and given ownerships written from first to last
#        variable ownership determins if the household actually owns bonds or not
# output Yes-1 if the ownership was selected, 2 otherwise
######################################
{
 all_ownerships = length(dat_frame_own)

 dat <- NA
 dat[dat_frame_own[1]==own_number & ownership==1] <- 1
 dat[dat_frame_own[1]==-1 & ownership==1] <- -1
 dat[dat_frame_own[1]==-2 & ownership==1] <- -2

 for(i in c(2:all_ownerships)){
	dat[dat_frame_own[i]==own_number & ownership==1] <- 1
 }

 dat[is.na(dat) & ownership==1] <- 2
 names(dat) <- var_name
 return(dat)
} # fill_ownership


######################################
# Function get.amount
# Alenka Frim
# Date: 14.02.2011
# Last change: 05.05.2011

# DESCRIPTION
# Function amount_correction takes a data.frame (part), finds a variable in the data.frame (part) with the name "variable",
# selects all 18 following variables that determins the amount of the variable "variable" (subsection):
	#01 SA0010
	#02 Value index (1 EUR, 2 other currencies, 98, 99) 
	#03 Value in EUR
	#04 Value in other
	#05 Index of lower or upper limit (1 lower in EUR, 2 upper in EUR, 5 in other, 6 in other, 98, 99)
	#06 Index of upper limit (2 upper in EUR, 6 upper in other)
	#07 Value of lower limit in EUR
	#08 Value of upper limit in EUR
	#09 Value of lower limit in other
	#10 Value of upper limit in other
	#11 Group index (from 1 to 20, 98, 99)
	#12 Check no. 11
	#13 Currency (1 SIT, 2 DEM, 3 DIN, 4 CHF, 5 USD, 6 other)
	#14 Currency in text if no. 13 is 6
	#15 Check no. 03 or 04
	#16 Checking lower < upper limit in no. 06-10
	#17 Check no. 06-10
	#18 Check no. 07, 09
	#19 Gross or net amount (1 Gross, 2 Net)
# and returns 6 variables as data frame (dat), which includes
	#1 x (additional variable)
	#2 Value
	#3 Value of lower limit
	#4 Value of upper limit
	#5 Currency
	#6 Gross/Net (only if not all NA) (1 = Gross  2 = Net)
#-------------------------------------------------------------------------------------------------------------------------
get.amount <- function (part, variable)
{
	
	# Defining output object
	dat <- data.frame(part$SA0010)
	names(dat)[names(dat)=="part.SA0010"] <- "SA0010"

	# Selecting subsection of part for further analysis
	all_variables <- colnames(part)
	index <- match(variable,all_variables)
	subn <- c(index:(index+17))
	subsection <- data.frame(part$SA0010,part[,subn])
	names(subsection)[names(subsection)=="part.SA0010"] <- "SA0010"

	# Correction of subsection
	name <- c ("SA0010", "Value", "Lower", "Upper", "Currency", "Gross.Net")

	#--- Value in EUR ---#
	dat_11 <- subset(subsection,subsection[,2]==1 & (subsection[,15]==1 | is.na(subsection[,15])),select=c(1,3))
	dat_12 <- data.frame(rep(NA,nrow(dat_11)),rep(NA,nrow(dat_11)),rep(7,nrow(dat_11)))
	dat_13 <- subset(subsection,subsection[,2]==1 & (subsection[,15]==1 | is.na(subsection[,15])),select=19)
	
	dat_1 <- data.frame(dat_11,dat_12,dat_13)
	names(dat_1) <- name

	#--- Value in other ---#
	dat_21 <- subset(subsection,subsection[,2]==2 & (subsection[,15]==1 | is.na(subsection[,15])),select=c(1,4))
	dat_22 <- data.frame(rep(NA,nrow(dat_21)),rep(NA,nrow(dat_21)))
	dat_23 <- subset(subsection,subsection[,2]==2 & (subsection[,15]==1 | is.na(subsection[,15])),select=c(13,19))
	
	dat_2 <- data.frame(dat_21,dat_22,dat_23)
	names(dat_2) <- name

	#--- Value of lower or/and upper limit in EUR ---#
	dat_31 <- subset(subsection,
			(subsection[,5]==1 | subsection[,5]==2 | subsection[,6]==2) & 
			(subsection[,16]!=1 | is.na(subsection[,16])) &
			(subsection[,17]==1 | is.na(subsection[,17])) &
			(subsection[,18]==1 | is.na(subsection[,18])),
			select=1
	)
	dat_32 <- rep(-1,nrow(dat_31))
	dat_33 <- subset(subsection,
			(subsection[,5]==1 | subsection[,5]==2 | subsection[,6]==2) & 
			(subsection[,16]!=1 | is.na(subsection[,16])) &
			(subsection[,17]==1 | is.na(subsection[,17])) &
			(subsection[,18]==1 | is.na(subsection[,18])),
			select=c(7,8)
	)
	dat_34 <- rep(7,nrow(dat_31))
	dat_35 <- subset(subsection,
			(subsection[,5]==1 | subsection[,5]==2 | subsection[,6]==2) & 
			(subsection[,16]!=1 | is.na(subsection[,16])) &
			(subsection[,17]==1 | is.na(subsection[,17])) &
			(subsection[,18]==1 | is.na(subsection[,18])),
			select=19
	)

	dat_3 <- data.frame(dat_31,dat_32,dat_33,dat_34,dat_35)
	names(dat_3) <- name

	#--- Value of lower or/and upper limit in other ---#
	dat_41 <- subset(subsection,
			(subsection[,5]==5 | subsection[,5]==6 | subsection[,6]==6) & 
			(subsection[,16]!=1 | is.na(subsection[,16])) &
			(subsection[,17]==1 | is.na(subsection[,17])) &
			(subsection[,18]==1 | is.na(subsection[,18])),
			select=1
	)
	dat_42 <- rep(-1,nrow(dat_41))
	dat_43 <- subset(subsection,
			(subsection[,5]==5 | subsection[,5]==6 | subsection[,6]==6) & 
			(subsection[,16]!=1 | is.na(subsection[,16])) &
			(subsection[,17]==1 | is.na(subsection[,17])) &
			(subsection[,18]==1 | is.na(subsection[,18])),
			select=c(9,10,13,19)
	)

	dat_4 <- data.frame(dat_41,dat_42,dat_43)
	names(dat_4) <- name

	#--- Imamo razred ---#
	dat_5 <- as.data.frame(rep(list(num = double(0)), each = 6)) #Defining empty data.frame
	
	groups <- c(1,100,500,1000,2500,5000,7500,10000,25000,50000,75000,100000,250000,500000,1000000,5000000,10000000,25000000,50000000,100000000,NA)
	for (i in c(1:20)){
		dat_51 <- subset(subsection,subsection[,11]==i & (subsection[,12]==1 | is.na(subsection[,12])),select=1)
		dat_52 <- data.frame(rep(-1,nrow(dat_51)),rep(groups[i],nrow(dat_51)),rep(groups[i+1],nrow(dat_51)),rep(7,nrow(dat_51)))
		dat_53 <- subset(subsection,subsection[,11]==i & (subsection[,12]==1 | is.na(subsection[,12])),select=19)

		temp <- data.frame(dat_51,dat_52,dat_53)
		names(temp) <- name

		dat_5 <- rbind(dat_5,temp)
	}

	#--- No answer (98) ---#
	dat_61 <- subset(subsection,subsection[,11]==98 | (subsection[,2]==98&is.na(subsection[,5])),select=1)
	dat_62 <- data.frame(rep(-2,nrow(dat_61)),rep(NA,nrow(dat_61)),rep(NA,nrow(dat_61)),rep(8,nrow(dat_61)),rep(NA,nrow(dat_61)))

	dat_6 <- data.frame(dat_61,dat_62)
	names(dat_6) <- name

	#--- Don't know (99) ---#
	dat_71 <- subset(subsection,subsection[,11]==99| (subsection[,2]==99&is.na(subsection[,5])),select=1)
	dat_72 <- data.frame(rep(-1,nrow(dat_71)),rep(NA,nrow(dat_71)),rep(NA,nrow(dat_71)),rep(8,nrow(dat_71)),rep(NA,nrow(dat_71)))
	
	dat_7 <- data.frame(dat_71,dat_72)
	names(dat_7) <- name

	#--- Errors ---#
	dat_81 <- subset(subsection,subsection[,12]==2 | subsection[,15]==2 | subsection[,16]==1 | subsection[,17]==2 | subsection[,18]==2,select=1)
	dat_82 <- data.frame(rep(-3,nrow(dat_81)),rep(NA,nrow(dat_81)),rep(NA,nrow(dat_81)),rep(8,nrow(dat_81)),rep(NA,nrow(dat_81)))
	
	dat_8 <- data.frame(dat_81,dat_82)
	names(dat_8) <- name

	#--- Empty ---#
	dat_91 <- subset(subsection,is.na(subsection[,2]) & is.na(subsection[,5]) & is.na(subsection[,11]),select=c(1:4))
	dat_92 <- rep(8,nrow(dat_91))
	dat_93 <- subset(subsection,is.na(subsection[,2]) & is.na(subsection[,5]) & is.na(subsection[,11]),select=6)

	dat_9 <- data.frame(dat_91,dat_92,dat_93)
	names(dat_9) <- name

	#--- Zdruzimo vse moznosti ---#
	values = rbind(
		dat_1,
		dat_2,
		dat_3,
		dat_4,
		dat_5,
		dat_6,
		dat_7,
		dat_8,
		dat_9
	)

	dat_copy <- dat
	dat_copy <- merge(dat_copy,values,by="SA0010",all=TRUE,sort=FALSE)
	dat <- merge(dat,dat_copy,by="SA0010",all=TRUE,sort=FALSE)
	
	# Rewriting currencies
	rate <- c ("SIT", "DEM", "DIN", "CHF", "USD", "other", "EUR", "")
	dat$Currency <- as.integer(dat$Currency)
	dat$Currency <- rate [dat$Currency]
	dat$Gross.Net <- as.factor (as.character (dat$Gross.Net))

	# If $Gross.net all NA - delete the variable
	if(all(is.na(dat$Gross.Net))){dat$Gross.Net <- NA}

	# Replacing SA0010 key with additional variable x
	dat$SA0010 <- dat$Value
	dat$SA0010[dat$Currency != "EUR" & dat$Currency!="" & dat$Value>0] <- NA 
	names(dat)[names(dat)=="SA0010"] <- "x"
	#--- Output ---#

	# take care for correct type of result
 	dat$x         <- as.numeric (as.character (dat$x))
 	dat$Value     <- as.numeric (as.character (dat$Value))
 	dat$Lower     <- as.numeric (as.character (dat$Lower))
 	dat$Upper     <- as.numeric (as.character (dat$Upper))
 	dat$Currency  <- as.factor (as.character (dat$Currency))
 	dat$Gross.Net <- as.factor (as.character (dat$Gross.Net))

	return (dat)
}  #  get.amount


# Function interest_rate
# Alenka Frim
# Date: 08.03.2011

# DESCRIPTION
# The function interest_rate gets two variables for input: "variable" as a string and "part" of the questionnaire as data.frame.
# The variable should represent the question for the !current rate of interest! that has to be calculated.
# The function calculates interest rates for the selected variable, if possible. The value loops (get.amount()) are considered!
# Output of the function is a data.frame "dat", where the first variable is SA0010 and the second is the interest rate.

# IMPORTANT
# Position of function in program R_prevedi_dat:
# Function interest_rate is positioned after the process of correcting currencies and gross/net income and before
# deletion of any variables from value loops (get.amount) or any other empty set.

# Two variations of the function as for the different position of variables in different parts of the questionnaire:
# interest_rate_2 for part n.21 and n.22
# interest_rate_3 for part n.3

#-------------------------------------------------------------------------------------------------------------------------

################### part 2 ###################
# Variables in 2011
# part 2.1
## HB1901
## HB1902
# part 2.2
## HB3901
## HB3902

interest_rate_2 <- function (variable, part)
{
 # Defining output object
 dat <- data.frame(part$SA0010)
 names(dat)[names(dat)=="part.SA0010"] = "SA0010"

 # Index of the variable
 all_variables <- colnames(part)
 index <- match(variable, all_variables)
	
 #--- (1) Current interest rate ---#
 dat1 <- subset(part, part[,index]<998 & part[,index]>0, select=c(1,index))

 #--- (2) No current interest, not an adjustable interest rate ---#
 id        <- subset(part, (part[,index-1]!=1|is.na(part[,index-1])) & (part[,index]==-1|part[,index]==-2|is.na(part[,index])) & 
	      (part[,index-15]!=-1 & part[,index-8]!=-1 & part[,index+3]!=-1 & part[,index-15]!=-2 & part[,index-8]!=-2 & part[,index+3]!=-2),
	      select=1)
 m_payment <- subset(part, (part[,index-1]!=1|is.na(part[,index-1])) & (part[,index]==-1|part[,index]==-2|is.na(part[,index])) & 
	      (part[,index-15]!=-1 & part[,index-8]!=-1 & part[,index+3]!=-1 & part[,index-15]!=-2 & part[,index-8]!=-2 & part[,index+3]!=-2), 
	      select=index+3)
 length    <- subset(part, (part[,index-1]!=1|is.na(part[,index-1])) & (part[,index]==-1|part[,index]==-2|is.na(part[,index])) & 
	      (part[,index-15]!=-1 & part[,index-8]!=-1 & part[,index+3]!=-1 & part[,index-15]!=-2 & part[,index-8]!=-2 & part[,index+3]!=-2),
	      select=index-8)
 G0 	   <- subset(part, (part[,index-1]!=1|is.na(part[,index-1])) & (part[,index]==-1|part[,index]==-2|is.na(part[,index])) & 
	      (part[,index-15]!=-1 & part[,index-8]!=-1 & part[,index+3]!=-1 & part[,index-15]!=-2 & part[,index-8]!=-2 & part[,index+3]!=-2),
	      select=index-15)
	
 m_payment <- as.numeric(as.vector(m_payment[,1]))
 length    <- as.numeric(as.vector(length[,1]))
 G0 	   <- as.numeric(as.vector(G0[,1]))
 Gn 	   <- 12*m_payment*length

 dat2	       <-  data.frame(id, 100*((Gn/G0)^(1/(length*12))-1))
 names(dat2)[] <- c("SA0010",variable)
	

 #--- (3) No current interest, an adjustable interest rate ---#
 # (3.1) Seperately
 id     <- subset(part, part[,index-1]==1 & (part[,index]==-1|part[,index]==-2|is.na(part[,index])) & 
	   part[,index+1]!=-1 &  part[,index+1]!=-2 & part[,index+2]!=-1 &  part[,index+2]!=-2 & !is.na(part[,index+1]) & !is.na(part[,index+2]),
	   select=1)
 dat3_1 <- subset(part, part[,index-1]==1 & (part[,index]==-1|part[,index]==-2|is.na(part[,index])) & 
	   part[,index+1]!=-1 &  part[,index+1]!=-2 & part[,index+2]!=-1 &  part[,index+2]!=-2 & !is.na(part[,index+1]) & !is.na(part[,index+2]), 
	   select=index+1)
 dat3_2 <- subset(part, part[,index-1]==1 & (part[,index]==-1|part[,index]==-2|is.na(part[,index])) & 
	   part[,index+1]!=-1 &  part[,index+1]!=-2 & part[,index+2]!=-1 &  part[,index+2]!=-2 & !is.na(part[,index+1]) & !is.na(part[,index+2]), 
	   select=index+2)

 # Remembering variable interest rates for further calculations
 var_interest_rates <- c("3 mesecni EURIBOR", "6 mesecni EURIBOR", "12 mesecni EURIBOR", 
 				"3 mesecni LIBOR", "6 mesecni EURIBOR", "12 mesecni EURIBOR", 
				"EURIBOR, ne vem kateri", "LIBOR, ne vem kateri", "drugo")
 st <- as.numeric(dat3_1)
 # Samo izpise opozorilo
 if(!is.na(st)){print(data.frame(id, var_interest_rates[st],dat3_2))}
	
 dat3 <- data.frame(id, dat3_1)
 names(dat3)[] <- c("SA0010",variable)
	
 # (3.2) Through monthly payment
 id 	   <- subset(part, part[,index-1]==1 & (part[,index]==-1|part[,index]==-2|is.na(part[,index])) & (is.na(part[,index+1])|part[,index+2]==-1|part[,index+2]==-2|is.na(part[,index+2])) & 
	      (part[,index-15]!=-1 & part[,index-8]!=-1 & part[,index+3]!=-1 & part[,index-15]!=-2 & part[,index-8]!=-2 & part[,index+3]!=-2 & 
	      !is.na(part[,index-15]) & !is.na(part[,index-8]) & !is.na(part[,index+3])), 
	      select=1)	
 m_payment <- subset(part, part[,index-1]==1 & (part[,index]==-1|part[,index]==-2|is.na(part[,index])) & (is.na(part[,index+1])|part[,index+2]==-1|part[,index+2]==-2|is.na(part[,index+2])) & 
	      (part[,index-15]!=-1 & part[,index-8]!=-1 & part[,index+3]!=-1 & part[,index-15]!=-2 & part[,index-8]!=-2 & part[,index+3]!=-2 & 
	      !is.na(part[,index-15]) & !is.na(part[,index-8]) & !is.na(part[,index+3])), 
	      select=index+3)
 length    <- subset(part, part[,index-1]==1 & (part[,index]==-1|part[,index]==-2|is.na(part[,index])) & (is.na(part[,index+1])|part[,index+2]==-1|part[,index+2]==-2|is.na(part[,index+2])) & 
	      (part[,index-15]!=-1 & part[,index-8]!=-1 & part[,index+3]!=-1 & part[,index-15]!=-2 & part[,index-8]!=-2 & part[,index+3]!=-2 &  
	      !is.na(part[,index-15]) & !is.na(part[,index-8]) & !is.na(part[,index+3])), 
	      select=index-8)
 G0 	   <- subset(part, part[,index-1]==1 & (part[,index]==-1|part[,index]==-2|is.na(part[,index])) & (is.na(part[,index+1])|part[,index+2]==-1|part[,index+2]==-2|is.na(part[,index+2])) & 
	      (part[,index-15]!=-1 & part[,index-8]!=-1 & part[,index+3]!=-1 & part[,index-15]!=-2 & part[,index-8]!=-2 & part[,index+3]!=-2 &  
	      !is.na(part[,index-15]) & !is.na(part[,index-8]) & !is.na(part[,index+3])), 
	      select=index-15)
	
 m_payment <- as.numeric(as.vector(m_payment[,1]))
 length    <- as.numeric(as.vector(length[,1]))
 G0 	   <- as.numeric(as.vector(G0[,1]))
 Gn 	   <- 12*m_payment*length
	
 dat4 	       <- data.frame(id, 100*((Gn/G0)^(1/(length*12))-1))
 names(dat4)[] <- c("SA0010",variable)	

 #--- (4) No answer ---#
 dat5_1 <- subset(part, part[,index]==-2 & part[,index-1]==1 & ((part[,index+2]==-1|part[,index+2]==-2)|is.na(part[,index+1])|is.na(part[,index+2])) &  
	   (part[,index-15]==-1|part[,index-8]==-1|part[,index+3]==-1|part[,index-15]==-2|part[,index-8]==-2|part[,index+3]==-2|  
	   is.na(part[,index-15])|is.na(part[,index-8])|is.na(part[,index+3])),
	   select=c(1,index))
 dat5_2 <- subset(part, part[,index]==-2 & part[,index-1]!=1 & ((part[,index+2]==-1|part[,index+2]==-2)|is.na(part[,index+1])|is.na(part[,index+2])) &  
	   (part[,index-15]==-1|part[,index-8]==-1|part[,index+3]==-1|part[,index-15]==-2|part[,index-8]==-2|part[,index+3]==-2|
	   is.na(part[,index-15])|is.na(part[,index-8])|is.na(part[,index+3])),
	   select=c(1,index))

 #--- (5) Don't know ---#

 dat6_1 <- subset(part, part[,index]==-1 & part[,index-1]==1 & ((part[,index+2]==-1|part[,index+2]==-2)|is.na(part[,index+1])|is.na(part[,index+2])) &  
	   (part[,index-15]==-1|part[,index-8]==-1|part[,index+3]==-1|part[,index-15]==-2|part[,index-8]==-2|part[,index+3]==-2|  
	   is.na(part[,index-15])|is.na(part[,index-8])|is.na(part[,index+3])),
	   select=c(1,index))
 dat6_2 <- subset(part, part[,index]==-1 & part[,index-1]!=1 & ((part[,index+2]==-1|part[,index+2]==-2)|is.na(part[,index+1])|is.na(part[,index+2])) &  
	   (part[,index-15]==-1|part[,index-8]==-1|part[,index+3]==-1|part[,index-15]==-2|part[,index-8]==-2|part[,index+3]==-2|
	   is.na(part[,index-15])|is.na(part[,index-8])|is.na(part[,index+3])),
	   select=c(1,index))

 # Bind all possibilities
 values_1 <- rbind (dat1, dat2, dat3, dat4, dat5_1, dat5_2, dat6_1, dat6_2)
 values_2 <- merge (dat,values_1, by="SA0010", all=TRUE, sort=FALSE)
 dat <- merge (dat,values_2, by="SA0010", all=TRUE, sort=FALSE)
 names(dat)[] <- c ("SA0010", variable)
 return(dat[,2])
}


################### part 3 ###################
# Variables in 2011
## HC0901
## HC0902

interest_rate_3 <- function (variable, part)
{
 # Defining output object
 dat <- data.frame(part$SA0010)
 names(dat)[names(dat)=="part.SA0010"] <- "SA0010"

 # Index of the variable
 all_variables <- colnames(part)
 index <- match(variable, all_variables)
	
 #--- (1) Current interest rate ---#
 dat1 <- subset(part, part[,index]<998 & part[,index]>0, select=c(1,index))

 #--- (2) No current interest, not an adjustable interest rate ---#
 id 	   <- subset(part, (part[,index-1]!=1|is.na(part[,index-1])) & (part[,index]==-1|part[,index]==-2|is.na(part[,index])) & 
	      (part[,index-14]!=-1 & part[,index-8]!=-1 & part[,index+3]!=-1 & part[,index-14]!=-2 & part[,index-8]!=-2 & part[,index+3]!=-2),
	      select=1)
 m_payment <- subset(part, (part[,index-1]!=1|is.na(part[,index-1])) & (part[,index]==-1|part[,index]==-2|is.na(part[,index])) & 
	      (part[,index-14]!=-1 & part[,index-8]!=-1 & part[,index+3]!=-1 & part[,index-14]!=-2 & part[,index-8]!=-2 & part[,index+3]!=-2),
	      select=index+3)
 length    <- subset(part, (part[,index-1]!=1|is.na(part[,index-1])) & (part[,index]==-1|part[,index]==-2|is.na(part[,index])) & 
	      (part[,index-14]!=-1 & part[,index-8]!=-1 & part[,index+3]!=-1 & part[,index-14]!=-2 & part[,index-8]!=-2 & part[,index+3]!=-2),
	      select=index-8)
 G0 	   <- subset(part, (part[,index-1]!=1|is.na(part[,index-1])) & (part[,index]==-1|part[,index]==-2|is.na(part[,index])) & 
	      (part[,index-14]!=-1 & part[,index-8]!=-1 & part[,index+3]!=-1 & part[,index-14]!=-2 & part[,index-8]!=-2 & part[,index+3]!=-2),
	      select=index-14)

 m_payment <- as.numeric(as.vector(m_payment[,1]))
 length    <- as.numeric(as.vector(length[,1]))
 G0        <- as.numeric(as.vector(G0[,1]))
 Gn 	   <- 12*m_payment*length

 dat2 	       <-  data.frame(id, 100*((Gn/G0)^(1/(length*12))-1))
 names(dat2)[] <- c("SA0010",variable)
	
 #--- (3) No current interest, an adjustable interest rate ---#
 # (3.1) Seperately
 id     <- subset(part, part[,index-1]==1 & (part[,index]==-1|part[,index]==-2|is.na(part[,index])) 
	   & part[,index+1]!=-1 &  part[,index+1]!=-2 & part[,index+2]!=-1 &  part[,index+2]!=-2 &  
	   !is.na(part[,index+1]) & !is.na(part[,index+2]),
	   select=1)
 dat3_1 <- subset(part, part[,index-1]==1 & (part[,index]==-1|part[,index]==-2|is.na(part[,index])) 
	   & part[,index+1]!=-1 &  part[,index+1]!=-2 & part[,index+2]!=-1 &  part[,index+2]!=-2 & 
	   !is.na(part[,index+1]) & !is.na(part[,index+2]), 
	   select=index+1)
 dat3_2 <- subset(part, part[,index-1]==1 & (part[,index]==-1|part[,index]==-2|is.na(part[,index])) 
	   & part[,index+1]!=-1 &  part[,index+1]!=-2 & part[,index+2]!=-1 &  part[,index+2]!=-2 &   
	   !is.na(part[,index+1]) & !is.na(part[,index+2]), 
	   select=index+2)

 # Remembering variable interest rates for further calculations
 var_interest_rates = c("3 mesecni EURIBOR", "6 mesecni EURIBOR", "12 mesecni EURIBOR", 
 				"3 mesecni LIBOR", "6 mesecni EURIBOR", "12 mesecni EURIBOR", 
				"EURIBOR, ne vem kateri", "LIBOR, ne vem kateri", "drugo")
 st <- as.numeric(dat3_1)
 # Samo izpise opozorilo
 if(!is.na(st)){print(data.frame(id, var_interest_rates[st],dat3_2))}

 dat3_1 <- as.numeric(as.vector(dat3_1[,1]))
 dat3_2 <- as.numeric(as.vector(dat3_2[,1]))

 dat3 <- data.frame(id, dat3_1)
 names(dat3)[] <- c("SA0010",variable)

 # (3.2) Through monthly payment
 id 	   <- subset(part, part[,index-1]==1 & (part[,index]==-1 | part[,index]==-2 | is.na(part[,index])) &  
	      (is.na(part[,index+1]) | part[,index+2]==-1 | part[,index+2]==-2 | is.na(part[,index+2])) & 
	      (part[,index-14]!=-1 & part[,index-8]!=-1 & part[,index+3]!=-1 & part[,index-14]!=-2 & part[,index-8]!=-2 & part[,index+3]!=-2 & 
	      !is.na(part[,index-14]) & !is.na(part[,index-8]) & !is.na(part[,index+3])), 
	      select=1)	
 m_payment <- subset(part, part[,index-1]==1 & (part[,index]==-1 | part[,index]==-2 | is.na(part[,index])) &  
	      (is.na(part[,index+1]) | part[,index+2]==-1 | part[,index+2]==-2 | is.na(part[,index+2])) & 
	      (part[,index-14]!=-1 & part[,index-8]!=-1 & part[,index+3]!=-1 & part[,index-14]!=-2 & part[,index-8]!=-2 & part[,index+3]!=-2 & 
	      !is.na(part[,index-14]) & !is.na(part[,index-8]) & !is.na(part[,index+3])),
	      select=index+3)
 length    <- subset(part, part[,index-1]==1 & (part[,index]==-1 | part[,index]==-2 | is.na(part[,index])) &  
	      (is.na(part[,index+1]) | part[,index+2]==-1 | part[,index+2]==-2 | is.na(part[,index+2])) & 
	      (part[,index-14]!=-1 & part[,index-8]!=-1 & part[,index+3]!=-1 & part[,index-14]!=-2 & part[,index-8]!=-2 & part[,index+3]!=-2 & 
	      !is.na(part[,index-14]) & !is.na(part[,index-8]) & !is.na(part[,index+3])), 
	      select=index-8)
 G0 	   <- subset(part, part[,index-1]==1 & (part[,index]==-1 | part[,index]==-2 | is.na(part[,index])) &  
	      (is.na(part[,index+1]) | part[,index+2]==-1 | part[,index+2]==-2 | is.na(part[,index+2])) & 
	      (part[,index-14]!=-1 & part[,index-8]!=-1 & part[,index+3]!=-1 & part[,index-14]!=-2 & part[,index-8]!=-2 & part[,index+3]!=-2 & 
	      !is.na(part[,index-14]) & !is.na(part[,index-8]) & !is.na(part[,index+3])), 
	      select=index-14)
		
 m_payment <- as.numeric(as.vector(m_payment[,1]))
 length    <- as.numeric(as.vector(length[,1]))
 G0 	   <- as.numeric(as.vector(G0[,1]))
 Gn 	   <- 12*m_payment*length

 dat4 <- data.frame (id, 100*((Gn/G0)^(1/(length*12))-1))
 names(dat4)[] <- c ("SA0010", variable)	

 #--- (4) No answer ---#
 dat5_1 <- subset(part, part[,index]==-2 & part[,index-1]==1 & ((part[,index+2]==-1|part[,index+2]==-2)| is.na(part[,index+1])|is.na(part[,index+2])) &  
	   (part[,index-14]==-1 | part[,index-8]==-1 | part[,index+3]==-1 | part[,index-14]==-2 | part[,index-8]==-2 | part[,index+3]==-2 |  
	   is.na(part[,index-14])|is.na(part[,index-8])|is.na(part[,index+3])),
	   select=c(1,index))
 dat5_2 <- subset(part, part[,index]==-2 & part[,index-1]!=1 & ((part[,index+2]==-1|part[,index+2]==-2)| is.na(part[,index+1])|is.na(part[,index+2])) &  
	   (part[,index-14]==-1 | part[,index-8]==-1 | part[,index+3]==-1 | part[,index-14]==-2 | part[,index-8]==-2 | part[,index+3]==-2 |  
	   is.na(part[,index-14])|is.na(part[,index-8])|is.na(part[,index+3])),
	   select=c(1,index))

 #--- (5) Don't know ---#
 dat6_1 <- subset(part, part[,index]==-1 & part[,index-1]==1 & ((part[,index+2]==-1|part[,index+2]==-2)| is.na(part[,index+1])|is.na(part[,index+2])) &  
	   (part[,index-14]==-1 | part[,index-8]==-1 | part[,index+3]==-1 | part[,index-14]==-2 | part[,index-8]==-2 | part[,index+3]==-2 |  
	   is.na(part[,index-14])|is.na(part[,index-8])|is.na(part[,index+3])),
	   select=c(1,index))
 dat6_2 <- subset(part, part[,index]==-1 & part[,index-1]!=1 & ((part[,index+2]==-1|part[,index+2]==-2)| is.na(part[,index+1])|is.na(part[,index+2])) &  
	   (part[,index-14]==-1 | part[,index-8]==-1 | part[,index+3]==-1 | part[,index-14]==-2 | part[,index-8]==-2 | part[,index+3]==-2 |  
	   is.na(part[,index-14])|is.na(part[,index-8])|is.na(part[,index+3])),
	   select=c(1,index))

 # Bind all possibilities
 values_1     <- rbind (dat1, dat2, dat3, dat4, dat5_1, dat5_2, dat6_1, dat6_2)
 values_2     <- merge (dat, values_1, by="SA0010", all=TRUE, sort=FALSE)
 dat          <- merge (dat, values_2, by="SA0010", all=TRUE, sort=FALSE)
 names(dat)[] <- c ("SA0010", variable)
 return (dat[,2])
}
