######################################
# Projekt: HFCN
# Alenka Frim
# 09.01.2012
# First raw version created by Matjaž Jeran

# Prepare data for final checks before imputation (h.ECB, r.ECB, p.ECB, s)
# Prepare data for MeDaMi (Tibor Zavadil) program in Stata (HData, PData)

# input:  n_111.RData
# output: 
#  for ECB: h, r, p, s
#  for MeDaMi: HData, PData
######################################	 


rm (list = ls (all = TRUE))

source (file ="RR_setwd.txt")   # set working directory
source (file ="RR_desc_stat.txt") # get descriptive statistics functions

library (foreign)

Adult.Lim <- 18


######################################
#
# FUNCTIONS
#
######################################

first.last.call <- function (b,c,d)
# Opis
{
 b[!is.na(d)] <- d[!is.na(d)]
 b[!is.na(c) & is.na(d)] <- c[!is.na(c)&is.na(d)]
 return (b)
}   # first.last.call


first.last.call.8 <- function (b,c,d,b.8,c.8,d.8)
# Opis
{
 b.8[!is.na(d)] <- d.8[!is.na(d)]
 b.8[!is.na(c) & is.na(d)] <- c.8[!is.na(c)&is.na(d)]
 return (b.8)
}   # first.last.call


round.data.frame <- function(x, digits) 
{ 
    x_round <- data.frame(lapply(x, function(y) if(is.numeric(y)) round(y, digits) else y)) 
    return(x_round)
}   # round.data.frame


######################################
#
# DATA PREPARATION
#
######################################

# read sample data
# n.1, n.21, n.22, n.23, n.3, n.41, n.42, n.5, n.6, n.71, n.72, n.8, n.9, ostalo, s.file

load (file = "n_111.RData")  


######################################
#
# PROGRAM
#
######################################
# Additinal manual removal of one household
# Merging files to H, P, S and R-file
# Merging files to HData and PData for MeDaMi
# Rounding numerical values, flagging


######################################
# Merging files to H, P, S and R-file

# -----------------------------
# Sample register file (S-File)
# -----------------------------

# Povozi star S-file z novejšim
s.file <- read.table(file = "S-file_koncen_petra.txt", header = TRUE, sep = "\t", quote = "\"", dec = ",")
names(s.file)[names(s.file)=="SCO100"] <- "SC0100"

s <- s.file
s$VZOREC <- NULL
s$Število.članov.16. <- NULL
s$utež.po.št..članov.16. <- NULL
s$končna.utež.po.št..članov.16. <- NULL
s$utež.glede.na.osn..rez..Vzorec.1 <- NULL
s$utež.glede.na.osn..rez..Vzorec.2 <- NULL
s$ustreznost <- NULL
s$neodgovori <- NULL

# Corresting first/last call from 4 to 2 calls for ECB
# s$SB0102 <- first.last.call(s$SB0102,s$SB0103,s$SB0104)
# s$SB0202 <- first.last.call(s$SB0202,s$SB0203,s$SB0204)
# s$SB0302 <- first.last.call(s$SB0302,s$SB0303,s$SB0304)
# s$SB0402 <- first.last.call(s$SB0402,s$SB0403,s$SB0404)
# s$SB0502 <- first.last.call(s$SB0502,s$SB0503,s$SB0504)
# s$SB0602 <- first.last.call(s$SB0602,s$SB0603,s$SB0604)
# s$SB0802 <- first.last.call.8(s$SB0702,s$SB0703,s$SB0704,s$SB0802,s$SB0803,s$SB0804)# Filter! 
# s$SB0702 <- first.last.call(s$SB0702,s$SB0703,s$SB0704)

# s$SB0103 <- NULL; s$SB0104 <- NULL
# s$SB0203 <- NULL; s$SB0204 <- NULL
# s$SB0303 <- NULL; s$SB0304 <- NULL
# s$SB0403 <- NULL; s$SB0404 <- NULL
# s$SB0503 <- NULL; s$SB0504 <- NULL
# s$SB0603 <- NULL; s$SB0604 <- NULL
# s$SB0703 <- NULL; s$SB0704 <- NULL
# s$SB0803 <- NULL; s$SB0804 <- NULL

# Identifiers for merge with part h, p and r
s.v <- s[s$SE0100==1,] # opazovan vzorec
s1  <- s.v[1:2]
s2  <- data.frame(s.v[1:2],IM0100 = 1)


# ---------------------------------
# Household File (H-File): H1 nd H2
# ---------------------------------

other_h <- ostalo[1:49]

# H1 sections 2 and 3 of the questionaire
h1 <- merge (x = merge (x = merge (x = n.21, y = n.22, by = "SA0010"), y = n.23, by = "SA0010"), y = n.3, by = "SA0010")
# H2 sections 4, 5, 7, 8 and 9 of the questionaire
h2 <- merge (x = merge (x = merge (x = merge (x = merge (x = n.41, y = n.42, by = "SA0010"), 
  y = n.72, by = "SA0010"), y = n.8, by = "SA0010"), y =  n.9, by = "SA0010"), y = other_h, by = "SA0010")

s3 <- data.frame(s.v[1:2], s.v[37], IM0100 = 1)
h  <- merge( x = s3 , y = merge (x = h1, y = h2, by = "SA0010"), by = "SA0010") #344 enot


# -------------------------------
# Personal register file (R-File)
# -------------------------------

r <- merge(x = s2, y = n.1, by = "SA0010") #965 enot
r$PA0100 <- NULL
r$PA0200 <- NULL


# ------------------------------------
# Personal questionnaire file (P-File)
# ------------------------------------

tmp <- subset (x = n.1, select = c ("SA0010", "RA0010", "PA0100", "PA0200"))
p   <- merge( x = s2, y = merge( x = tmp, y = merge (x = n.5, y = merge (x = n.6, y = n.71, by = c("SA0010", "RA0010"))))) #851 enot, 16+


# -----------------------------------
# The Replicate Weights file (W-File) 
# -----------------------------------


######################################
# Round necessary fields
# Some data with definition of 0 decimal places have decimal places - they have to be rounded up, 
# Variables with the possibility of decimal places are turned into characters, so they stay as they are

h$HB0500 <- as.character(h$HB0500); h$HB1901 <- as.character(h$HB1901); h$HB1902 <- as.character(h$HB1902)
h$HB2701 <- as.character(h$HB2701); h$HB2702 <- as.character(h$HB2702); h$HB3901 <- as.character(h$HB3901); h$HB3902 <- as.character(h$HB3902)
h$HC0901 <- as.character(h$HC0901); h$HC0902 <- as.character(h$HC0902); h$HD0701 <- as.character(h$HD0701); h$HD0702 <- as.character(h$HD0702)
p$PE0600 <- as.character(p$PE0600); p$PE0700 <- as.character(p$PE0700); p$PF0200 <- as.character(p$PF0200); p$PF0400 <- as.character(p$PF0400)

h <- round.data.frame(h,0)
r <- round.data.frame(r,0)
p <- round.data.frame(p,0)

h$HB0500 <- as.numeric(h$HB0500); h$HB1901 <- as.numeric(h$HB1901); h$HB1902 <- as.numeric(h$HB1902)
h$HB2701 <- as.numeric(h$HB2701); h$HB2702 <- as.numeric(h$HB2702); h$HB3901 <- as.numeric(h$HB3901); h$HB3902 <- as.numeric(h$HB3902)
h$HC0901 <- as.numeric(h$HC0901); h$HC0902 <- as.numeric(h$HC0902); h$HD0701 <- as.numeric(h$HD0701); h$HD0702 <- as.numeric(h$HD0702)
p$PE0600 <- as.numeric(p$PE0600); p$PE0700 <- as.numeric(p$PE0700); p$PF0200 <- as.numeric(p$PF0200); p$PF0400 <- as.numeric(p$PF0400)

h$HC0901 <- round(h$HC0901,2)
h$HC0902 <- round(h$HC0902,2)  # Data for HC0901/2 had 3 decimal places, instead of defined 2



######################################
# Manual removal of houshold with id 1430 due to high non-response

h <- subset(h, h$SA0010!=1430)
r <- subset(r, r$SA0010!=1430)
p <- subset(p, p$SA0010!=1430)

s$SB0100[s$SA0010==1430] <- 80
s$SE0100[s$SA0010==1430] <- 2
s.v <- s[s$SE0100==1,] # opazovan vzorec

#V tej verziji že naložimo S_file_koncen_petra.txt
#s.tmp <- read.table (file = "S-file_koncen_petra.txt", header = TRUE, sep = "\t", quote = "\"", dec = ",")
#s.utezi <- data.frame (
# SA0010     = s.tmp$SA0010,
# SD0300_V2  = s.tmp$SD0300_V2,
# SD0300_V24 = s.tmp$SD0300_V24)

#s.utezi <- merge ( x = s.v, y = s.utezi, all.y=FALSE)




######################################
# Flagging h, p, r, s

# -----------------------------------------------
# The Sample Register Status Flags File (FS-File) 
# -----------------------------------------------

fs <- s
fs[3:length(fs)] <- 1
names(fs)[3:length(fs)] <- paste("F",names(fs)[3:length(fs)],sep="")
fs[is.na(s)] <- 0
fs[s==-1] <- 1050
fs[s==-2] <- 1051

# Additional flagging of households (2051)

fs$FSA0110 <- 2051
fs$FSA0210 <- 2051


fs.v <- s.v
fs.v[3:length(fs.v)] <- 1
names(fs.v)[3:length(fs.v)] <- paste("F",names(fs.v)[3:length(fs.v)],sep="")
fs.v[is.na(s.v)] <- 0
fs.v[s.v==-1] <- 1050
fs.v[s.v==-2] <- 1051

# Additional flagging of households (2051)

fs.v$FSA0110 <- 2051
fs.v$FSA0210 <- 2051

# -----------------------------------------
# The Household Status Flags file (FH-File) 
# -----------------------------------------

fh <- h
fh[4:length(fh)] <- 1
names(fh)[4:length(fh)] <- paste("F",names(fh)[4:length(fh)],sep="")
fh[is.na(h)] <- 0
fh[h==-1] <- 1050
fh[h==-2] <- 1051

load("Flag1053.RData")
for(i in flag1053[2:length(flag1053[,1]),]){ if(i %in% names(h)){
	eval (parse (text = paste ("fh$F", i, "[!is.na(h$", i, ".Lower) | !is.na(h$", i, ".Upper)] <- 1053", sep = ""))) }}
rm(i)

# Additional flagging of third-loop data (2051)

fh[grep("[0-9][0-9]3$",names(fh))] <- 2051 ##Two no. infront for ommiting Comm.3 and Comm.23
fh[grep("3[a-z]$",names(h))]       <- 2051
fh[grep("3[a-z][a-z]$",names(h))]  <- 2051

# Additional flagging of calculations (5050)

load(file = "Flag5050_H.RData") #flag5050.h
for (i in c(2:length(flag5050.h[1,]))){ 
	id <- flag5050.h$SA0010[flag5050.h[,i]==1 & !is.na(flag5050.h[,i])]
	for (j in id) { 
		if (h[h$SA0010==j, names(h)==names(flag5050.h[i])]>0 & !is.na(h[h$SA0010==j, names(h)==names(flag5050.h[i])]))
			{fh[fh$SA0010==j, names(fh)==paste("F",names(flag5050.h)[i],sep="")] <- 5050 }}}
rm (i)

# -------------------------------------------------
# The Personal Register Status Flags file (FR-File)
# -------------------------------------------------

fr <- r
fr[5:length(fr)] <- 1
names(fr)[5:length(fr)] <- paste("F",names(fr)[5:length(fr)],sep="")
fr[is.na(r)] <- 0
fr[r==-1] <- 1050
fr[r==-2] <- 1051

load("Flag1053.RData")
for(i in flag1053[2:length(flag1053[,1]),]){ if(i %in% names(r)){
	eval (parse (text = paste ("fr$F", i, "[!is.na(r$", i, ".Lower) | !is.na(r$", i, ".Upper)] <- 1053", sep = ""))) }}
rm(i)

# Additional flagging (2051)
fr$FRA0020 <- 2051
fr$FRA0030 <- 2051
fr$FRA0040 <- 2051

# ------------------------------------------------------
# The Personal Questionnaire Status Flags file (FP-File)
# ------------------------------------------------------
 
fp <- p
fp[5:length(fp)] <- 1
names(fp)[5:length(fp)] <- paste("F",names(fp)[5:length(fp)],sep="")
fp[is.na(p)] <- 0
fp[p==-1] <- 1050
fp[p==-2] <- 1051

load("Flag1053.RData")
for(i in flag1053[2:length(flag1053[,1]),]){ if(i %in% names(p)){
	eval (parse (text = paste ("fp$F", i, "[!is.na(p$", i, ".Lower) | !is.na(p$", i, ".Upper)] <- 1053", sep = ""))) }}
rm(i)

# Additional flagging of ids (2051)
fp$FPF0500 <- 2051
fp$FPF0510 <- 2051

# ------------------------
# Flagging from Excel file
# ------------------------

# prevent data.frames to have factors and to asure correct handling of NA's
options (stringsAsFactors = FALSE)

# Corrections
tmp <- read.table (file = "Zastavice.txt", header = TRUE, sep = "\t", quote = "\"", dec = ".")
flag1052 <- data.frame (
 var    = tmp$Spremenljivka,
 chap   = tmp$PoglavjeECB,
 SA0010 = tmp$MYID,
 RA0010 = tmp$RA0010,
 flag   = tmp$Flag)

# adjust empty fields where approriate
flag1052$RA0010 [flag1052$RA0010 == ""] <- NA

# separate records for persons (2 key fields) and households (only 1 key field)

pers <- subset (x = flag1052, subset = ! is.na (RA0010))
other <- subset (x = flag1052, subset = is.na (RA0010))

# apply all corrections to data
if (dim (pers) [1] > 0) {
 eval (parse (text = paste ("f", pers$chap, "$F", pers$var, " [", pers$chap, "$SA0010 == ", pers$SA0010, " & ", pers$chap, "$RA0010 == ",    pers$RA0010, "] <- ", pers$flag, sep = "")))
}  # if

# apply all corrections to data
if (dim (other) [1] > 0) {
 eval (parse (text = paste ("f", other$chap, "$F", other$var, " [", other$chap, "$SA0010 == ", other$SA0010, "] <- ", other$flag, sep = "")))
}  # if


#########################################
# Writing data

ext <- ".txt"
# ext <- ".csv"

# -------------------------------
# Write core variables for MeDaMi
# -------------------------------
# text files
# HData <- h + s
# PData <- p + r

HData <- merge (x = h, y = s.v, all.y=TRUE) # h 343 vrstic, samo opazovan vzorec
PData <- merge (x = r, y = p, all.x=FALSE) # 850 vrstic, zanemarimo clane mlajše od 16let v dat r

HData[grep("Comm.", names(HData))]  <- list(NULL)
PData[grep("Comm.", names(PData))]  <- list(NULL)

# -------------------------------
# Flags
# FHData <- fh + fs
# FPData <- fp + fr

FHData <- merge (x = fh, y = fs.v, all.y=TRUE)
FPData <- merge (x = fr, y = fp, all.x=FALSE)

FHData[grep("FComm.", names(FHData))]  <- list(NULL)
FPData[grep("FComm.", names(FPData))]  <- list(NULL)

# Merge data.frames with flags - for stata use
HData <- merge (x = HData, y = FHData, by = grep("^[^F]",names(FHData)))
PData <- merge (x = PData, y = FPData, by = grep("^[^F]",names(FPData)))

RData <- merge (x = r, y = fr, all.y=TRUE)
SData <- merge (x = s, y = fs, all.y=TRUE)

# Correct names
# ".Lower" -> "_lb"
# ".Upper" -> "_ub"

tmp.pl <- sapply(strsplit(names(PData)[grep(".Lower$", names(PData))],'[.]'), '[[', 1) #Selecting the names of var ending with ".Lower"
names(PData)[grep(".Lower$", names(PData))] <- paste(tmp.pl, "_lb", sep="")
tmp.pu <- sapply(strsplit(names(PData)[grep(".Upper$", names(PData))],'[.]'), '[[', 1) #Selecting the names of var ending with ".Upper"
names(PData)[grep(".Upper$", names(PData))] <- paste(tmp.pu, "_ub", sep="")

tmp.hl <- sapply(strsplit(names(HData)[grep(".Lower$", names(HData))],'[.]'), '[[', 1) #Selecting the names of var ending with ".Lower"
names(HData)[grep(".Lower$", names(HData))] <- paste(tmp.hl, "_lb", sep="")
tmp.hu <- sapply(strsplit(names(HData)[grep(".Upper$", names(HData))],'[.]'), '[[', 1) #Selecting the names of var ending with ".Upper"
names(HData)[grep(".Upper$", names(HData))] <- paste(tmp.hu, "_ub", sep="")

write.table (x = HData, file = paste ("HData_regije", ext, sep = ""), 
  append = FALSE, quote = TRUE, sep = ";", eol = "\n", na = ".", dec = ".", row.names = FALSE, col.names = TRUE)
write.table (x = PData, file = paste ("PData", ext, sep = ""),
  append = FALSE, quote = TRUE, sep = ";", eol = "\n", na = ".", dec = ".", row.names = FALSE, col.names = TRUE)

write.table (x = RData, file = paste ("RData", ext, sep = ""),
  append = FALSE, quote = TRUE, sep = ";", eol = "\n", na = ".", dec = ".", row.names = FALSE, col.names = TRUE)
write.table (x = SData, file = paste ("SData", ext, sep = ""),
  append = FALSE, quote = TRUE, sep = ";", eol = "\n", na = ".", dec = ".", row.names = FALSE, col.names = TRUE)
