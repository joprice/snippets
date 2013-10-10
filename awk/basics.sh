#!/usr/bin/env bash

# specify separator as comma
awk -F, '{print $1}' data 

# multiple fields
awk -F, '{print $1,$2}' data

# multiple patterns
awk '/2002/;/2004/' data

# NF represents field count and can also be used to print last field
awk -F, '{print $NF,NF}' data

# BEGIN and END are run once before and after body
# $0 prints all columns
tail data | awk -F, 'BEGIN {print "State\tType of Crime\tCrime\tYear\tCount";} 
{print $1,"\t",$2,"\t",$3,"\t",$4,"\t",$5;} 
END {print "Complete\n"}' 

# rows with more than 500 crimes
awk -F, '$NF >500' data

# get all rows with years in the 90's
awk -F, '$4 ~/19/' data

awk -F, 'BEGIN {count=0;} $4 ~/19/ {print $NF;count=count+$NF;} END {print "Records from nineties: ",count;}' data
awk -F, 'BEGIN {count=0;total=0;}
$4 ~/19/ {count++; total=total+$NF}
END {print "Records from 90s: ",count, "Crimes from the 90s: ",total;}' data

# print all but the last line - sets the buffer and then prints it in the next 
# iteration, lagging behind by one line
awk 'NR>1{print buf}{buf = $0}'


