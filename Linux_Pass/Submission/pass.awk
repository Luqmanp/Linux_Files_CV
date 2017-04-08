# extract subset of rows where VEHNO = 1 and OCCLOC = 02 (passenger)
BEGIN { FS = "|" } 
NR == 1 || ($2 == "1" && $3 == "02") {print}


