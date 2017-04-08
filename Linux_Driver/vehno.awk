# extract subset of rows where VEHNO = 1
BEGIN { FS = "|" } 
NR == 1 || $2 == "1" {print}


