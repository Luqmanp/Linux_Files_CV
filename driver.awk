# extract subset of rows where VEHNO = 1 and OCCLOC = 01 (driver)
BEGIN { FS = "|" } 
NR == 1 || ($2 == "1" && $3 == "01") {print}


