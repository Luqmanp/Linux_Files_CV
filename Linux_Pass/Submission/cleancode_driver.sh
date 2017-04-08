awk -f driver.awk occ.psv > occ01.psv
awk -f driver.awk rest.psv > rest01.psv
awk -f vehno.awk veh.psv > veh1.psv
sed -f stripquotes.sed bar.psv > barclean.psv
sed -f stripquotes.sed occ01.psv > occ01clean.psv
sed -f stripquotes.sed rest01.psv > rest01clean.psv
sed -f stripquotes.sed test.psv > testclean.psv
sed -f stripquotes.sed veh1.psv > veh1clean.psv
