awk -f pass.awk occ.psv > occ02.psv
awk -f pass.awk rest.psv > rest02.psv
awk -f vehno.awk veh.psv > veh1.psv
sed -f stripquotes.sed bar.psv > barclean.psv
sed -f stripquotes.sed occ02.psv > occ02clean.psv
sed -f stripquotes.sed rest02.psv > rest02clean.psv
sed -f stripquotes.sed test.psv > testclean.psv
sed -f stripquotes.sed veh1.psv > veh1clean.psv
