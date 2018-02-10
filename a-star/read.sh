# Create node file
awk -F "|" '($1 == "node"){print $0}' spain.csv | cut -f2,3,10,11 -d "|" > spa-nodes.csv

# Crate ways file
awk -F "|" '($1 == "way"){print $0}' spain.csv | cut -f8,10- -d "|" > spa-ways.csv



