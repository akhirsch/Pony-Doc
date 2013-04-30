unset key
set title "Interface Code"
unset xlabel
set ylabel "Interface Code (Lines of Code)"
set output "interface_code.png"
set yrange [0:7000]
set boxwidth 0.5
set style fill solid
plot "interface_code.dat" using 1:3:xtic(2) with boxes
