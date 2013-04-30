unset key
set title "System C Code"
unset xlabel
set ylabel "System C Code (Lines of Code)"
set output "error_code.png"
set yrange [0:21000]
set boxwidth 0.5
set style fill solid
plot "error_code.dat" using 1:3:xtic(2) with boxes
