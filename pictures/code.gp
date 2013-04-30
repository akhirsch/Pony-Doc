unset key
set title "Interface Size"
unset xlabel
set ylabel "Interface Size (Lines of Code)"
set output 'code.png'
set yrange [0:150]
set boxwidth 0.5
set style fill solid
plot "code.dat" using 1:3:xtic(2) with boxes
