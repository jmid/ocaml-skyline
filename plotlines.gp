set xrange [-1:20]
set yrange [-1:10]
set tics scale 1

plot "lines.dat" using 1:2:3:4 title "buildings" with vectors nohead lw 4, \
     "skyline.dat" using 1:2:3:4 title "skyline" with vectors nohead lw 2
