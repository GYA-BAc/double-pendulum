set terminal gif animate delay 10
set output 'output.gif'
stats 'm1.dat' nooutput
stats 'm2.dat' nooutput
set xrange [-10:10]
set yrange [-10:10]

do for [i=1:int(STATS_blocks)] {
	 if (i%100==0) {
		  stats 'm1.dat' index (i-1) u (X1=$1,Y1=$2) nooutput
		  stats 'm2.dat' index (i-1) u (X2=$1,Y2=$2) nooutput
			set object 1 circle at X1, Y1
			set object 2 circle at X2, Y2
	 		plot 'm1.dat' index (i-1) w l t "m1", 'm2.dat' index (i-1) w l t "m2"
	 }
}
