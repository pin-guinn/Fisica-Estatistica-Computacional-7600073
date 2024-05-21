# Generate Unknown Pleasures Cover
#set term png large truecolor size 720,1080 enhanced

set term gif animate delay 1 size 640,480 enhanced font "verdana,10"
set output "teste.gif"

set xrange [0:1]
set yrange [-1:1]

do for [i=0:200]{
plot 'data.out' index i u 1:2 w l lc 'blue' lw 2 notitle
}
