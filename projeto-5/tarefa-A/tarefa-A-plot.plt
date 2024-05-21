#!/usr/local/bin/gnuplot -persist
reset
set terminal pngcairo enhanced color size 800,800 font 'Courier Bold,16' #'pcr,bx'

set output './tarefa-A1/A1-L-60-M.png' 
set autoscale; set yrange [-1.1:1.1]; set xrange [0:5000]
set ylabel 'M/N' enhanced; set xlabel 'Iterações de Monte Carlo' enhanced
#set title '{/Symbol b} = 3, L = 60'
unset grid
plot './tarefa-A1/A1-L-60-M.dat' u 1:2 w lp lt -1 notitle

set output './tarefa-A1/A1-L-100-M.png' 
set autoscale; set yrange [-1.1:1.1]; set xrange [0:5000]
set ylabel 'M/N' enhanced; set xlabel 'Iterações de Monte Carlo' enhanced
#set title '{/Symbol b} = 3, L = 100'
unset grid
plot './tarefa-A1/A1-L-100-M.dat' u 1:2 w lp lt -1 notitle

set output './tarefa-A2/A2-L-60-M.png' 
set autoscale; set yrange [-1.1:1.1]; set xrange [0:5000]
set ylabel 'M/N' enhanced; set xlabel 'Iterações de Monte Carlo' enhanced
#set title '{/Symbol b} = 0.1, L = 60'
unset grid
plot './tarefa-A2/A2-L-60-M.dat' u 1:2 w lp lt -1 notitle

set output './tarefa-A2/A2-L-100-M.png' 
set autoscale; set yrange [-1.1:1.1]; set xrange [0:5000]
set ylabel 'M/N' enhanced; set xlabel 'Iterações de Monte Carlo' enhanced
#set title '{/Symbol b} = 0.1, L = 100'
unset grid
plot './tarefa-A2/A2-L-100-M.dat' u 1:2 w lp lt -1 notitle

unset xlabel; unset ylabel
set xlabel 'x'; set ylabel 'y'

unset xtics; unset ytics
set output './tarefa-A1/A1-L-60-final-config.png' 
set autoscale
#set title 'Configuração final, {/Symbol b} = 3, L = 60'
set cbrange [0:1]; set palette defined (0 'black', 1 'white')
set xrange [-0.5:59.5]; set yrange [-0.5:59.5]; set xtics add ('60' 59); set ytics add ('60' 59); set tic scale 0
unset grid; unset colorbox
plot './tarefa-A1/A1-L-60-final-config.out' matrix w image notitle

unset xtics; unset ytics
set output './tarefa-A1/A1-L-100-final-config.png' 
set autoscale
#set title 'Configuração final, {/Symbol b} = 3, L = 100'
set cbrange [0:1]; set palette defined (0 'black', 1 'white')
set xrange [-0.5:99.5]; set yrange [-0.5:99.5]; set xtics add ('100' 99); set ytics add ('100' 99); set tic scale 0
unset grid; unset colorbox
plot './tarefa-A1/A1-L-100-final-config.out' matrix w image notitle

unset xtics; unset ytics
set output './tarefa-A2/A2-L-60-final-config.png' 
set autoscale
#set title 'Configuração final, {/Symbol b} = 0.1, L = 60'
set cbrange [0:1]; set palette defined (0 'black', 1 'white')
set xrange [-0.5:59.5]; set yrange [-0.5:59.5]; set xtics add ('60' 59); set ytics add ('60' 59); set tic scale 0
unset grid; unset colorbox
plot './tarefa-A2/A2-L-60-final-config.out' matrix w image notitle

unset xtics; unset ytics
set output './tarefa-A2/A2-L-100-final-config.png' 
set autoscale
#set title 'Configuração final, {/Symbol b} = 0.1, L = 100'
set cbrange [0:1]; set palette defined (0 'black', 1 'white')
set xrange [-0.5:99.5]; set yrange [-0.5:99.5]; set xtics add ('100' 99); set ytics add ('100' 99); set tic scale 0
unset grid; unset colorbox
plot './tarefa-A2/A2-L-100-final-config.out' matrix w image notitle
