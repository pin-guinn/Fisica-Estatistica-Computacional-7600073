#!/usr/local/bin/gnuplot -persist
reset
set terminal pngcairo enhanced color size 800, 800 font "Courier Bold,16"

set output './tarefa-B1/B1-Annealing-L-60-Energy.png' 
set autoscale; set yrange [-2.2:*]; set xrange [0:2500]
set ylabel 'E/N'; ;set xlabel 'Iterações de Monte Carlo'
unset grid
plot './tarefa-B1/B1-Annealing-L-60-Energy.dat' u 1:2 w lp lt -1 notitle

set output './tarefa-B2/B2-Quenching-L-60-Energy.png' 
set autoscale; set yrange [-2.2:*]; set xrange [0:2500]
set ylabel 'E/N'; ;set xlabel 'Iterações de Monte Carlo'
unset grid
plot './tarefa-B2/B2-Quenching-L-60-Energy.dat' u 1:2 w lp lt -1 notitle

unset xtics; unset ytics
set xlabel 'x'; set ylabel 'y'
set output './tarefa-B1/B1-Annealing-L-60-Final-Config.png' 
set autoscale
#set title 'Recozimento, configuração final, L = 60'
set cbrange [0:1]; set palette defined (0 'black', 1 'white')
set xrange [-0.5:59.5]; set yrange [-0.5:59.5]; set xtics add ('60' 59); set ytics add ('60' 59); set tic scale 0
unset grid; unset colorbox
plot './tarefa-B1/B1-Annealing-L-60-Final-Config.out' matrix w image notitle

unset xtics; unset ytics
set output './tarefa-B2/B2-Quenching-L-60-Final-Config.png' 
set autoscale
#set title 'Têmpera, configuração final, L = 60'
set cbrange [0:1]; set palette defined (0 'black', 1 'white')
set xrange [-0.5:59.5]; set yrange [-0.5:59.5]; set xtics add ('60' 59); set ytics add ('60' 59); set tic scale 0
unset grid; unset colorbox
plot './tarefa-B2/B2-Quenching-L-60-Final-Config.out' matrix w image notitle
