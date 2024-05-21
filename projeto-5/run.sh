#!/bin/bash

T="$(date +%s)"

pwd=$(pwd)

## Compilar
gfortran -O4 tarefa-A/tarefa-A-12694668.f90 -o tarefa-A/tarefa-A-12694668.exe
gfortran -O4 tarefa-B/tarefa-B1/tarefa-B1-12694668.f90 -o tarefa-B/tarefa-B1/tarefa-B1-12694668.exe
gfortran -O4 tarefa-B/tarefa-B2/tarefa-B2-12694668.f90 -o tarefa-B/tarefa-B2/tarefa-B2-12694668.exe
gfortran -O4 tarefa-C/tarefa-C1/tarefa-C1-12694668.f90 -o tarefa-C/tarefa-C1/tarefa-C1-12694668.exe
gfortran -O4 tarefa-C/tarefa-C2/tarefa-C2-12694668.f90 -o tarefa-C/tarefa-C2/tarefa-C2-12694668.exe
gfortran -O4 tarefa-D/tarefa-D-12694668.f90 -o tarefa-D/tarefa-D-12694668.exe


## Executar um de cada vez
cd $pwd/tarefa-A/ && ./tarefa-A-12694668.exe
cd $pwd/tarefa-B/tarefa-B1/ && ./tarefa-B1-12694668.exe
cd $pwd/tarefa-B/tarefa-B2/ && ./tarefa-B2-12694668.exe
cd $pwd/tarefa-C/tarefa-C1/ && ./tarefa-C1-12694668.exe
cd $pwd/tarefa-C/tarefa-C2/ && ./tarefa-C2-12694668.exe
cd $pwd/tarefa-D/ && ./tarefa-D-12694668.exe

T="$(($(date +%s)-T))"
printf "O projeto demorou %02d Horas %02d Minutos e %02d Segundos para rodar.\n" $((T/3600)) $((T%3600/60)) $((T%60))  

## criar imagems completas
cd $pwd/tarefa-A/ && gnuplot tarefa-A-plot.plt
cd $pwd/tarefa-B/ && gnuplot tarefa-B-plot.plt
cd $pwd/tarefa-C/tarefa-C1/
xmgrace -hardcopy -printfile C1-L-60-Histerese.ps -graph 0 C1-L-60-E-Beta-1E3.dat C1-L-60-E-Beta-1E4.dat -param p0.par -graph 1 C1-L-60-E-Beta-1E3.dat C1-L-60-E-Beta-1E4.dat -param p1.par
xmgrace -hardcopy -printfile C1-L-80-Histerese.ps -graph 0 C1-L-80-E-Beta-1E3.dat C1-L-80-E-Beta-1E4.dat -param p0.par -graph 1 C1-L-80-E-Beta-1E3.dat C1-L-80-E-Beta-1E4.dat -param p1.par
xmgrace -hardcopy -printfile C1-L-100-Histerese.ps -graph 0 C1-L-100-E-Beta-1E3.dat C1-L-100-E-Beta-1E4.dat -param p0.par -graph 1 C1-L-100-E-Beta-1E3.dat C1-L-100-E-Beta-1E4.dat -param p1.par

## criar imagens parciais
cd $pwd/tarefa-C/tarefa-C2/
cat C2-L-60-Ct.dat C2-L-60-Ct-Extra.dat > C2-L-60-Ct-Final.dat
cat C2-L-80-Ct.dat C2-L-80-Ct-Extra.dat > C2-L-80-Ct-Final.dat
cat C2-L-100-Ct.dat C2-L-100-Ct-Extra.dat > C2-L-100-Ct-Final.dat
xmgrace *Final.dat -param pCt.par -saveall C2-L-All-Ct.dat &

xmgrace -hardcopy -printfile C2-L-60-E.ps -nxy C2-L-60-E.dat -maxpath 200000 -param pE.par
xmgrace -hardcopy -printfile C2-L-80-E.ps -nxy C2-L-80-E.dat -maxpath 200000 -param pE.par
xmgrace -hardcopy -printfile C2-L-100-E.ps -nxy C2-L-100-E.dat -maxpath 200000 -param pE.par 

cd $pwd/tarefa-D/
xmgrace -graph 0 -param p0.par -block D-Tempo.dat -bxy 1:2 -graph 1 -param p1.par -block D-Tempo.dat -bxy 1:3 -saveall D-Tempo.dat &
xmgrace -hardcopy -printfile D-Magnetization.ps D-Magnetization.dat -maxpath 500000 -param pM.par &
