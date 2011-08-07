#!/bin/sh

if [ ! -e ./a ]
then
    echo "./a not found"
    exit 0
fi

if [ ! $# -eq 1 ]
then
    echo "usage: ./run.sh (num battle)"
    exit 0
fi

a=0
x=0
y=0
z=0
while [ $a -lt $1 ]
do
    tmp=`./a | cut -d " " --output-delimiter=" " -f 3` 
    p0=`echo $tmp | cut -d " " -f 1`
    p1=`echo $tmp | cut -d " " -f 2`
    
    if [ $p0 -gt $p1 ]
    then x=`expr $x + 1`
    elif [ $p0 -lt $p1 ]
    then y=`expr $y + 1`
    else z=`expr $z + 1`
    fi
    a=`expr $a + 1`
done

echo "player0 wins: "$x
echo "player1 wins: "$y
echo "draw :        "$z