#!/bin/bash

# Script que reemplaza algunos caracteres

# El primer argumento es el archivo que existe y el segundo argumento
# es un archivo a ser creado. 
# Debe tener sed instalado. 


sed "s/�/@'a/g" $1 > $2
sed "s/�/@'e/g" $2 > $1
sed "s/�/@'i/g" $1 > $2
sed "s/�/@'o/g" $2 > $1
sed "s/�/@'u/g" $1 > $2
sed "s/�/@~n/g" $2 > $1
sed "s/@�a/@'a/g" $1> $2
sed "s/@�e/@'e/g" $2 > $1
sed "s/@�i/@'i/g" $1 > $2
sed "s/@�o/@'o/g" $2 > $1
sed "s/@�u/@'u/g" $1 > $2
sed "s/@'i/@'{@dotless{i}}/g" $2 > $1
sed "s/@'{i}/@'{@dotless{i}}/g" $1 > $2 
cat $2 > $1

echo 'Listo mi Pez !!!'


