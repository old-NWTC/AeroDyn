#!/bin/bash

clear

echo "starts script"

cd /mnt/mimer/ebarlas/NoiseStandAlone/
rm -rf build 
mkdir build
cd build
cmake ../src
make
cd  /mnt/mimer/ebarlas/NoiseStandAlone/build/Noise_Driver
./NoiseStandAlon          /mnt/mimer/ebarlas/NoiseStandAlone/DriverFileNRELOffshrBsline5MW_Onshore_Test03.dvr
