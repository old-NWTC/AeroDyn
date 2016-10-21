#!/bin/bash

clear

echo "starts script"

cd /mnt/mimer/ebarlas/NoiseStandAlone/build
make
cd  /mnt/mimer/ebarlas/NoiseStandAlone/build/Noise_Driver
./NoiseStandAlon          /mnt/mimer/ebarlas/NoiseStandAlone/DriverFileNRELOffshrBsline5MW_Onshore_Test03.dvr
