There are 5 folders:
doc -> Relevant documentation
inputs -> For each experimental case, two files are included:
	History: Time, angle of attack, chord and wind speed
	Polar: Angle of attack, Cn, Ct (chord direction), Cm
exp -> For each experimental case, a file with the experimental measurements is included: Angle of attack, Cl, Cd, Cm
results -> Output files of the computations (it should contain at least: Angle of attack, Cl (or Cn), Cd (or Ct), Cm
comparison -> Scripts to generate figures with the comparison of computations and experimental data (Matlab or gnuplot)

Name convention for the cases:
[(LE surface),(angle of attack amplitude),(oscillation frequency),(Reynolds),AM(mean angle of attack),_(Airfoil)]
LE surface
	C -> clean
	T -> Tripped
Angle of attack amplitude:
	5 -> 5º
	10 -> 10º
Oscillation frequency:
	l -> (low) 0.6Hz
	m -> (middle) 1.2Hz
	h -> (high) 1.8Hz
Reynolds:
	75 -> 750000
	100 -> 1000000
	125 -> 1250000
	150 -> 1500000
Mean angle of attack:
	8 -> 8º
	14 -> 14º
	20 -> 20º
Airfoil:
	s809 -> S809
	l417 -> LS(1)0417MOD