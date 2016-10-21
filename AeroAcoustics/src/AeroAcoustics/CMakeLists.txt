
generate_f90types(Noise_Registry.txt Noise_Types.f90)

add_library(Noise  Noise.f90 Noise_IO.f90 Noise_Types.f90)

target_link_libraries(Noise nwtclibs)

