
generate_f90types(Noise_Driver_Registry.txt Noise_Driver_Types.f90 -noextrap)

add_library(NoiseStandAlonlib 
  Noise_Driver.f90
  Noise_Driver_Subs.f90
  Noise_Driver_Types.f90)
add_executable(NoiseStandAlon Noise_Driver.f90)

target_link_libraries(NoiseStandAlonlib nwtclibs Noise)

target_link_libraries(NoiseStandAlon NoiseStandAlonlib)

install(TARGETS NoiseStandAlon NoiseStandAlonlib
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)
  
