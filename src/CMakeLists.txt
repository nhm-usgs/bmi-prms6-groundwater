add_library(${bmiprms6groundwater_lib} SHARED
  bmi.f90 bmi_prms_groundwater.f90 m_prms_groundwater.f90)
target_link_libraries(${bmiprms6groundwater_lib} ${EXTRA_LIBS})

install(
  TARGETS ${bmiprms6groundwater_lib}
  EXPORT ${bmiprms6groundwater_lib}-config
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
  INCLUDES DESTINATION include)

install(
  DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/
  DESTINATION include
  FILES_MATCHING PATTERN "*.mod"
  PATTERN "CMakeFiles" EXCLUDE)

install(
  EXPORT ${bmiprms6groundwater_lib}-config
  DESTINATION lib/cmake)
export(
  TARGETS ${bmiprms6groundwater_lib}
  FILE ${bmiprms6groundwater_lib}-config.cmake)
