program test_get_grid_node_count

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmsgroundwater
  use fixtures, only: config_file, status

  implicit none

  integer, parameter :: grid_id = 0
  integer, parameter :: expected = 14
  type (bmi_prms_groundwater) :: m
  integer :: count

  status = m%initialize(config_file)
  status = m%get_grid_node_count(grid_id, count)
  status = m%finalize()
  
  if (count /= expected) then
     write(*,*) count
     stop BMI_FAILURE
  end if
end program test_get_grid_node_count
