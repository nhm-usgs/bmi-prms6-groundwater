program test_initialize

  use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
  use bmiprmsgroundwater
  use fixtures, only: status, config_file

  implicit none

  type (bmi_prms_groundwater) :: m
  integer :: check_status

  check_status = m%initialize(config_file)
  status = m%finalize()
  if (check_status /= BMI_SUCCESS) then
     stop BMI_FAILURE
  end if
end program test_initialize
