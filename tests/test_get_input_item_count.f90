program test_get_input_item_count

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmsgroundwater
  use fixtures, only: status

  implicit none

  integer, parameter :: expected = 15
  type (bmi_prms_groundwater) :: m
  integer :: count

  status = m%get_input_item_count(count)
  
  if (count /= expected) then
     stop BMI_FAILURE
  end if
end program test_get_input_item_count
