program test_get_component_name

  use bmif_2_0, only: BMI_FAILURE, BMI_MAX_COMPONENT_NAME
  use bmiprmsgroundwater
  use fixtures, only: status

  implicit none

  character (len=BMI_MAX_COMPONENT_NAME), parameter :: &
       expected = "prms6-groundwater-BMI"
  type (bmi_prms_groundwater) :: m
  character (len=BMI_MAX_COMPONENT_NAME), pointer :: name

  status = m%get_component_name(name)

  if (name.ne.expected) then
     stop BMI_FAILURE
  end if
end program test_get_component_name
