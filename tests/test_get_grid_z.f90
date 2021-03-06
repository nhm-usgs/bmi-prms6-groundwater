program test_get_grid_z

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmsgroundwater
  use fixtures, only: config_file, status

  implicit none

  integer, parameter :: grid_id = 0
  integer, parameter :: nz = 14
  integer, parameter, dimension(nz) :: expected_z = [ &
       568, 483, 563, 564, 519, 483, 543, 503, 490, 489, &
       519, 474, 476, 553 ]

  type (bmi_prms_groundwater) :: m
  integer :: grid_size
  double precision, allocatable :: grid_z(:)
  integer :: i

  status = m%initialize(config_file)
  status = m%get_grid_size(grid_id, grid_size)
  allocate(grid_z(grid_size))
  status = m%get_grid_z(grid_id, grid_z)
  status = m%finalize()

  do i = 1, nz
     if (int(grid_z(i)) /= expected_z(i)) then
        write(*,*) grid_z
        stop BMI_FAILURE
     end if
  end do

  deallocate(grid_z)
end program test_get_grid_z
