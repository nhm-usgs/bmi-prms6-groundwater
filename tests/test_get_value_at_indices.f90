program test_get_value_at_indices

    use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
    use bmiprmsgroundwater
    use fixtures, only: config_file, status, print_1darray, isReal4EqualReal4, &
        isReal8EqualReal8, print_i_1darray, print_array, isintEqualint, print_d_1darray

    implicit none

    type (bmi_prms_groundwater) :: m
    integer :: retcode

    !test r32 gwres_flow
    retcode = test1()
    if (retcode.ne.BMI_SUCCESS) then
        stop BMI_FAILURE
    end if

    !r64 by nhru gwres_stor_ante.
    retcode = test2()
    if (retcode.ne.BMI_SUCCESS) then
        stop BMI_FAILURE
    end if
        
    contains

! Test getting r32 hru_type.
function test1() result(status)
    character (len=*), parameter :: &
         var_name = "gwres_flow"
    integer, parameter :: rank = 1
    integer, parameter :: size = 7
    integer, parameter, dimension(rank) :: shape = (/ 7 /)
    integer, parameter, dimension(7) :: &
       indices = (/ 2, 4, 6, 8, 10, 12, 14 /)
    real, parameter, dimension(shape(1)) :: &
         expected = (/ 6.2567892E-04, &
            4.6067876E-03, 6.0999682E-03, &
            6.7496800E-04, 2.0469553E-03, &
            6.5202604E-04, 4.4758637E-03 /)
    real :: tval(size)
    integer :: i, status
    
    status = m%initialize(config_file)
    status = m%update()
    status = m%get_value_at_indices(var_name, tval, indices)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 1"
        
    write(*,*) "get Value"
    call print_1darray(tval, shape)
        
    write(*,*) "Expected"
    call print_1darray(expected, shape)

    status = BMI_SUCCESS
    do i = 1, size
       if (tval(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
  end function test1
  
! Test getting r64 by nhru
    function test2() result(status)
    character (len=*), parameter :: &
         var_name = "gwres_stor_ante"
    integer, parameter :: rank =1
    integer, parameter :: size = 7
    integer, parameter, dimension(rank) :: shape = (/ 7 /)
    integer, parameter, dimension(size) :: &
       indices = (/ 2, 4, 6, 8, 10, 12, 14 /)
    double precision, parameter, dimension(shape(1)) :: &
         expected = (/ 1.370599959045649e-002, &
            0.113049998879433, 0.162882998585701, &
            1.985199935734272e-002, 4.814099892973900e-002, &
            1.853399910032749e-002, 0.110981002449989 /)
    double precision :: val(size)
    integer :: i, status
    double precision :: endtime

    status = m%initialize(config_file)
    status = m%update()
    status = m%get_value_at_indices(var_name, val, indices)
    status = m%finalize()

    
    ! Visual inspection.
    write(*,*) "Test 2"
    
    write(*,*) "get Value"
    call print_d_1darray(val, shape)
    
    write(*,*) "Expected"
    call print_d_1darray(expected, shape)
    
    status = BMI_SUCCESS
    do i = 1, size
       if (val(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
  end function test2

end program test_get_value_at_indices
